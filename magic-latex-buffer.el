;;; -*- lexical-binding: t -*-
;;; magic-latex-buffer.el --- Magically enhance LaTeX-mode font-locking for semi-WYSIWYG editing

;; Copyright (C) 2014-2015 zk_phi

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

;; Author: zk_phi
;; URL: http://zk-phi.github.io/
;; Version: 0.4.0
;; Package-Requires: ((cl-lib "0.5") (emacs "25.1"))

;;; Commentary:

;; Put this script into a "load-path"ed directory, and load it in your
;; init file.
;;
;;   (require 'magic-latex-buffer)
;;
;; Then you can enable highlighting with "M-x magic-latex-buffer" in a
;; latex-mode-buffer. If you may enable highlighting automatically,
;; add to the mode hook.
;;
;;   (add-hook 'latex-mode-hook 'magic-latex-buffer)
;;
;; For more informations, see Readme.org.

;;; Change Log:
;; 0.0.0 test release
;; 0.1.0 add highlights, fix fatal bugs
;; 0.1.1 implement nested sub/super-scripts
;; 0.2.0 add option to disable some prettifiers
;; 0.3.0 add support for alignment commands
;; 0.4.0 add option `magic-latex-enable-minibuffer-echo'
;; 0.4.1 migrate to nadvice.el

;;; Code:

(require 'font-lock)
(require 'jit-lock)
(require 'tex-mode)
(require 'iimage)
(require 'cl-lib)

(defconst magic-latex-buffer-version "0.4.1")

;; + customizable vars

(defgroup magic-latex-buffer nil
  "magical syntax highlighting for LaTeX-mode buffers"
  :group 'emacs)

(defcustom magic-latex-ignored-properties
  '(font-lock-comment-face
    font-lock-comment-delimiter-face
    font-lock-constant-face
    tex-verbatim)
  "List of faces which magic-latex should ignore."
  :type '(list face)
  :group 'magic-latex-buffer)

(defcustom magic-latex-enable-block-highlight t
  "When non-nil, prettify blocks like \"{\\large ...}\"."
  :type 'boolean
  :group 'magic-latex-buffer)

(defcustom magic-latex-enable-block-align t
  "When non-nil, align blocks like \"{\\centering ...}\"."
  :type 'boolean
  :group 'magic-latex-buffer)

(defcustom magic-latex-enable-suscript t
  "When non-nil, prettify subscripts and superscripts like
\"a_1\", \"e^{-x}\"."
  :type 'boolean
  :group 'magic-latex-buffer)

(defcustom magic-latex-enable-pretty-symbols t
  "When non-nil, prettify symbols with unicode characters and
character composition."
  :type 'boolean
  :group 'magic-latex-buffer)

(defcustom magic-latex-enable-inline-image t
  "When non-nil, `iimage-mode' is enabled automatically."
  :type 'boolean
  :group 'magic-latex-buffer)

(defcustom magic-latex-enable-minibuffer-echo t
  "When non-nil, actual command is displayed in the modeline."
  :type 'boolean
  :group 'magic-latex-buffer)

;; + vars, consts

(defconst ml/syntax-table
  (let ((st (copy-syntax-table tex-mode-syntax-table)))
    (modify-syntax-entry ?$ "\"" st)
    (modify-syntax-entry ?\' "." st)
    st)
  "like `tex-mode-syntax-table' but treat $ as a string quote,
for correct inline-math recognition. Also make the quote ' be considered a delimiter (to correctly detect symbols)")

(defvar-local ml/jit-point nil
  "store the point while font-locking")
(define-advice jit-lock-fontify-now (:around (fn &rest args) ml/ad-jit-lock)
  (let ((ml/jit-point (point)))
    (apply fn args)))

;; + faces

(defface ml/title '((t (:inherit font-lock-function-name-face :height 2.0)))
  "Face used for title command in magic LaTeX buffers."
  :group 'magic-latex-buffer)
(defface ml/chapter '((t (:inherit font-lock-function-name-face :height 1.8)))
  "Face used for chapter command in magic LaTeX buffers."
  :group 'magic-latex-buffer)
(defface ml/section '((t (:inherit font-lock-function-name-face :height 1.6)))
  "Face used for section command in magic LaTeX buffers."
  :group 'magic-latex-buffer)
(defface ml/subsection '((t (:inherit font-lock-function-name-face :height 1.2)))
  "Face used for subsection command in magic LaTeX buffers."
  :group 'magic-latex-buffer)

(defface ml/box '((t (:box t)))
  "Face used for box command in magic LaTeX buffers."
  :group 'magic-latex-buffer)
(defface ml/overline '((t (:overline t)))
  "Face used for overline command in magic LaTeX buffers."
  :group 'magic-latex-buffer)
(defface ml/type '((t (:inherit 'fixed-pitch)))
  "Face used for type command in magic LaTeX buffers."
  :group 'magic-latex-buffer)

(defface ml/black '((t (:foreground "black")))
  "Face used for color{black} command in magic LaTeX buffers."
  :group 'magic-latex-buffer)
(defface ml/white '((t (:foreground "white")))
  "Face used for color{white} command in magic LaTeX buffers."
  :group 'magic-latex-buffer)
(defface ml/red '((t (:foreground "red")))
  "Face used for color{red} command in magic LaTeX buffers."
  :group 'magic-latex-buffer)
(defface ml/green '((t (:foreground "green")))
  "Face used for color{green} command in magic LaTeX buffers."
  :group 'magic-latex-buffer)
(defface ml/blue '((t (:foreground "blue")))
  "Face used for color{blue} command in magic LaTeX buffers."
  :group 'magic-latex-buffer)
(defface ml/cyan '((t (:foreground "cyan")))
  "Face used for color{cyan} command in magic LaTeX buffers."
  :group 'magic-latex-buffer)
(defface ml/magenta '((t (:foreground "magenta")))
  "Face used for color{magenta} command in magic LaTeX buffers."
  :group 'magic-latex-buffer)
(defface ml/yellow '((t (:foreground "yellow")))
  "Face used for color{yellow} command in magic LaTeX buffers."
  :group 'magic-latex-buffer)

(defface ml/tiny '((t (:height 0.7)))
  "Face used for tiny command in magic LaTeX buffers."
  :group 'magic-latex-buffer)
(defface ml/script '((t (:height 0.8)))
  "Face used for script command in magic LaTeX buffers."
  :group 'magic-latex-buffer)
(defface ml/footnote '((t (:height 0.8)))
  "Face used for footnote command in magic LaTeX buffers."
  :group 'magic-latex-buffer)
(defface ml/small '((t (:height 0.9)))
  "Face used for small command in magic LaTeX buffers."
  :group 'magic-latex-buffer)
(defface ml/large '((t (:height 1.2)))
  "Face used for large command in magic LaTeX buffers."
  :group 'magic-latex-buffer)
(defface ml/llarge '((t (:height 1.44)))
  "Face used for Large command in magic LaTeX buffers."
  :group 'magic-latex-buffer)
(defface ml/xlarge '((t (:height 1.72)))
  "Face used for LARGE command in magic LaTeX buffers."
  :group 'magic-latex-buffer)
(defface ml/huge '((t (:height 2.07)))
  "Face used for huge command in magic LaTeX buffers."
  :group 'magic-latex-buffer)
(defface ml/hhuge '((t (:height 2.49)))
  "Face used for Huge command in magic LaTeX buffers."
  :group 'magic-latex-buffer)

(defface ml/lbar '((t (:background "red")))
  "Face used to render vertical line for quote environments in
  magic LaTeX buffers."
  :group 'magic-latex-buffer)

;; + utilities
;;   + general

(defmacro ml/safe-excursion (&rest body)
  "Like `progn' but moves the point only when BODY succeeded."
  `(let ((pos (point)))
     (condition-case err (progn ,@body)
       (error (goto-char pos) (error (error-message-string err))))))

(defun ml/command-regexp-opt (strings)
  "Like `regexp-opt' but for LaTeX command names."
  (concat "\\\\" (regexp-opt strings) "\\>"))

(defun ml/overlay-at (point prop val)
  "Return an overlay at point, whose property PROP is VAL. If
some overlays satisfy the condition, overlay with the highest
priority is returned. If there's no such overlays, return nil."
  (cl-some (lambda (ov) (when (equal (overlay-get ov prop) val) ov))
           (sort (overlays-at point)
                 (lambda (a b) (let ((pa (overlay-get a 'priority))
                                     (pb (overlay-get b 'priority)))
                                 (or (null pb) (and pa (>= pa pb))))))))

(defun ml/column-at-eol ()
  (save-excursion (end-of-line) (current-column)))

(defun ml/column-at-indentation ()
  (save-excursion (back-to-indentation) (current-column)))

;;   + LaTeX-specific

(defun ml/skip-comments-and-verbs (&optional backward)
  "Skip forward this comment or verbish environment. Return
non-nil iff the cursor is moved."
  (when (and (not (eobp))             ; return non-nil only when moved
             (memq (get-text-property (point) 'face)
                   magic-latex-ignored-properties)
             (memq (get-text-property (1- (point)) 'face)
                   magic-latex-ignored-properties))
    (let ((pos (if backward
                   (previous-single-property-change (point) 'face)
                 (next-single-property-change (point) 'face))))
      (goto-char (or pos (if backward (point-min) (point-max))))
      (when pos (ml/skip-comments-and-verbs backward))
      t)))

(defun ml/search-regexp (regex &optional bound backward point-safe)
  "Like `search-regexp' but skips escaped chars, comments and
verbish environments. This function raise an error on
failure. When POINT-SAFE is non-nil, the point must not be in the
matching string."
  (ml/safe-excursion
   (let ((case-fold-search nil))
     (if backward
         (search-backward-regexp regex bound)
       (search-forward-regexp regex bound)))
   (or (save-match-data
         (save-excursion
           (and (goto-char (match-beginning 0))
                (not (and point-safe
                          (< (point) ml/jit-point)
                          (< ml/jit-point (match-end 0))))
                (looking-back "\\([^\\\\]\\|^\\)\\(\\\\\\\\\\)*" (point-min))
                (not (ml/skip-comments-and-verbs backward)))))
       (ml/search-regexp regex bound backward point-safe))))

(defun ml/skip-blocks (n &optional exclusive backward brace-only)
  "Skip blocks forward until the point reaches n-level upwards.
examples:
 [n=1]
   inclusive (a b (|c d (e f) g) h) -> (a b (c d (e f) g)| h)
   exclusive (a b (|c d (e f) g) h) -> (a b (c d (e f) g|) h)
 [n=0]
   inclusive (a b (|c d (e f) g) h) -> (a b (c d (e f)| g) h)
   exclusive (a b (|c d (e f) g) h) -> (a b (c d (e f|) g) h)"
  (ml/safe-excursion
   (save-match-data
     (condition-case nil
         (ml/search-regexp
          (if brace-only
              "\\({\\)\\|\\(}\\)"
            "\\(\\\\begin\\>\\|{\\|\\[\\)\\|\\(\\\\end\\>\\|}\\|]\\)")
          nil backward)
       (error (error "unmatched blocks")))
     (setq n (if backward
                 (+ n
                    (if (match-beginning 1) -1 0)
                    (if (match-beginning 2) 1 0))
               (+ n
                  (if (match-beginning 1) 1 0)
                  (if (match-beginning 2) -1 0))))
     (cond ((< n 0)
            (error "unexpected end-of-block"))
           ((> n 0)
            (ml/skip-blocks n exclusive backward brace-only))
           (exclusive
            (if backward
                (goto-char (match-end 0))
              (goto-char (match-beginning 0))))
           (t
            t)))))

(defun ml/read-args (&optional option args)
  "Look forward something like \"[OPT]{ARG0}...{ARGn}}\" and
set (match-string k) to K-th ARG. this function does not moves
the point."
  (while (and option (looking-at " *\\["))
    (ml/skip-blocks 0))
  (when args
    (let (res)
      (dotimes (_ args)
        (unless (looking-at " *{")
          (error "too few arguments"))
        (push (match-end 0) res)
        (ml/skip-blocks 0 nil nil t)
        (push (1- (point)) res))
      (setq res (reverse res))
      (set-match-data res)
      res)))

;; + basic keyword highlighting via font-lock

(defun ml/command-matcher (regex &optional option args point-safe)
  "Generate a forward search function that matches something like
\"REGEX[OPT]{ARG1}...{ARGn}\" and moves the cursor just after the
NAME. (match-string 0) will be NAME and (match-string k) will be
K-th ARG if succeeded."
  `(lambda (&optional limit)
     (ignore-errors
       (ml/search-command ,regex ,option ,args ,point-safe limit))))

(defun ml/search-command (regex &optional option args point-safe limit)
  "(Internal function for `ml/command-matcher')"
  (ml/safe-excursion
   (ml/search-regexp regex limit nil point-safe)
   (let ((beg (match-beginning 0))
         (end (match-end 0)))
     (condition-case nil
         (let* ((args (save-excursion (ml/read-args option args)))
                (res (cons beg (cons end args))))
           (set-match-data res)
           res)
       (error (ml/search-command regex option args point-safe limit))))))

(defconst ml/keywords-1
  (let ((headings
         (ml/command-matcher
          (ml/command-regexp-opt
           '("title"  "begin" "end" "chapter" "part" "section" "subsection"
             "subsubsection" "paragraph" "subparagraph" "subsubparagraph"
             "newcommand" "renewcommand" "providecommand" "newenvironment"
             "renewenvironment" "newtheorem" "renewtheorem"
             "title*"  "begin*" "end*" "chapter*" "part*" "section*" "subsection*"
             "subsubsection*" "paragraph*" "subparagraph*" "subsubparagraph*"
             "newcommand*" "renewcommand*" "providecommand*" "newenvironment*"
             "renewenvironment*" "newtheorem*" "renewtheorem*")) t 1))
        (variables
         (ml/command-matcher
          (ml/command-regexp-opt
           '("newcounter" "newcounter*" "setcounter" "addtocounter"
             "setlength" "addtolength" "settowidth")) nil 1))
        (includes
         (ml/command-matcher
          (ml/command-regexp-opt
           '("input" "include" "includeonly" "bibliography"
             "epsfig" "psfig" "epsf" "nofiles" "usepackage"
             "documentstyle" "documentclass" "verbatiminput"
             "includegraphics" "includegraphics*")) t 1))
        (verbish
         (ml/command-matcher
          (ml/command-regexp-opt '("url" "nolinkurl" "path")) t 1))
        (definitions                    ; i have no idea what this is
          "^[ \t]*\\\\def *\\\\\\(\\(\\w\\|@\\)+\\)"))
    `((,headings 1 font-lock-function-name-face keep)
      (,variables 1 font-lock-variable-name-face)
      (,includes 1 font-lock-constant-face)
      (,verbish 1 'tex-verbatim)
      (,definitions 1 font-lock-function-name-face)))
  "Highlighting keywords based on `tex-font-lock-keywords-1'.")

(defconst ml/keywords-2
  (let ((bold
         (ml/command-matcher
          (ml/command-regexp-opt
           '("textbf" "mathbf" "textsc" "textup" "boldsymbol" "pmb" "bm")) nil 1))
        (italic
         (ml/command-matcher
          (ml/command-regexp-opt '("textit" "mathit" "textsl" "emph")) nil 1))
        (citations
         (ml/command-matcher
          (ml/command-regexp-opt
           '("label" "ref" "pageref" "vref" "eqref" "cite" "Cite"
             "nocite" "index" "glossary" "bibitem" "citep" "citet")) t 1))
        (quotes
         (concat (regexp-opt `("``" "\"<" "\"`" "<<" "«") t)
                 "[^'\">{]+" (regexp-opt `("''" "\">" "\"'" ">>" "»") t)))
        (specials-1
         (ml/command-matcher "\\\\\\(?:\\\\\\*?\\)" nil nil))
        (specials-2
         (ml/command-matcher
          (ml/command-regexp-opt
           '("linebreak" "nolinebreak" "pagebreak" "nopagebreak"
             "newline" "newpage" "clearpage" "cleardoublepage"
             "displaybreak" "allowdisplaybreaks" "enlargethispage")) nil nil))
        (other-commands
         (ml/command-matcher "\\\\\\(?:[a-zA-Z@]+\\**\\|[^ \t\n]\\)")))
    `((,bold 1 'bold append)
      (,italic 1 'italic append)
      (,citations 1 font-lock-constant-face)
      (,quotes . font-lock-string-face)
      (,specials-1 . font-lock-warning-face)
      (,specials-2 . font-lock-warning-face)
      (,other-commands . font-lock-keyword-face)))
  "Highlighting keywords based on `tex-font-lock-keywords-2'.")

(defconst ml/keywords-3
  (let ((title (ml/command-matcher "\\\\title\\>\\*?" nil 1))
        (chapter (ml/command-matcher "\\\\chapter\\>\\*?" t 1))
        (section (ml/command-matcher "\\\\section\\>\\*?" t 1))
        (subsection (ml/command-matcher "\\\\subsection\\>\\*?" t 1))
        (diminish "{}\\|&")
        (underline (ml/command-matcher "\\\\underline\\>" nil 1))
        (overline (ml/command-matcher "\\\\overline\\>" nil 1))
        (color (ml/command-matcher "\\\\textcolor\\>" nil 2))
        (type
         (ml/command-matcher
          (ml/command-regexp-opt
           '("texttt" "mathtt" "textmd" "textrm" "mathrm" "textsf" "mathsf")) nil 1))
        (box
         (ml/command-matcher
          (ml/command-regexp-opt
           '("ovalbox" "Ovalbox" "fbox" "doublebox" "shadowbox")) nil 1)))
    `((,title 1 'ml/title append)
      (,chapter 1 'ml/chapter append)
      (,section 1 'ml/section append)
      (,subsection 1 'ml/subsection append)
      (,diminish 0 'shadow append)
      (,underline 1 'underline append)
      (,overline 1 'ml/overline append)
      (,color 2 (let ((str (match-string 1)))
                  (cond ((string= str "black") 'ml/black)
                        ((string= str "white") 'ml/white)
                        ((string= str "red") 'ml/red)
                        ((string= str "green") 'ml/green)
                        ((string= str "blue") 'ml/blue)
                        ((string= str "cyan") 'ml/cyan)
                        ((string= str "magenta") 'ml/magenta)
                        ((string= str "yellow") 'ml/yellow))) append)
      (,type 1 'ml/type append)
      (,box 1 'ml/box append)))
  "Extra highlighting keywords.")

(defconst ml/keywords
  (append ml/keywords-1 ml/keywords-2 ml/keywords-3)
  "Font lock keywords for `latex-mode' buffers.")

;; + block highlighter

(defun ml/block-matcher (regex &optional option args point-safe)
  "Generate a forward search function that matches something like
\"REGEX[OPT]{ARG1}...{ARGn} ... BODY ... \\end{env}\" and moves
the cursor just after the NAME. (match-string 0) will be
NAME, (match-string 1) will be BODY, and (match-string (1+ k))
will be K-th ARG if succeeded."
  `(lambda (&optional limit)
     (ignore-errors
       (ml/search-block ,regex ,option ,args ,point-safe limit))))

(defun ml/search-block (regex &optional option args point-safe limit)
  "(Internal function for `ml/block-matcher')"
  (ml/safe-excursion
   (ml/search-regexp regex limit nil point-safe)
   (let ((command-beg (match-beginning 0))
         (command-end (match-end 0)))
     (condition-case nil
         (save-excursion
           (let* ((res (ml/read-args option args))
                  (content-beg (point))
                  (content-end (condition-case nil
                                   (progn (ml/skip-blocks 1 t) (point))
                                 (error (1+ (buffer-size))))))
             (setq res (cons command-beg
                             (cons command-end
                                   (cons content-beg
                                         (cons content-end res)))))
             (set-match-data res)
             res))
       (error (ml/search-block regex option args point-safe limit))))))

(defconst ml/block-commands
  (let ((tiny (ml/block-matcher "\\\\tiny\\>" nil nil))
        (script (ml/block-matcher "\\\\scriptsize\\>" nil nil))
        (footnote (ml/block-matcher "\\\\footnotesize\\>" nil nil))
        (small (ml/block-matcher "\\\\small\\>" nil nil))
        (large (ml/block-matcher "\\\\large\\>" nil nil))
        (llarge (ml/block-matcher "\\\\Large\\>" nil nil))
        (xlarge (ml/block-matcher "\\\\LARGE\\>" nil nil))
        (huge (ml/block-matcher "\\\\huge\\>" nil nil))
        (hhuge (ml/block-matcher "\\\\Huge\\>" nil nil))
        (type (ml/block-matcher "\\\\tt\\>" nil nil))
        (italic (ml/block-matcher "\\\\\\(?:em\\|it\\|sl\\)\\>" nil nil))
        (bold (ml/block-matcher "\\\\bf\\(?:series\\)?\\>" nil nil))
        (color (ml/block-matcher "\\\\color" nil 1)))
    `((,tiny . 'ml/tiny)
      (,script . 'ml/script)
      (,footnote . 'ml/footnote)
      (,small . 'ml/small)
      (,large . 'ml/large)
      (,llarge . 'ml/llarge)
      (,xlarge . 'ml/xlarge)
      (,huge . 'ml/huge)
      (,hhuge . 'ml/hhuge)
      (,type . 'ml/type)
      (,italic . 'italic)
      (,bold . 'bold)
      (,color . (let ((col (match-string 2)))
                  (cond ((string= col "black") 'ml/black)
                        ((string= col "white") 'ml/white)
                        ((string= col "red") 'ml/red)
                        ((string= col "green") 'ml/green)
                        ((string= col "blue") 'ml/blue)
                        ((string= col "cyan") 'ml/cyan)
                        ((string= col "magenta") 'ml/magenta)
                        ((string= col "yellow") 'ml/yellow))))))
  "An alist of (MATCHER . FACE). MATCHER is a function that takes
an argument, limit of the search, and does a forward search like
`search-forward-regexp' then sets match-data as needed. FACE is
*a sexp* which evaluates to a face. (match-string 1) will be
propertized with the face.")

(defun ml/make-block-overlay (command-beg command-end content-beg content-end &rest props)
  "Make a pair of overlays, a content overlay and a command
overlay. The command overlay will have `partner' property, that
points the content overlay which the command is associated
with. The content overlay will have PROPS as its properties."
  (let* ((ov1 (make-overlay command-beg command-end))
         (ov2 (make-overlay content-beg content-end)))
    (overlay-put ov1 'category 'ml/ov-block)
    (overlay-put ov1 'partner ov2)
    (while props
      (overlay-put ov2 (car props) (cadr props))
      (setq props (cddr props)))
    ov2))

(defun ml/remove-block-overlays (beg end)
  "Remove all command overlays and their content overlays from
BEG END."
  (dolist (ov (overlays-in beg end))
    (when (eq (overlay-get ov 'category) 'ml/ov-block)
      (delete-overlay (overlay-get ov 'partner))
      (delete-overlay ov))))

(defun ml/jit-block-highlighter (_ end)
  (when magic-latex-enable-block-highlight
    (condition-case nil
        (progn (ml/skip-blocks 1 nil t) (point))
      (error (goto-char 1)))
    (ml/remove-block-overlays (point) end)
    (dolist (command ml/block-commands)
      (save-excursion
        (while (funcall (car command) end)
          (ml/make-block-overlay (match-beginning 0) (match-end 0)
                                 (match-beginning 1) (match-end 1)
                                 'face (eval (cdr command))))))))

;; + block aligner

(defconst ml/align-commands
  `((,(ml/block-matcher "\\\\\\(?:centering\\>\\|begin{center}\\)" nil nil) . center)
    (,(ml/block-matcher "\\\\\\(?:raggedleft\\>\\|begin{flushleft}\\)" nil nil) . left)
    (,(ml/block-matcher "\\\\\\(?:raggedright\\>\\|begin{flushright}\\)" nil nil) . right)
    (,(ml/block-matcher "\\\\begin{\\(?:quot\\(?:e\\|ation\\)\\|leftbar\\)}" nil nil) . quote))
  "An alist of (MATCHER . POSITION). MATCHER is a function that
takes an argument, limit of the search, and does a forward search
like `search-forward-regexp' then sets match-data as
needed. POSITION can be one of 'center 'right 'left 'quote.")

(defun ml/make-align-overlay (command-beg command-end content-beg content-end position)
  "Make a command overlay and alignment overlay(s) like
`ml/make-block-overlay'. The command overlay will have `partners'
property, which is bound to the list of all alignment
overlay(s)."
  (save-excursion
    (goto-char content-end)
    (end-of-line 0)
    (setq content-end (point))
    (when (< content-beg content-end)
      (remove-overlays content-beg content-end 'category 'ml/ov-align-alignment)
      (let ((ov (make-overlay command-beg command-end))
            ovs)
        (goto-char content-beg)
        (forward-line 1)
        (while (<= (point-at-eol) content-end)
          (cond ((ignore-errors
                   (ml/search-regexp "{\\|\\\\begin\\_>" (min content-end (point-at-eol))))
                 (let* ((block-end (condition-case nil
                                       (save-excursion (ml/skip-blocks 1) (point))
                                     (error (point-max))))
                        (bols (list (point-at-bol)))
                        (width (ml/column-at-eol))
                        (indentation (ml/column-at-indentation)))
                   (while (progn
                            (forward-line 1)
                            (< (point) block-end))
                     (push (point) bols)
                     (setq width       (max width (ml/column-at-eol))
                           indentation (min indentation (ml/column-at-indentation))))
                   (setq ovs (nconc (mapcar
                                     (lambda (p)
                                       (ml/make-align-overlay-1 p width indentation position))
                                     bols)
                                    ovs))))
                (t
                 (push (ml/make-align-overlay-1
                        (point) (ml/column-at-eol) (ml/column-at-indentation) position)
                       ovs)
                 (forward-line 1))))
        (overlay-put ov 'category 'ml/ov-align)
        (overlay-put ov 'partners ovs)))))

(defun ml/make-align-overlay-1 (pos width indentation position)
  "*internal function for `ml/make-align-overlay'*"
  (let ((prop (cl-case position
                ((left) `((space :align-to left)))
                ((center) `((space :align-to (- center ,(/ width 2) ,(/ indentation 2)))))
                ((right) `((space :align-to (- right ,width))))
                ((quote) (propertize " " 'face 'ml/lbar))))
        (ov (make-overlay pos pos)))
    (overlay-put ov 'before-string (propertize " " 'display prop))
    (overlay-put ov 'category 'ml/ov-align-alignment)
    ov))

(defun ml/remove-align-overlays (beg end)
  "Remove all command overlays and their alignment overlays
between BEG and END."
  (dolist (ov (overlays-in beg end))
    (when (eq (overlay-get ov 'category) 'ml/ov-align)
      (mapc 'delete-overlay (overlay-get ov 'partners))
      (delete-overlay ov))))

(defun ml/jit-block-aligner (_ end)
  (when magic-latex-enable-block-align
    (condition-case nil
        (progn (ml/skip-blocks 1 nil t) (point))
      (error (goto-char 1)))
    (ml/remove-align-overlays (point) end)
    (dolist (command ml/align-commands)
      (save-excursion
        (while (funcall (car command) end)
          (ml/make-align-overlay (match-beginning 0) (match-end 0)
                                 (match-beginning 1) (match-end 1)
                                 (cdr command)))))))

;; + pretty symbol/suscript

(defconst ml/decoration-commands
  '(("\\\\\\(\\(?:\\(?:text\\|math\\)\\(?:md\\|rm\\|sf\\|tt\\)\\)\\|verb\\)\\>"
     . (propertize "T" 'face 'ml/type))
    ("\\\\\\(?:emph\\|\\(?:text\\|math\\)\\(?:it\\|sl\\)\\)\\>"
     . (propertize "I" 'face 'italic))
    ("\\\\\\(?:b\\(?:m\\|oldsymbol\\)\\|pmb\\|text\\(?:bf\\|sc\\|up\\)\\)\\>"
     . (propertize "B" 'face 'bold))
    ("\\\\underline\\>"
     . (propertize "U" 'face 'underline))
    ("\\\\overline\\>"
     . (propertize "O" 'face 'ml/overline))
    ("\\\\mbox\\>" . "'")
    ("\\\\mathcal\\>" . "ℭ")
    ))

(defconst ml/relation-symbols
  '(
    ;; basic
    ("\\\\eq\\>" . "＝") ("\\\\doteq\\>" . "≐") ("\\\\equiv\\>" . "≡")
    ("\\\\sim\\>" . "～") ("\\\\simeq\\>" . "≃") ("\\\\cong\\>" . "≅")
    ("\\\\succ\\>" . "≻") ("\\\\prec\\>" . "≺")
    ("\\\\succeq\\>" . "≽") ("\\\\preceq\\>" . "≼")
    ("\\\\approx\\>" . "≒")
    ("\\\\le\\(?:q\\)?\\>" . "≦") ("\\\\ge\\(?:q\\)?\\>" . "≧")
    ("\\\\lesssim\\>" . "≲") ("\\\\gtrsim\\>" . "≳")
    ("\\\\ll\\>" . "≪") ("\\\\gg\\>" . "≫")
    ("\\\\to\\>" . "→") ("\\\\mapsto\\>" . "↦")
    ("\\\\colon\\>" . ":")
    ("\\\\propto\\>" . "∝")

    ;; set
    ("\\\\subseteq\\>" . "⊆") ("\\\\subset\\>" . "⊂")
    ("\\\\supseteq\\>" . "⊇") ("\\\\supset\\>" . "⊃")
    ("\\\\in\\>" . "∈") ("\\\\ni\\>" . "∋")
    ("\\\\sqsubseteq\\>" . "⊑") ("\\\\sqsupseteq\\>" . "⊒")

    ;; logic
    ("\\\\models\\>" . "⊧") ("\\\\vDash\\>" . "⊨")
    ("\\\\vdash\\>" . "⊢") ("\\\\dashv\\>" . "⊣")
    ("\\\\rightarrow\\>" . "→") ("\\\\leftarrow\\>" . "←")
    ("\\\\leftrightarrow\\>" . "↔") ("\\\\Leftarrow\\>" . "⇐")
    ("\\\\Rightarrow\\>" . "⇒") ("\\\\Leftrightarrow\\>" . "⇔")
    ("\\\\implies\\>" . "⟹")

    ;; geometry
    ("\\\\parallel\\>" . "∥") ("\\\\perp\\>" . "⊥")

    ;; ???
    ("\\\\asymp\\>" . "≍") ("\\\\smile\\>" . "⌣") ("\\\\frown\\>" . "⌢")
    ("\\\\lhd//>" . "⊲") ("\\\\unlhd\\>" . "⊴")
    ("\\\\rhd\\>" . "⊳") ("\\\\unrhd\\>" . "⊵")
    ("\\\\\\(?:bowtie\\|Join\\)\\>" . "⋈")
    ))

;; Use composition so that it matches with \not command
(defconst ml/negrel-symbols
  '(("\\\\neq\\>" . (compose-chars ?／ ?＝))
    ("\\\\notin\\>" . (compose-chars ?／ ?∈))
    ("\\\\notni\\>" . (compose-chars ?／ ?∋))))

(defconst ml/operator-symbols
  '(
    ;; set
    ("\\\\mid\\>" . "｜") ("\\\\\\(?:set\\)?minus\\>" . "＼")
    ("\\\\\\(?:big\\)?cup\\>" . "∪") ("\\\\\\(?:big\\)?cap\\>" . "∩")
    ("\\\\\\(?:big\\)?sqcup\\>" . "⊔") ("\\\\\\(?:big\\)?sqcap\\>" . "⊓")
    ("\\\\\\(?:big\\)?uplus\\>" . "⨄") ("\\\\amalg\\>" . "⨿")

    ;; logic
    ("\\\\exists\\>" . "∃") ("\\\\forall\\>" . "∀") ("\\\\\\(?:neg\\|lnot\\)\\>" . "￢")
    ("\\\\land\\>" . "∧") ("\\\\lor\\>" . "∨")

    ;; algebra
    ("\\\\cdot\\>" . "・") ("\\\\times\\>" . "×") ("\\\\div)\\>" . "÷")
    ("\\\\\\(?:big\\)?wedge\\>" . "∧") ("\\\\\\(?:big\\)?vee\\>" . "∨")
    ("\\\\prod\\>" . "∏") ("\\\\coprod\\>" . "∐") ("\\\\sum\\>" . "∑")

    ("\\\\\\(?:big\\)?odot\\>" . "⊙") ("\\\\oslash\\>" . "⊘")
    ("\\\\\\(?:big\\)?otimes\\>" . "⊗") ("\\\\\\(?:big\\)?oplus\\>" . "⊕")
    ("\\\\ominus\\>" . "⊖") ("\\\\ast\\>" . "∗")

    ;; analysis
    ("\\\\mp\\>" . "∓") ("\\\\pm\\>" . "±")
    ("\\\\Re\\>" . "ℜ") ("\\\\Im\\>" . "ℑ") ("\\\\angle\\>" . "∠")
    ("\\\\s\\(?:urd\\|qrt\\)\\>" . "√") ("\\\\partial\\>" . "∂")
    ("\\\\int\\>" . "∫") ("\\\\iint\\>" . "∬")
    ("\\\\iiint\\>" . "∭") ("\\\\iiiint\\>" . "⨌")
    ("\\\\oint\\>" . "∮") ("\\\\oiint\\>" . "∯") ("\\\\oiiint\\>" . "∰")
    ("\\\\ointclockwise\\>" . "∲") ("\\\\ointctrclockwise\\>" . "∳")
    ("\\\\varlimsup\\>" . (propertize "lim" 'face 'ml/overline))
    ("\\\\varliminf\\>" . (propertize "lim" 'face 'underline))

    ;; ???
    ("\\\\wp\\>" . "≀")
    ("\\\\bullet\\>" . "●")
    ("\\\\circ\\>" . "ｏ") ("\\\\bigcirc\\>" . "○")
    ("\\\\diamond\\>" . "♢") ("\\\\Diamond\\>" . "◇")
    ("\\\\star\\>" . "★") ("\\\\triangle\\>" . "△")
    ("\\\\triangleleft\\>" . "◁") ("\\\\triangleright\\>" . "▷")
    ("\\\\trianglelefteq\\>" . "⊴") ("\\\\trianglerighteq\\>" . "⊵")
    ("\\\\bigtriangleup\\>" . "△") ("\\\\bigtriangledown\\>" . "▽")
    ))

(defconst ml/arrow-symbols
  '(
    ;; harpoon
    ("\\\\leftharpoonup\\>" . "↼") ("\\\\rightharpoonup\\>" . "⇀")
    ("\\\\leftharpoondown\\>" . "↽") ("\\\\rightharpoondown\\>" . "⇁")
    ("\\\\leftrightharpoons\\>" . "⇋") ("\\\\rightleftharpoons\\>" . "⇌")
    ("\\\\upharpoonleft\\>" . "↿") ("\\\\upharpoonright\\>" . "↾")
    ("\\\\downharpoonleft\\>" . "⇃") ("\\\\downharpoonright\\>" . "⇂")

    ;; long
    ("\\\\longrightarrow\\>" . "⟶") ("\\\\longleftarrow\\>" . "⟵")
    ("\\\\Longrightarrow\\>" . "⟹") ("\\\\Longleftarrow\\>" . "⟸")
    ("\\\\longmapsto\\>" . "⟼")
    ("\\\\longleftrightarrow\\>" . "⟷") ("\\\\Longleftrightarrow\\>" . "⟺")

    ;; 45deg
    ("\\\\nearrow\\>" . "↗") ("\\\\searrow\\>" . "↘")
    ("\\\\nwarrow\\>" . "↖") ("\\\\swarrow\\>" . "↙")

    ;; 90deg
    ("\\\\uparrow\\>" . "↑") ("\\\\downarrow\\>" . "↓") ("\\\\updownarrow\\>" . "↕")
    ("\\\\upuparrows\\>" . "⇈") ("\\\\downdownarrows\\>" . "⇊")
    ("\\\\Uparrow\\>" . "⇑") ("\\\\Downarrow\\>" . "⇓") ("\\\\Updownarrow\\>" . "⇕")

    ;; others
    ("\\\\hookleftarrow\\>" . "↩") ("\\\\hookrightarrow\\>" . "↪")
    ("\\\\twoheadleftarrow\\>" . "↞") ("\\\\twoheadrightarrow\\>" . "↠")
    ("\\\\looparrowleft\\>" . "↫") ("\\\\looparrowright\\>" . "↬")
    ("\\\\rightsquigarrow\\>" . "⇝") ("\\\\leftrightsquigarrow\\>" . "↭")
    ("\\\\leftleftarrows\\>" . "⇇") ("\\\\rightrightarrows\\>" . "⇉")
    ("\\\\leftrightarrows\\>" . "⇆") ("\\\\rightleftarrows\\>" . "⇄")
    ("\\\\Lleftarrow\\>" . "⇚") ("\\\\Rrightarrow\\>" . "⇛")
    ))

(defconst ml/letter-symbols
  '(
    ;; greek (upper)
    ("\\\\Alpha\\>" .  "Α") ("\\\\Beta\\>" .  "Β")
    ("\\\\Gamma\\>" . "Γ") ("\\\\Delta\\>" . "Δ")
    ("\\\\Epsilon\\>" . "Ε") ("\\\\Zeta\\>" . "Ζ")
    ("\\\\Eta\\>" . "Η") ("\\\\Theta\\>" . "Θ")
    ("\\\\Iota\\>" . "Ι") ("\\\\Kappa\\>" . "Κ")
    ("\\\\Lambda\\>" . "Λ") ("\\\\Mu\\>" . "Μ")
    ("\\\\Nu\\>" . "Ν") ("\\\\Xi\\>" . "Ξ")
    ("\\\\Omicron\\>" . "Ο") ("\\\\Pi\\>" . "Π")
    ("\\\\Rho\\>" . "Ρ") ("\\\\Sigma\\>" . "Σ")
    ("\\\\Tau\\>" . "Τ") ("\\\\Upsilon\\>" . "Υ")
    ("\\\\Phi\\>" . "Φ") ("\\\\Chi\\>" . "Χ")
    ("\\\\Psi\\>" . "Ψ") ("\\\\Omega\\>" . "Ω")

    ;; greek (lower)
    ("\\\\alpha\\>" . "α") ("\\\\beta\\>" . "β")
    ("\\\\gamma\\>" . "γ") ("\\\\delta\\>" . "δ")
    ("\\\\\\(?:var\\)?epsilon\\>" . "ε") ("\\\\zeta\\>" . "ζ")
    ("\\\\eta\\>" . "η") ("\\\\\\(?:var\\)?theta\\>" . "θ")
    ("\\\\iota\\>" . "ι") ("\\\\kappa\\>" . "κ")
    ("\\\\lambda\\>" . "λ") ("\\\\mu\\>" . "μ")
    ("\\\\nu\\>" . "ν") ("\\\\xi\\>" . "ξ")
    ("\\\\omicron\\>" . "ο") ("\\\\\\(?:var\\)?pi\\>" . "π")
    ("\\\\\\(?:var\\)?rho\\>" . "ρ") ("\\\\\\(?:var\\)?sigma\\>" . "σ")
    ("\\\\tau\\>" . "τ") ("\\\\upsilon\\>" . "υ")
    ("\\\\\\(?:var\\)?phi\\>" . "φ") ("\\\\chi\\>" . "χ")
    ("\\\\psi\\>" . "ψ") ("\\\\omega\\>" . "ω")

    ;; latin / accented
    ("\\\\ss\\>" . "ß") ("\\\\aa\\>" . "å") ("\\\\AA\\>" . "Å")
    ("\\\\ae\\>" . "æ") ("\\\\oe\\>" . "œ") ("\\\\AE\\>" . "Æ") ("\\\\OE\\>" . "Œ")
    ("\\\\o\\>" . "ø") ("\\\\O\\>" . "Ø")

    ;; others
    ("\\\\aleph\\>" . "ℵ")
    ("\\\\hbar\\>" . "ℏ") ("\\\\ell\\>" . "ℓ") ("\\\\wp\\>" . "℘")
    ("\\\\l\\>" . "ł") ("\\\\L\\>" . "Ł") ("\\\\S\\>" . "§")
    ))

(defconst ml/other-symbols
  '(
    ;; TeX commands
    ("\\\\begin\\>" . "▽") ("\\\\end\\>" . "△")
    ;; ("\\\\begin\\>" . "◸") ("\\\\end\\>" . "◺")
    ("\\\\\\(?:bib\\)?item\\>" . "＊") ("\\\\par\\>" . "¶")
    ("\\\\ref\\>" . "☞") ("\\\\\\(?:c\\|C\\)ite\\>" . "†")
    ("\\\\footnote\\(?:mark\\)?\\>" . "‡")
    ("\\\\left\\>" . "¡") ("\\\\right\\>" . "!")
    ("~\\|\\\\\\(?:[,;\s]\\|\\(?:hs\\(?:pace\\|kip\\)\\|q?quad\\)\\>\\)" . "␣")
    ("\\\\\\(?:newline\\>\\|\\\\\\)" . "⏎")
    ("\\\\multicolumn\\>" . "|↔|")
    ("\\\\TeX\\>" . (compose-chars ?T '(cr cl -20 -45) ?E '(cr cl -20 24) ?X))
    ("\\\\LaTeX\\>" . (compose-chars ?L '(cr cl -60 35) ?A '(cr cl -18 -20)
                                     ?T '(cr cl -18 -60) ?E '(cr cl -20 5) ?X))

    ;; escaped symbols
    ("\\\\\\$" . "＄") ("\\\\%" . "％") ("\\\\#" . "＃") ("\\\\_" . "＿")
    ;; alignment character (&)
    ("&" . (compose-chars ?& ?|))

    ;; parens
    ("\\\\\\(?:{\\|lbrace\\>\\)" . "⎨")
    ("\\\\\\(?:}\\|rbrace\\>\\)" . "⎬")
    ("\\\\|" . "║")
    ("\\\\lbrack\\>" . "[") ("\\\\rbrack\\>" . "]")
    ("\\\\\\(?:double\\[\\|lBrack\\>\\)"
     . (compose-chars ?\[ '(cr cl -90 0) ?\[))
    ("\\\\\\(?:double\\]\\|rBrack\\>\\)"
     . (compose-chars ?\] '(cr cl -90 0) ?\]))
    ("\\\\lceil\\>" . "⌈") ("\\\\rceil\\>" . "⌉")
    ("\\\\lgroup\\>" . "〔") ("\\\\rgroup" . "〕")
    ("\\\\langle\\>" . "〈") ("\\\\rangle\\>" . "〉")
    ("\\\\lfloor\\>" . "⌊") ("\\\\rfloor\\>" . "⌋")

    ;; math
    ("\\\\emptyset\\>" . "∅") ("\\\\bot\\>" . "⊥") ("\\\\top\\>" . "⊤")
    ("\\\\therefore\\>" . "∴") ("\\\\because\\>" . "∵")
    ("\\\\infty\\>" . "∞") ("\\\\nabla\\>" . "∇")

    ;; music (?)
    ("\\\\flat\\>" . "♭") ("\\\\natural\\>" . "♮") ("\\\\sharp\\>" . "＃")

    ;; others
    ("\\\\dots\\>" . "…") ("\\\\cdots\\>" . "⋯")
    ("\\\\vdots\\>" . "⋮") ("\\\\ddots\\>" . "⋱")
    ("\\\\\\(?:text\\)?backslash\\>" . "＼")
    ("\\\\\\(?:qed\\|Box\\)\\>" . "□") ("\\\\lightning\\>" . "Ϟ")
    ("\\\\dag\\(?:ger\\)?\\>" . "†") ("\\\\ddag\\(?:ger\\)?\\>" . "‡")
    ("\\\\copyright\\>" . "©") ("\\\\texistregistered\\?" . "®")
    ("\\\\texttrademark\\>" . "™")
    ("\\\\clubsuit\\>" . "♣") ("\\\\diamondsuit\\>" . "♦")
    ("\\\\heartsuit\\>" . "♥") ("\\\\spadesuit\\>" . "♠")
    ("\\\\pounds\\>"  . "£") ("\\\\P\\>" . "¶")
    ("\\\\lvert\\>"  . "|") ("\\\\rvert\\>" . "|")
    ("\\\\lVert\\>"  . "ǁ") ("\\\\rVert\\>" . "ǁ")
    ))

(defconst ml/accents
  `(("\\\\\\(?:mathbb\\){\\([^}]\\)}"
     ;; . (let ((ch (string-to-char (match-string 1)))) (compose-chars ch '(cc cl -85 0) ch))
     . (let ((ch (string-to-char (match-string 1)))) (compose-chars ch '(cc cl -96 0) ch))
     )
    ("\\\\\\(?:vec\\){\\([^}]\\)}"
     . (compose-chars (string-to-char (match-string 1)) '(cc Bc 0 60) ?→))
    ("\\\\\\(?:tilde\\|~\\){\\([^}]\\)}"
     . (compose-chars (string-to-char (match-string 1)) '(cc Bc 0 60) ?~))
    ("\\\\\\(?:bar\\|=\\){\\([^}]\\)}"
     . (compose-chars (string-to-char (match-string 1)) '(cc Bc 0 60) ?-))
    ("\\\\\\(?:dot\\|\\.\\){\\([^}]\\)}"
     . (compose-chars (string-to-char (match-string 1)) '(cc Bc 0 60) ?・))
    ("\\\\\\(?:hat\\|\\^\\){\\([^}]\\)}"
     . (compose-chars (string-to-char (match-string 1)) '(cc Bc 0 45) ?^))
    ("\\\\\\(?:acute\\|'\\){\\([^}]\\)}"
     . (compose-chars (string-to-char (match-string 1)) '(cc Bc 0 45) ?'))
    ("\\\\\\(?:\"\\|H\\|ddot\\){\\([^}]\\)}"
     . (compose-chars (string-to-char (match-string 1)) '(cc Bc 0 45) ?\"))
    ("\\\\\\(?:grave\\|`\\){\\([^}]\\)}"
     . (compose-chars (string-to-char (match-string 1)) '(cc Bc 0 30) ?`))
    ("\\\\\\(?:check\\|`\\){\\([^}]\\)}"
     . (compose-chars (string-to-char (match-string 1)) '(cc Bc 0 60) ?v))
    ("\\\\\\(?:breve\\|`\\){\\([^}]\\)}"
     . (compose-chars (string-to-char (match-string 1)) '(cc Bc 0 60) ?⌣))
    ("\\\\r{\\([^}]\\)}"
     . (compose-chars (string-to-char (match-string 1)) '(cc Bc 0 60) ?o))
    ))

(defconst ml/symbols
  (append (mapcar (lambda (pattern)
                    (cons (concat "\\\\not[ \t\n]*" (car pattern))
                          (compose-string (concat "／" (cdr pattern)))))
                  (append ml/relation-symbols ml/arrow-symbols))
          ml/decoration-commands
          ml/relation-symbols
          ml/negrel-symbols
          ml/operator-symbols
          ml/arrow-symbols
          ml/letter-symbols
          ml/other-symbols
          ml/accents)
  "An alist of (REGEXP . EXPR). REGEXP is a regular expression
that matches to a command that will be prettified, and EXPR is *a
sexp* which evaluates to a display string for the command. You
can assume that the expressions are evaluated immediately after
regex search, so that you can use match data in the
expressions.")

(defun ml/make-pretty-overlay (from to &rest props)
  "Make an overlay from FROM to TO, that has PROPS as its
properties. The overlay is removed as soon as the text between
FROM and TO is modified."
  (let* ((ov (make-overlay from to))
         (hooks (list `(lambda (&rest _) (delete-overlay ,ov)))))
    (overlay-put ov 'category 'ml/ov-pretty)
    (overlay-put ov 'modification-hooks hooks)
    (overlay-put ov 'insert-in-front-hooks hooks)
    (while props
      (overlay-put ov (car props) (cadr props))
      (setq props (cddr props)))
    ov))

(defun ml/remove-pretty-overlays (beg end)
  "Remove all overlays created with `ml/make-pretty-overlay'
between BEG and END."
  (remove-overlays beg end 'category 'ml/ov-pretty))

(defun ml/search-suscript (point-safe limit)
  "Search forward something like \"^{BODY}\" or \"_{BODY}\" and
set (match-string 0) to \"_\" or \"^\", (match-string 1) to
\"{BODY}\". when POINT-SAFE is non-nil, the point must not be in
the command name."
  (ml/safe-excursion
   (ml/search-regexp
    ;; 1( _^ delims ) 2(  character   |   \  (       command-name          )  | 3( { )  )
    "\\([_^][ \t]*\\)\\([^ \t\n\\{}]\\|\\\\\\(?:[a-zA-Z@]+\\**\\|[^ \t\n]\\)\\|\\({\\)\\)"
    limit nil point-safe)
   (let ((delim-beg (match-beginning 1))
         (delim-end (match-end 1))
         (body-beg (match-beginning 2))
         (body-end (match-end 2))
         (brace-beg (match-beginning 3)))
     (condition-case nil
         (let* ((brace-end (when brace-beg
                             (ml/skip-blocks 1 nil nil t)
                             (point)))
                (res (list delim-beg delim-end body-beg
                           (or brace-end body-end))))
           (goto-char delim-end)
           (set-match-data res)
           res)
       (error (ml/search-suscript point-safe limit))))))

(defun ml/jit-prettifier (beg end)
  (goto-char beg)
  (ml/remove-pretty-overlays beg end)
  ;; prettify suscripts
  (when magic-latex-enable-suscript
    (save-excursion
      (while (ignore-errors (ml/search-suscript t end))
        (let* ((body-beg (match-beginning 1))
               (body-end (match-end 1))
               (delim-beg (match-beginning 0))
               (delim-end (match-end 0))
               ;; the point can be already prettified in a recursive
               ;; suscript like "a_{b_c}".
               (oldov (ml/overlay-at body-beg 'category 'ml/ov-pretty))
               (oldprop (and oldov (overlay-get oldov 'display)))
               (priority-base (and oldov (or (overlay-get oldov 'priority) 0)))
               (raise-base (or (cadr (assoc 'raise oldprop)) 0.0))
               (height-base (or (cadr (assoc 'height oldprop)) 1.0))
               (_ (ml/make-pretty-overlay delim-beg delim-end 'invisible t))
               ;; new overlay must have higher priority than the old
               ;; one.
               (ov (ml/make-pretty-overlay
                    body-beg body-end 'priority (when oldov (1+ priority-base)))))
          (cl-case (string-to-char (match-string 0))
            ((?_) (overlay-put
                   ov 'display
                   `((raise ,(- raise-base 0.2)) (height ,(* height-base 0.8)))))
            ((?^) (overlay-put
                   ov 'display
                   `((raise ,(+ raise-base 0.2)) (height ,(* height-base 0.8))))))))))
  ;; prettify symbols
  (when magic-latex-enable-pretty-symbols
    (dolist (symbol ml/symbols)
      (save-excursion
        (let ((regex (car symbol)))
          (while (ignore-errors (ml/search-regexp regex end nil t))
            (let* ((oldov (ml/overlay-at (match-beginning 0) 'category 'ml/ov-pretty))
                   (priority-base (and oldov (or (overlay-get oldov 'priority) 1)))
                   (oldprop (and oldov (overlay-get oldov 'display))))
              (unless (stringp oldprop)
                (ml/make-pretty-overlay
                 (match-beginning 0) (match-end 0)
                 'priority (when oldov (1+ priority-base))
                 'display (propertize (eval (cdr symbol)) 'display oldprop))))))))))

;; + activate

(defun ml/post-command-function ()
  (let ((ov (ml/overlay-at (point) 'category 'ml/ov-pretty))
        (message-log-max nil)) ; do not to insert to *Messages* buffer
    (when (and ov magic-latex-enable-minibuffer-echo)
      (message (buffer-substring-no-properties (overlay-start ov) (overlay-end ov))))))

;;;###autoload
(define-minor-mode magic-latex-buffer
  "Minor mode that highlights latex document magically."
  :init-value nil
  :global nil
  :lighter " mLaTeX"
  (if magic-latex-buffer
      (progn
        (add-hook 'post-command-hook 'ml/post-command-function nil t)
        (jit-lock-mode 1)
        (setq-local font-lock-multiline t)
        (set-syntax-table ml/syntax-table)
        (font-lock-add-keywords nil ml/keywords 'set)
        (jit-lock-register 'ml/jit-prettifier)
        (jit-lock-register 'ml/jit-block-highlighter)
        (jit-lock-register 'ml/jit-block-aligner)
        ;; our prettifiers assume that the region is already fontified
        ;; (to recognize verbatim environments, constants and
        ;; comments), thus we need to push `font-lock-fontify-region'
        ;; before our prettifiers.
        (jit-lock-register 'font-lock-fontify-region)
        (set (make-local-variable 'iimage-mode-image-regex-alist)
             `((,(concat "\\\\includegraphics[\s\t]*\\(?:\\[[^]]*\\]\\)?[\s\t]*"
                         "{\\(" iimage-mode-image-filename-regex "\\)}") . 1)))
        (when magic-latex-enable-inline-image (iimage-mode 1)))
    (remove-hook 'post-command-hook 'ml/post-command-function t)
    (set-syntax-table tex-mode-syntax-table)
    (jit-lock-unregister 'ml/jit-prettifier)
    (jit-lock-unregister 'ml/jit-block-highlighter)
    (jit-lock-unregister 'ml/jit-block-aligner)
    (jit-lock-unregister 'font-lock-fontify-region)
    (ml/remove-block-overlays (point-min) (point-max))
    (ml/remove-pretty-overlays (point-min) (point-max))
    (ml/remove-align-overlays (point-min) (point-max))
    (font-lock-refresh-defaults)
    (iimage-mode -1)))

;; + provide

(provide 'magic-latex-buffer)

;;; magic-latex-buffer.el ends here
