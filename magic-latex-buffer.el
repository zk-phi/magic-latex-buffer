;;; magic-latex-buffer.el --- magical syntax highlighting for LaTeX-mode buffers

;; Copyright (C) 2014 zk_phi

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
;; URL: http://hins11.yu-yake.com/
;; Version: 0.1.0
;; Package-Requires: ((cl-lib "0.5"))

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

;;; Code:

(require 'font-lock)
(require 'jit-lock)
(require 'tex-mode)
(require 'iimage)
(require 'cl-lib)

(defconst magic-latex-buffer-version "0.1.0")

;; + customizable vars

(defgroup magic-latex-buffer nil
  "magical syntax highlighting for LaTeX-mode buffers"
  :group 'emacs)

(defcustom magic-latex-ignored-properties
  '(font-lock-comment-face
    font-lock-comment-delimiter-face
    font-lock-constant-face
    tex-verbatim)
  "list of faces which magic-latex should ignore"
  :group 'emacs)

;; + vars, consts

(defconst ml/syntax-table
  (let ((st (copy-syntax-table tex-mode-syntax-table)))
    (modify-syntax-entry ?$ "\"" st)
    st)
  "like `tex-mode-syntax-table' but treat $ as a string quote for
correct inline-math recognition.")

(defvar-local ml/jit-point nil
  "store the point while font-locking")

(defvar-local ml/buffer-fancy-p nil
  "whether this latex buffer is fancy")

;; + faces

(make-face 'ml/title)
(set-face-attribute 'ml/title nil
                    :inherit font-lock-function-name-face
                    :height 2.0)

(make-face 'ml/chapter)
(set-face-attribute 'ml/chapter nil
                    :inherit font-lock-function-name-face
                    :height 1.8)

(make-face 'ml/section)
(set-face-attribute 'ml/section nil
                    :inherit font-lock-function-name-face
                    :height 1.6)

(make-face 'ml/subsection)
(set-face-attribute 'ml/subsection nil
                    :inherit font-lock-function-name-face
                    :height 1.2)

(make-face 'ml/box)
(set-face-attribute 'ml/box nil :box t)
(make-face 'ml/overline)
(set-face-attribute 'ml/overline nil :overline t)
(make-face 'ml/type)
(set-face-attribute 'ml/type nil :inherit 'fixed-pitch)

(make-face 'ml/black)
(set-face-attribute 'ml/black nil :foreground "black")
(make-face 'ml/white)
(set-face-attribute 'ml/white nil :foreground "white")
(make-face 'ml/red)
(set-face-attribute 'ml/red nil :foreground "red")
(make-face 'ml/green)
(set-face-attribute 'ml/green nil :foreground "green")
(make-face 'ml/blue)
(set-face-attribute 'ml/blue nil :foreground "blue")
(make-face 'ml/cyan)
(set-face-attribute 'ml/cyan nil :foreground "cyan")
(make-face 'ml/magenta)
(set-face-attribute 'ml/magenta nil :foreground "magenta")
(make-face 'ml/yellow)
(set-face-attribute 'ml/yellow nil :foreground "yellow")

(make-face 'ml/tiny)
(set-face-attribute 'ml/tiny nil :height 0.7)
(make-face 'ml/script)
(set-face-attribute 'ml/script nil :height 0.8)
(make-face 'ml/footnote)
(set-face-attribute 'ml/footnote nil :height 0.8)
(make-face 'ml/small)
(set-face-attribute 'ml/small nil :height 0.9)
(make-face 'ml/large)
(set-face-attribute 'ml/large nil :height 1.2)
(make-face 'ml/llarge)
(set-face-attribute 'ml/llarge nil :height 1.44)
(make-face 'ml/xlarge)
(set-face-attribute 'ml/xlarge nil :height 1.72)
(make-face 'ml/huge)
(set-face-attribute 'ml/huge nil :height 2.07)
(make-face 'ml/hhuge)
(set-face-attribute 'ml/hhuge nil :height 2.49)

;; + utilities

(defmacro ml/safe-excursion (&rest body)
  "like `progn' but moves the point only when BODY succeeded with
no errors."
  `(let ((pos (point)))
     (condition-case err (progn ,@body)
       (error (goto-char pos) (error (error-message-string err))))))

(defun ml/regexp-opt (strings)
  "like regexp-opt but for LaTeX command names."
  (concat "\\\\" (regexp-opt strings) "\\>"))

(defun ml/skip-comments-and-verbs (&optional backward)
  "skip this comment or verbish environment"
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
  "like search-regexp but aware of escaped chars, comments and
verbish environments. raises errors on failure. when POINT-SAFE
is non-nil, the point must not be in the matching string."
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
                (looking-back "\\([^\\\\]\\|^\\)\\(\\\\\\\\\\)*")
                (not (ml/skip-comments-and-verbs backward)))))
       (ml/search-regexp regex bound backward point-safe))))

(defun ml/skip-blocks (n &optional exclusive backward brace-only)
  "skip blocks until the point reaches n-level upwards.
examples:
 [n=1]
   inclusive (a b (|c d (e f) g) h) -> (a b (c d (e f) g)| h)
   exclusive (a b (|c d (e f) g) h) -> (a b (c d (e f) g|) h)
 [n=0]
   inclusive (a b (|c d (e f) g) h) -> (a b (c d (e f)| g) h)
   exclusive (a b (|c d (e f) g) h) -> (a b (c d (e f|) g) h)"
  (ml/safe-excursion
   (save-match-data
     (if (not (ml/search-regexp
               (if brace-only
                   "\\({\\)\\|\\(}\\)"
                 "\\(\\\\begin\\>\\|{\\|\\[\\)\\|\\(\\\\end\\>\\|}\\|]\\)")
               nil backward))
         (error "unmatched blocks")
       (setq n (if backward
                   (+ n
                      (if (match-beginning 1) -1 0)
                      (if (match-beginning 2) 1 0))
                 (+ n
                    (if (match-beginning 1) 1 0)
                    (if (match-beginning 2) -1 0)))))
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
  "read something like \"[OPT]{ARG0}...{ARGn}}\" and
set (match-string k) to ARGk. this function does not moves the
point."
  (while (and option (looking-at " *\\["))
    (ml/skip-blocks 0))
  (when args
    (let (res)
      (dotimes (_ args)
        (unless (looking-at " *{")
          (error "too few arguments"))
        (push (match-end 0) res)
        (ml/skip-blocks 0 nil nil "\\({\\)\\|\\(}\\)")
        (push (1- (point)) res))
      (setq res (reverse res))
      (set-match-data res)
      res)))

;; + keyword highlighting via font-lock

(defun ml/command-matcher (name &optional option args point-safe)
  "generate a forward search command that matches something like
\"\\NAME[OPT]{ARG1}...{ARGn}\" and moves the cursor just after
the NAME. (match-string 0) will be NAME and (match-string k) will
be ARGk if succeeded."
  `(lambda (&optional limit)
     (ignore-errors
       (ml/search-command ,name ,option ,args ,point-safe limit))))

(defun ml/search-command (regex &optional option args point-safe limit)
  "an internal function for `ml/command-matcher'"
  (ml/safe-excursion
   (ml/search-regexp regex limit nil point-safe)
   (let ((beg (match-beginning 0))
         (end (match-end 0)))
     (condition-case nil
         (save-excursion
           (let ((res (cons beg
                            (cons end
                                  (save-excursion
                                    (ml/read-args option args))))))
             (set-match-data res)
             res))
       (error (ml/search-command regex option args point-safe limit))))))

(defconst ml/keywords-1
  (let ((headings
         (ml/command-matcher
          (ml/regexp-opt
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
          (ml/regexp-opt
           '("newcounter" "newcounter*" "setcounter" "addtocounter"
             "setlength" "addtolength" "settowidth")) nil 1))
        (includes
         (ml/command-matcher
          (ml/regexp-opt
           '("input" "include" "includeonly" "bibliography"
             "epsfig" "psfig" "epsf" "nofiles" "usepackage"
             "documentstyle" "documentclass" "verbatiminput"
             "includegraphics" "includegraphics*")) t 1))
        (verbish
         (ml/command-matcher
          (ml/regexp-opt '("url" "nolinkurl" "path")) t 1))
        (definitions                    ; i have no idea what this is
          "^[ \t]*\\\\def *\\\\\\(\\(\\w\\|@\\)+\\)"))
    `((,headings 1 font-lock-function-name-face keep)
      (,variables 1 font-lock-variable-name-face)
      (,includes 1 font-lock-constant-face)
      (,verbish 1 'tex-verbatim)
      (,definitions 1 font-lock-function-name-face)))
  "highlighting keywords based on `tex-font-lock-keywords-1'")

(defconst ml/keywords-2
  (let ((bold
         (ml/command-matcher
          (ml/regexp-opt
           '("textbf" "textsc" "textup" "boldsymbol" "pmb" "bm")) nil 1))
        (italic
         (ml/command-matcher
          (ml/regexp-opt '("textit" "textsl" "emph")) nil 1))
        (citations
         (ml/command-matcher
          (ml/regexp-opt
           '("label" "ref" "pageref" "vref" "eqref" "cite" "Cite"
             "nocite" "index" "glossary" "bibitem" "citep" "citet")) t 1))
        (quotes
         (concat (regexp-opt `("``" "\"<" "\"`" "<<" "«") t)
                 "[^'\">{]+" (regexp-opt `("''" "\">" "\"'" ">>" "»") t)))
        (specials-1
         (ml/command-matcher "\\\\\\(?:\\\\\\*?\\)" nil nil))
        (specials-2
         (ml/command-matcher
          (ml/regexp-opt
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
  "highlighting keywords based on `tex-font-lock-keywords-2'")

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
          (ml/regexp-opt '("texttt" "textmd" "textrm" "textsf")) nil 1))
        (box
         (ml/command-matcher
          (ml/regexp-opt
           '("ovalbox" "Ovalbox" "fbox" "doublebox" "shadowbox")) nil 1)))
    `((,title 1 'ml/title t)
      (,chapter 1 'ml/chapter t)
      (,section 1 'ml/section t)
      (,subsection 1 'ml/subsection t)
      (,diminish 0 'shadow t)
      (,underline 1 'underline t)
      (,overline 1 'ml/overline t)
      (,color 2 (let ((str (match-string 1)))
                  (cond ((string= str "black") 'ml/black)
                        ((string= str "white") 'ml/white)
                        ((string= str "red") 'ml/red)
                        ((string= str "green") 'ml/green)
                        ((string= str "blue") 'ml/blue)
                        ((string= str "cyan") 'ml/cyan)
                        ((string= str "magenta") 'ml/magenta)
                        ((string= str "yellow") 'ml/yellow))) t)
      (,type 1 'ml/type t)
      (,box 1 'ml/box t)))
  "extra highlighting keywords")

(defconst ml/keywords
  (append ml/keywords-1 ml/keywords-2 ml/keywords-3))

;; + block highlighting

(defun ml/block-matcher (name &optional option args point-safe)
  "generate a forward search command that matches something like
\"\\begin{env} \\NAME[OPT]{ARG1}...{ARGn} ... BODY
... \\end{env}\" and moves the cursor just after the
NAME. (match-string 0) will be NAME, (match-string 1) will be
BODY, and (match-string (1+ k)) will be ARGk if succeeded."
  `(lambda (&optional limit)
     (ignore-errors
       (ml/search-block ,name ,option ,args ,point-safe limit))))

(defun ml/search-block (regex &optional option args point-safe limit)
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
  "an alist of (MATCHER . FACE). MATCHER is a function that takes
an argument, limit of the search, and does a forward search like
`search-forward-regexp' then sets match-data properly. FACE is *a
sexp* which is evaluated to a face. (match-string 1) will be
propertized with the face.")

(defun ml/make-block-overlay (com-from com-to hlt-from hlt-to &rest props)
  (let* ((ov1 (make-overlay com-from com-to))
         (ov2 (make-overlay hlt-from hlt-to))
         (hooks (list (lambda (ov goahead from to &optional len)
                        (when goahead
                          (move-overlay ov
                                        (min from (overlay-start ov))
                                        (max to (overlay-end ov))))))))
    (overlay-put ov1 'category 'ml/ov-block)
    (overlay-put ov1 'partner ov2)
    (overlay-put ov2 'insert-in-front-hooks hooks)
    (overlay-put ov2 'insert-behind-hooks hooks)
    (while props
      (overlay-put ov2 (car props) (cadr props))
      (setq props (cddr props)))
    ov2))

(defun ml/remove-block-overlays (beg end)
  (dolist (ov (overlays-in beg end))
    (when (eq (overlay-get ov 'category) 'ml/ov-block)
      (delete-overlay (overlay-get ov 'partner))
      (delete-overlay ov))))

(defun ml/jit-block-highlighter (beg end)
  (condition-case nil
      (progn (ml/skip-blocks 1 nil t) (point))
    (error (goto-char 1)))
  (ml/remove-block-overlays (point) end)
  (dolist (command ml/block-commands)
    (save-excursion
      (let ((regexp (car command)))
        (while (funcall regexp end)
          (ml/make-block-overlay (match-beginning 0) (match-end 0)
                                 (match-beginning 1) (match-end 1)
                                 'face (eval (cdr command))))))))

;; + pretty symbol/suscript

(defconst ml/decoration-commands
  '(("\\\\\\(?:text\\(?:md\\|rm\\|sf\\|tt\\)\\)\\>"
     . (propertize "T" 'face 'ml/type))
    ("\\\\\\(?:emph\\|text\\(?:it\\|sl\\)\\)\\>"
     . (propertize "I" 'face 'italic))
    ("\\\\\\(?:b\\(?:m\\|oldsymbol\\)\\|pmb\\|text\\(?:bf\\|sc\\|up\\)\\)\\>"
     . (propertize "B" 'face 'bold))
    ("\\\\underline\\>"
     . (propertize "U" 'face 'underline))
    ("\\\\overline\\>"
     . (propertize "O" 'face 'ml/overline))))

(defconst ml/relation-symbols
  '(
    ;; basic
    ("\\\\eq\\>" . "＝") ("\\\\equiv\\>" . "≡")
    ("\\\\approx\\>" . "≒") ("\\\\cong\\>" . "≅")
    ("\\\\le\\>" . "≦") ("\\\\ge\\>" . "≧")
    ("\\\\to\\>" . "→") ("\\\\mapsto\\>" . "↦")
    ("\\\\propto\\>" . "∝")
    ;; set
    ("\\\\subseteq\\>" . "⊆") ("\\\\subset\\>" . "⊂")
    ("\\\\supseteq\\>" . "⊇") ("\\\\supset\\>" . "⊃")
    ("\\\\in\\>" . "∈") ("\\\\ni\\>" . "∋")
    ("\\\\sqsubseteq\\>" . "⊑") ("\\\\sqsupseteq\\>" . "⊒")
    ;; logic
    ("\\\\models\\>" . "⊧") ("\\\\vdash\\>" . "⊢") ("\\\\dashv\\>" . "⊣")
    ("\\\\rightarrow\\>" . "→") ("\\\\leftarrow\\>" . "←")
    ("\\\\leftrightarrow\\>" . "↔") ("\\\\Leftarrow\\>" . "⇐")
    ("\\\\Rightarrow\\>" . "⇒") ("\\\\Leftrightarrow\\>" . "⇔")
    ;; geometry
    ("\\\\parallel\\>" . "∥") ("\\\\perp\\>" . "⊥")
    ))

(defconst ml/negrel-symbols
  '(("\\\\neq\\>" . (compose-chars ?／ ?＝))
    ("\\\\notin\\>" . (compose-chars ?／ ?∈))
    ("\\\\notni\\>" . (compose-chars ?／ ?∋))))

(defconst ml/operator-symbols
  '(
    ;; set
    ("\\\\mid\\>" . "｜") ("\\\\emptyset\\>" . "∅") ("\\\\\\(?:set\\)?minus\\>" . "＼")
    ("\\\\\\(?:big\\)?cup\\>" . "∪") ("\\\\\\(?:big\\)cap\\>" . "∩")
    ("\\\\sqcup\\>" . "⊔") ("\\\\sqcap\\>" . "⊓")
    ;; logic
    ("\\\\exists\\>" . "∃") ("\\\\forall\\>" . "∀") ("\\\\\\(neg\\|lnot\\)\\>" . "￢")
    ("\\\\land\\>" . "∧") ("\\\\lor\\>" . "∨")
    ;; algebra
    ("\\\\times\\>" . "×") ("\\\\div)\\>" . "÷")
    ("\\\\\\(?:big\\)?wedge\\>" . "∧") ("\\\\\\(?:big\\)?vee\\>" . "∨")
    ("\\\\prod\\>" . "∏") ("\\\\sum\\>" . "∑")
    ("\\\\triangleleft\\>" . "◁") ("\\\\triangleright\\>" . "▷")
    ("\\\\bigtriangleup\\>" . "△") ("\\\\bigtriangledown\\>" . "▽")
    ("\\\\odot\\>" . "⊙") ("\\\\oslash\\>" . "⊘") ("\\\\otimes\\>" . "⊗")
    ("\\\\ominus\\>" . "⊖") ("\\\\oplus\\>" . "⊕") ("\\\\ast\\>" . "∗")
    ;; analysis
    ("\\\\mp\\>" . "∓") ("\\\\pm\\>" . "±")
    ("\\\\Re\\>" . "ℜ") ("\\\\Im\\>" . "ℑ") ("\\\\angle\\>" . "∠")
    ("\\\\s\\(?:urd\\|qrt\\)\\>" . "√") ("\\\\partial\\>" . "∂")
    ("\\\\int\\>" . "∫") ("\\\\iint\\>" . "∬") ("\\\\iiint\\>" . "∭")
    ("\\\\oint\\>" . "∮")
    ("\\\\varlimsup\\>" . (propertize "lim" 'face 'ml/overline))
    ("\\\\varliminf\\>" . (propertize "lim" 'face 'underline))
    ;; computers
    ("\\\\react\\>" . ":-")
    ))

(defconst ml/arrow-symbols
  '(
    ;; harpoon
    ("\\\\leftharpoonup\\>" . "↼") ("\\\\rightharpoonup\\>" . "⇀")
    ("\\\\leftharpoondown\\>" . "↽") ("\\\\rightharpoondown\\>" . "⇁")
    ("\\\\leftrightharpoons\\>" . "⇋") ("\\\\rightleftharpoons\\>" . "⇌")
    ("\\\\upharpoonleft\\>" . "↿") ("\\\\upharpoonright\\>" . "↾")
    ("\\\\downharpoonleft\\>" . "⇃") ("\\\\downharpoonright\\>" . "⇂")
    ;; vertical
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
    ;; greek
    ("\\\\Gamma\\>" . "Γ") ("\\\\Delta\\>" . "Δ") ("\\\\Theta\\>" . "Θ")
    ("\\\\Lambda\\>" . "Λ") ("\\\\Xi\\>" . "Ξ") ("\\\\Pi\\>" . "Π")
    ("\\\\Sigma\\>" . "Σ") ("\\\\Upsilon\\>" . "Υ") ("\\\\Phi\\>" . "Φ")
    ("\\\\Psi\\>" . "Ψ") ("\\\\Omega\\>" . "Ω") ("\\\\alpha\\>" . "α")
    ("\\\\beta\\>" . "β") ("\\\\gamma\\>" . "γ") ("\\\\delta\\>" . "δ")
    ("\\\\\\(?:var\\)?epsilon\\>" . "ε") ("\\\\zeta\\>" . "ζ")
    ("\\\\eta\\>" . "η") ("\\\\\\(?:var\\)?theta\\>" . "θ")
    ("\\\\iota\\>" . "ι") ("\\\\kappa\\>" . "κ") ("\\\\lambda\\>" . "λ")
    ("\\\\mu\\>" . "μ") ("\\\\nu\\>" . "ν") ("\\\\xi\\>" . "ξ")
    ("\\\\\\(?:var\\)?pi\\>" . "π") ("\\\\\\(?:var\\)?rho\\>" . "ρ")
    ("\\\\\\(?:var\\)?sigma\\>" . "σ") ("\\\\tau\\>" . "τ")
    ("\\\\upsilon\\>" . "υ") ("\\\\\\(?:var\\)?phi\\>" . "φ")
    ("\\\\chi\\>" . "χ") ("\\\\psi\\>" . "ψ") ("\\\\omega\\>" . "ω")
    ;; latin / accented
    ("\\\\ss\\>" . "ß") ("\\\\aa\\>" . "å") ("\\\\AA\\>" . "Å")
    ("\\\\ae\\>" . "æ") ("\\\\oe\\>" . "œ") ("\\\\AE\\>" . "Æ") ("\\\\OE\\>" . "Œ")
    ("\\\\o\\>" . "ø") ("\\\\O\\>" . "Ø")
    ;; math
    ("\\\\aleph\\>" . "ℵ") ("\\\\bot\\>" . "⊥") ("\\\\top\\>" . "⊤")
    ("\\\\therefore\\>" . "∴") ("\\\\because\\>" . "∵")
    ("\\\\infty\\>" . "∞") ("\\\\nabla\\>" . "∇") ("\\\\triangle\\>" . "△")
    ;; others
    ("\\\\cdot\\>" . "・") ("\\\\dots\\>" . "…") ("\\\\cdots\\>" . "⋯")
    ("\\\\vdots\\>" . "⋮") ("\\\\ddots\\>" . "⋱")
    ("\\\\\\(?:text\\)?backslash\\>" . "＼") ("\\\\circ\\>" . "ｏ")
    ("\\\\star\\>" . "⋆") ("\\\\S\\>" . "§")
    ("\\\\dagger\\>" . "†") ("\\\\ddag\\>" . "‡")
    ("\\\\copyright\\>" . "©") ("\\\\texistregistered\\?" . "®")
    ("\\\\texttrademark\\>" . "™")
    ("\\\\pounds\\>"  . "£") ("\\\\P\\>" . "¶")
    ))

(defconst ml/other-symbols
  '(
    ;; TeX commands
    ("\\\\begin\\>" . "▽") ("\\\\end\\>" . "△")
    ("\\\\\\(?:bib\\)?item\\>" . "＊") ("\\\\par\\>" . "¶")
    ("\\\\ref\\>" . "☞") ("\\\\\\(?:c\\|C\\)ite\\>" . "†")
    ("\\\\footnote\\(?:mark\\)?\\>" . "‡")
    ("\\\\left\\>" . "¡") ("\\\\right\\>" . "!")
    ("~\\|\\\\\\(?:[,;\s]\\|hspace\\>\\)" . "␣")
    ("\\\\\\(?:newline\\>\\|\\\\\\)" . "⏎")
    ("\\\\TeX\\>"
     . (compose-chars ?T '(cr cl -20 -45) ?E '(cr cl -20 24) ?X))
    ("\\\\LaTeX\\>"
     . (compose-chars ?L '(cr cl -60 35) ?A '(cr cl -18 -20)
                      ?T '(cr cl -18 -60) ?E '(cr cl -20 5) ?X))
    ;; parens
    ("\\\\{" . (compose-chars ?\{ ?⎨))
    ("\\\\}" . (compose-chars ?⎬ ?\}))
    ("\\\\|" . "║")
    ("\\\\\\(?:double\\[\\|lBrack\\)"
     . (compose-chars ?\[ '(cr cl -90 0) ?\[))
    ("\\\\\\(?:double\\]\\|rBrack\\)"
     . (compose-chars ?\] '(cr cl -90 0) ?\]))
    ("\\\\langle\\>" . "〈") ("\\\\rangle\\>" . "〉")
    ("\\\\lceil\\>" . "⌈") ("\\\\rceil\\>" . "⌉")
    ("\\\\lfloor\\>" . "⌊") ("\\\\rfloor\\>" . "⌋")
    ;; "&"
    ("&" . (compose-chars ?& ?|))))

(defconst ml/accents
  `(("\\\\\\(?:mathbb\\){\\([^}]\\)}"
     . (let ((ch (string-to-char (match-string 1)))) (compose-chars ch '(cc cl -86 0) ch)))
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
     . (compose-chars (string-to-char (match-string 1)) '(cc Bc 0 30) ?`))))

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
          ml/accents))

(defun ml/make-pretty-overlay (from to &rest props)
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
  (remove-overlays beg end 'category 'ml/ov-pretty))

(defun ml/search-suscript (point-safe limit)
  "search forward something like \"^{BODY}\" or \"_{BODY}\" and
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
  (save-excursion
    (while (ignore-errors (ml/search-suscript t end))
      (let* ((beg (match-beginning 1))
             (end (match-end 1))
             (ov1 (ml/make-pretty-overlay
                   (match-beginning 0) (match-end 0) 'invisible t))
             (ov2 (ml/make-pretty-overlay
                   beg end)))
        (cl-case (string-to-char (match-string 0))
          ((?_) (overlay-put ov2 'display '((raise -0.2) (height 0.8))))
          ((?^) (overlay-put ov2 'display '((raise 0.2) (height 0.8))))))))
  ;; prettify symbols
  (dolist (symbol ml/symbols)
    (save-excursion
      (let ((regex (car symbol)))
        (while (ignore-errors (ml/search-regexp regex end nil t))
          (let* ((ov (catch 'found
                       (dolist (ov (overlays-at (match-beginning 0)))
                         (when (eq (overlay-get ov 'category) 'ml/ov-pretty)
                           (throw 'found ov)))))
                 (oldprop (and ov (overlay-get ov 'display))))
            (unless (stringp oldprop)
              (ml/make-pretty-overlay
               (match-beginning 0) (match-end 0) 'priority 1
               'display (propertize (eval (cdr symbol)) 'display oldprop)))))))))

;; + activate

(defun magic-latex-buffer ()
  (interactive)
  (jit-lock-mode 1)
  (setq-local ml/buffer-fancy-p t)
  (setq-local font-lock-multiline t)
  (set-syntax-table ml/syntax-table)
  (font-lock-add-keywords nil ml/keywords 'set)
  (jit-lock-register 'ml/jit-prettifier)
  (jit-lock-register 'ml/jit-block-highlighter)
  ;; jit-lock highlighters assume that the region is already fontified
  ;; (so that they can recognize verbatim, constant and comment)
  (jit-lock-register 'font-lock-fontify-region)
  (set (make-local-variable 'iimage-mode-image-regex-alist)
       `((,(concat "\\\\includegraphics[\s\t]*\\(?:\\[[^]]*\\]\\)?[\s\t]*"
                   "{\\(" iimage-mode-image-filename-regex "\\)}") . 1)))
  (iimage-mode 1)
  (add-to-list 'tex-verbatim-environments "Verbatim")
  (add-to-list 'tex-verbatim-environments "lstlisting"))

(defadvice jit-lock-fontify-now (around ml/ad-jit-lock activate)
  (let ((ml/jit-point (point)))
    ad-do-it))

;; + provide

(provide 'magic-latex-buffer)

;;; magic-latex-buffer.el ends here
