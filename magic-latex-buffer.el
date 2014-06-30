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
;; Version: 0.0.0

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

;;; Code:

(require 'font-lock)
(require 'jit-lock)
(require 'tex-mode)

;; + development notes

;; * センタリング
;;
;; (defun test ()
;;   (interactive)
;;   (save-excursion
;;     (let* ((lpoint (progn (back-to-indentation)
;;                           (point)))
;;            (lpixel (posn-x-y (posn-at-point)))
;;            (rpoint (progn (end-of-visual-line)
;;                           (point)))
;;            (rpixel (posn-x-y (posn-at-point)))
;;            (width (- (car rpixel) (car lpixel))))
;;       (let ((ov (make-overlay lpoint lpoint)))
;;         (overlay-put ov 'before-string
;;                      (propertize " " 'display
;;                                  `((space :align-to (- center (,(/ width 2)))))))))))
;;
;; - 右揃えなら (- right (,width))
;;
;; - begin~endとかのカタマリは全体でセンタリング

;; * screenとかの枠？

;; * 集合 （mathbb） (insert (compose-chars ?Q '(cc cl -86 0) ?Q))

;; * ベクトルとかアクセント記号とか たぶんcomposeうまく使えばできる

;; * point-safeをmultiple-cursorsに対応したい （カーソルごとに結果が変わる）

;; + vars, consts

(defconst ml/syntax-table
  (let ((st (copy-syntax-table tex-mode-syntax-table)))
    (modify-syntax-entry ?$ "\"" st)
    st)
  "like `tex-mode-syntax-table' but treat $ as a string quote for
correct inline-math recognition.")

(defconst magic-latex-ignored-properties
  '(font-lock-comment-face
    font-lock-comment-delimiter-face
    font-lock-constant-face
    tex-verbatim)
  "list of faces which magic-latex should ignore")

(defvar-local ml/jit-point nil
  "save the point while font-locking")

(defvar-local ml/buffer-fancy-p nil
  "whether this latex buffer is fancy")

;; + faces

(make-face 'ml/chapter)
(set-face-attribute 'ml/chapter nil
                    :inherit font-lock-function-name-face
                    :height 1.7)

(make-face 'ml/section)
(set-face-attribute 'ml/section nil
                    :inherit font-lock-function-name-face
                    :height 1.6)

(make-face 'ml/overline)
(set-face-attribute 'ml/overline nil :overline t)
(make-face 'ml/type)
(set-face-attribute 'ml/type nil :inherit 'fixed-pitch)
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

;; + font-lock-keywords

(defun ml/read-args (&optional option args)
  "look at something like \"[OPT]{ARG0}...{ARGn}}\" and
set (match-string k) to ARGk. this function does not moves the
point."
  (save-excursion
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
        res))))

(defun ml/generate-command-matcher (name &optional option args point-safe)
  "generate a forward search command that matches something like
\"\\NAME[OPT]{ARG1}...{ARGn}\" and moves the cursor just after
the NAME. (match-string 0) will be NAME and (match-string k) will
be ARGk if succeeded."
  (defun ml/search-command (regex &optional option args point-safe limit)
    (ml/safe-excursion
     (ml/search-regexp regex limit nil point-safe)
     (let ((beg (match-beginning 0))
           (end (match-end 0)))
       (condition-case nil
           (save-excursion
             (let ((res (cons beg (cons end (ml/read-args option args)))))
               (set-match-data res)
               res))
         (error (ml/search-command regex option args point-safe limit))))))
  `(lambda (&optional limit)
     (ignore-errors
       (ml/search-command ,name ,option ,args ,point-safe limit))))

(defun ml/generate-block-matcher (name &optional option args point-safe)
  "generate a forward search command that matches something like
\"\\begin{env} \\NAME[OPT]{ARG1}...{ARGn} ... BODY
... \\end{env}\" and moves the cursor just after the
NAME. (match-string 0) will be NAME, (match-string 1) will be
BODY, and (match-string (1+ k)) will be ARGk if succeeded."
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
                                   (error (1- (buffer-size))))))
               (setq res (cons command-beg
                               (cons command-end
                                     (cons content-beg
                                           (cons content-end res)))))
               (set-match-data res)
               res))
         (error (ml/search-block regex option args point-safe limit))))))
  `(lambda (&optional limit)
     (ignore-errors
       (ml/search-block ,name ,option ,args ,point-safe limit))))

;; equivalent of tex-font-lock-keywords-1
(defconst ml/font-lock-keywords-1
  (let ((headings
         (ml/generate-command-matcher
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
         (ml/generate-command-matcher
          (ml/regexp-opt
           '("newcounter" "newcounter*" "setcounter" "addtocounter"
             "setlength" "addtolength" "settowidth")) nil 1))
        (includes
         (ml/generate-command-matcher
          (ml/regexp-opt
           '("input" "include" "includeonly" "bibliography"
             "epsfig" "psfig" "epsf" "nofiles" "usepackage"
             "documentstyle" "documentclass" "verbatiminput"
             "includegraphics" "includegraphics*")) t 1))
        (verbish
         (ml/generate-command-matcher
          (ml/regexp-opt '("url" "nolinkurl" "path")) t 1))
        (definitions                    ; i have no idea what this is
          "^[ \t]*\\\\def *\\\\\\(\\(\\w\\|@\\)+\\)"))
    `((,headings 1 font-lock-function-name-face keep)
      (,variables 1 font-lock-variable-name-face)
      (,includes 1 font-lock-constant-face)
      (,verbish 1 'tex-verbatim)
      (,definitions 1 font-lock-function-name-face))))

;; equivalent of tex-font-lock-keywords-2
(defconst ml/font-lock-keywords-2
  (append ml/font-lock-keywords-1
          (let ((bold
                 (ml/generate-command-matcher
                  (ml/regexp-opt
                   '("textbf" "textsc" "textup" "boldsymbol" "pmb" "bm")) nil 1))
                (italic
                 (ml/generate-command-matcher
                  (ml/regexp-opt '("textit" "textsl" "emph")) nil 1))
                (citations
                 (ml/generate-command-matcher
                  (ml/regexp-opt
                   '("label" "ref" "pageref" "vref" "eqref" "cite"
                     "nocite" "index" "glossary" "bibitem" "citep" "citet")) t 1))
                (quotes
                 (concat (regexp-opt `("``" "\"<" "\"`" "<<" "«") t)
                         "[^'\">{]+" (regexp-opt `("''" "\">" "\"'" ">>" "»") t)))
                (specials-1
                 (ml/generate-command-matcher "\\\\\\(?:\\\\\\*?\\)" nil nil))
                (specials-2
                 (ml/generate-command-matcher
                  (ml/regexp-opt
                   '("linebreak" "nolinebreak" "pagebreak" "nopagebreak"
                     "newline" "newpage" "clearpage" "cleardoublepage"
                     "displaybreak" "allowdisplaybreaks" "enlargethispage")) nil nil))
                (other-commands
                 (ml/generate-command-matcher "\\\\\\(?:[a-zA-Z@]+\\**\\|[^ \t\n]\\)")))
            `((,bold 1 'bold append)
              (,italic 1 'italic append)
              (,citations 1 font-lock-constant-face)
              (,quotes . font-lock-string-face)
              (,specials-1 . font-lock-warning-face)
              (,specials-2 . font-lock-warning-face)
              (,other-commands . font-lock-keyword-face)))))

;; NOT compatible with tex-font-lock-keywords
(defconst ml/font-lock-keywords-3
  (append ml/font-lock-keywords-2
          (let ((chapter (ml/generate-command-matcher "\\\\chapter\\>\\*?" t 1))
                (section (ml/generate-command-matcher "\\\\section\\>\\*?" t 1))
                (diminish "{}\\|&")
                (underline (ml/generate-command-matcher "\\\\underline\\>" nil 1))
                (overline (ml/generate-command-matcher "\\\\overline\\>" nil 1))
                (type
                 (ml/generate-command-matcher
                  (ml/regexp-opt '("texttt" "textmd" "textrm" "textsf")) nil 1))
                (tiny (ml/generate-block-matcher "\\\\tiny\\>"))
                (scriptsize (ml/generate-block-matcher "\\\\scriptsize\\>"))
                (footnotesize (ml/generate-block-matcher "\\\\footnotesize\\>"))
                (small (ml/generate-block-matcher "\\\\small\\>"))
                (large (ml/generate-block-matcher "\\\\large\\>"))
                (Large (ml/generate-block-matcher "\\\\Large\\>"))
                (LARGE (ml/generate-block-matcher "\\\\LARGE\\>"))
                (huge (ml/generate-block-matcher "\\\\huge\\>"))
                (Huge (ml/generate-block-matcher "\\\\Huge\\>"))
                (type-old (ml/generate-block-matcher "\\\\tt\\>"))
                (italic-old
                 (ml/generate-block-matcher
                  (ml/regexp-opt '("em" "it" "sl"))))
                (bold-old
                 (ml/generate-block-matcher
                  (ml/regexp-opt '("bf" "bfseries")))))
            `((,chapter 1 'ml/chapter t)
              (,section 1 'ml/section t)
              (,diminish . 'shadow)
              (,underline 1 'underline)
              (,overline 1 'ml/overline)
              (,type 1 'ml/type)
              (,tiny 1 'ml/tiny append)
              (,scriptsize 1 'ml/script append)
              (,footnotesize 1 'ml/footnote append)
              (,small 1 'ml/small append)
              (,large 1 'ml/large append)
              (,Large 1 'ml/llarge append)
              (,LARGE 1 'ml/xlarge append)
              (,huge 1 'ml/huge append)
              (,Huge 1 'ml/hhuge append)
              (,italic-old 1 'italic append)
              (,bold-old 1 'bold append)
              (,type-old 1 'ml/type append)))))

;; + jit-lock highlighters

(defconst ml/decoration-commands
  '(("\\\\texttt\\>" . #("T" 0 1 (face ml/type)))
    ("\\\\textmd\\>" . #("T" 0 1 (face ml/type)))
    ("\\\\textrm\\>" . #("T" 0 1 (face ml/type)))
    ("\\\\textsf\\>" . #("T" 0 1 (face ml/type)))
    ("\\\\underline\\>" . #("U" 0 1 (face underline)))
    ("\\\\overline\\>" . #("O" 0 1 (face ml/overline)))
    ("\\\\textit\\>" . #("I" 0 1 (face italic)))
    ("\\\\textsl\\>" . #("I" 0 1 (face italic)))
    ("\\\\emph\\>" . #("I" 0 1 (face italic)))
    ("\\\\textbf\\>" . #("B" 0 1 (face bold)))
    ("\\\\textsc\\>" . #("B" 0 1 (face bold)))
    ("\\\\textup\\>" . #("B" 0 1 (face bold)))
    ("\\\\boldsymbol\\>" . #("B" 0 1 (face bold)))
    ("\\\\pmb\\>" . #("B" 0 1 (face bold)))
    ("\\\\bm\\>" . #("B" 0 1 (face bold)))))

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
  '(
    ("\\\\neq\\>" ;; (compose-chars ?／ ?＝)
     . #("／＝" 0 2 (composition ((2)))))
    ("\\\\notin\\>" . #("／∈" 0 2 (composition ((2)))))
    ("\\\\notni\\>" . #("／∋" 0 2 (composition ((2)))))
    ))

(defconst ml/operator-symbols
  '(
    ;; set
    ("\\\\mid\\>" . "｜") ("\\\\emptyset\\>" . "∅") ("\\\\\\(?:set\\)?minus\\>" . "＼")
    ("\\\\\\(?:big\\)?cup\\>" . "∪") ("\\\\\\(?:big\\)cap\\>" . "∩")
    ("\\\\sqcup\\>" . "⊔") ("\\\\sqcap\\>" . "⊓")
    ;; logic
    ("\\\\exists\\>" . "∃") ("\\\\forall\\>" . "∀") ("\\\\neg\\>" . "￢")
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
    ("\\\\varlimsup\\>" . #("lim" 0 3 (face ml/overline)))
    ("\\\\varliminf\\>" . #("lim" 0 3 (face underline)))
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
    ("\\\\epsilon\\>" . "ε") ("\\\\zeta\\>" . "ζ") ("\\\\eta\\>" . "η")
    ("\\\\theta\\>" . "θ") ("\\\\iota\\>" . "ι") ("\\\\kappa\\>" . "κ")
    ("\\\\lambda\\>" . "λ") ("\\\\mu\\>" . "μ") ("\\\\nu\\>" . "ν")
    ("\\\\xi\\>" . "ξ") ("\\\\pi\\>" . "π") ("\\\\rho\\>" . "ρ")
    ("\\\\sigma\\>" . "σ") ("\\\\tau\\>" . "τ") ("\\\\upsilon\\>" . "υ")
    ("\\\\phi\\>" . "φ") ("\\\\chi\\>" . "χ") ("\\\\psi\\>" . "ψ")
    ("\\\\omega\\>" . "ω")
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
    ("\\\\\\(?:text\\)?backslash\\>" . "＼") ("\\\\circ\\>" . "○")
    ("\\\\star\\>" . "⋆") ("\\\\S\\>" . "§")
    ("\\\\dagger\\>" . "†") ("\\\\ddag\\>" . "‡")
    ))

(defconst ml/other-symbols
  '(
    ;; TeX commands
    ("\\\\begin\\>" . "▽") ("\\\\end\\>" . "△")
    ("\\\\\\(bib\\)?item\\>" . "＊") ("\\\\par\\>" . "¶")
    ("\\\\ref\\>" . "☞") ("\\\\cite\\>" . "†")
    ("\\\\left\\>" . "¡") ("\\\\right\\>" . "!")
    ("~\\|\\\\\\(?:[,;\s]\\|hspace\\>\\)" . "␣")
    ("\\\\\\(?:newline\\>\\|\\\\\\)" . "⏎")
    ("\\\\TeX\\>"
     ;; (compose-chars ?T '(cr cl -20 -45) ?E '(cr cl -20 24) ?X)
     . #("TEX" 0 3 (composition ((3 84 7099277 69 7117965 88)))))
    ("\\\\LaTeX\\>"
     ;; (compose-chars ?L '(cr cl -60 35) ?A '(cr cl -18 -20)
     ;;                ?T '(cr cl -18 -60) ?E '(cr cl -20 5) ?X)
     . #("LATEX" 0 5 (composition ((5 76 4498317 65 7236749 84 7226509 69 7112077 88)))))
    ;; parens
    ("\\\\{" . #("{⎨" 0 2 (composition ((2)))))
    ("\\\\}" . #("⎬}" 0 2 (composition ((2)))))
    ;; (compose-chars ?\[ '(cr cl -90 0) ?\[)
    ("\\\\\\(?:double\\[\\|lBrack\\)" . #("[[" 0 2 (composition ((2 91 2523277 91)))))
    ("\\\\\\(?:double\\]\\|rBrack\\)" . #("]]" 0 2 (composition ((2 93 2523277 93)))))
    ("\\\\langle\\>" . "〈") ("\\\\rangle\\>" . "〉")
    ;; "&"
    ("&" . #("&|" 0 2 (composition ((2)) face shadow)))))

(defconst ml/symbols
  (append (mapcar (lambda (pattern)
                    (cons (concat "\\\\not[ \t\n]*" (car pattern))
                          (compose-string (concat "／" (cdr pattern)))))
                  (append ml/relation-symbols
                          ml/arrow-symbols))
          ml/decoration-commands
          ml/relation-symbols
          ml/negrel-symbols
          ml/operator-symbols
          ml/arrow-symbols
          ml/letter-symbols
          ml/other-symbols))

(defun ml/make-overlay (from to &rest props)
  (let* ((ov (make-overlay from to))
         (hooks (list `(lambda (&rest _) (delete-overlay ,ov)))))
    (overlay-put ov 'modification-hooks hooks)
    (overlay-put ov 'insert-in-front-hooks hooks)
    (overlay-put ov 'category 'magic-latex)
    (while props
      (overlay-put ov (car props) (cadr props))
      (setq props (cddr props)))
    ov))

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

(defun ml/jit-lock-highlighter (beg end)
  (mapc (lambda (ov)
          (when (eq (overlay-get ov 'category) 'magic-latex)
            (delete-overlay ov)))
        (overlays-in beg end))
  (goto-char beg)
  ;; prettify suscripts
  (save-excursion
    (while (ignore-errors (ml/search-suscript t end))
      (let ((ov1 (ml/make-overlay (match-beginning 0) (match-end 0)
                                  'invisible t 'intangible t))
            (ov2 (ml/make-overlay (match-beginning 1) (match-end 1))))
        (cl-case (string-to-char (match-string 0))
          ((?_) (overlay-put ov2 'display '((raise -0.2) (height 0.8))))
          ((?^) (overlay-put ov2 'display '((raise 0.2) (height 0.8))))))))
  ;; prettify symbols
  (dolist (symbol ml/symbols)
    (save-excursion
      (while (ignore-errors (ml/search-regexp (car symbol) end nil t))
        (let* ((ov (catch 'found
                     (dolist (ov (overlays-at (match-beginning 0)))
                       (when (eq (overlay-get ov 'category) 'magic-latex)
                         (throw 'found ov)))))
               (oldprop (and ov (overlay-get ov 'display))))
          (cond ((null ov)
                 (ml/make-overlay (match-beginning 0) (match-end 0) 'display (cdr symbol)))
                ((stringp oldprop)
                 nil)
                (t
                 (overlay-put ov 'display (propertize (cdr symbol) 'display oldprop)))))))))

;; + activate

(defun magic-latex-buffer ()
  (interactive)
  (jit-lock-mode 1)
  (setq-local ml/buffer-fancy-p t)
  (setq-local font-lock-multiline t)
  (set-syntax-table ml/syntax-table)
  (font-lock-add-keywords nil ml/font-lock-keywords-3 'set)
  (jit-lock-register 'ml/jit-lock-highlighter t))

(defadvice jit-lock-fontify-now (around ml/ad-jit-lock activate)
  (if (not ml/buffer-fancy-p)
      ad-do-it
    (ad-set-arg 0 (condition-case nil
                      (save-excursion
                        (goto-char (ad-get-arg 0))
                        (ml/skip-blocks 1 nil t)
                        (point))
                    (error 1)))
    (ad-set-arg 1 (condition-case nil
                      (save-excursion
                        (goto-char (ad-get-arg 1))
                        (ml/skip-blocks 1)
                        (point))
                    (error (point-max))))
    (let ((ml/jit-point (point)))
      ad-do-it)))

(add-to-list 'tex-verbatim-environments "Verbatim")

;; + provide

(provide 'magic-latex-buffer)

;;; magic-latex-buffer.el ends here
