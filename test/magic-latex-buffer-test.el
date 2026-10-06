;;; magic-latex-buffer-test.el --- Tests for Magic LaTeX -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-2.0-or-later
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Behavioral-contract tests for Magic LaTeX's filtered regexp search.
;; Run with `make test'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'magic-latex-buffer)

(defun ml-test/reference-search-regexp
    (regex &optional bound backward point-safe)
  "Original recursive implementation of `ml/search-regexp'."
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
       (ml-test/reference-search-regexp
        regex bound backward point-safe))))

(defun ml-test/search-outcome (search regex backward point-safe)
  "Capture the observable result of calling SEARCH."
  (condition-case error-data
      (let ((value (funcall search regex nil backward point-safe)))
        (list 'success value (point) (match-data t) (match-string 0)))
    (error
     (list 'error (car error-data) (error-message-string error-data) (point)))))

(defun ml-test/compare-searches
    (content regex start jit-point &optional backward point-safe ignored-range)
  "Compare reference and iterative searches over CONTENT from START."
  (with-temp-buffer
    (insert content)
    (when ignored-range
      (put-text-property (car ignored-range) (cdr ignored-range)
                         'face 'font-lock-comment-face))
    (let ((ml/jit-point jit-point))
      (goto-char start)
      (let ((reference
             (ml-test/search-outcome
              #'ml-test/reference-search-regexp regex backward point-safe)))
        (goto-char start)
        (should
         (equal reference
                (ml-test/search-outcome
                 #'ml/search-regexp regex backward point-safe)))))))

(ert-deftest ml-test/search-regexp-preserves-reference-contract ()
  (let ((alpha "\\\\alpha\\>"))
    ;; Forward and backward matches.
    (ml-test/compare-searches "\\alpha and \\alpha" alpha 1 1)
    (ml-test/compare-searches "\\alpha and \\alpha" alpha 18 1 t)
    ;; Escaped matches are skipped in both directions.
    (ml-test/compare-searches "\\\\alpha then \\alpha" alpha 1 1)
    (ml-test/compare-searches "\\alpha then \\\\alpha" alpha 20 1 t)
    ;; A match containing the JIT point is skipped.
    (ml-test/compare-searches "\\alpha then \\alpha" alpha 1 4 nil t)
    ;; Fontified comments and verbatim regions are skipped.
    (ml-test/compare-searches
     "x\\alpha then \\alpha" alpha 1 1 nil nil '(1 . 8))
    ;; Ordinary failure and invalid regexps preserve the original behavior.
    (ml-test/compare-searches "\\\\alpha" alpha 1 1)
    (ml-test/compare-searches "text" "[" 3 1)))

(ert-deftest ml-test/search-regexp-handles-backslash-parity ()
  (with-temp-buffer
    (let (one three)
      (dolist (count '(1 2 3 4))
        (let ((begin (point)))
          (insert (make-string count ?\\) "alpha ")
          (pcase count
            (1 (setq one begin))
            (3 (setq three (+ begin 2))))))
      (let ((ml/jit-point (point-min)))
        (goto-char (point-min))
        (should (ml/search-regexp "\\\\alpha\\>"))
        (should (= one (match-beginning 0)))
        (should (ml/search-regexp "\\\\alpha\\>"))
        (should (= three (match-beginning 0)))
        (should-error (ml/search-regexp "\\\\alpha\\>"))))))

(ert-deftest ml-test/search-regexp-noerror-reports-ordinary-failure ()
  (with-temp-buffer
    (insert "plain text")
    (goto-char (point-min))
    (let ((ml/jit-point (point-min)))
      (should-not (ml/search-regexp-noerror "\\\\alpha\\>")))))

(ert-deftest ml-test/search-regexp-handles-many-rejected-matches ()
  (with-temp-buffer
    (dotimes (_ 2000)
      (insert "\\\\alpha "))
    (let ((expected (point)))
      (insert "\\alpha")
      (goto-char (point-min))
      (let ((ml/jit-point (point-min)))
        (should (ml/search-regexp "\\\\alpha\\>"))
        (should (= expected (match-beginning 0)))))))

(ert-deftest ml-test/skip-blocks-preserves-match-data ()
  (with-temp-buffer
    (insert "{outer {inner} tail}")
    (goto-char (point-min))
    (set-match-data '(1 1))
    (should (ml/skip-blocks 0))
    (should (= (point) (point-max)))
    (should (equal '(1 1) (match-data t)))))

(defconst ml-test/root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name)))))

(defconst ml-test/fixture
  (expand-file-name "test/fixtures/large-document.tex" ml-test/root))

(defun ml-test/reference-prettify-symbols (beg end)
  "Run the original one-regexp-per-symbol implementation from BEG to END."
  (dolist (symbol ml/symbols)
    (save-excursion
      (goto-char beg)
      (let ((regex (car symbol)))
        (while (ignore-errors (ml/search-regexp regex end nil t))
          (let* ((old-overlay
                  (ml/overlay-at
                   (match-beginning 0) 'category 'ml/ov-pretty))
                 (priority-base
                  (and old-overlay
                       (or (overlay-get old-overlay 'priority) 1)))
                 (old-display
                  (and old-overlay (overlay-get old-overlay 'display))))
            (unless (stringp old-display)
              (ml/make-pretty-overlay
               (match-beginning 0) (match-end 0)
               'priority (when old-overlay (1+ priority-base))
               'display
               (propertize
                (eval (cdr symbol)) 'display old-display)))))))))

(defun ml-test/symbol-snapshot (prettifier content)
  "Run PRETTIFIER over CONTENT and return canonical symbol overlays."
  (with-temp-buffer
    (insert content)
    (setq buffer-file-name "magic-latex-symbol-parity.tex")
    (latex-mode)
    (font-lock-mode 1)
    (magic-latex-buffer 1)
    (font-lock-ensure)
    (remove-overlays)
    (let ((ml/jit-point (point-max)))
      (set-syntax-table ml/syntax-table)
      (goto-char (point-min))
      (funcall prettifier (point-min) (point-max))
      (sort
       (mapcar
        (lambda (overlay)
          (list
           (overlay-start overlay)
           (overlay-end overlay)
           (buffer-substring-no-properties
            (overlay-start overlay) (overlay-end overlay))
           (format "%S" (overlay-get overlay 'display))
           (format "%S" (overlay-get overlay 'priority))))
        (cl-remove-if-not
         (lambda (overlay)
           (eq (overlay-get overlay 'category) 'ml/ov-pretty))
         (overlays-in (point-min) (point-max))))
       (lambda (left right)
         (or (< (car left) (car right))
             (and (= (car left) (car right))
                  (< (cadr left) (cadr right)))))))))

(ert-deftest ml-test/symbol-plan-preserves-every-rule ()
  (let ((rules
         (apply
          #'+
          (mapcar
           (lambda (segment)
             (if (eq 'exact (car segment))
                 (hash-table-count (nth 2 segment))
               1))
           (ml/build-symbol-plan)))))
    (should (= (length ml/symbols) rules))
    (should (< (length (ml/build-symbol-plan)) (length ml/symbols)))))

(ert-deftest ml-test/segmented-symbols-match-reference-on-generic-fixture ()
  (let ((content (with-temp-buffer
                   (insert-file-contents ml-test/fixture)
                   (buffer-string))))
    (let ((snapshot (ml-test/symbol-snapshot #'ml/prettify-symbols content)))
      (should (> (string-bytes content) (* 120 1024)))
      (dolist (source '("\\alpha" "\\sum" "\\mathbb{R}" "\\vec{x}" "~"))
        (should (cl-find source snapshot :key #'caddr :test #'equal)))
      (should
       (equal (ml-test/symbol-snapshot #'ml-test/reference-prettify-symbols content)
              snapshot)))))

(ert-deftest ml-test/comments-and-verbatim-remain-literal ()
  (let ((snapshot
         (ml-test/symbol-snapshot
           #'ml/prettify-symbols
          (concat
           "Visible: $\\alpha$.\n"
           "% Hidden: \\alpha.\n"
           "\\begin{verbatim}\n\\alpha\n\\end{verbatim}\n"))))
    (should (= 1 (cl-count "\\alpha" snapshot :key #'caddr :test #'equal)))))

(ert-deftest ml-test/symbol-plan-preserves-overlapping-rule-order ()
  (dolist
      (symbols
       '((("\\\\f\\(?:oo\\)\\>" . "regexp-first")
          ("\\\\foo\\>" . "exact-second"))
         (("\\\\foo\\>" . "exact-first")
          ("\\\\f\\(?:oo\\)\\>" . "regexp-second"))
         (("\\\\foo\\>" . "duplicate-first")
          ("\\\\foo\\>" . "duplicate-second"))
         (("\\\\\\([[:alpha:]]+\\)\\>" . (upcase (match-string 1)))
          ("\\\\foo\\>" . "exact-after-capture"))))
    (let ((ml/symbols symbols)
          (ml/symbol-plan-cache nil)
          (ml/symbol-plan-source nil)
          (content "\\foo \\\\foo % \\foo\n\\begin{verbatim}\\foo\\end{verbatim}\n"))
      (should
       (equal
        (ml-test/symbol-snapshot #'ml-test/reference-prettify-symbols content)
        (ml-test/symbol-snapshot #'ml/prettify-symbols content))))))

(ert-deftest ml-test/symbol-plan-tracks-in-place-edits ()
  ;; Start each case with a populated cache, then edit without replacing
  ;; the top-level list.  Include destructive string edits as well as conses.
  (dolist (edit
           (list
            (lambda () (setcar (car ml/symbols) "\\\\bar\\>"))
            (lambda () (aset (caar ml/symbols) 2 ?b))
            (lambda () (setcar ml/symbols
                              (cons "\\\\foo\\>" "replacement")))
            (lambda () (setcdr (car ml/symbols) "new display"))
            (lambda () (setcdr ml/symbols
                              (list (cons "\\\\bar\\>" "added"))))))
    (let ((ml/symbols (list (cons (copy-sequence "\\\\foo\\>") "original")))
          (ml/symbol-plan-source nil)
          (ml/symbol-plan-cache nil))
      (ml/symbol-plan)
      (funcall edit)
      (should (equal (mapcar #'cadr (ml/symbol-plan))
                     (mapcar #'cadr (ml/build-symbol-plan))))
      (let ((content "\\foo \\bar \\boo"))
        (should
         (equal
          (ml-test/symbol-snapshot #'ml-test/reference-prettify-symbols content)
          (ml-test/symbol-snapshot #'ml/prettify-symbols content)))))))

(ert-deftest ml-test/symbol-plan-reuses-unchanged-cache ()
  (let ((ml/symbols (list (cons "\\\\foo\\>" "display")))
        (ml/symbol-plan-source nil)
        (ml/symbol-plan-cache nil))
    (let ((plan (ml/symbol-plan)))
      (should (eq plan (ml/symbol-plan))))))

;;; magic-latex-buffer-test.el ends here
