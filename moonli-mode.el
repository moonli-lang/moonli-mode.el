
(require 'slime)

(defvar moonli-keywords
  '("t"
    "nil"
    "end"
    "if" "ifelse"
    "declare"
    "declaim"
    "loop" "let" "let+"
    "progn" "in-package"
    "labels"
    "optima:match"
    "match"
    "slots"
    "options"))

(defvar moonli-block-start-keywords
  '("if"
    "defun"
    "defpackage"
    "defclass"
    "defstruct"
    "deftype"
    "let"
    "let+"
    "labels"
    "loop"))

(defvar moonli-type-keywords
  '("integer" "fixnum" "ratio" "rational" "real" "complex"
    "float" "single-float" "double-float" "long-float" "short-float"
    "character" "base-char"
    "string" "simple-string"
    "vector" "simple-vector"
    "array" "simple-array"
    "symbol" "hash-table"))

(defvar moonli-punctuation-characters-rx
  `(or line-start
       line-end
       (any "|" ":" "," "$" "'" "\"" "(" ")" "[" "]" "{" "}" ";"
            whitespace)))

(defvar moonli-symbol-characters-rx
  `(any alnum
        "!" "@" "%" "^" "&" "*"
        "-" "_" "+" "=" "<" ">" "/" "?" ":"))

(defvar moonli-definition-pattern
  (rx--to-expr `(seq (group symbol-start
                            "def"
                            (* ,moonli-symbol-characters-rx)
                            symbol-end)
                     (* whitespace)
                     (group (+ ,moonli-symbol-characters-rx)))))

(defvar moonli-symbol-regex
  (rx--to-expr
   `(seq (or line-start
             line-end
             ,moonli-punctuation-characters-rx)
         (group (+ ,moonli-symbol-characters-rx))
         (or line-start
             line-end
             ,moonli-punctuation-characters-rx))))

(cl-defun moonli-function-matcher (bound)
  (interactive)
  ;; (message "%f" bound)
  (let ((current-point (point)))
    (while (re-search-forward moonli-symbol-regex bound t)
      (let ((match (upcase (match-string-no-properties 1)))
            (match-start (match-beginning 1))
            (match-end (match-end 1)))
        ;; (message match)
        ;; (sleep-for 1)
        (when (and (null (nth 3 (syntax-ppss match-start))) ; not in string
                   (null (nth 4 (syntax-ppss match-start))) ; not in comment
                   (slime-eval
                    `(cl:let ((sym (cl:find-symbol ,match "COMMON-LISP")))
                        (cl:and sym
                                (cl:fboundp sym)
                                (cl:null (cl:macro-function sym))))))
          (set-match-data (list match-start match-end))
          (cl-return-from moonli-function-matcher t))
        (goto-char (1+ match-end))))))

(defvar moonli-font-lock-keywords
  (list (cons (rx line-start
                  (* space)
                  "#"
                  (* not-newline)
                  line-end)
              'font-lock-comment-face)
        (cons (rx--to-expr `(seq ,moonli-punctuation-characters-rx
                                 (group (or ,@moonli-keywords))
                                 ,moonli-punctuation-characters-rx))
              '((1 font-lock-keyword-face)))
        (cons moonli-definition-pattern
              `((1 font-lock-constant-face)
                (2 font-lock-variable-name-face)))
        (cons (rx--to-expr `(seq ,moonli-punctuation-characters-rx
                                 (group (any ":" "$")
                                        (+ ,moonli-symbol-characters-rx))
                                 ,moonli-punctuation-characters-rx))
              '((1 font-lock-constant-face)))
        (cons 'moonli-function-matcher
              'font-lock-builtin-face)

        (cons (rx--to-expr `(seq ,moonli-punctuation-characters-rx
                                 (group (or ,@moonli-type-keywords))
                                 ,moonli-punctuation-characters-rx))
              '((1 font-lock-type-face)))
        (cons (rx--to-expr `(seq ,moonli-punctuation-characters-rx
                                 (group "*"
                                        (+ ,moonli-symbol-characters-rx)
                                        "*")
                                 ,moonli-punctuation-characters-rx))
              '((1 font-lock-constant-face)))
        (cons (rx--to-expr `(seq ,moonli-punctuation-characters-rx
                                 (group "+"
                                        (+ ,moonli-symbol-characters-rx)
                                        "+")
                                 ,moonli-punctuation-characters-rx))
              '((1 font-lock-constant-face)))))


(defun moonli-indent-line ()
  (print (syntax-ppss))
  (indent-line-to
   (* 2 (car (syntax-ppss (point)))))) ; indent by nesting depth

(defun moonli-point-in-string-or-comment ()
  (let ((syntax (syntax-ppss)))
    (or (nth 3 syntax)
        (nth 4 syntax))))

(defun moonli-backward-skip-string-or-comment ()
  (interactive)
  (let ((syntax (syntax-ppss)))
    (when (or (nth 3 syntax)
              (nth 4 syntax))
      (goto-char (nth 8 syntax)))))

(defun moonli-forward-skip-string-or-comment ()
  (interactive)
  (let ((syntax (syntax-ppss)))
    (when (or (nth 3 syntax)
              (nth 4 syntax))
      (goto-char (nth 8 syntax))
      (forward-sexp 2 t))))

(defun moonli-beginning-of-defun (&optional arg)
  (interactive)
  (moonli-skip-whitespace-or-comments)
  (let (final-pos)
    (dotimes (_i (or arg 1))
      (loop do
        (moonli-backward-skip-string-or-comment)
        (backward-char)
        (let ((pos (re-search-backward (rx (and line-start
                                                word-boundary))
                                       nil
                                       t)))
          (if (string= "end" (symbol-at-point))
              (moonli-beginning-of-defun)
            (setf final-pos pos)))
        while (moonli-point-in-string-or-comment)))
    final-pos))

(defun moonli-end-of-block-raw ()
  (re-search-forward (rx line-start
                         "end"
                         (* (any " " "	")))
                     nil
                     t)
  (end-of-line))

(cl-defun moonli-end-of-block-or-expression ()
  (interactive)
  (beginning-of-line)
  (let ((current-point (point)))

    (when (member (symbol-name (symbol-at-point))
                  moonli-block-start-keywords)
      (moonli-end-of-block-raw)
      (loop while (moonli-point-in-string-or-comment)
        do (moonli-forward-skip-string-or-comment)
        (moonli-end-of-block-raw))
      (return-from moonli-end-of-block-or-expression (point)))

    (goto-char current-point)
    (re-search-forward (rx line-start
                           (not whitespace)
                           (* not-newline)
                           (* whitespace)
                           line-end))
    (end-of-line)
    (skip-chars-backward " \t")

    (let ((char-at-point (char-to-string (char-after))))
      (cond ((member char-at-point '("(" "{"))
             (re-search-forward (rx (and line-start
                                         (+ (any "}" ")" ";"))))
                                nil
                                t))
            ((member char-at-point '(":"))
             (next-line)
             (moonli-end-of-block-or-expression)))
      (moonli-skip-whitespace-or-comments)
      (return-from moonli-end-of-block-or-expression (point)))))

(defvar moonli-end-of-defun-functions
  '(moonli-end-of-block-or-expression))

(defun moonli-skip-whitespace-or-comments ()
  (interactive)
  (re-search-forward (rx--to-expr `(seq line-start
                                        ,moonli-symbol-characters-rx))
                     nil
                     t)
  (beginning-of-line))

(defun moonli-end-of-defun (&optional arg)
  (interactive)
  (let (final-pos)
    (dotimes (_i (or arg 1))
      (loop do
        (moonli-forward-skip-string-or-comment)
        (moonli-skip-whitespace-or-comments)
        (let* ((current-point (point))
               (end-pos (mapcar (lambda (fn)
                                  (goto-char current-point)
                                  (or (funcall fn) (point-max)))
                                moonli-end-of-defun-functions))
               (pos (apply #'min end-pos)))
          (goto-char pos)
          (setf final-pos pos))
        while (moonli-point-in-string-or-comment)))
    final-pos))


(defun moonli-search-buffer-package ()
  (let ((case-fold-search t)
        (regexp (rx--to-expr `(seq line-start
                                   (* whitespace)
                                   (? (or "cl:"
                                          "common-lisp:"))
                                   "in-package"
                                   (+ whitespace)
                                   (group (+ ,moonli-symbol-characters-rx))
                                   (* ,moonli-punctuation-characters-rx)))))
    (or (save-excursion
          (when (or (re-search-backward regexp nil t)
                    (re-search-forward regexp nil t))
            (match-string-no-properties 1)))
        ":common-lisp-user")))

(define-derived-mode moonli-mode prog-mode "Moonli"
  (setq-local font-lock-defaults '(moonli-font-lock-keywords))
  (setq-local indent-line-function 'moonli-indent-line)

  (setq-local end-of-defun-function 'moonli-end-of-defun)
  (setq-local beginning-of-defun-function 'moonli-beginning-of-defun)

  (setq-local slime-find-buffer-package-function 'moonli-search-buffer-package))

(add-to-list 'auto-mode-alist '("\\.moonli\\'" . moonli-mode))

(defun moonli-compile-region (start end)
  "Compile the region."
  (interactive "r")
  ;; Check connection before running hooks things like
  ;; slime-flash-region don't make much sense if there's no connection
  (slime-connection)
  (slime-flash-region start end)
  (run-hook-with-args 'slime-before-compile-functions start end)
  (let ((string (format "(eval (moonli:read-moonli-from-string %s))"
                        (prin1-to-string
                         (format "\n%s"
                                 (buffer-substring-no-properties start end))))))
    (slime-compile-string string start)))

(defun moonli-compile-defun (&optional raw-prefix-arg)
  "Compile the current toplevel form.

With (positive) prefix argument the form is compiled with maximal
debug settings (`C-u'). With negative prefix argument it is compiled for
speed (`M--'). If a numeric argument is passed set debug or speed settings
to it depending on its sign."
  (interactive "P")
  (let ((slime-compilation-policy (slime-compute-policy raw-prefix-arg)))
    (if (use-region-p)
        (moonli-compile-region (region-beginning) (region-end))
      (apply #'moonli-compile-region (slime-region-for-defun-at-point)))))

(defun moonli-last-expression ()
  (buffer-substring-no-properties
   (save-excursion (beginning-of-defun) (point))
   (point)))

(defun moonli-eval-last-expression ()
  (interactive)
  (slime-interactive-eval
   (format "(eval (moonli:read-moonli-from-string %s))"
           (prin1-to-string (moonli-last-expression)))))

(provide 'moonli)
