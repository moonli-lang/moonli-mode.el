
(require 'slime)

(defvar moonli-keywords
  '("end"
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

(defvar moonli-builtin-functions
  '("format"
    "print"
    "values"
    "list"
    "gethash"))

(defvar moonli-punctuation-characters-rx
  `(any "|" ":" "," "$" "'" "\"" "(" ")" "[" "]" "{" "}"
        whitespace))

(defvar moonli-symbol-characters-rx
  `(any alnum
        "!" "@" "#" "%" "^" "&" "*"
        "-" "_" "+" "=" "<" ">" "/" "?" ":"))

(defvar moonli-definition-pattern
  (rx--to-expr `(seq (group symbol-start
                            "def"
                            (* ,moonli-symbol-characters-rx)
                            symbol-end)
                     (* whitespace)
                     (group (+ ,moonli-symbol-characters-rx)))))

(defvar moonli-font-lock-keywords
  (list (cons (rx--to-expr `(seq (or line-start
                                     line-end
                                     (+ ,moonli-punctuation-characters-rx))
                                 (group (or ,@moonli-keywords))
                                 (or line-start
                                     line-end
                                     (+ ,moonli-punctuation-characters-rx))))
              'font-lock-keyword-face)
        (cons moonli-definition-pattern
              `((1 font-lock-constant-face)
                (2 font-lock-variable-name-face)))
        (cons (rx symbol-start
                  (or "t" "nil")
                  symbol-end)
              'font-lock-keyword-face)
        (cons (rx--to-expr `(seq (or line-start
                                     line-end
                                     (+ ,moonli-punctuation-characters-rx))
                                 (group (or ,@moonli-builtin-functions))
                                 (or line-start
                                     line-end
                                     (+ ,moonli-punctuation-characters-rx))))
              '((1 font-lock-builtin-face)))))


(defun moonli-indent-line ()
  (print (syntax-ppss))
  (indent-line-to
   (* 2 (car (syntax-ppss (point)))))) ; indent by nesting depth

(defun moonli-beginning-of-defun (&optional arg)
  (backward-char)
  (let ((pos (re-search-backward (rx (and line-start
                                          word-boundary))
                                 nil
                                 t)))
    (if (string= "end" (symbol-at-point))
        (moonli-beginning-of-defun)
      pos)))

(defun moonli-next-close-paren ()
  (re-search-forward (rx (and line-start
                              (+ (any "}" ")"))))
                     nil
                     t))

(defun moonli-next-end-of-expr ()
  (interactive)
  (re-search-forward (rx line-start
                         (not whitespace)
                         (* not-newline)
                         line-end)))

(defvar moonli-end-of-defun-functions
  '(moonli-next-end-of-expr
    moonli-next-close-paren))

(defun moonli-skip-whitespace-or-comments ()
  (interactive)
  (re-search-forward (rx line-start
                         (not (any whitespace "#")))))

(defun moonli-end-of-defun (&optional arg)
  (moonli-skip-whitespace-or-comments)
  (let* ((current-point (point))
         (end-pos (mapcar (lambda (fn)
                            (goto-char current-point)
                            (or (funcall fn) (point-max)))
                          moonli-end-of-defun-functions))
         (final-pos (apply #'min end-pos)))
    (goto-char final-pos)
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
    (save-excursion
      (when (or (re-search-backward regexp nil t)
                (re-search-forward regexp nil t))
        (match-string-no-properties 1)))))

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
