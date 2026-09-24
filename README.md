# moonli-mode for emacs

[moonli-mode.el](./moonli-mode.el) adds support for the [Moonli Programming Language](https://moonli-lang.github.io/) to [Emacs](https://www.gnu.org/software/emacs/).

## Installation

### Method 1: Manual

If you use [use-package](https://github.com/jwiegley/use-package) (which you should!):

```emacs-lisp
(use-package moonli-mode
  :load-path "/path/to/moonli-mode.el/moonli-mode.el"
  :mode "\\.moonli\\'"
  :bind (:map moonli-mode-map
         ("C-c C-c" . moonli-compile-defun)
         ("C-x C-e" . moonli-eval-last-expression)
         ("C-c C-t" . slime-toggle-trace-fdefinition)
         ("C-c C-e" . moonli-transpile-region)
         ("C-c C-d d" . slime-describe-symbol))
  :config
  (add-to-list 'slime-company-major-modes 'moonli-mode)
  (defun moonli-company-setup ()
    (setq-local company-backends '((company-capf company-slime))))
  (add-hook 'moonli-mode-hook 'moonli-company-setup))
```

If you don't use `use-package`, add the following to your config:

```emacs-lisp
(autoload 'moonli-mode "/path/to/moonli-mode.el/moonli-mode.el" nil t)
```

### Method 2: Melpa

TODO

## Current and Planned Features

[x] Basic syntax highlighting
[x] beginning/end-of-defun
[x] Eval last expression
[ ] Indentation

