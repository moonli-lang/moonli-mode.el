# moonli-mode for emacs

[moonli-mode.el](./moonli-mode.el) adds support for the [Moonli Programming Language](https://moonli-lang.github.io/) to [Emacs](https://www.gnu.org/software/emacs/).

## Installation

### Method 1: Manual

If you use [use-package](https://github.com/jwiegley/use-package) (which you should!):

```emacs-lisp
(use-package moonli
  :load-path "/path/to/moonli-mode.el/moonli-mode.el"
  :bind (:map moonli-mode-map
         ("C-c C-c" . moonli-compile-defun)
         ("C-x C-e" . moonli-eval-last-expression)))
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

