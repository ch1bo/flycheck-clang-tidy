# flycheck-clang-tidy
Flycheck syntax checker using clang-tidy

This library provides a [flycheck][] checker for C/C++ source code
using [clang-tidy][].

Installation
------------

You'll need Emacs 24 for `flycheck`, so the recommended way to get
`flycheck-clang-tidy` is as a package from the [MELPA][melpa]
repository.

Alternatively you have to ensure `flycheck` is installed, then 
download this code and add the directory to your Emacs `load-path`.

Then, in your `init.el`:

```lisp
(eval-after-load 'flycheck
  '(require 'flycheck-hdevtools))
```

Make sure that the `clang-tidy` binary is present on Emacs' `exec-path`, or
customize `flycheck-clang-tidy-executable` to point to the `clang-tidy`
binary.

Usage
-----

When `flycheck` is enabled (e.g. with `global-flycheck-mode`), `c-mode` and 
`c++-mode` buffers will be automatically checked using this checker.

[flycheck]: https://github.com/flycheck/flycheck
[clang-tidy]: http://clang.llvm.org/extra/clang-tidy
[melpa]: http://melpa.org
