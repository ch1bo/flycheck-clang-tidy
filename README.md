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
  '(add-hook 'flycheck-mode-hook #'flycheck-clang-tidy-setup))
```

Or, if you use [use-package][]:

``` emacs-lisp
(use-package flycheck-clang-tidy
  :after flycheck
  :hook
  (flycheck-mode . flycheck-clang-tidy-setup)
  )
```

Make sure that the `clang-tidy` binary is present on Emacs' `exec-path`, or
customize `flycheck-clang-tidy-executable` to point to the `clang-tidy`
binary.

Usage
-----

When `flycheck` is enabled (e.g. with `global-flycheck-mode`), `c-mode` and
`c++-mode` buffers will be automatically checked using this checker.

To have `clang-tidy` work correctly, you usally require a compile command
database as described in `clang-tidy --help`. For example using CMake a file
named `compile_commands.json` can be created using
`-DCMAKE_EXPORT_COMPILE_COMMANDS=ON`. `flycheck-clang-tidy` then looks for this
file via variable `flycheck-clang-tidy-build-path` which defaults to `build`.

You can pass additional options to `clang-tidy` using the variable
`flycheck-clang-tidy-extra-options`.

This checker includes an error explainer. Invoking `flycheck-explain-error-at-point`
will search clang.llvm.org for the documentation of the clang-tidy check under point
and render the result HTML in a Help buffer. This requires that Emacs is
compiled with XML support.

Other solutions
---------------

If you use [lsp-mode][] with [clangd][clangd-ct] 9.0 or above, you can use the
embedded clang-tidy by adding `--clang-tidy` to `lsp-clients-clangd-args`.

[flycheck]: https://github.com/flycheck/flycheck
[clang-tidy]: http://clang.llvm.org/extra/clang-tidy
[melpa]: http://melpa.org
[use-package]: https://github.com/jwiegley/use-package
[lsp-mode]: https://github.com/emacs-lsp/lsp-mode
[clangd-ct]: https://clang.llvm.org/extra/clangd/Features.html#clang-tidy-checks
