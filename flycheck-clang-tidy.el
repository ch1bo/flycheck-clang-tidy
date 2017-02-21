(require 'dash)
(require 'flycheck)

(flycheck-def-config-file-var flycheck-clang-tidy c/c++-clang-tidy ".clang-tidy"
  :safe #'stringp)

(flycheck-def-option-var flycheck-clang-tidy-build-path "build" c/c++-clang-tidy
  "Build path to read a compile command database.

For example, it can be a CMake build directory in which a file named
compile_commands.json exists (use -DCMAKE_EXPORT_COMPILE_COMMANDS=ON
CMake option to get this output)."
  :safe #'stringp)

(defun flycheck-clang-tidy-find-default-directory (checker)
  (file-name-directory (flycheck-locate-config-file flycheck-clang-tidy checker))
  )

(flycheck-define-checker c/c++-clang-tidy
  "A C/C++ syntax checker using clang-tidy.

See URL `https://github.com/ch1bo/flycheck-clang-tidy'."
  :command ("clang-tidy"
            ;; TODO: clang-tidy expects config file contents, no way to change path
            ;; (config-file "-config=" flycheck-clang-tidy)
            (option "-p" flycheck-clang-tidy-build-path)
            source-inplace)
  :error-patterns
  ((warning line-start
            (file-name) ":" line ":" column ": warning: "
            (message (one-or-more not-newline))
            line-end))
  :modes (c-mode c++-mode)
  :working-directory flycheck-clang-tidy-find-default-directory
  ;; :next-checkers ((warning . haskell-hlint)))
  )

(add-to-list 'flycheck-checkers 'c/c++-clang-tidy)

(provide 'flycheck-clang-tidy)
