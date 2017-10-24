;;; flycheck-clang-tidy.el --- Flycheck syntax checker using clang-tidy

;; Author: Sebastian Nagel<sebastian.nagel@ncoding.at>
;; URL: https://github.com/ch1bo/flycheck-clang-tidy
;; Keywords: convenience languages tools
;; Package-Version: 0.0.1
;; Package-Requires: ((flycheck "0.30"))

;; This file is NOT part of GNU Emacs.
;; See LICENSE

;;; Commentary:

;; Adds a Flycheck syntax checker for C/C++ based on clang-tidy.

;;; Usage:

;;     (eval-after-load 'flycheck
;;       '(add-hook 'flycheck-mode-hook #'flycheck-clang-tidy-setup))


;;; Code:

(require 'flycheck)
(require 'json)

(flycheck-def-config-file-var flycheck-clang-tidy c/c++-clang-tidy ".clang-tidy"
  :safe #'stringp)

(flycheck-def-option-var flycheck-clang-tidy-build-path "build" c/c++-clang-tidy
  "Build path to read a compile command database.

For example, it can be a CMake build directory in which a file named
compile_commands.json exists (use -DCMAKE_EXPORT_COMPILE_COMMANDS=ON
CMake option to get this output)."
  :safe #'stringp)

(defun flycheck-clang-tidy-find-default-directory (checker)
  (let ((config_file_location (flycheck-locate-config-file flycheck-clang-tidy checker)))
    (if config_file_location
        (file-name-directory config_file_location)
      (message "Unable to find config file for %s, you need to create .clang-tidy file in your project root" checker))))

(defun flycheck-clang-tidy-compile-command (file)
  "Fetch compile command for FILE."
  (let* ((cmds-file (concat (file-name-as-directory flycheck-clang-tidy-build-path)
                            "compile_commands.json"))
         (cmds (json-read-file cmds-file)))
    (seq-find (lambda (cmd) (string= (alist-get 'file cmd) file)) cmds)))

(defun flycheck-clang-tidy-temp-compile-command (file source)
  "Return compile command for FILE with source located in SOURCE."
  (let* ((cmd (flycheck-clang-tidy-compile-command file))
         (directory (alist-get 'directory cmd))
         (command (alist-get 'command cmd)))
    (list (cons 'directory directory)
          (cons 'command (replace-regexp-in-string (regexp-quote file)
                                                   source command))
          (cons 'file source))))

(defun flycheck-clang-tidy-make-temp-compile-command (file source)
  "Create a temporary build command file for FILE with SOURCE."
  (let* ((directory (flycheck-temp-dir-system))
         (cmds-dir (file-name-as-directory (make-temp-name (expand-file-name "flycheck" directory))))
         (cmds-file (concat cmds-dir "compile_commands.json"))
         (cmd (flycheck-clang-tidy-temp-compile-command file source)))
    (prog1 cmds-dir
      (mkdir cmds-dir)
      (with-temp-file cmds-file
        (insert (json-encode (vector cmd)))))))

(defun flycheck-clang-tidy-inplace-build-command ()
  "Generate temporary compile_commands.json file.

This functions parses the compile_commands.json file and generate
a new one for the flycheck temporary source file so that
clang-tidy understand what to do with it."
  (let ((source (flycheck-save-buffer-to-temp #'flycheck-temp-file-inplace)))
    (format "-p=%s"
            (flycheck-clang-tidy-make-temp-compile-command (buffer-file-name)
                                                           source))))

(flycheck-define-checker c/c++-clang-tidy
  "A C/C++ syntax checker using clang-tidy.

See URL `https://github.com/ch1bo/flycheck-clang-tidy'."
  :command ("clang-tidy"
            ;; TODO: clang-tidy expects config file contents, no way to change path
            ;; (config-file "-config=" flycheck-clang-tidy)
            (eval (flycheck-clang-tidy-inplace-build-command))
            (eval (flycheck-save-buffer-to-temp #'flycheck-temp-file-inplace)))
  :error-patterns
  ((error line-start (file-name) ":" line ":" column ": error: "
          (message (one-or-more not-newline) "\n"
                   (one-or-more not-newline) "\n"
                   (one-or-more not-newline) "\n"
                   (one-or-more not-newline))
          line-end)
   (warning line-start (file-name) ":" line ":" column ": warning: "
            (message (one-or-more not-newline) "\n"
                     (one-or-more not-newline) "\n"
                     (one-or-more not-newline) "\n"
                     (one-or-more not-newline))
            line-end)
   (info line-start (file-name) ":" line ":" column ": note: "
         (message (one-or-more not-newline) "\n"
                  (one-or-more not-newline) "\n"
                  (one-or-more not-newline) "\n"
                  (one-or-more not-newline))
         line-end))
  :modes (c-mode c++-mode)
  :working-directory flycheck-clang-tidy-find-default-directory
  )

;;;###autoload
(defun flycheck-clang-tidy-setup ()
  "Setup Flycheck clang-tidy."
  (add-to-list 'flycheck-checkers 'c/c++-clang-tidy))

(provide 'flycheck-clang-tidy)
;;; flycheck-clang-tidy.el ends here
