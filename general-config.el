;; Set PATH correctly from the shell
;; Utility function to help find the ag executable.
;; Needed for OS X environments

(defun aw/set-env-from-shell (env-var)
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.

This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
  (interactive)
  (let ((env-var-from-shell (replace-regexp-in-string
                             "[ \t\n]*$" ""
                             (shell-command-to-string
                              (format "$SHELL --login -i -c 'echo $%s'" env-var)))))
    (setenv env-var env-var-from-shell)))

;; and execute.
(aw/set-env-from-shell "PATH")
(setq exec-path (split-string (getenv "PATH") path-separator))
(aw/set-env-from-shell "GOPATH")
(aw/set-env-from-shell "GOOS")
(aw/set-env-from-shell "GOARCH")

;; deal with certain tramp errors with an overly-long temp directory
(let ((tmpdir "/tmp/emacs"))
  (unless (file-directory-p "/tmp/emacs")
    (mkdir tmpdir))
  (setenv "TMPDIR" tmpdir))
