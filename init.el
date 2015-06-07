;; emacs init file for drew, with thanks to gleit.

;; for emacsclient support
(server-start)

(when (member "Menlo" (font-family-list))
  (set-face-attribute 'default nil
                      :family "Menlo"
                      :height 120))

;; Helper to load files based on relative paths
(defun relative-to-full-path (filename)
  (concat (file-name-directory (or load-file-name buffer-file-name)) filename))

(load (relative-to-full-path "custom-functions.el"))
(load (relative-to-full-path "general-config.el"))
(load (relative-to-full-path "init-packages.el"))
(load (relative-to-full-path "init-ui.el"))
(load (relative-to-full-path "init-keys.el"))
