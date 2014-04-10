;; emacs init file for drew, with thanks to gleit.

;; Helper to load files based on relative paths
(defun relative-to-full-path (filename)
  (concat (file-name-directory (or load-file-name buffer-file-name)) filename))

(load (relative-to-full-path "init-packages.el"))
(load (relative-to-full-path "init-ui.el"))
(load (relative-to-full-path "init-keys.el"))
