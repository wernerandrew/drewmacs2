;; emacs init file for drew, with thanks to gleit.

;; General keyboard and mouse config
(define-key function-key-map "\e[1;2A" [S-up])

(require 'mouse)
(xterm-mouse-mode t)
(defun track-mouse (e))
(setq mouse-sel-mode t)

;; Command as meta
(when (boundp 'mac-command-modifier)
  (setq mac-command-modifier 'meta))

;; Backup directories
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(defun relative-to-full-path (filename)
  (concat (file-name-directory (or load-file-name buffer-file-name)) filename))

(load (relative-to-full-path "init-packages.el"))
(load (relative-to-full-path "init-ui.el"))
