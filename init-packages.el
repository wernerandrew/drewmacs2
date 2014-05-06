;; Use a package manager
;; Requires emacs 24+
;; TODO: make more backwards compatible

(require 'package)
(package-initialize)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)

(defun my-init-packages (packages)
  (when packages
    (let ((this-package (car packages))
	  (rest (cdr packages)))
      (add-to-list 'package-load-list (cons this-package t))
      (my-init-packages rest))))

(defvar local-packages
  '(color-theme
    less-css-mode
    web-mode
    js2-mode
    cython-mode
    enh-ruby-mode
    haskell-mode
    auto-complete
    projectile
    epc
    jedi
    ac-js2
    ag
    magit))

(defun uninstalled-packages (packages)
  (delq nil
	(mapcar (lambda (p) (if (package-installed-p p) nil p)) packages)))

(let ((need-to-install (uninstalled-packages local-packages)))
  (when need-to-install
    (progn
      (package-refresh-contents)
      (dolist (p need-to-install)
	(package-install p)))))

;; Init packages after load
(defvar local-mode-init-file-path
  (relative-to-full-path "init-mode.el"))
(add-hook
 'after-init-hook
 '(lambda ()
    (require 'color-theme)
    (color-theme-initialize)
    (color-theme-hober)

    (require 'magit)
    (global-set-key (kbd "C-c g s") 'magit-status)
    (global-set-key (kbd "C-c g b") 'magit-blame-mode)
    ;; Mode specific init
    (load local-mode-init-file-path)))
