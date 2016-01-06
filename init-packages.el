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
  '(color-theme ;; themes
    sublime-themes
    solarized-theme
    zenburn-theme
    less-css-mode ;; programming modes
    web-mode
    js2-mode
    coffee-mode
    tss
    cython-mode
    enh-ruby-mode
    haskell-mode
    markdown-mode
    go-mode
    puppet-mode
    yaml-mode
    lua-mode
    auto-complete ;; misc productivity
    projectile
    epc
    jedi
    ac-js2
    go-autocomplete
    go-eldoc
    emacs-eclim
    ag
    magit
    multiple-cursors
    helm
    twittering-mode
    hackernews))

;; suppress magit startup warning
(setq magit-last-seen-setup-instructions "1.4.0")

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

;; For stuff outside MELPA that we want to live in VC
(add-to-list 'load-path (relative-to-full-path "local-packages"))

(add-hook
 'after-init-hook
 '(lambda ()
    (setq solarized-use-variable-pitch nil)
    (setq x-underline-at-descent-line t)
    (load-theme 'solarized-dark t)

    (require 'magit)
    (global-set-key (kbd "C-c g s") 'magit-status)
    (global-set-key (kbd "C-c g b") 'magit-blame)
    ;; my little github helper, here for thematic consistency
    (global-set-key (kbd "C-c g h") 'aw/get-github-link)

    ;; Mode specific init
    (load local-mode-init-file-path)

    ;; local directory
    (let ((local-elisp (file-name-as-directory
                        (expand-file-name "~/elisp-local/"))))
      (when (file-directory-p local-elisp)
        (let ((subdirs (directory-files local-elisp)))
          (dolist (s subdirs)
            (when (not (string-match-p "^\\." s))
              (add-to-list 'load-path (concat local-elisp s)))))))
    ;; Datadog
    (require 'datadog)
    (global-set-key (kbd "C-c d d") 'datadog)

    ;; multiple-cursors
    (require 'multiple-cursors)
    (global-set-key (kbd "C-.") 'mc/mark-next-like-this)
    (global-set-key (kbd "C-,") 'mc/mark-previous-like-this)
    (global-set-key (kbd "C-c C-,") 'mc/mark-all-like-this)))

