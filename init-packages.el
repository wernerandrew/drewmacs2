;; Use a package manager
;; Requires emacs 24+

(require 'package)
(package-initialize)

(setq package-archives
      '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(setq package-archive-priorities
      '(("melpa-stable" . 10)
        ("gnu-elpa" . 5)
        ("melpa" . 0)))

(defun my-init-packages (packages)
  (when packages
    (let ((this-package (car packages))
	  (rest (cdr packages)))
      (add-to-list 'package-load-list (cons this-package t))
      (my-init-packages rest))))

(defvar local-packages
  '(use-package ;; this is what we're gonna do going forward
    less-css-mode ;; programming modes
    web-mode
    js2-mode
    typescript-mode
    tide
    json-mode
    cython-mode
    enh-ruby-mode
    haskell-mode
    markdown-mode
    go-mode
    puppet-mode
    yaml-mode
    auto-complete ;; misc productivity
    company
    projectile
    epc
    jedi
    elpy
    company-web
    go-autocomplete
    go-eldoc
    ag
    multiple-cursors
    expand-region
    helm
    twittering-mode
    hackernews))

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
(add-to-list 'custom-theme-load-path (relative-to-full-path "local-themes"))

;; Safe to start doing use-package here, I think
(eval-when-compile
  (require 'use-package))

(use-package magit :ensure t :pin manual)
(use-package forge :ensure t :pin manual)

(add-hook
 'after-init-hook
 '(lambda ()
    (setq x-underline-at-descent-line t)
    (load-theme 'blackboard t)

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

    ;; Enable Company mode everywhere
    (require 'company)
    (global-company-mode)
    (setq company-idle-delay 0.1)
    (require 'company-web-html)

    ;; multiple-cursors
    (require 'multiple-cursors)
    (global-set-key (kbd "C-.") 'mc/mark-next-like-this)
    (global-set-key (kbd "C-,") 'mc/mark-previous-like-this)
    (global-set-key (kbd "C-c C-,") 'mc/mark-all-like-this)

    (require 'expand-region)
    (global-set-key (kbd "C-=") 'er/expand-region)))

