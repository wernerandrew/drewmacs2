;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MAJOR MODES

;; Global
;; Never, ever use tabs
(setq-default indent-tabs-mode nil)

;; Text mode
(add-to-list 'auto-mode-alist '("\\.txt\\'" . (lambda()
                                                (text-mode)
                                                (auto-fill-mode)
                                                (flyspell-mode))))

;; Info mode
(require 'info nil t)

;; c-mode / c++-mode - prefer 4 spaces
(setq-default c-basic-offset 4)
;; Default .h -> c++ mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ejs$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mako$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.handlebars$" . web-mode))
;; default to django templating for all HTML files
(add-to-list 'web-mode-engine-file-regexps
             '("django" . "\\.html$"))
(setq web-mode-engines-alist
      '(("ctemplate". "\\.handlebars$")))
;; for react
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

;; pyxl-mode
(require 'pyxl-mode)
(add-to-list 'auto-mode-alist '("\\.py$" . pyxl-mode))

;; javascript
(require 'js2-mode)
;; (require 'ac-js2)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq js2-cleanup-whitespace t ;; clear whitespace on save
      js2-mirror-mode nil)     ;; but do NOT match parens.
(add-hook 'js2-mode-hook
          '(lambda ()
             ;; Don't redefine M-j - we use it to navigate
             (define-key js2-mode-map (kbd "M-j") nil)))
;; Extra autocompletion fun
;; (add-hook 'js2-mode-hook 'ac-js2-mode)

;; typescript
(require 'typescript)
(setq typescript-indent-level 2)
(add-hook 'typescript-mode-hook
          '(lambda ()
             (define-key typescript-mode-map (kbd "M-.") nil)))
(add-to-list 'auto-mode-alist '("\\.ts$" . typescript-mode))

(require 'tss)
(setq tss-popup-help-key "M-?")
(setq tss-jump-to-definition-key "M-.")
(tss-config-default)

;; ruby
(require 'ruby-mode)
(setq ruby-indent-level 2)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))

;; php
(require 'php-mode)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))

;; cython
(require 'cython-mode)

;; haskell
(require 'haskell-mode)
(defun aw/haskell-mode-setup ()
  (haskell-indent-mode)
  (make-local-variable 'aw/auto-align-list)
  (setq aw/auto-align-list '("=" "->" "|")))
(add-hook 'haskell-mode-hook 'aw/haskell-mode-setup)

;; markdown
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; lua
(require 'lua-mode)
(setq lua-indent-level 2)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UTILITY CONFIG

;; projectile
(require 'projectile)
(projectile-global-mode)
(setq projectile-completion-system 'ido)

;; Display ido results vertically, rather than horizontally
(setq ido-decorations
      (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]"
	      " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))

(defun ido-disable-line-truncation () (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)
(defun ido-define-keys () ;; C-n/p is more intuitive in vertical layout
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)
  ;; and include our custom ones also
  (define-key ido-completion-map (kbd "M-k") 'ido-next-match)
  (define-key ido-completion-map (kbd "M-i") 'ido-prev-match))
(add-hook 'ido-setup-hook 'ido-define-keys)

;; ag, if we can find the executable
(if (executable-find "ag") ; Require only if executable exists
    (progn
      (require 'ag)
      ;; same buffer for every search
      (setq ag-reuse-buffers t)
      (setq ag-results-pane nil))) ;; disable for now

;; Autocomplete
(require 'auto-complete-config)
(ac-config-default)
;; Show menu more quickly after autocomplete starts
(setq ac-auto-show-menu (* ac-delay 2))
;; custom modes for autocomplete
(add-to-list 'ac-modes 'typescript-mode)

;; Flymake - show in minibuffer when over
(defun aw/flymake-show-help ()
  (when (get-char-property (point) 'flymake-overlay)
   (let ((help (get-char-property (point) 'help-echo)))
    (when help (popup-tip help)))))

(add-hook 'post-command-hook 'aw/flymake-show-help)

;; Python mode
;; Don't accidentally make python buffer
(add-hook 'python-mode-hook
	  '(lambda () (local-set-key (kbd "C-c C-p") nil)))

;; Jedi
;; This is a bit of a doozy
;; Requires the aw/guess-best-root-for-buffer defined in
;; custom-functions.el

(defun setup-jedi-extra-args ()
  (let ((project-base (aw/guess-best-root-for-buffer
                       (current-buffer) ".git" "__init__.py")))
    (make-local-variable 'jedi:server-args)
    (when project-base (set 'jedi:server-args (list "--sys-path" project-base)))))

(require 'jedi)
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)
;; Only manually see in function tooltip
(setq jedi:get-in-function-call-delay 10000000)
(setq jedi:server-command
      (list (executable-find "python")
	    (cadr jedi:server-command)))
(add-to-list 'ac-sources 'ac-source-jedi-direct)
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook 'setup-jedi-extra-args)
;; jedi-specific keybindings
(add-hook 'python-mode-hook
	  '(lambda ()
             (local-set-key (kbd "M-?") 'jedi:show-doc)
	     (local-set-key (kbd "M-.") 'jedi:goto-definition)
	     (local-set-key (kbd "M-,") 'jedi:goto-definition-pop-marker)
             (local-set-key (kbd "M-/") 'jedi:get-in-function-call)))

;; Go
(require 'go-autocomplete)
(require 'go-eldoc)

(add-hook 'go-mode-hook
          '(lambda ()
             ;; eldoc stuff
             (go-eldoc-setup)

             ;; gofmt
             (add-hook 'before-save-hook 'gofmt-before-save)

             ;; keys
             (local-set-key (kbd "M-.") 'godef-jump)
             (local-set-key (kbd "M-,") 'pop-tag-mark)
             (local-set-key (kbd "M-?") 'godoc-at-point)))

;; Java / Scala

;; Someday...someday...

;; (require 'eclim)
;; (require 'ac-emacs-eclim-source)
;; (ac-emacs-eclim-config)
;; (require 'eclimd)
;; ;; don't wait for startup
;; (setq eclimd-wait-for-process nil)
;; (add-hook 'java-mode-hook
          ;; (lambda ()
            ;; (unless eclimd-process
              ;; (let* ((root-dir (expand-file-name
                                ;; (vc-find-root (buffer-file-name (current-buffer))
                                              ;; ".git")))
                     ;; (project-dir (read-directory-name "Enter project directory: "
                                                       ;; root-dir nil t)))
                ;; (start-eclimd project-dir)))))



;; Shell
(require 'tramp)
(ansi-color-for-comint-mode-on)

;; Org-mode
(require 'org)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
;; little hack to avoid lockfiles
(setq org-agenda-files
      (mapcar 'expand-file-name
              (file-expand-wildcards "~/Dropbox (Dropbox)/org/[A-Za-z]*.org")))
(setq org-log-done t)

;; EWW
(add-hook 'eww-mode-hook
          '(lambda ()
             (setq show-trailing-whitespace nil)))

;; Twittering
(require 'twittering-mode)
(add-hook 'twittering-mode-hook
          (lambda ()
            (setq show-trailing-whitespace nil)))

;; Hacker News
(require 'hackernews)
