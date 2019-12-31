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

;; Moving to preferring company as an autocompletion backend
;; Note that the company initialization happends in the after-init-hook
;; Defined in init-packages.el

;; LSP mode for new fancy autocompletion (outside of web-mode)
(require 'lsp-mode)

;; Info mode
(require 'info nil t)

;; c-mode / c++-mode - prefer 4 spaces
(setq-default c-basic-offset 4)
;; Default .h -> c++ mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.j2$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mako$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.handlebars$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache$" . web-mode))
;; default to django templating for all HTML files
(add-to-list 'web-mode-engine-file-regexps
             '("django" . "\\.html$"))
(add-to-list 'web-mode-engine-file-regexps
             '("django" . "\\.j2$"))
(setq web-mode-engines-alist
      '(("ctemplate" . "\\.handlebars$")
        ("ctemplate" . "\\.mustache$")))

;; prefer two spaces for html / js / css indent
(defun aw/web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))
(add-hook 'web-mode-hook  'aw/web-mode-hook)
(add-hook 'web-mode-hook 'add-node-modules-path)
(add-hook
 'web-mode-hook
 '(lambda ()
    (unless (and (boundp 'web-mode-minor-engine)
                 ;; Exclude django from prettier
                 (string-equal web-mode-minor-engine "django"))
      (prettier-js-mode))))

;; No special alignment for method calls
(add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))

;; for react
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

;; javascript
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq js2-cleanup-whitespace t  ;; clear whitespace on save
      js2-mirror-mode nil       ;; but do NOT match parens.
      js2-basic-offset 2)       ;; 2 space offset
(add-hook
 'js2-mode-hook
 '(lambda ()
    ;; Don't redefine M-j - we use it to navigate
    (define-key js2-mode-map (kbd "M-j") nil)))

;; typescript
(require 'typescript-mode)
(require 'tide)
(add-to-list 'auto-mode-alist '("\\.ts$" . typescript-mode))
(add-hook 'typescript-mode-hook 'add-node-modules-path)
(add-hook 'typescript-mode-hook 'prettier-js-mode)

;; global formatting things
(setq typescript-indent-level 2)
(setq typescript-expr-indent-offset 0)

;; Setup tide on buffer visit
(add-hook 'typescript-mode-hook
          '(lambda ()
             (tide-setup)
             (tide-hl-identifier-mode t)
             (setq-local company-tooltip-align-annotations t)))

;; tide customizations for web-mode
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (setq web-mode-markup-indent-offset 2)
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (tide-setup))))

;; CSS, etc.
(setq css-indent-offset 2)

;; json
(require 'json-mode)
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))

;; ruby
(require 'ruby-mode)
(setq ruby-indent-level 2)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))

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
(setq markdown-fontify-code-blocks-natively t)
(defun aw/markdown-mode-setup ()
  (visual-line-mode))
(add-hook 'markdown-mode-hook 'aw/markdown-mode-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UTILITY CONFIG

;; projectile
(require 'projectile)
(projectile-global-mode)
(setq projectile-completion-system 'ido)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

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

;; Python mode
;; Don't accidentally make python buffer
(add-hook 'python-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-c C-p") nil)))

;; Default virtualenv; can be settable with .dir-locals.el
;; Doesn't work great for switching between projects
(setq aw/python-development-venv "/Users/werner/src/torchweb/venv-dev")
(pyvenv-activate aw/python-development-venv)

;; Python completion library
(require 'lsp-python-ms)
(add-hook 'python-mode-hook #'lsp)

;; Go
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

;; Shell
(require 'tramp)
(ansi-color-for-comint-mode-on)

;; Org-mode
(require 'org)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)

;; todo stuff
(setq org-agenda-files '("~/Dropbox (Personal)/org/torch.org"
                         "~/Dropbox (Personal)/org/recruiting.org"))
(setq org-todo-keywords
      '((sequence "TODO" "|" "DONE" "CANCELLED")))

;; Allow quick task capture
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-capture-templates
      '(("t" "Task" entry (file+datetree "~/Dropbox (Personal)/org/torch.org")
         "**** TODO %?\n     DEADLINE: %t\n")))
(setq org-log-done t)
(add-hook 'org-mode-hook
          '(lambda()
             (visual-line-mode)))

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
