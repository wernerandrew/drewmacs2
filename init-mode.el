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
(add-to-list 'auto-mode-alist '("\\.mako$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.handlebars$" . web-mode))
;; default to django templating for all HTML files
(add-to-list 'web-mode-engine-file-regexps
             '("django" . "\\.html$"))
(setq web-mode-engines-alist
      '(("ctemplate". "\\.handlebars$")))
;; prefer two spaces for html indent
(defun aw/web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
)
(add-hook 'web-mode-hook  'aw/web-mode-hook)

;; for react
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

;; javascript
(require 'js2-mode)
;; (require 'ac-js2)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq js2-cleanup-whitespace t ;; clear whitespace on save
      js2-mirror-mode nil)     ;; but do NOT match parens.
(add-hook
 'js2-mode-hook
 '(lambda ()
    ;; Don't redefine M-j - we use it to navigate
    (define-key js2-mode-map (kbd "M-j") nil)))
;; Extra autocompletion fun
;; (add-hook 'js2-mode-hook 'ac-js2-mode)

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
              (file-expand-wildcards "~/Dropbox/org/[A-Za-z]*.org")))
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
