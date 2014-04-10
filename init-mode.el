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

;; javascript
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq js2-cleanup-whitespace t ;; clear whitespace on save
      js2-mirror-mode nil)     ;; but do NOT match parens.

;; ruby
(require 'ruby-mode)
(setq ruby-indent-level 2)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))

;; cython
(require 'cython-mode)

;; haskell
(require 'haskell-mode)
(add-hook 'haskell-mode-hook (lambda () (haskell-indent-mode)))

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

;; ag
;; Utility function to help find the ag executable.
;; Needed for OS X environments
(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.

This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string
			  "[ \t\n]*$" ""
			  (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)

;; import ag, if possible
(if (executable-find "ag") ; Require only if executable exists
    (progn
      (require 'ag)
      ;; same buffer for every search
      (setq ag-reuse-buffers t)
      (setq ag-results-pane nil))) ;; disable for now

;; Autocomplete
(require 'auto-complete-config)
(ac-config-default)

;; Python mode
;; Don't accidentally make python buffer
(add-hook 'python-mode-hook
	  '(lambda () (local-set-key (kbd "C-c C-p") nil)))

;; JEDI
;; This is a bit of a doozy
(defun guess-best-python-root-for-buffer (buf)
  "Guesses that the python root is the less 'deep' of either:
     -- the root directory of the repository, or
     -- the directory before the first directory after the root
	having an __init__.py file."
  (interactive)
  (let ((found-project-root nil)
	(found-init-py nil)
	(search-depth 0)
	(root-depth nil)
	(max-search-depth 0)
	(base-directory (file-name-directory (buffer-file-name buf))))
    (setq max-search-depth (length (split-string base-directory "/")))

    (defun path-at-depth (base depth-val)
      (if (> depth-val 0)
	  (concat (path-at-depth base (- depth-val 1)) "../")
	base))

    (defun exists-at-current-depth (fname)
      (file-exists-p (concat (path-at-depth base-directory search-depth) fname)))

    ;; head down to the root of the git repo
    (while (and (< search-depth max-search-depth) (not found-project-root))
      (if (exists-at-current-depth ".git") ; add others?
	  (setq found-project-root t)
	(setq search-depth (+ search-depth 1))))
    (setq root-depth search-depth)
    ;; heading back, pick the directory _before_ the first
    ;; directory having an __init__.py file
    (while (and (>= search-depth 0) (not found-init-py))
      (if (exists-at-current-depth "__init__.py")
	  (progn
	    ;; if it's not the root, then go back a directory
	    (if (not (eq search-depth root-depth))
		(setq search-depth (+ search-depth 1)))
	    (setq found-init-py t))
	(setq search-depth (- search-depth 1))))
    ;; return the full absolute path at the search depth
    (path-at-depth base-directory search-depth)
    )
  )

(defun setup-jedi-extra-args ()
  (let ((project-base (guess-best-python-root-for-buffer (current-buffer))))
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
	     (local-set-key (kbd "M-.") 'jedi:goto-definition)
	     (local-set-key (kbd "M-,") 'jedi:get-in-function-call)))
