;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mode CONFIGURATION

;; No Toolbar
(if window-system (tool-bar-mode -1))

;; No Scrollbar
(if (fboundp 'tool-scroll-bar) (tool-scroll-bar -1))
(if (fboundp 'toggle-scroll-bar) (toggle-scroll-bar -1))

;; Hide password prompts
(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt)

; copy and paste from X
(setq x-select-enable-clipboard t)

;; Use colorization for all modes
(global-font-lock-mode t)
(font-lock-mode +1)
(cond ((fboundp 'global-font-lock-mode)
       ;; Turn on font-lock in all modes that support it
       (global-font-lock-mode t)
       ;; Maximum colors
       (setq font-lock-maximum-decoration t)))

;; Show the matching immediately, we are in a hurry
(setq show-paren-delay 0)
(show-paren-mode t)

;; Activate the dynamic completion of buffer names
(iswitchb-mode 1)

;; Activate the dynamic completion in the mini-buffer
(icomplete-mode 1)

;; Activate a number of variables
(setq
 ;; avoid GC as much as possible
 gc-cons-threshold 2500000
 ;; no startup message
 inhibit-startup-message t
 ;; no message in the scratch buffer
 initial-scratch-message nil
 ;; do not fill my buffers, you fool
 next-line-add-newlines nil
 ;; keep the window focused on the messages during compilation
 compilation-scroll-output t
 ;; blink the screen instead of beeping
 ;; visible-bell t
 ;; take the CR when killing a line
 kill-whole-line t
 ;; Show all lines, even if the window is not as large as the frame
 truncate-partial-width-windows nil
 ;; Do not keep tracks of the autosaved files
 auto-save-list-file-prefix nil
 ;; Show me empty lines at the end of the buffer
 default-indicate-empty-lines t
 ;; Show me the region until I do something on it
 transient-mark-mode t
 ;; Don't bother me with questions even if "unsafe" local variables
 ;; are set
 enable-local-variables :all

 ;; The backups
 temporary-file-directory "/tmp/"
 vc-make-backup-files t
 backup-directory-alist '((".*" . "~/archives/emacs.backups/"))
 version-control t ;; Use backup files with numbers
 kept-new-versions 10
 kept-old-versions 2
 delete-old-versions t
 backup-by-copying-when-linked t
 )

(setq-default
 ;; Show white spaces at the end of lines
 show-trailing-whitespace t
 ;; Do not show the cursor in non-active window
 cursor-in-non-selected-windows nil
 use-dialog-box nil
 ;; when on a TAB, the cursor has the TAB length
 x-stretch-cursor t
 )

;; Show the column number
(column-number-mode 1)

;; What modes for what file extentions
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(add-to-list 'auto-mode-alist '("\\.txt\\'" . (lambda()
                                                (text-mode)
                                                (auto-fill-mode)
                                                (flyspell-mode))))

; Hippie-expand
(setq hippie-expand-try-functions-list '(try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-expand-line try-complete-lisp-symbol-partially try-complete-lisp-symbol))
(global-set-key (kbd "M-/") 'hippie-expand)

; Auto-revert
(global-auto-revert-mode 1)

; Uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

; Don't echo shell
(defun my-comint-init ()
           (setq comint-process-echoes t))
         (add-hook 'comint-mode-hook 'my-comint-init)

;; Navigation

(defun select-next-window ()
  "Switch to the next window"
  (interactive)
  (select-window (next-window)))

(defun select-previous-window ()
  "Switch to the previous window"
  (interactive)
  (select-window (previous-window)))

(defun ff/scroll-down ()
  "Scroll the buffer down one line and keep the cursor at the same location."
  (interactive)
  (condition-case nil
      (scroll-down 1)
    (error nil)))

(defun ff/scroll-up ()
  "Scroll the buffer up one line and keep the cursor at the same location."
  (interactive)
  (condition-case nil
      (scroll-up 1)
    (error nil)))

(defun ff/comment-and-go-down (arg)
  "Comments and goes down ARG lines."
  (interactive "p")
  (condition-case nil
      (comment-region (point-at-bol) (point-at-eol)) (error nil))
  (next-line 1)
  (if (> arg 1) (ff/comment-and-go-down (1- arg))))

(defun ff/uncomment-and-go-up (arg)
  "Uncomments and goes up ARG lines."
  (interactive "p")
  (condition-case nil
      (uncomment-region (point-at-bol) (point-at-eol)) (error nil))
  (next-line -1)
  (if (> arg 1) (ff/uncomment-and-go-up (1- arg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key mapping

;; Navigation

(global-set-key (kbd "M-}") 'select-next-window)
(global-set-key (kbd "M-{") 'select-previous-window)

;; Map navigation shortcuts to i-j-k-l keys
;; In addition to usual C-p, C-n stuff

(global-set-key (kbd "M-l") 'forward-char)
(global-set-key (kbd "M-j") 'backward-char)
(global-set-key (kbd "M-k") 'next-line)
(global-set-key (kbd "M-i") 'previous-line)
(global-set-key (kbd "M-K") 'end-of-defun)
(global-set-key (kbd "M-I") 'beginning-of-defun)
(global-set-key (kbd "M-J") 'beginning-of-line)
(global-set-key (kbd "M-L") 'end-of-line)
(global-set-key (kbd "C-M-i") 'scroll-up-command)
(global-set-key (kbd "C-M-k") 'scroll-down-command)
(define-key global-map [(meta up)] 'ff/scroll-down)
(define-key global-map [(meta down)] 'ff/scroll-up)
(define-key global-map [(shift down)] 'ff/comment-and-go-down)
(define-key global-map [(shift up)] 'ff/uncomment-and-go-up)

;; Window resizing

(define-key global-map [(control })] 'enlarge-window-horizontally)
(define-key global-map [(control {)] 'shrink-window-horizontally)
(define-key global-map [(control \")] 'enlarge-window)
(define-key global-map [(control :)] 'shrink-window)

;; Commands
(global-set-key (kbd "M-r") 'replace-string)
;; Transposing words is evil
(global-set-key (kbd "M-t") 'replace-regexp)
;; Don't accidentally suspend
(global-set-key (kbd "C-z") nil)
;; Don't accidentally go to highlight modes

;; Info mode

(require 'info nil t)
