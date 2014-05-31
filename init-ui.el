;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mode CONFIGURATION

;; General keyboard and mouse config
(define-key function-key-map "\e[1;2A" [S-up])

(require 'mouse)
(xterm-mouse-mode t)
(defun track-mouse (e))
(setq mouse-sel-mode t)

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
