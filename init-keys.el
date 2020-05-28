;; Non mode-specific keyboard mapping goes here

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key mapping

;; Command as meta
(when (boundp 'mac-command-modifier)
  (setq mac-command-modifier 'meta))

;; Window splitting
;; Rebind these since rarely used
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-below)
(global-set-key (kbd "M-3") 'split-window-right)
(global-set-key (kbd "M-+") 'balance-windows)

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
;; Hack to get around <select> mapping
(when (null window-system)
  (define-key global-map [(select)] 'ff/uncomment-and-go-up))

;; Window resizing

(define-key global-map [(control })] 'enlarge-window-horizontally)
(define-key global-map [(control {)] 'shrink-window-horizontally)
(define-key global-map [(control \")] 'enlarge-window)
(define-key global-map [(control :)] 'shrink-window)

;; Commands
(global-set-key (kbd "M-r") 'replace-string)
(global-set-key (kbd "C-|") 'aw/align-vertical-region)
;; Transposing words is evil
(global-set-key (kbd "M-t") 'replace-regexp)
;; Don't accidentally suspend
(global-set-key (kbd "C-z") nil)

;; Misc useful
(global-set-key (kbd "C-c o") 'aw/open-buffer-dir)
