;; Non mode-specific keyboard mapping goes here

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key mapping

;; Command as meta
(when (boundp 'mac-command-modifier)
  (setq mac-command-modifier 'meta))

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
