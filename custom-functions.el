;; Here we define functions used elsewhere in the config

;; Helper to find the best project root
(defun aw/guess-best-root-for-buffer (buf repo-sentry &optional init-sentry)
  "Guesses that the python root is the less 'deep' of either:
     -- the root directory of the repository, or
     -- the directory before the first directory after the root
        having an __init__.py file."

  ;; make list of directories from root, removing empty
  (defun make-dir-list (path)
    (delq nil (mapcar (lambda (x) (and (not (string= x "")) x))
                      (split-string path "/"))))
  ;; convert a list of directories to a path starting at "/"
  (defun dir-list-to-path (dirs)
    (mapconcat 'identity (cons "" dirs) "/"))
  ;; a little something to try to find the "best" root directory
  (defun try-find-best-root (base-dir buffer-dir current)
    (cond
     (base-dir ;; traverse until we reach the base
      (try-find-best-root (cdr base-dir) (cdr buffer-dir)
                          (append current (list (car buffer-dir)))))

     (buffer-dir ;; try until we hit the current directory
      (let* ((next-dir (append current (list (car buffer-dir))))
             (sentry-file (concat (dir-list-to-path next-dir) "/" init-sentry)))
        (if (file-exists-p sentry-file)
            (dir-list-to-path current)
          (try-find-best-root nil (cdr buffer-dir) next-dir))))

     (t nil)))

  (let* ((buffer-dir (expand-file-name (file-name-directory (buffer-file-name buf))))
         (vc-root-dir (vc-find-root buffer-dir repo-sentry)))
    (if (and init-sentry vc-root-dir)
        (try-find-best-root
         (make-dir-list (expand-file-name vc-root-dir))
         (make-dir-list buffer-dir)
         '())
      vc-root-dir))) ;; default to vc root if sentry not given

;; Various UI Helpers

;; We will bind these functions to keys
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

;; Shell helpers
(defun create-shell (shell-name)
  (interactive "sEnter shell name: ")
  (let ((new-shell (concat "*" shell-name "*")))
    (if (not (get-buffer new-shell))
        (shell (switch-to-buffer new-shell))
      (error (format "Buffer %s already exists" new-shell)))))
