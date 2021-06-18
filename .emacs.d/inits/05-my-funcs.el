;; funcs for go-build defined in 40-with-packages.el
(defun run-shell-command-when-buffer-is-not-open
    (current-buffer command)
  (let ((buffer-name current-buffer))
    (cond ((null (get-buffer buffer-name))
           (shell buffer-name)
           (process-send-string buffer-name command))
          (t
           (shell buffer-name)
           ))))

(defun run-shell-command
    (current-buffer command)
  (let ((buffer-name current-buffer))
    (shell buffer-name)
    (process-send-string buffer-name command)))

;; Open a new buffer
(defun clip-region ()
  "Create a new buffer with the current region."
  (interactive)
  (if (use-region-p)
      (let ((str (buffer-substring (region-beginning) (region-end)))
            (buffer (generate-new-buffer "untitled")))
        (set-buffer buffer)
        (insert str)
        (pop-to-buffer buffer))
    (let ((buffer (get-buffer-create "*scratch*")))
      (pop-to-buffer buffer)
      (lisp-interaction-mode))))

(global-set-key (kbd "C-c n") #'clip-region)
