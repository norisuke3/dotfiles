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

