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
;; https://stackoverflow.com/questions/25791605/emacs-how-do-i-create-a-new-empty-buffer-whenever-creating-a-new-frame
(defun lunaryorn-new-buffer-frame ()
    "Create a new frame with a new empty buffer."
  (interactive)
  (if (use-region-p)
      (copy-to-new-buffer
       (save-excursion
         (goto-char (region-beginning))
         (line-beginning-position))
       (save-excursion
         (goto-char (region-end))
         (line-end-position)))
    (let ((buffer (generate-new-buffer "untitled")))
      (pop-to-buffer buffer))))

(global-set-key (kbd "C-c n") #'lunaryorn-new-buffer-frame)

(defun copy-to-new-buffer (beginning end)
  (progn
    (kill-ring-save beginning end)
    (let ((buffer (generate-new-buffer "untitled")))
      (pop-to-buffer buffer))
    (yank)))

