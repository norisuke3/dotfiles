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

;; Ctrl-t で window split/move
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p) (split-window-horizontally))
  (other-window 1))
(global-set-key (kbd "C-t") 'other-window-or-split)

;; Read Only ファイルを開いた時に view-mode にする。
(setq view-read-only t)
