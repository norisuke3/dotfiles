;;; Code:

;;
;; Exclude *Messages* buffer from wipe-windows on a resume.
;;
(defun exclude-message-buffer (buffer)
  (not (string= (buffer-name buffer) "*Messages*")))

(defun wipe-windows-except-messages-buffer (orig &optional no-ask)
  "Kill all buffers except *Messages* buffer. Optional argument NO-ASK non-nil skips query."
  (advice-add 'kill-buffer :before-while #'exclude-message-buffer)
  (apply orig '(no-ask))
  (advice-remove 'kill-buffer #'exclude-message-buffer))

(advice-add 'wipe-windows :around #'wipe-windows-except-messages-buffer)

;;
;; Save a configuration for a resume in the future.
;;
(defcustom win:last-configuration "~/.windows"
  "The last used windows configuration."
  :type 'string)

(win-switch-task t win:last-configuration)

(defun win:save-config (force-save &rest config-file)
  "Save the used windows configuration for a resume in the future."
  (customize-save-variable 'win:last-configuration win:local-config-file))

(advice-add 'win-switch-task :after #'win:save-config)
