(require 'gptel)

;; read_buffer
(gptel-make-tool
 :name "read_buffer"                    ; javascript-style snake_case name
 :function (lambda (buffer)                  ; the function that will run
             (unless (buffer-live-p (get-buffer buffer))
               (error "error: buffer %s is not live." buffer))
             (with-current-buffer  buffer
               (buffer-substring-no-properties (point-min) (point-max))))
 :description "return the contents of an emacs buffer"
 :args (list '(:name "buffer"
               :type string            ; :type value must be a symbol
               :description "the name of the buffer whose contents are to be retrieved"))
 :category "emacs")                     ; An arbitrary label for grouping
