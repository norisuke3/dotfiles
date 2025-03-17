(require 'gptel)
(require 'mcp)

(defun log-message-to-buffer (buffer-name format-string &rest args)
  "バッファ BUFFER-NAME に書式付きメッセージを挿入します。
FORMAT-STRING は書式文字列、ARGS は書式文字列に渡す引数です。
message 関数と同様に動作しますが、BUFFER-NAME で指定されたバッファに出力し、最後に改行を追加します。"
  (with-current-buffer (get-buffer-create buffer-name)
    (let ((msg (apply 'format format-string args)))
      (insert msg)
      (insert "\n")
      msg)))

(defun log-gptel-message (format-string &rest args)
  "\"*gptel event*\" バッファに書式付きメッセージを挿入し、バッファを表示します。
FORMAT-STRING は書式文字列、ARGS は書式文字列に渡す引数です。
message 関数と同様に動作しますが、\"*gptel event*\" バッファに固定で出力し、最後に改行を追加します。"
  (apply 'log-message-to-buffer "*gptel event*" format-string args))

;; *
;; * local functions
;; *

;; read_url
(gptel-make-tool
 :name "read_url"
 :function (lambda (url)
             (with-current-buffer (url-retrieve-synchronously url)
               (goto-char (point-min)) (forward-paragraph)
               (let ((dom (libxml-parse-html-region (point) (point-max))))
                 (run-at-time 0 nil #'kill-buffer (current-buffer))
                 (with-temp-buffer
                   (shr-insert-document dom)
                   (buffer-substring-no-properties (point-min) (point-max))))))
 :description "Fetch and read the contents of a URL"
 :args (list '(:name "url"
               :type "string"
               :description "The URL to read"))
 :category "web")

;; append_to_buffer
(gptel-make-tool
 :name "append_to_buffer"
 :function (lambda (buffer text)
             (with-current-buffer (get-buffer-create buffer)
               (save-excursion
                 (goto-char (point-max))
                 (insert text)))
             (format "Appended text to buffer %s" buffer))
 :description "Append text to the an Emacs buffer.  If the buffer does not exist, it will be created."
 :args (list '(:name "buffer"
               :type "string"
               :description "The name of the buffer to append text to.")
             '(:name "text"
               :type "string"
               :description "The text to append to the buffer."))
 :category "emacs")

;; echo_message
(gptel-make-tool
 :name "echo_message"
 :function (lambda (text)
             (message "%s" text)
             (format "Message sent: %s" text))
 :description "Send a message to the *Messages* buffer"
 :args (list '(:name "text"
               :type "string"
               :description "The text to send to the messages buffer"))
 :category "emacs")

;; read_buffer
(gptel-make-tool
 :name "read_buffer"
 :function (lambda (buffer)
             (unless (buffer-live-p (get-buffer buffer))
               (error "Error: buffer %s is not live." buffer))
             (with-current-buffer  buffer
               (buffer-substring-no-properties (point-min) (point-max))))
 :description "Return the contents of an Emacs buffer"
 :args (list '(:name "buffer"
               :type "string"
               :description "The name of the buffer whose contents are to be retrieved"))
 :category "emacs")


;; list_directory
(gptel-make-tool
 :name "list_directory"
 :function (lambda (directory)
	     (mapconcat #'identity
                        (directory-files directory)
                        "\n"))
 :description "List the contents of a given directory"
 :args (list '(:name "directory"
	       :type "string"
	       :description "The path to the directory to list"))
 :category "filesystem")

;; make_directory
(gptel-make-tool
 :name "make_directory"
 :function (lambda (parent name)
             (condition-case nil
                 (progn
                   (make-directory (expand-file-name name parent) t)
                   (format "Directory %s created/verified in %s" name parent))
               (error (format "Error creating directory %s in %s" name parent))))
 :description "Create a new directory with the given name in the specified parent directory"
 :args (list '(:name "parent"
	       :type "string"
	       :description "The parent directory where the new directory should be created, e.g. /tmp")
             '(:name "name"
	       :type "string"
	       :description "The name of the new directory to create, e.g. testdir"))
 :category "filesystem")

;; create_file
(gptel-make-tool
 :name "create_file"
 :function (lambda (path filename content)
             (let ((full-path (expand-file-name filename path)))
               (with-temp-buffer
                 (insert content)
                 (write-file full-path))
               (format "Created file %s in %s" filename path)))
 :description "Create a new file with the specified content"
 :args (list '(:name "path"
	       :type "string"
	       :description "The directory where to create the file")
             '(:name "filename"
	       :type "string"
	       :description "The name of the file to create")
             '(:name "content"
	       :type "string"
	       :description "The content to write to the file"))
 :category "filesystem")

;; read_file
(gptel-make-tool
 :name "read_file"
 :function (lambda (filepath)
	     (with-temp-buffer
	       (insert-file-contents (expand-file-name filepath))
	       (buffer-string)))
 :description "Read and display the contents of a file"
 :args (list '(:name "filepath"
	       :type "string"
	       :description "Path to the file to read.  Supports relative paths and ~."))
 :category "filesystem")

