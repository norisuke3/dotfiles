(defvar anything-c-source-print-test
  '((name . "Print test")
    (candidates . (lambda () '("Foo" "Bar" "Buzz")))
    (action ("Display Message" . message))))

(defun my-anything ()
  (interactive)
  (anything-other-buffer '(anything-c-source-print-test)
                         "My Anything"))
