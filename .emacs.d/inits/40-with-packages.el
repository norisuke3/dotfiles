;; anything.el
(require 'anything-startup)
(global-set-key "\C-x;" 'anything-for-files)
(define-key global-map [?\C-;] 'anything-for-files)
