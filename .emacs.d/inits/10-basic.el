;; wdired
(require 'wdired)
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)

;; dired
(setq dired-recursive-deletes 'top)
(load "dired-x")
(put 'dired-find-alternate-file 'disabled nil)

;; 行間を広げる
(setq-default line-spacing 1)

;; C-h を backspace として使う
(keyboard-translate ?\C-h ?\C-?)

;; Starting emacs server
(server-start)

;; 右端で折り返す
(setq truncate-partial-width-windows nil)

;; 時刻をモードラインに表示
(display-time)

;;; customizeの出力先設定
(setq custom-file "~/.emacs.d/inits/90-custom-file.el")
(if (file-exists-p (expand-file-name custom-file))
    (load (expand-file-name custom-file) t nil nil))
