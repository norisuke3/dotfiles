;; ~/.emacs.d/site-lisp 以下全部読み込み
(let ((default-directory (expand-file-name "~/.emacs.d/site-lisp")))
  (add-to-list 'load-path default-directory)
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
      (normal-top-level-add-subdirs-to-load-path)))

;; load init.local if it's exists
(load (expand-file-name "~/.emacs.d/init.local") t)

(require 'init-loader)
(setq init-loader-show-log-after-init nil)
(init-loader-load "~/.emacs.d/inits")

;; Customize の出力先設定
;; 【Emacs】init-loaderで初期設定ファイルを整理 | The modern stone age.
;; https://www.yokoweb.net/2017/01/08/emacs-init-loader/
(setq custom-file "~/.emacs.d/inits/35-custom-file.el")
(if (file-exists-p (expand-file-name custom-file))
    (load (expand-file-name custom-file) t nil nil))
