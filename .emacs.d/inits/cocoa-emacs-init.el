;; メタキーの設定
(setq ns-command-modifier (quote meta))

;; Emacs の Welcome message を非表示
(setq inhibit-startup-message t)

;; ツールバーを非表示にする
(tool-bar-mode 0)

;; ウィンドウサイズの設定
(setq default-frame-alist
      (append
       '((foreground-color . "black")
         (background-color . "white")
         (width . 199)
         (height . 70)
         (top . 0)
         (left . 100)
         )
       default-frame-alist))

;; /C-z でウィンドウが最小化するのを止める
(put 'suspend-frame 'disabled t)
