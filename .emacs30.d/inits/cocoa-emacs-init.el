;; メタキーの設定
(setq ns-command-modifier (quote meta))

;; Emacs の Welcome message を非表示
(setq inhibit-startup-message t)
;; ‘*scratch*’ バッファを最初に表示する。
(setq initial-buffer-choice t)

;; ツールバーを非表示にする
(tool-bar-mode 0)

;; ウィンドウサイズの設定
(setq default-frame-alist
      (append
       '((foreground-color . "black")
         (background-color . "white")
         (width . 230)
         (height . 71)
         (top . 0)
         (left . 20)
         )
       default-frame-alist))
(setq initial-frame-alist default-frame-alist)

;; /C-z でウィンドウが最小化するのを止める
(put 'suspend-frame 'disabled t)

;; MacOS の finder を 'z' で起動する
(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map "z" 'dired-fiber-find)))

(defun dired-fiber-find ()
  (interactive)
  (let ((file (dired-get-filename)))
    (if (file-directory-p file)
        (start-process "finder" "mac-finder" "open" file))))

;; font 色設定
(set-face-foreground 'font-lock-comment-face "chartreuse4")
(set-face-foreground 'font-lock-string-face "VioletRed")
;;(set-fontset-font t 'japanese-jisx0208 (font-spec :family "hirakaku_w3"))
;;(set-default-font "hirakaku_w6:pixelsize=12:spacing=0")
