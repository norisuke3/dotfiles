(setq-default indent-tabs-mode nil)

;; dired
(setq dired-recursive-deletes 'top)
(load "dired-x")
(put 'dired-find-alternate-file 'disabled nil)

;; 現在行に色を付ける
(global-hl-line-mode t)
(set-face-background 'hl-line "navajo white")

;; バファ履歴を次回Emacs起動時に保存する。
(savehist-mode t)

;; yesと入力するのは面倒なので y で十分
(defalias 'yes-or-no-p 'y-or-n-p)

;; ファイル内のカーソル位置を記憶する
(save-place-mode)

;; 対応するカッコをハイライトする
(show-paren-mode)

;; 現在位置のファイル/URLを開く
(ffap-bindings)

;; goto-line (デフォルトの M-g g は２ストロークなので、M-g に直接割り当てる。)
(global-set-key (kbd "M-g") 'goto-line)


;; 行間を広げる
(setq-default line-spacing 1)

;; 行番号、桁番号をミニバッファに表示
(line-number-mode 1)
(column-number-mode 1)

;; C-h を backspace として使う
(keyboard-translate ?\C-h ?\C-?)

;; Starting emacs server
(server-start)

;; 右端で折り返す
(setq truncate-partial-width-windows nil)

;; 時刻をモードラインに表示
(display-time)

;; color-moccur
(require 'color-moccur)

;; moccur-grep-find
(defun mgf (dir inputs)
  "proxy function to call moccur-grep-find"
  (interactive
   (list (moccur-grep-read-directory)
         (moccur-grep-read-regexp moccur-grep-default-mask)))
  (moccur-grep-find dir inputs))

;; moccur-grep で特定のファイルを対象外としたい
;; http://higepon.hatenablog.com/entry/20080717/1216264518
(setq moccur-grep-exclusion-buffer-name-regexp "\\(vendor\\)\\|\\(\\.git\\)")
;(defun delete-string-match (reg lst)
;  (cond
;   ((null lst) (reverse lst))
;   ((string-match reg (car lst))
;    (delete-string-match reg (cdr lst)))
;   (t (cons (car lst) (delete-string-match reg (cdr lst))))))
(defun delete-string-match (reg lst)
  (let ((ret nil))
    (while lst
      (unless (string-match reg (car lst))
        (setq ret (cons (car lst) ret)))
      (setq lst (cdr lst)))
    (reverse ret)))
(defadvice moccur-search-files (before moccur-search-files-with-exclusion)
  "enable moccur-grep-exclusion-buffer-name-regexp"
  (ad-set-arg 1 (delete-string-match moccur-grep-exclusion-buffer-name-regexp (ad-get-arg 1))))
(ad-enable-advice 'moccur-search-files 'before 'moccur-search-files-with-exclusion)
(ad-activate 'moccur-search-files)

;; EmacsでPATHの設定が引き継がれない問題をエレガントに解決する
;; https://qiita.com/catatsuy/items/3dda714f4c60c435bb25
;; Mac で初めて設定する場合、ログインシェルをzshに変更して再起動する必要がある。
;; システム環境設定 –> ユーザとグループ –> 左下の鍵を外す –> ユーザを右クリック –> 詳細オプション
;; http://keisanbutsuriya.hateblo.jp/entry/2017/06/21/010257
(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by
the user's shell. This is particularly useful under Mac OSX, where GUI apps are
 not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

 (set-exec-path-from-shell-PATH)

;; ウィンドウ間を Shift + arrow で移動する。
;; (windmove-default-keybindings)
;; (setq windmove-wrap-around t)
;; (define-key global-map [?\C->] 'windmove-right)
;; (define-key global-map [?\C-<] 'windmove-lef)t

;; M-r でバッファを再読込する
(defun revert-buffer-no-confirm (&optional force-reverting)
  "Interactive call to revert-buffer. Ignoring the auto-save
 file and not requesting for confirmation. When the current buffer
 is modified, the command refuses to revert it, unless you specify
 the optional argument: force-reverting to true."
  (interactive "P")
  ;;(message "force-reverting value is %s" force-reverting)
  (if (or force-reverting (not (buffer-modified-p)))
      (revert-buffer :ignore-auto :noconfirm)
    (error "The buffer has been modified")))
;; reload buffer
(global-set-key "\M-r" 'revert-buffer-no-confirm)

;; shell-script-mode for .zshrc.loccal
(add-to-list 'auto-mode-alist '("\\.zshrc.local$" . shell-script-mode))

(defun other-window-or-split ()
  (interactive)
  (when (one-window-p) (split-window-horizontally))
  (other-window 1))
(global-set-key (kbd "C-t") 'other-window-or-split)

(global-set-key (kbd "C-x o") 'nil)

;; info
(add-to-list 'Info-directory-list "/Users/norihamamoto/.emacs.d/info")

;; dabbrev-expand
(define-key input-decode-map [?\C-m] [C-m])
(global-set-key (kbd "<C-m>") 'dabbrev-expand)

;; fold-this
(global-set-key (kbd "C-c C-f") 'fold-this)
