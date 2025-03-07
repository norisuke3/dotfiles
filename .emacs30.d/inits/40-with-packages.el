;; gptel のために OpenRouter の API Key を環境変数に設定
(defun get-mac-keychain-password (account service)
  "Retrieve a password from the macOS Keychain."
  (let ((output (shell-command-to-string
                 (format "security find-generic-password -a %s -s %s -w 2>/dev/null"
                         (shell-quote-argument account)
                         (shell-quote-argument service)))))
    (unless (string-empty-p output)
      (string-trim output))))

;; キーチェーンからAPIキーを取得し、環境変数に設定
(let ((api-key (get-mac-keychain-password "norisuke3@gmail.com" "OpenRouter")))
  (when api-key
    (setenv "OPENROUTER_API_KEY_GPTEL" api-key)))

;; gptel
(setq gptel-model   'google/gemini-2.0-flash-001
    gptel-backend
    (gptel-make-openai "OpenRouter"
      :host "openrouter.ai"
      :endpoint "/api/v1/chat/completions"
      :stream t
      :key (lambda () (getenv "OPENROUTER_API_KEY_GPTEL"))
      :models '(google/gemini-2.0-flash-001
                google/gemini-2.0-flash-thinking-exp:free
                anthropic/claude-3.7-sonnet:thinking
                anthropic/claude-3.7-sonnet
                openai/o3-mini
                openai/o3-mini-high
                perplexity/llama-3.1-sonar-large-128k-online
                deepseek/deepseek-r1
                deepseek/deepseek-r1:free
                deepseek/deepseek-chat
                deepseek/deepseek-chat:free
                ;; mistralai/mixtral-8x7b-instruct
                ;; meta-llama/codellama-34b-instruct
                ;; codellama/codellama-70b-instruct
                ;; google/palm-2-codechat-bison-32k
                ;; google/gemini-pro
                )))
(global-set-key (kbd "C-c RET") 'gptel-send)

;; #11 Emacs に革命を起こすパッケージ「helm」
;; http://emacs.rubikitch.com/sd1503-helm/ (Software Design 2015年3月号掲載記事)
;; http://emacs.rubikitch.com/sd1504-helm/ (Software Design 2015年4月号掲載記事)
;; (require 'helm-config)                                ;; 新しい Version では、helm-config が無くてエラーになる。
;; (global-set-key (kbd "C-c h") 'helmv-command-prefix)  ;; Describe variable 'helm-command-map' 参照。
                                                         ;; helmv-command-prefixは無くなったみたい。

;; 初心者〜初級者のための Emacs-Helm 事始め : 前編
;; https://qiita.com/jabberwocky0139/items/86df1d3108e147c69e2c
(use-package helm
  :init
  (helm-mode 1)
  (setq helm-split-window-default-side 'right)
  :bind (("C-;" . helm-for-files)
         ("C-'" . helm-swoop)      ;; http://emacs.rubikitch.com/helm-swoop/
         ("C-o" . helm-ag)
         ("C-c o" . helm-org-agenda-files-headings)
         ("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("M-'" . helm-resume)
         ("C-x C-'" . helm-resume)
         ("C-x C-f" . helm-find-files)
         ("C-M-s" . helm-imenu)
         ("M-{" . helm-bm)
         )
  )

;; from isearch to helm-swoop by M-i
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)

(use-package helm-ag
  :init
  (setq helm-ag-base-command "rg --vimgrep --no-heading")
  ;;; 現在のシンボルをデフォルトのクエリにする
  (setq helm-ag-insert-at-point 'symbol)
  ;;; C-M-gはちょうどあいてる
  (global-set-key (kbd "C-M-k") 'backward-kill-sexp) ;推奨
  )

;; helm-ghq
(require 'helm-ghq)


(use-package wdired
  :commands wdired-change-to-wdired-mode
  :init
  (bind-key  "r" 'wdired-change-to-wdired-mode dired-mode-map))

(define-key dired-mode-map (kbd "C-t") 'other-window-or-split)
(define-key dired-mode-map (kbd "C-o") 'helm-ag)
(define-key dired-mode-map (kbd "f") 'helm-find)


;; migemo 設定
(require 'migemo)
(setq migemo-command "cmigemo")
(setq migemo-options '("-q" "--emacs"))
(setq migemo-dictionary "/opt/homebrew/Cellar/cmigemo/20110227/share/migemo/utf-8/migemo-dict")
(setq migemo-user-dictionary nil)
(setq migemo-regex-dictionary nil)
(setq migemo-coding-system 'utf-8-unix)
(load-library "migemo")
(migemo-init)

;; iflipb
;; 詳説！Alt+Tab感覚で賢くバッファをワンタッチで切り替える
;; http://emacs.rubikitch.com/iflipb/
(use-package iflipb
  :ensure
  :bind (("C-," . iflipb-previous-buffer)
         ("C-." . iflipb-next-buffer)))

;; bm.el (行をオレンジ色で色付ける, Bookmark)
(use-package bm
  :init
  (bind-keys :map global-map
             ([?\C-\M- ]  . bm-toggle)
             ((kbd "M-[") . bm-next)
             ((kbd "M-]") . bm-previous))
  ;; マークのセーブ
  (setq-default bm-buffer-persistence t)
  ;; セーブファイル名の設定
  (setq bm-repository-file "~/.emacs.d/.bm-repository")
  ;; 起動時に設定のロード
  (setq bm-restore-repository-on-load t)
  (add-hook 'after-init-hook 'bm-repository-load)
  (add-hook 'find-file-hooks 'bm-buffer-restore)
  (add-hook 'after-revert-hook 'bm-buffer-restore)
  :config
  (set-face-background 'bm-persistent-face "Orange")
  ;; 設定ファイルのセーブ
  (add-hook 'kill-buffer-hook 'bm-buffer-save)
  (add-hook 'auto-save-hook 'bm-buffer-save)
  (add-hook 'after-save-hook 'bm-buffer-save)
  (add-hook 'vc-before-checkin-hook 'bm-buffer-save)
  ;; Saving the repository to file when on exit
  ;; kill-buffer-hook is not called when emacs is killed, so we
  ;; must save all bookmarks first
  (add-hook 'kill-emacs-hook '(lambda nil
                                (bm-buffer-save-all)
                                (bm-repository-save))))

;; org-mode
(use-package org
  :config
  (bind-keys :map org-mode-map
             ((kbd "C-'") . helm-swoop)
             ("M-{" . helm-bm)
             ((kbd "C-c RET") . gptel-send)
             ) ;; overriding org-mode default mapping
  )

;; カーソルの位置を戻す
;; https://qiita.com/icb54615/items/5ff996a5c708631824aa
;; 「Emacsテクニックバイブル」P115
(use-package point-undo
  :bind (([f7] . point-undo)
         ([S-f7] . point-redo)))

;; 最後の変更箇所にジャンプする
;; 「Emacsテクニックバイブル」P117
(use-package goto-chg
  :bind (([f6] . goto-last-change)
         ([S-f6] . goto-last-change-reverse)))

;; sequential-command
(use-package sequential-command-config
  :config
  (sequential-command-setup-keys)
  )


;; 以下、use-package の upgrade で use-package がエラーを出すようになったので、直接設定を行うことにした。
;; ido-vertical-mode.el : idoの候補を縦に並べ、helmっぽい見た目にする！
;; (shell-command "open http://emacs.rubikitch.com/ido-vertical-mode/")
;; (use-package ido-mode
;;   :init
;;   (setq ido-enable-flex-matching t)
;;   (setq ido-max-window-height 0.75)
;;   (setq ido-vertical-define-keys 'C-n-and-C-p-only)
;;   (ido-vertical-mode 1)
;;   (ido-mode t)
;;   )
(setq ido-enable-flex-matching t)
(setq ido-max-window-height 0.75)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)
(ido-vertical-mode 1)
(ido-mode t)

;; sticky
;; 大文字入力を楽にする (Emacsテクニックバイブル 2-4)
(use-package sticky
  :config
  (use-sticky-key ";" sticky-alist:en))

;; restclient
(use-package restclient
  :mode (("\\.rest\\'" . restclient-mode)))

;; magit
(use-package magit
  :init
  (bind-key "C-x g" 'magit-status)
  :config
  ;; http://yamakichi.hatenablog.com/entry/2016/06/29/133246
  (custom-set-faces
   ;; '(magit-diff-added ((t (:background "black" :foreground "green"))))
   ;; '(magit-diff-added-highlight ((t (:background "white" :foreground "green"))))
   ;; '(magit-diff-removed ((t (:background "black" :foreground "blue"))))
   ;; '(magit-diff-removed-hightlight ((t (:background "white" :foreground "blue"))))
   '(magit-hash ((t (:foreground "red")))))
  )

;; cua-mode
(cua-mode t)
(setq cua-enable-cua-keys nil)
