;; gptel のために OpenRouter の API Key を環境変数に設定
(defun get-mac-keychain-password (account service)
  "Retrieve a password from the macOS Keychain."
  (let ((output (shell-command-to-string
                 (format "security find-generic-password -a %s -s %s -w 2>/dev/null"
                         (shell-quote-argument account)
                         (shell-quote-argument service)))))
    (unless (string-empty-p output)
      (string-trim output))))

;; OpenAI
(setq gptel-api-key (get-mac-keychain-password "norisuke3@gmail.com" "OpenAI"))

;; Antropic Calude
;; (setq
;;  gptel-model 'claude-3-sonnet-20240229 ;  "claude-3-opus-20240229" also available
;;  gptel-backend (gptel-make-anthropic "Claude"
;;                  :stream t :key (get-mac-keychain-password "miscs3@gmail.com" "Anthropic")))


;; OpenRouter
(gptel-make-openai "OpenRouter"
  :host "openrouter.ai"
  :endpoint "/api/v1/chat/completions"
  :stream t
  :key (get-mac-keychain-password "norisuke3@gmail.com" "OpenRouter")
  :models '(google/gemini-2.0-flash-001
            google/gemini-2.0-flash-lite-001
            google/gemini-2.0-flash-lite-preview-02-05:free
            google/gemini-2.0-flash-thinking-exp:free
            anthropic/claude-3.7-sonnet:thinking
            anthropic/claude-3.7-sonnet
            anthropic/claude-3.5-sonnet
             openai/gpt-4o-2024-11-20
            openai/gpt-4o-mini
            openai/o3-mini
            openai/o3-mini-high
            perplexity/sonar
            deepseek/deepseek-r1
            deepseek/deepseek-r1:free
            deepseek/deepseek-chat
            deepseek/deepseek-chat:free
            ;; mistralai/mixtral-8x7b-instruct
            ;; meta-llama/codellama-34b-instruct
            ;; codellama/codellama-70b-instruct
            ;; google/palm-2-codechat-bison-32k
            ;; google/gemini-pro
            ))

(global-set-key (kbd "C-c RET") 'gptel-send)


(defun translate-get-text ()
  "選択されたテキストを取得します。pdf-view-mode の場合は cua-copy-region を使用し、
それ以外は buffer-substring を使用します。"
  (let ((str nil))
    (if (eq major-mode 'pdf-view-mode)
        (progn
          (cua-copy-region)
          (setq str (current-kill 0)))
      (if (use-region-p)
          (setq str (buffer-substring (region-beginning) (region-end)))))
    str))

(defun translate (configs)
  "複数の言語への翻訳機能をセットアップします。
CONFIGS は (:lang, :fname, :name, :bind) のプロパティを持つ plist の配列です。"
  (dolist (config configs)
    (let ((lang (plist-get config :lang))
          (fname (plist-get config :fname))
          (name (plist-get config :name))
          (bind (plist-get config :bind)))

      ;; 翻訳関数の定義
      (defalias fname
        `(lambda ()
           ,(format "選択したリージョンを%sに翻訳して新しいバッファに表示します。
翻訳バッファが存在する場合はそれを利用し、存在しない場合は新規に作成します。" name)
           (interactive)
           (let ((str (translate-get-text)))
             (if str
                 (let ((buffer (or (get-buffer "*Translation*") (generate-new-buffer "*Translation*"))))
                   (with-current-buffer buffer
                     ;; コールバックを指定
                     (gptel-request str
                       :system ,(format "translate to %s" lang)
                       :callback (lambda (response info)
                                   (setq buffer-read-only nil)
                                   (goto-char (point-max))
                                   (if (stringp response)
                                       (insert (format "---\n%s\n\n" response))
                                     (insert (format "Error: %s" (plist-get info :status))))
                                   (setq buffer-read-only t)))
                     ;; バッファを読み取り専用に設定
                     (read-only-mode 1)
                     ;; q でquit-windowを呼ぶ
                     (local-set-key (kbd "q") 'quit-window)
                     )
                   ;; バッファを表示
                   (pop-to-buffer buffer))
               (let ((buffer (get-buffer-create "*scratch*")))
                 (pop-to-buffer buffer)
                 (lisp-interaction-mode))))))

      ;; キーバインドの設定
      (global-set-key (kbd bind) fname))))

;; 翻訳先言語とキーバインディング設定
(translate
 (list
  '(:lang "Japanese" :fname translate-to-japanese :name "日本語" :bind "C-c t j")
  '(:lang "Korean" :fname translate-to-korean :name "韓国語" :bind "C-c t k")
  '(:lang "Traditional Chinese" :fname translate-to-tchinese :name "中国語(繁体字)" :bind "C-c t c t")
  '(:lang "Simplified Chinese" :fname translate-to-schinese :name "中国語(簡体字)" :bind "C-c t c s")
  '(:lang "Thai" :fname translate-to-thai :name "タイ語" :bind "C-c t t")
  '(:lang "English" :fname translate-to-english :name "英語" :bind "C-c t e")
  '(:lang "French" :fname translate-to-french :name "フランス語" :bind "C-c t f")
  '(:lang "Spanish" :fname translate-to-spanish :name "スペイン語" :bind "C-c t s")
  '(:lang "German" :fname translate-to-german :name "ドイツ語" :bind "C-c t d")
  '(:lang "Vietnamese" :fname translate-to-vietnamese :name "ベトナム語" :bind "C-c t v")))

;; 旧キーバインドの互換性維持
(global-set-key (kbd "C-c j") #'translate-to-japanese)


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

;; Meadow/Emacs memo: ウィンドウ/フレーム関連 ― 分割・サイズ変更
;; http://www.bookshelf.jp/soft/meadow_30.html#SEC404
;; Note: ウィンドウの分割情報を任意のディレクトリのファイル(.windows)に保存/resume　C-z C-s
(use-package windows
  :commands win:startup-with-window
  :init
  (setq win:switch-prefix "\C-z")
  (define-key global-map win:switch-prefix nil)
  (setq win:base-key ?`)         ;; ` は「直前の状態」
  (setq win:max-configs 27)      ;; ` ～ z は27文字
  (setq win:quick-selection nil) ;; C-c英字 に割り当てない
  :config
  (setq win:use-frame nil)
  (bind-key "\C-xC" 'see-you-again))
(win:startup-with-window)


;; pdf-tools
;; - Emacsでpdfを読む (pdf-tools) (2019.07.17追記) | A perfect autumn day
;;   https://taipapamotohus.com/post/pdf-tools/
(use-package pdf-tools
  :ensure t
  :config
  ;; initialise
  (pdf-tools-install)
  ;; PDF Tools does not work well together with linum-mode
  (add-hook 'pdf-view-mode-hook (lambda() (nlinum-mode -1)))
  ;; 自動的に注釈モードを有効にする
  (add-hook 'pdf-view-mode-hook 'pdf-annot-minor-mode)
  ;; open pdfs scaled to fit page
  ;; (setq-default pdf-view-display-size 'fit-page)
  ;; automatically annotate highlights
  (setq pdf-annot-activate-created-annotations t)
  ;; use normal isearch
  ;; (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  ;; more fine-grained zooming
  (setq pdf-view-resize-factor 1.1)
  )
