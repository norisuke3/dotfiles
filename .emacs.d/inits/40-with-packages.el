;; use-packageで可読性の高いinit.elを書く
;; (shell-command "open https://qiita.com/kai2nenobu/items/5dfae3767514584f5220")
(use-package anything-startup
  :bind (("C-x C-;" . anything-for-files)
         ;; comment-line が元々 "C-x C-;" に割り当てられていたので "M-;" に移動。
         ("M-;" . comment-line)))

;; #11 Emacs に革命を起こすパッケージ「helm」
;; http://emacs.rubikitch.com/sd1503-helm/ (Software Design 2015年3月号掲載記事)
;; http://emacs.rubikitch.com/sd1504-helm/ (Software Design 2015年4月号掲載記事)
(require 'helm-config)
(global-set-key (kbd "C-c h") 'helm-command-prefix)

;; 初心者〜初級者のための Emacs-Helm 事始め : 前編
;; https://qiita.com/jabberwocky0139/items/86df1d3108e147c69e2c
(use-package helm
  :init
  (helm-mode 1)
  :bind (("C-;" . helm-for-files)
         ("C-'" . helm-swoop)      ;; http://emacs.rubikitch.com/helm-swoop/
         ("C-o" . helm-ag)
         ("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("M-'" . helm-resume)
         ("C-x C-'" . helm-resume)
         ("C-x C-f" . helm-find-files)
         ("C-M-s" . helm-imenu)
         )
  )

(use-package helm-ag
  :init
  (setq helm-ag-base-command "rg --vimgrep --no-heading")
  ;;; 現在のシンボルをデフォルトのクエリにする
  (setq helm-ag-insert-at-point 'symbol)
  ;;; C-M-gはちょうどあいてる
  (global-set-key (kbd "C-M-k") 'backward-kill-sexp) ;推奨
  )

(use-package wdired
  :commands wdired-change-to-wdired-mode
  :init
  (bind-key  "r" 'wdired-change-to-wdired-mode dired-mode-map))
(define-key dired-mode-map (kbd "C-t") 'other-window-or-split)
(define-key dired-mode-map (kbd "C-o") 'helm-ag)
(define-key dired-mode-map (kbd "f") 'helm-find)

;; emacsの矩形選択モード紹介
;; https://techblog.kayac.com/emacs-rectangle.html
(use-package cua-mode
  :init
  (cua-mode t)
  (setq cua-enable-cua-keys nil))

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

(use-package yasnippet
  :init
  (yas-global-mode 1)
  :config
  (require 'dropdown-list)
  (setq yas-prompt-functions '(yas-dropdown-prompt
                               yas-ido-prompt
                               yas-completing-prompt))
  (bind-keys :map yas-minor-mode-map
             ((kbd "C-x i i") . yas-insert-snippet)
             ((kbd "C-x i n") . yas-new-snippet)
             ((kbd "C-x i v") . yas-visit-snippet-file))
  ;; anything-c-yasnippetを使わずにyasnippetをanythingインタフェースで選択する
  ;; http://memo.sugyan.com/entry/20120111/1326288445
  ;; https://github.com/sugyan/dotfiles/blob/master/.emacs.d/inits/04-yasnippet.el
  (defun my-yas/prompt (prompt choices &optional display-fn)
    (let* ((names (loop for choice in choices
                        collect (or (and display-fn (funcall display-fn choice))
                                    choice)))
           (selected (anything-other-buffer
                      `(((name . ,(format "%s" prompt))
                         (candidates . names)
                         (action . (("Insert snippet" . (lambda (arg) arg))))))
                      "*anything yas/prompt*")))
      (if selected
          (let ((n (position selected names :test 'equal)))
            (nth n choices))
        (signal 'quit "user quit!"))))
  (custom-set-variables '(yas/prompt-functions '(my-yas/prompt))))

(use-package yasnippet-config
  :config
  (bind-keys :map yas-minor-mode-map
             ((kbd "C-x y") . yas/register-oneshot-snippet)
             ((kbd "C-x C-y") . yas/expand-oneshot-snippet)
             ))

(defun yas-expand-oneshot-snippet ()
  (interactive)
  (yas-expand-snippet yas/oneshot-snippet))


(use-package auto-complete
  :commands auto-complete
  :init
  (require 'auto-complete-config)
  (global-auto-complete-mode t)
  (setq ac-auto-start 3)
  :config
  (ac-config-default)
  (setq ac-dwim t)
  (bind-keys :map ac-completing-map
             ((kbd "C-n") . ac-next)
             ((kbd "C-p") . ac-previous)))

;; 自動バイトコンパイル
;; http://d.hatena.ne.jp/rubikitch/20100423/bytecomp
(use-package auto-async-byte-compile
  :commands enable-auto-async-byte-compile-mode
  :init
  (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)
  :config
  ;; 自動コンパイルを無効にするファイルの正規表現
  (setq auto-async-byte-compile-exclude-files-regexp "/junk/")
  )

;; (add-hook 'after-init-hook #'global-flycheck-mode)

(use-package org
  :mode (("\\.org$" . org-mode))
  :init
  (bind-keys :map global-map
             ((kbd "\C-cl") . org-store-link)
             ((kbd "\C-ca") . org-agenda))
;;  (setq browse-url-browser-function 'w3m-browse-url)
  (global-set-key "\C-xm" 'browse-url-at-point)
  ;; org-capture
  (define-key global-map "\C-cc" 'org-capture)
  (setq org-capture-templates
        '(("n" "メモ" entry (file+headline "~/Dropbox/org/notes.org" "Notes")
           "* %?\n %U\n %i")
          ("k" "家計簿" entry (file+headline "~/Dropbox/org/kakeibo.org" "家計簿")
           "* %U\n %i" :prepend t)
          ("r" "レシピ" entry (file+headline "~/Dropbox/org/recipe.org" "レシピ")
           "* %U\n %i")
          ("e" "英単語" entry (file+headline "~/Dropbox/org/ewords.org" "英単語")
           "* %U\n %i" :prepend t)
          ))
  :config
  (setq org-agenda-files (list "~/inbox.org"))
  ;; org-mode with w3m browsing
  ;; org todo keywords
  (setq org-todo-keywords '("TODO" "WAIT" "DONE")
        org-todo-interpretation 'sequence)
  (setq org-log-done t)
  (setq org-agenda-custom-commands
        '(("n" occur-tree "nextAction")
          ("p" occur-tree "project")
          ("w" occur-tree "wait")
          ("d" occur-tree "schedule")
          ("s" occur-tree "someday")
          ))
  (setq org-stuck-projects '("+LEVEL=2" ("TODO" "Wait")
                             ("someday" "material" "schedule" "wait" "nextAction")))
  ;;How to fix “Symbol's function definition is void: org-babel-get-header”
  ;; https://emacs.stackexchange.com/questions/37692/how-to-fix-symbols-function-definition-is-void-org-babel-get-header
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (shell . t)
     ;; (python . t)
     ;; (R . t)
     ;; (ruby . t)
     ;; (ditaa . t)
     ;; (dot . t)
     ;; (octave . t)
     ;; (sqlite . t)
     ;; (perl . t)
     ))
  )


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

;; bm.el (行をオレンジ色で色付ける, Bookmark)
;; bm.el
(use-package bm
  :init
  (bind-keys :map global-map
             ([?\C-\M- ]  . bm-toggle)
             ((kbd "M-[") . bm-previous)
             ((kbd "M-]") . bm-next))
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

(use-package restclient
  :mode (("\\.rest\\'" . restclient-mode)))

;; Zen coding by emmet-mode
(use-package emmet-mode
  :commands emmet-mode
  :init
  (add-hook 'sgml-mode-hook 'emmet-mode) ;; マークアップ言語全部で使う
  (add-hook 'html-mode-hook 'emmet-mode) ;; マークアップ言語全部で使う
  (add-hook 'css-mode-hook  'emmet-mode) ;; CSSにも使う
  (add-hook 'jsx-mode-hook 'emmet-mode)  ;; jsx にも使う
  (add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2))) ;; indent はスペース2個
  :config
  (keyboard-translate ?\C-i ?\H-i) ;;C-i と Tabの被りを回避
  (bind-keys :map emmet-mode-keymap
             ("C-j" . nil)                  ;; C-j は newline のままにしておく
             ("H-i" . emmet-expand-line)))

;; js-mode
(add-hook 'js-mode-hook (lambda ()(setq js-indent-level 2)))

(use-package jsx-mode
  :commands jsx-mode
  :mode (("\\.jsx\\'" . jsx-mode))
  :config
  (setq jsx-indent-level 2))

(use-package js-comint
  :commands run-js
  :mode (("\\.js\\'" . js2-mode))
  :init
  (defun inferior-js-mode-hook-setup ()
    (add-hook 'comint-output-filter-functions 'js-comint-process-output))
  (add-hook 'inferior-js-mode-hook 'inferior-js-mode-hook-setup t)
  :config
  (bind-keys :map js2-mode-map
             ("C-x C-e" . js-send-last-sexp)
             ("C-M-x"   . js-send-last-sexp-and-go)
             ("C-c C-l"   . js-send-buffer)
             ("C-c C-b" . js-send-buffer-and-go)
             ("C-c l"   . js-load-file-and-go)))

;; https://github.com/serras/emacs-haskell-tutorial/blob/master/tutorial.md
(use-package haskell-mode
  :commands haskell-mode
  :init
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook #'hindent-mode)
  (custom-set-variables
   '(haskell-process-type 'stack-ghci) ;; Stack の ghci を Emacs から使うのに必要な設定
   '(haskell-tags-on-save t)
   '(haskell-process-suggest-remove-import-lines t)
   '(haskell-process-auto-import-loaded-modules t)
   '(haskell-process-log t)
   )
  :config
  (bind-key [f7] 'haskell-navigate-imports haskell-mode-map))

(eval-after-load 'haskell-mode '(progn
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)))
(eval-after-load 'haskell-cabal '(progn
  (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))

(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))

(use-package solidity-mode)

(use-package key-chord
  :init
  (key-chord-mode 1)
  (setq key-chord-two-keys-delay 0.04)
  (key-chord-define-global "kj" 'view-mode)
  )

(use-package view
  :config
  ;; less 感覚の操作
  (define-key view-mode-map (kbd "N") 'View-search-last-regexp-backward)
  (define-key view-mode-map (kbd "?") 'View-search-regexp-backward)
  (define-key view-mode-map (kbd "G") 'end-of-buffer)
  (define-key view-mode-map (kbd "b") 'View-scroll-page-backward)
  (define-key view-mode-map (kbd "f") 'View-scroll-page-forward)
  ;; vi/w3m 感覚の操作
  (define-key view-mode-map (kbd "h") 'backward-char)
  (define-key view-mode-map (kbd "j") 'next-line)
  (define-key view-mode-map (kbd "k") 'previous-line)
  (define-key view-mode-map (kbd "l") 'forward-char)
  (define-key view-mode-map (kbd "J") 'View-scroll-line-forward)
  (define-key view-mode-map (kbd "K") 'View-scroll-line-backward)
  ;; bm.el の設定
  (define-key view-mode-map (kbd "m") 'bm-toggle)
  (define-key view-mode-map (kbd "[") 'bm-next)
  (define-key view-mode-map (kbd "]") 'bm-previous)
  ;; not occur, but helm-ag
  (define-key view-mode-map (kbd "o") 'helm-ag)
  ;; other-window-or-split
  (define-key view-mode-map (kbd "t") 'other-window-or-split)
  ;; helm-imenu
  (define-key view-mode-map (kbd "i") 'helm-imenu)
  )

(use-package viewer
  :config
  ;; 書き込み不能ファイルでview-modeから抜けなくなる
  (viewer-stay-in-setup)
  ;; モードラインの色を指定する
  (setq viewer-modeline-color-unwritable "tomato")
  (setq viewer-modeline-color-view "orange")
  (viewer-change-modeline-color-setup)
  ;; 特定のファイルを view-mode で開く
  (setq view-mode-by-default-regexp "\\.log$")
  ;; メジャーモードに合わせて view-mode のキーバーインドを設定する
  (define-overriding-view-mode-map emacs-lisp-mode
    ("RET" . find-function-at-point))
  (define-overriding-view-mode-map go-mode
    (";" . go-goto-function-name)
    ("RET" . godef-jump)
    ("," . xref-pop-marker-stack)
    )
  (define-overriding-view-mode-map python-mode ;; for iPython Notebook minor mode
    ("M-p" . ein:worksheet-move-cell-up-km)
    ("M-n" . ein:worksheet-move-cell-down-km)
    ("e" . ein:worksheet-execute-cell-km)
    ("j" . ein:worksheet-goto-next-input-km)
    ("k" . ein:worksheet-goto-prev-input-km)
    ("l" . ein:worksheet-clear-output-km)
    ("n" . ein:worksheet-goto-next-input-km)
    ("p" . ein:worksheet-goto-prev-input-km)
    ("u" . ein:worksheet-toggle-cell-type-km)
    ("w" . ein:worksheet-copy-cell-km)
    ("y" . ein:worksheet-yank-cell-km)
    )
  (define-overriding-view-mode-map ein:markdown-mode
    ("M-p" . ein:worksheet-move-cell-up-km)
    ("M-n" . ein:worksheet-move-cell-down-km)
    ("e" . ein:worksheet-execute-cell-km)
    ("j" . ein:worksheet-goto-next-input-km)
    ("k" . ein:worksheet-goto-prev-input-km)
    ("n" . ein:worksheet-goto-next-input-km)
    ("p" . ein:worksheet-goto-prev-input-km)
    ("u" . ein:worksheet-toggle-cell-type-km)
    ("w" . ein:worksheet-copy-cell-km)
    ("y" . ein:worksheet-yank-cell-km)
    )
  )

(use-package go-mode
  :mode (("\\.go$" . go-mode))
  :init
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  :config
  (require 'go-autocomplete)
  (defun go-build nil
    "go build"
    (interactive)
    (run-shell-command
     "*Shell/Go*" (concat "go run " (buffer-file-name) "\n")))
  (bind-keys :map go-mode-map
             ("C-c C-l" . go-build)))

;; sequential-command
(use-package sequential-command-config
  :config
  (sequential-command-setup-keys)
  )

;; ido-vertical-mode.el : idoの候補を縦に並べ、helmっぽい見た目にする！
;; (shell-command "open http://emacs.rubikitch.com/ido-vertical-mode/")
(use-package ido-mode
  :init
  (setq ido-enable-flex-matching t)
  (setq ido-max-window-height 0.75)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only)
  (ido-vertical-mode 1)
  (ido-mode t)
  )

;; sticky
;; 大文字入力を楽にする (Emacsテクニックバイブル 2-4)
(use-package sticky
  :config
  (use-sticky-key ";" sticky-alist:en))

;; iflipb
;; 詳説！Alt+Tab感覚で賢くバッファをワンタッチで切り替える
;; http://emacs.rubikitch.com/iflipb/
(use-package iflipb
  :bind (("C-," . iflipb-previous-buffer)
         ("C-." . iflipb-next-buffer)))

;; 起動時に遅いので、以下コメントアウト
;; ;; Emacs 上で正しいバージョンの ruby を選択する
;; ;; https://github.com/senny/rvm.el
;; (use-package rvm
;;   :init
;;   (rvm-use-default))

;; ;; Emacs で Pry
;; ;; - http://d.hatena.ne.jp/rubikitch/20140627/pry
;; (use-package inf-ruby
;;   :init
;;   (setq inf-ruby-default-implementation "pry")
;;   (setq inf-ruby-eval-binding "Pry.toplevel_binding")
;;   ;; riなどのエスケープシーケンスを処理し、色付けする
;;   (add-hook 'inf-ruby-mode-hook 'ansi-color-for-comint-mode-on))

(use-package vue-mode
  :init
  (add-hook 'before-save-hook 'delete-trailing-whitespace))
