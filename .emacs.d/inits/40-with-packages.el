;; use-packageで可読性の高いinit.elを書く
;; https://qiita.com/kai2nenobu/items/5dfae3767514584f5220
(use-package anything-startup
  :bind (("C-;" . anything-for-files)))

(use-package wdired
  :commands wdired-change-to-wdired-mode
  :init
  (bind-key  "r" 'wdired-change-to-wdired-mode dired-mode-map))

(use-package kill-summary
  :bind (("M-y" . kill-summary)))

;; Meadow/Emacs memo: ウィンドウ/フレーム関連 ― 分割・サイズ変更
;; http://www.bookshelf.jp/soft/meadow_30.html#SEC404
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

(use-package org
  :mode (("\\.org$" . org-mode))
  :init
  (bind-keys :map global-map
             ((kbd "\C-cl") . org-store-link)
             ((kbd "\C-ca") . org-agenda))
  (setq browse-url-browser-function 'w3m-browse-url)
  (global-set-key "\C-xm" 'browse-url-at-point)
  ;; org-capture
  (define-key global-map "\C-cc" 'org-capture)
  (setq org-capture-templates
        '(("n" "Note" entry (file+headline "~/Dropbox/org/notes.org" "Notes")
           "* %?\n %U\n %i")))
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
  )

;; Emacs 上で正しいバージョンの ruby を選択する
;; https://github.com/senny/rvm.el
(use-package rvm
  :init
  (rvm-use-default))

;; Emacs で Pry
;; - http://d.hatena.ne.jp/rubikitch/20140627/pry
(use-package inf-ruby
  :init
  (setq inf-ruby-default-implementation "pry")
  (setq inf-ruby-eval-binding "Pry.toplevel_binding")
  ;; riなどのエスケープシーケンスを処理し、色付けする
  (add-hook 'inf-ruby-mode-hook 'ansi-color-for-comint-mode-on))

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

(use-package haskell-mode
  :commands haskell-mode
  :init
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode))
