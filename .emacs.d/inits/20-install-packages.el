;; インストールされてないパッケージの自動インストール
;; https://qiita.com/catatsuy/items/5f1cd86e2522fd3384a0
(require 'cl)

(when (require 'package nil t)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (package-initialize))

(defvar installing-package-list
  '(
    ;; ここに使っているパッケージを書く。
    ace-isearch
    ace-jump-mode
    ag
    anything
    auto-async-byte-compile
    auto-complete
    auto-install
    bm
    color-moccur
    company-ghc
    dropdown-list
    emmet-mode
    flycheck
    ghc
    go-mode
    go-autocomplete
    haskell-mode
    helm
    helm-swoop
    helm-w3m
    helm-ag
    hindent
    ido-vertical-mode
    inf-ruby
    init-loader
    js2-mode
    js-comint
    jsx-mode
    key-chord
    magit
    migemo
    minor-mode-hack
    ob-mongo
    restclient
    rvm
    recentf-ext
    sequential-command
    smex
    solidity-mode
    sticky
    use-package
    viewer
    w3m
    yasnippet
    yaml-mode
    ))

(let ((not-installed (loop for x in installing-package-list
                            when (not (package-installed-p x))
                            collect x)))
  (when not-installed
    (package-refresh-contents)
    (dolist (pkg not-installed)
        (package-install pkg))))

;; package に登録されていない elファイルのインストール
;; auto-install (shell-command "open http://rubikitch.com/package-initialize/")
;; 以下のコマンドが使えるようになる。
;; M-x install-elisp URL
;; M-x install-elisp-from-emacswiki EmacsWikiのページ名
;; M-x install-elisp-from-gist gist-id
(require 'auto-install)
(setq auto-install-directory "~/.emacs.d/site-lisp/")
(auto-install-compatibility-setup)
