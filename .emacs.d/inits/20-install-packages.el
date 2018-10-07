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
    haskell-mode
    helm
    helm-swoop
    helm-w3m
    hindent
    inf-ruby
    init-loader
    js2-mode
    js-comint
    jsx-mode
    key-chord
    magit
    migemo
    restclient
    rvm
    recentf-ext
    solidity-mode
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
;; auto-install (http://rubikitch.com/package-initialize/)
;; 以下のコマンドが使えるようになる。
;; M-x install-elisp URL
;; M-x install-elisp-from-emacswiki EmacsWikiのページ名
;; M-x install-elisp-from-gist gist-id
(use-package auto-install
  :config
  (setq auto-install-directory "~/.emacs.d/site-lisp/")
  (auto-install-compatibility-setup))
