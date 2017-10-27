;; インストールされてないパッケージの自動インストール
;; https://qiita.com/catatsuy/items/5f1cd86e2522fd3384a0
(require 'cl)

(when (require 'package nil t)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (package-initialize))

(defvar installing-package-list
  '(
    ;; ここに使っているパッケージを書く。
    anything
    auto-async-byte-compile
    auto-complete
    auto-install
    bm
    color-moccur
    dropdown-list
    emmet-mode
    haskell-mode
    inf-ruby
    init-loader
    js2-mode
    jsx-mode
    magit
    restclient
    rvm
    use-package
    w3m
    yasnippet
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
