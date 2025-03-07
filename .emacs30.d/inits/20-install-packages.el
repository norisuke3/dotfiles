;; インストールされてないパッケージの自動インストール
;; https://qiita.com/catatsuy/items/5f1cd86e2522fd3384a0
(when (require 'package nil t)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
  (package-initialize))
