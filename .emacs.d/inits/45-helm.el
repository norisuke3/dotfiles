;; 最近のファイル500個を保存する
(setq recentf-max-saved-items 500)
;; 最近使ったファイルに加えないファイルを
;; 正規表現で指定する
(setq recentf-exclude
      '("/TAGS$" "/var/tmp/" "/vendor/")
      )
;; 情報源の設定
(setq helm-for-files-preferred-list
      '(helm-source-bookmarks
        helm-source-buffers-list
        helm-source-recentf
        helm-source-file-cache
        helm-source-files-in-current-dir
        ;; 必要とあれば
        helm-source-bookmark-set
        helm-source-locate))
