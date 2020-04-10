;;; init-recentf.el --- Initialize recentf configurations. -*- lexical-binding: t -*-

;; Copyright (C) 2020 王北洛

;; Author: 王北洛 <wbeiluo@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs


;;; Commentary:


;;; Code:

(use-package recentf
  :ensure nil
  :bind ([f5] . recentf-open-files)
  :hook (after-init . recentf-mode)
  :init
  (setq recentf-max-saved-items 100)
  (setq recentf-exclude '((expand-file-name package-user-dir)
                          ".cache"
                          ".cask"
                          ".elfeed"
                          "bookmarks"
                          "cache"
                          "ido.*"
                          "persp-confs"
                          "recentf"
                          "undo-tree-hist"
                          "url"
                          "COMMIT_EDITMSG\\'")))

(provide 'init-recentf)

;;; init-recentf.el ends here
