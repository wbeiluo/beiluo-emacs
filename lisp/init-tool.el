;;; init-tool.el --- Tool Configuration -*- lexical-binding: t -*-

;; Copyright (C) 2020~2023 王北洛

;; Author: 王北洛 <wbeiluo@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;;; Code:

;; (use-package sdcv
;;   :quelpa (sdcv :fetcher github :repo "manateelazycat/sdcv")
;;   :commands (sdcv-search-pointer+)
;;   :bind ("C-," . sdcv-search-pointer+)
;;   :config
;;   (setq sdcv-say-word-p t)
;;   (setq sdcv-dictionary-data-dir (expand-file-name "~/Backup/stardict/"))
;;   (setq sdcv-dictionary-simple-list
;;         '("懒虫简明英汉词典"
;;           "懒虫简明汉英词典"))
;;   (setq sdcv-dictionary-complete-list
;;         '("朗道英汉字典5.0"
;;           "牛津英汉双解美化版"
;;           "21世纪双语科技词典"
;;           "quick_eng-zh_CN"
;;           "新世纪英汉科技大词典"))
;;   (setq sdcv-tooltip-timeout 10)
;;   (setq sdcv-fail-notify-string "没找到释义")
;;   (setq sdcv-tooltip-border-width 2)
;;   )

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page)
  (bind-keys :map pdf-view-mode-map
             ("\\" . hydra-pdftools/body)
             ("<s-spc>" .  pdf-view-scroll-down-or-next-page)
             ("g"  . pdf-view-first-page)
             ("G"  . pdf-view-last-page)
             ("l"  . image-forward-hscroll)
             ("h"  . image-backward-hscroll)
             ("j"  . pdf-view-next-page)
             ("k"  . pdf-view-previous-page)
             ("e"  . pdf-view-goto-page)
             ("u"  . pdf-view-revert-buffer)
             ("al" . pdf-annot-list-annotations)
             ("ad" . pdf-annot-delete)
             ("aa" . pdf-annot-attachment-dired)
             ("am" . pdf-annot-add-markup-annotation)
             ("at" . pdf-annot-add-text-annotation)
             ("y"  . pdf-view-kill-ring-save)
             ("i"  . pdf-misc-display-metadata)
             ("s"  . pdf-occur)
             ("b"  . pdf-view-set-slice-from-bounding-box)
             ("r"  . pdf-view-reset-slice))
  )

(provide 'init-tool)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-tool.el ends here
