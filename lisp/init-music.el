;;; init-music.el --- 网易云音乐配置 -*- lexical-binding: t -*-

;; Copyright (C) 2021 王北洛

;; Author: 王北洛 <beiluo.wang@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;; Code:

(use-package request :ensure t)

;; Add it to load path
;(add-to-list 'load-path "~/.emacs.d/extensions/netease-cloud-music.el/")

(use-package netease-cloud-music
  :load-path "extensions/netease-cloud-music.el/"
  :bind ("C-c m" . netease-cloud-music))

;; Require it
;(require 'netease-cloud-music)


(provide 'init-music)

;;; init-music.el ends here
