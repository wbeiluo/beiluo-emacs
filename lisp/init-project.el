;;; init-project.el --- Project configuration -*- lexical-binding: t -*-

;; Copyright (C) 2020~2023 王北洛

;; Author: 王北洛 <wbeiluo@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;; 项目管理配置

;;; Code:

(use-package project
  :bind-keymap ("M-p" . project-prefix-map))

(provide 'init-project)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-project.el ends here
