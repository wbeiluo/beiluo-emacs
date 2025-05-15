;; init-edit.el --- edit configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2020~2025 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;; 光标移动及文本编辑操作
;;

;;; Code:

(require 'drag-stuff)
(require 'multiple-cursors)

;; 文本移动
;; Drag stuff (lines, words, region, etc...) around
;; <M-up> <M-down> <M-right> <M-left>
(drag-stuff-define-keys)
;; Enable drag-stuff globally
(drag-stuff-global-mode 1)

(provide 'init-edit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-edit.el ends here
