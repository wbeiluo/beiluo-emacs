;; init-key-binding.el --- edit configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2022 王北洛

;; Author: 王北洛 <wbeiluo@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;; 常用快捷键
;;

;;; Code:

(global-set-key (kbd "<f5>") 'eshell)                           ; EShell

(global-set-key (kbd "C->") 'forward-word)                      ; 向前移动一个字
(global-set-key (kbd "C-<") 'backward-word)                     ; 向后移动一个字


(provide 'init-key-binding)

;;; init-key-binding.el ends here
