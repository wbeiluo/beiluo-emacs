;; init-key-binding.el --- edit configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2022 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;; 常用快捷键
;;

;;; Code:

(use-package emacs
  :bind (("<f5>" . eshell)
         ("<f6>" . shell)
         ("<f7>" . recentf-open-files)
         ("C-c t t" . insert-current-time)
         ("C-c t d" . insert-current-data-time)
         ("C-c t w" . insert-current-data-week-time)
         ))

(provide 'init-key-binding)

;;; init-key-binding.el ends here
