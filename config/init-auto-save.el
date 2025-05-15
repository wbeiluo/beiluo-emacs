;;; init-auto-save.el --- Auto save configurations. -*- lexical-binding: t -*-

;; Copyright (C) 2025~2025 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;;  自动保存
;;

;;; Code:

(require 'super-save)
(require 'saveplace)

;; 自动保存文件
(setq auto-save-default nil) ;; 关闭内置的自动保存
(setq super-save-auto-save-when-idle t)
(setq super-save-idle-duration 1)
(setq save-silently t)
;; add integration with ace-window
(add-to-list 'super-save-triggers 'ace-window)
;; save on find-file
(add-to-list 'super-save-hook-triggers 'find-file-hook)
;; Enable super-save-mode
(super-save-mode 1)

;; 保存光标位置
(setq save-place-limit 100)                    ;; 最多保存100个文件位置
(save-place-mode 1)                            ;; 启用模式

(provide 'init-auto-save)

;;; init-auto-save.el ends here
