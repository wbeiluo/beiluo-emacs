;;; init-save-restore.el --- Auto save and restore configurations. -*- lexical-binding: t -*-

;; Copyright (C) 2025~2026 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;;  自动保存与恢复
;;

;;; Code:

;; 自动保存文件
(use-package super-save
  :ensure t
  :custom
  (auto-save-default nil)
  (super-save-auto-save-when-idle t)
  (super-save-idle-duration 1)
  (save-silently t)
  :config
  ;; add integration with ace-window
  (add-to-list 'super-save-triggers 'ace-window)
  ;; save on find-file
  (add-to-list 'super-save-hook-triggers 'find-file-hook)
  ;; Enable super-save-mode
  (super-save-mode 1))

;; 保存光标位置
(use-package saveplace
  :custom
  (save-place-limit 100)
  :config
  (save-place-mode 1))

;; Undo
(use-package vundo
  :ensure t
  :bind ("C-x u" . vundo)
  :custom
  (vundo-glyph-alist vundo-unicode-symbols))

(provide 'init-save-restore)
;;; init-auto-save.el ends here
