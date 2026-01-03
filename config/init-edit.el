;; init-edit.el --- edit configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2020~2025 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;; 光标移动及文本编辑操作
;;

;;; Code:

(defconst extensions-drag-stuff-dir
  (expand-file-name "extensions/drag-stuff" user-emacs-directory))
(defconst extensions-multiple-cursors-dir
  (expand-file-name "extensions/multiple-cursors" user-emacs-directory))

;; 文本移动
;; Drag stuff (lines, words, region, etc...) around
(use-package drag-stuff
  :ensure nil
  :load-path extensions-drag-stuff-dir
  :config
  ;; <M-up> <M-down> <M-right> <M-left>
  (drag-stuff-define-keys)
  ;; Enable drag-stuff globally
  (drag-stuff-global-mode 1))

;; 多行同步编辑
(use-package multiple-cursors
  :ensure nil
  :load-path extensions-multiple-cursors-dir
  :bind (("C-c C-c SPC" . mc/edit-lines)
	 ("C-c C-c n" . mc/mark-next-like-this)
	 ("C-c C-c p" . mc/mark-previous-like-thisc)
	 ("C-c C-c a" . mc/mark-all-like-this)))

(provide 'init-edit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-edit.el ends here
