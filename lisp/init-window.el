;;; init-window.el --- Window configurations -*- lexical-binding: t -*-

;; Copyright (C) 2020 王北洛

;; Author: 王北洛 <wbeiluo@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs


;;; Commentary:
;;; Code:

(use-package switch-window
  :ensure t
  :config
  (setq-default switch-window-shortcut-style 'alphabet)
  (setq-default switch-window-timeout nil)
  :bind ("C-x o" . 'switch-window))

;; 全屏显示
(defun fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
    (if (frame-parameter nil 'fullscreen) nil 'fullboth)))

(provide 'init-window)

;;; init-window.el ends here
