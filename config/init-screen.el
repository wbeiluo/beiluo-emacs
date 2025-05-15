;;; init-screen.el --- Screen Configurations -*- lexical-binding: t -*-

;; Copyright (C) 2025~2025 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;; Code:

;; 全屏显示
(defun fullscreen ()
  "Fullscreen Display."
  (interactive)
  (set-frame-parameter nil 'fullscreen
                       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))

(fullscreen)

;; 窗口透明度调整
(require 'transwin)

(provide 'init-screen)

;;; init-screen.el ends here
