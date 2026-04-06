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

;; 窗口透明度调整
(defun my/set-background-alpha (number)
  "设置当前 Frame 的背景透明度为 NUMBER (0-100)."
  (interactive "nInput background opacity(0-100): ")
  (set-frame-parameter nil 'alpha-background number)
  (add-to-list 'default-frame-alist `(alpha-background . ,number)))

(defun my/adjust-background-alpha (delta)
  "以 DELTA 为步长调整背景透明度。"
  (let* ((old-alpha (or (frame-parameter nil 'alpha-background) 100))
         (new-alpha (+ old-alpha delta)))
    (when (and (>= new-alpha 0) (<= new-alpha 100))
      (my/set-background-alpha new-alpha)
      (message "Current background opacity: %d%%" new-alpha))))

;; 定义快捷增加/减少函数
(defun my/increase-background-alpha () (interactive) (my/adjust-background-alpha 5))
(defun my/decrease-background-alpha () (interactive) (my/adjust-background-alpha -5))

(global-set-key (kbd "C-c t +") #'my/increase-background-alpha)
(global-set-key (kbd "C-c t -") #'my/decrease-background-alpha)
(global-set-key (kbd "C-c t s") #'my/set-background-alpha)

;; (my/set-background-alpha 95) ;; 设置初始透明度为95

(fullscreen)

;; (use-package transwin
;;   :ensure nil
;;   :load-path extensions-transwin-dir)
;; (require 'transwin)

(provide 'init-screen)

;;; init-screen.el ends here
