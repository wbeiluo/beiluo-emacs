;; init-func.el --- Function Configurations. -*- lexical-binding: t -*-

;; Copyright (C) 2025~2026 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;; 自定义函数
;;

;;; Code:

;; 快捷键
(global-set-key (kbd "C-c t t") 'insert-current-time)
(global-set-key (kbd "C-c t d") 'insert-current-data-time)
(global-set-key (kbd "C-c t w") 'insert-current-data-week-time)

;; 插入时间
(defun insert-current-time ()
  "Insert current time."
  (interactive)
  (insert (format-time-string "%H:%M")))

;; 插入日期时间
(defun insert-current-data-time ()
  "Insert current data and time."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M")))

;; 插入日期星期时间
(defun insert-current-data-week-time ()
  "Insert current data, week and time."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %A %H:%M")))

(provide 'init-func)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-func.el ends here
