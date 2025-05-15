;; init-time.el --- Time Configurations. -*- lexical-binding: t -*-

;; Copyright (C) 2025~2025 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;; 时间相关设置
;;

;;; Code:

;; 打开日期显示
(setq display-time-day-and-date t)
;; 打开时间显示
(display-time-mode 1)
;; 打开24小时显示模式
(setq display-time-24hr-format t)
;; 设置时间显示格式
(setq display-time-format "%H:%M")
;; 显示时间
(display-time)

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

(provide 'init-time)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-time.el ends here
