;; init-start-board.el --- Start board configurations. -*- lexical-binding: t -*-

;; Copyright (C) 2025~2025 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;;  BLEmacs启动界面及版本信息
;;

;;; Code:

(require 'dashboard)

;; 设置标题信息
(setq dashboard-banner-logo-title "欢迎来到王北洛的Emacs! V2.1.2")
;; 设置横幅Logo
(setq dashboard-startup-banner "~/.emacs.d/logo.txt")

;; 居中显示内容
(setq dashboard-center-content t)
;; vertically center content
(setq dashboard-vertically-center-content t)
;; 显示图标
(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)
;; 设置显示项目
(setq dashboard-items '((recents  . 15)
                        (projects . 15)
                        (agenda   . 30)))

(dashboard-setup-startup-hook)

(provide 'init-start-board)
;;; init-start-board.el ends here
