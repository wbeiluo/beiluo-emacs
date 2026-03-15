;; init-start-board.el --- Start board configurations. -*- lexical-binding: t -*-

;; Copyright (C) 2025~2026 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;;  BLEmacs启动界面及版本信息
;;

;;; Code:

(defconst extensions-emacs-dashboard-dir
  (expand-file-name "extensions/emacs-dashboard" user-emacs-directory))

(use-package dashboard
  :ensure nil
  :load-path extensions-emacs-dashboard-dir
  :init
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-navigator-icons t)
  (setq dashboard-display-icons-p t)
  (setq dashboard-icon-type 'nerd-icons)
  :config
  ;; 启动并初始化
  (dashboard-setup-startup-hook)
  
  ;; 设置内容项目
  (setq dashboard-items '((recents  . 10)   ;; 最近文件
                          (projects . 10)   ;; 项目列表
                          (agenda .   20))) ;; 日程

  ;; 视觉优化
  (setq dashboard-banner-logo-title "Happy hacking with 王北洛's Emacs!")
  (setq dashboard-startup-banner (expand-file-name "logo.txt" user-emacs-directory))
  (setq dashboard-center-content t)
  (setq dashboard-vertically-center-content t)
  (setq dashboard-show-shortcuts t)

  ;; 单独设置dashboard-banner字体，避免出现双宽字符导致logo变形
  (set-face-attribute 'dashboard-text-banner nil
                      :family "Maple Mono"
                      :weight 'regular)

  ;; 设置页脚
  (setq dashboard-footer-messages '("BLEmacs 2.2.1"))
  (setq dashboard-footer-icon (nerd-icons-sucicon "nf-custom-emacs" :height 1.1 :face 'font-lock-keyword-face))
)

(provide 'init-start-board)
;;; init-start-board.el ends here
