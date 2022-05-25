;;; init-basic.el --- Default configurations -*- lexical-binding: t -*-

;; Copyright (C) 2022 王北洛

;; Author: 王北洛 <wbeiluo@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;; Code:

(use-package server
  :ensure nil
  :hook (after-init . server-mode))

;; Encoding
;; UTF-8 as the default coding system
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))

(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; 隐藏 title bar
;; (setq default-frame-alist '((undecorated . t)))
;; (add-to-list 'default-frame-alist '(drag-internal-border . 1))
;; (add-to-list 'default-frame-alist '(internal-border-width . 1))

;; 界面设置
(tool-bar-mode -1)                           ; 禁用工具栏
(menu-bar-mode -1)                           ; 禁用菜单栏
(scroll-bar-mode -1)                         ; 禁用滚动条
(column-number-mode t)                       ; 显示列号
(show-paren-mode t)                          ; 高亮另一个括号
(global-auto-revert-mode t)                  ; 自动刷新buffer
(transient-mark-mode t)                      ; 标记高亮
(global-hl-line-mode t)                      ; 高亮当前行
(global-display-line-numbers-mode t)         ; 在 Window 显示行号
(setq display-time-format "%H:%M")           ; 设置时间显示格式
(display-time-mode t)                        ; 打开时间显示
(setq inhibit-startup-screen t)              ; 关闭启动界面
(setq inhibit-startup-message t)             ; 关闭启动消息
(setq make-backup-files nil)                 ; 关闭自动备份
(setq mouse-yank-at-point t)                 ; 粘贴于光标处,而不是鼠标指针处
(setq select-enable-clipboard t)             ; 支持emacs和外部程序的粘贴
(setq inhibit-compacting-font-caches t)      ; 使用字体缓存，避免卡顿
(setq auto-save-default nil)                 ; 关闭自动保存
(fset 'yes-or-no-p 'y-or-n-p)                ; 使用y/n替换yes/no

;; Show native line numbers if possible, otherwise use `linum'
(if (fboundp 'display-line-numbers-mode)
    (use-package display-line-numbers
      :ensure nil
      :hook (prog-mode . display-line-numbers-mode))
  (use-package linum-off
    :demand
    :defines linum-format
    :hook (after-init . global-linum-mode)
    :init (setq linum-format "%4d ")
    :config
    ;; Highlight current line number
    (use-package hlinum
      :defines linum-highlight-in-all-buffersp
      :custom-face (linum-highlight-face ((t (:inherit default :background nil :foreground nil))))
      :hook (global-linum-mode . hlinum-activate)
      :init (setq linum-highlight-in-all-buffersp t))))

;; 界面平滑移动
(use-package good-scroll
  :ensure t
  :init (good-scroll-mode))

;; 启动界面
(use-package dashboard
  :ensure t
  :config
  (setq dashboard-banner-logo-title "Welcome to Emacs!") ; 个性签名
  (setq dashboard-projects-backend 'project-el)          ; 项目
  (setq dashboard-startup-banner 'official)              ; 显示默认图片
  (setq dashboard-set-navigator t)                       ; 显示导航
  (setq dashboard-set-heading-icons t)                   ; 显示导航图片
  (setq dashboard-set-file-icons t)                      ; 显示文件图片
  (setq dashboard-items '((recents  . 8)                 ; 显示5个最近文件
			  (projects . 5)                 ; 显示多少个最近项目
			  (bookmarks . 5)                ; 显示多少个最近书签
			  ))
  (dashboard-setup-startup-hook))

(use-package hydra
  :ensure t)

(provide 'init-basic)

;;; init-basic.el ends here
