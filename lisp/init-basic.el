;;; init-basic.el --- Default configurations -*- lexical-binding: t -*-

;; Copyright (C) 2020 王北洛

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

;; Quiet Startup
(tool-bar-mode -1)                      ;禁用工具栏
(menu-bar-mode -1)                      ;禁用菜单栏
(scroll-bar-mode -1)                    ;禁用滚动条
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)
(blink-cursor-mode -1)                ;指针不闪动
(transient-mark-mode 1)               ;标记高亮
(setq-default comment-style 'indent)  ;设定自动缩进的注释风格
(global-hl-line-mode 1)               ;高亮当前行

(setq-default major-mode 'text-mode
              fill-column 80
              tab-width 4
              kill-whole-line t
              indent-tabs-mode nil)
              (defalias 'yes-or-no-p #'y-or-n-p)

(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-compacting-font-caches t) ;使用字体缓存，避免卡顿
(setq vc-handled-backends nil)          ;禁止版本控制工具，加速启动
(setq initial-buffer-choice t)          ;Restore emacs session
(setq mouse-yank-at-point t)            ;粘贴于光标处,而不是鼠标指针处
(setq select-enable-clipboard t)      ;支持emacs和外部程序的粘贴
(setq ring-bell-function 'ignore)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets) ; Show path if names are same
(setq delete-by-moving-to-trash t)    ; Deleting files go to OS's trash folder
(setq make-backup-files nil)          ; Forbide to make backup files
(setq auto-save-default nil)          ; Disable auto save

;;(setq display-time-day-and-date t)      ;打开日期显示
(display-time-mode 1)                   ;打开时间显示
(display-time)                          ;显示时间
(setq display-time-format "%H:%M")      ;设定时间显示格式
(setq display-time-24hr-format t)       ;打开24小时显示模式

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

(provide 'init-basic)

;;; init-basic.el ends here
