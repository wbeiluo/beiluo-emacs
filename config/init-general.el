;;; init-general.el --- General configurations -*- lexical-binding: t -*-

;; Copyright (C) 2022~2025 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;;  基础设置及通用库
;;

;;; Code:

;; 让窗口启动更平滑
(setq frame-inhibit-implied-resize t)
(setq-default inhibit-redisplay t
              inhibit-message t)
(add-hook 'window-setup-hook
          (lambda ()
            (setq-default inhibit-redisplay nil
                          inhibit-message nil)
            (redisplay)))

;; 不加载启动画面
(setq inhibit-startup-screen t)
;; 不加载启动消息
(setq inhibit-startup-message "")
;; 不显示缓冲区列表
(setq inhibit-startup-buffer-menu t)
;; 使用字体缓存，避免卡顿
(setq inhibit-compacting-font-caches t)
;; 默认用最简单的模式
(setq initial-major-mode 'fundamental-mode)
;; 不要自动启用package
(setq package-enable-at-startup nil)
(setq package--init-file-ensured t)
;; 不打印 lexical binding 日志
(setq warning-suppress-types '((files)))

;; recent file
(recentf-mode 1)
;; 高亮另一个括号
(show-paren-mode t)
;; 自动刷新buffer
(global-auto-revert-mode t)
;; 高亮当前行
(global-hl-line-mode t)
;; 不显示平均负载
(setq display-time-default-load-average nil)
                                        ; 打开电池显示
;; (display-battery-mode nil)
;; 在modeline上显示列号
(column-number-mode t)
;; 显示行号
(global-display-line-numbers-mode t)
;; 鼠标操作不使用对话框
(setq use-dialog-box nil)
;; 不加载 `default' 库
(setq inhibit-default-init t)
;; 设置大文件阈值为100MB，默认10MB
(setq large-file-warning-threshold 100000000)
;; 以16进制显示字节数
(setq display-raw-bytes-as-hex t)
;; 有输入时禁止 `fontification' 相关的函数钩子，能让滚动更顺滑
(setq redisplay-skip-fontification-on-input t)
;; 禁止响铃
(setq ring-bell-function 'ignore)
;; 在光标处而非鼠标所在位置粘贴
(setq mouse-yank-at-point t)
;; 选择文字时不拷贝
(setq select-enable-primary nil)
;; 拷贝时使用剪贴板
(setq select-enable-clipboard t)
;; 关闭自动备份和保存
(setq make-backup-files nil)
(setq auto-save-default nil)
;; 使用y/n替换yes/no
(fset 'yes-or-no-p 'y-or-n-p)
;; 设置自动折行宽度为120个字符，默认值为70
(setq-default fill-column 120)
;; 设置剪贴板历史长度200，默认为60
(setq kill-ring-max 200)
;; 平滑地进行半屏滚动，避免滚动后recenter操作
(setq scroll-step 1
      scroll-conservatively 10000)
;; 避免magit报错
(setq async-bytecomp-allowed-packages nil)
;; 按照中文折行
(setq word-wrap-by-category t)
;; 避免默认自动选择
(setq completion-auto-select nil)

;; Encoding
;; 配置所有的编码为UTF-8
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;; 设置国内镜像源
(setq package-archives '(("gnu" . "http://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.ustc.edu.cn/elpa/melpa/")
                         ("melpa-stable" . "http://mirrors.ustc.edu.cn/elpa/melpa-stable/")
                         ("org" . "http://mirrors.ustc.edu.cn/elpa/org/")))

(provide 'init-general)

;;; init-general.el ends here
