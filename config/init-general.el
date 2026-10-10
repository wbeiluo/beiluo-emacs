;;; init-general.el --- General configurations -*- lexical-binding: t -*-

;; Copyright (C) 2022~2026 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;;  基础设置及通用库
;;

;;; Code:

;; 设置国内镜像源
(setq package-archives '(("gnu" . "http://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.ustc.edu.cn/elpa/melpa/")
                         ("melpa-stable" . "http://mirrors.ustc.edu.cn/elpa/melpa-stable/")
                         ("org" . "http://mirrors.ustc.edu.cn/elpa/org/")))

;; Encoding
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

;; 让窗口启动更平滑
(setq frame-inhibit-implied-resize t)
(setq-default inhibit-redisplay t
              inhibit-message t)
(add-hook 'window-setup-hook
          (lambda ()
            (setq-default inhibit-redisplay nil
                          inhibit-message nil)
            (redisplay)))

;; 禁用工具栏
(tool-bar-mode -1)
;; 禁用菜单栏
(menu-bar-mode -1)
;; 禁用滚动条
(scroll-bar-mode -1)

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
;; 光标在括号内时高亮整对括号；另一端在屏幕外时在 echo area 提示其上下文
(setq show-paren-when-point-inside-paren t
      show-paren-context-when-offscreen t)
;; 自动刷新buffer
(global-auto-revert-mode t)
;; 高亮当前行
(global-hl-line-mode t)
;; 不显示平均负载
(setq display-time-default-load-average nil)
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
;; GUI 下像素级平滑滚动（滚轮/触控板逐像素）
(when (display-graphic-p)
  (pixel-scroll-precision-mode 1))
;; 光标使用细竖条
(setq-default cursor-type 'bar)
;; 避免magit报错
(setq async-bytecomp-allowed-packages nil)
;; 按照中文折行
(setq word-wrap-by-category t)
;; 避免默认自动选择
(setq completion-auto-select nil)

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

(provide 'init-general)

;;; init-general.el ends here
