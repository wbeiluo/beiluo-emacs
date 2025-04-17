;;; init-basic.el --- Default configurations -*- lexical-binding: t -*-

;; Copyright (C) 2022~2025 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;; Code:

(use-package server
  :ensure nil
  :hook (after-init . server-mode))

;; 基础设置
;; 不加载启动画面
(setq inhibit-startup-screen t)
;; 不加载启动消息
(setq inhibit-startup-message t)
;; 不显示缓冲区列表
(setq inhibit-startup-buffer-menu t)

;; 禁用工具栏
(tool-bar-mode -1)
;; 禁用菜单栏
(menu-bar-mode -1)
;; 禁用滚动条
(scroll-bar-mode -1)
;; 高亮另一个括号
(show-paren-mode t)
;; 自动刷新buffer
(global-auto-revert-mode t)
;; 高亮当前行
(global-hl-line-mode t)
;; 设置时间显示格式
;;(setq display-time-24hr-format t)
(setq display-time-format "%Y-%m-%d %H:%M")
;; 不显示平均负载
(setq display-time-default-load-average nil)
;; 打开时间显示
(display-time-mode t)
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

;; Encoding
;; 配置所有的编码为UTF-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;; 自动保存文件
(use-package super-save
  :ensure t
  :config
  (setq super-save-auto-save-when-idle t)
  (setq super-save-idle-duration 1)
  (setq save-silently t)
  ;; add integration with ace-window
  (add-to-list 'super-save-triggers 'ace-window)
  ;; save on find-file
  (add-to-list 'super-save-hook-triggers 'find-file-hook)
  (super-save-mode +1))

;; 保存和恢复 minibuffer 历史记录
(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :config
  ;; Allow commands in minibuffers, will affect `dired-do-dired-do-find-regexp-and-replace' command:
  (setq enable-recursive-minibuffers t)
  (setq history-length 1000)
  (setq savehist-additional-variables '(mark-ring
                                        global-mark-ring
                                        search-ring
                                        regexp-search-ring
                                        extended-command-history))
  (setq savehist-autosave-interval 300))

(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

(use-package hydra
  :ensure t)

(use-package which-key
    :config
    (which-key-mode))

(use-package diminish
  :ensure t)

;; 全屏显示
(defun fullscreen ()
  "Fullscreen Display."
  (interactive)
  (set-frame-parameter nil 'fullscreen
                       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))

;; @purcell 调整窗口透明度
(defun sanityinc/adjust-opacity (frame incr)
  "Adjust the background opacity of FRAME by increment INCR."
  (unless (display-graphic-p frame)
    (error "Cannot adjust opacity of this frame"))
  (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
         (oldalpha (if (listp oldalpha) (car oldalpha) oldalpha))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha newalpha))))))

;; (if (display-graphic-p)
;;     (sanityinc/adjust-opacity nil -5))

(global-set-key (kbd "C-M-8") (lambda () (interactive) (sanityinc/adjust-opacity nil -2)))
(global-set-key (kbd "C-M-9") (lambda () (interactive) (sanityinc/adjust-opacity nil 2)))
(global-set-key (kbd "C-M-7") (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))

(provide 'init-basic)

;;; init-basic.el ends here
