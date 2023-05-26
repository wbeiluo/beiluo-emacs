;;; init-basic.el --- Default configurations -*- lexical-binding: t -*-

;; Copyright (C) 2022~2023 王北洛

;; Author: 王北洛 <wbeiluo@139.com>
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
;; 设置时间显示格式
(setq display-time-24hr-format t)
;; 打开时间显示
(display-time-mode t)
; 打开电池显示
(display-battery-mode t)
;; 在modeline上显示列号
(column-number-mode t)

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

;; 鼠标滚动设置
(setq scroll-step 2)
(setq scroll-margin 2)
(setq hscroll-step 2)
(setq hscroll-margin 2)
(setq scroll-conservatively 101)
(setq scroll-up-aggressively 0.01)
(setq scroll-down-aggressively 0.01)
(setq scroll-preserve-screen-position 'always)

;; 界面平滑移动
;; (use-package good-scroll
;;   :ensure t
;;   :init (good-scroll-mode))

;; 设置自动折行宽度为100个字符，默认值为70
(setq-default fill-column 100)

;; 设置剪贴板历史长度200，默认为60
(setq kill-ring-max 200)

;; Encoding
;; 配置所有的编码为UTF-8，参考：
;; https://thraxys.wordpress.com/2016/01/13/utf-8-in-emacs-everywhere-forever/
(setq locale-coding-system 'utf-8)
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

;; Show native line numbers if possible, otherwise use `linum'
(if (fboundp 'display-line-numbers-mode)
    (use-package display-line-numbers
      :ensure nil
      :hook (prog-mode . display-line-numbers-mode))
  (use-package linum-off
    :demand
    :defines linum-format
    :hook (after-init . global-linum-mode)
    ;; :init (setq linum-format "%4d")
    :config
    ;; Highlight current line number
    (use-package hlinum
      :defines linum-highlight-in-all-buffersp
      :custom-face (linum-highlight-face ((t (:inherit default :background nil :foreground nil))))
      :hook (global-linum-mode . hlinum-activate)
      :init (setq linum-highlight-in-all-buffersp t))))

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
  (setq dashboard-items '((recents  . 10)                ; 显示10个最近文件
			  (projects . 10)                ; 显示10个最近项目
			  (bookmarks . 8)                ; 显示8个最近书签
			  ))
  (dashboard-setup-startup-hook))

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

;; (sanityinc/adjust-opacity nil -2)

(global-set-key (kbd "M-C-8") (lambda () (interactive) (sanityinc/adjust-opacity nil -2)))
(global-set-key (kbd "M-C-9") (lambda () (interactive) (sanityinc/adjust-opacity nil 2)))
(global-set-key (kbd "M-C-7") (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))

(provide 'init-basic)

;;; init-basic.el ends here
