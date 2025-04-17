;;; init.el --- Load the full configuration -*- lexical-binding: t -*-

;; Copyright (C) 2020~2025 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

(setq default-directory "~/")
;; Load configuration path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Adjust garbage collection threshold for early startup (see use of gcmh below)
(setq gc-cons-threshold (* 128 1024 1024))

;; Process performance tuning
(setq read-process-output-max (* 4 1024 1024))
(setq process-adaptive-read-buffering nil)

;; change custom file location
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Package Initialize
(eval-when-compile
  (require 'package)
  (setq package-archives '(("gnu" . "http://mirrors.ustc.edu.cn/elpa/gnu/")
                          ("melpa" . "http://mirrors.ustc.edu.cn/elpa/melpa/")
                          ("melpa-stable" . "http://mirrors.ustc.edu.cn/elpa/melpa-stable/")
                          ("org" . "http://mirrors.ustc.edu.cn/elpa/org/")))

  (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)
    (package-install 'bind-key))
  (setq use-package-always-ensure t)
  (setq use-package-expand-minimally t)
  (require 'use-package)
  (require 'bind-key))

;; General performance tuning
(use-package gcmh
  :config
  (setq gcmh-high-cons-threshold (* 128 1024 1024))
  (add-hook 'after-init-hook (lambda ()
                               (gcmh-mode)
                               (diminish 'gcmh-mode))))

;; Load configs for specific features and modes
(require 'init-basic)
(require 'init-theme)
(require 'init-fonts)
(require 'init-modeline)
(require 'init-completion)
(require 'init-edit)
(require 'init-program)
(require 'init-window)
(require 'init-shell)
(require 'init-treemacs)
(require 'init-lsp)
(require 'init-cc)
(require 'init-slime)
(require 'init-flycheck)
(require 'init-yasnippet)
(require 'init-org)
(require 'init-music)
(require 'init-windows)
(require 'init-key-binding)

(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  ;; (auto-package-update-maybe)
  )

;; 启动界面
(use-package dashboard
  :ensure t
  :config
  ;; 设置欢迎信息
  (setq dashboard-banner-logo-title "Welcome to WBL Emacs V0.01")
  (setq dashboard-startup-banner 'official)
  ;; 居中显示内容
  (setq dashboard-center-content t)
  ;; 显示图标
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  ;; 设置显示项目
  (setq dashboard-items '((recents  . 15)
			  (projects . 10)
			  (bookmarks . 10)
                          (agenda . 15)))
  (dashboard-setup-startup-hook))

;; Default display size
(fullscreen)
;(setq default-frame-alist '((height . 45) (width . 125)))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
