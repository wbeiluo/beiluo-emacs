;;; init.el --- Load the full configuration -*- lexical-binding: t -*-

;; Copyright (C) 2020 王北洛

;; Author: 王北洛 <wbeiluo@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

(setq default-directory "~/")
;; Load configuration path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;;----------------------------------------------------------------------------
;; Adjust garbage collection thresholds during startup, and thereafter
;;----------------------------------------------------------------------------
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;;----------------------------------------------------------------------------
;; Package Initialize
;;----------------------------------------------------------------------------
(eval-when-compile
  (require 'package)
  ;; (setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
  ;;                          ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
  ;;                          ("org"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))
  (setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                           ("melpa" . "http://elpa.emacs-china.org/melpa/")
                           ("org"   . "http://elpa.emacs-china.org/org/")))
  ;;(setq package-archives '(("gnu" . "http://mirrors.ustc.edu.cn/elpa/gnu/")
  ;;                         ("melpa" . "http://mirrors.ustc.edu.cn/elpa/melpa/")
  ;;                         ("melpa-stable" . "http://mirrors.ustc.edu.cn/elpa/melpa-stable/")
  ;;                         ("org" . "http://mirrors.ustc.edu.cn/elpa/org/")))

  (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)
    (package-install 'diminish)
    (package-install 'bind-key))
  (setq use-package-always-ensure t)
  (setq use-package-expand-minimally t)
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------
(require 'init-basic)
(require 'init-theme)
(require 'init-modeline)
(require 'init-vertico)
(require 'init-corfu)
(require 'init-edit)
(require 'init-project)
(require 'init-window)
(require 'init-eshell)
(require 'init-exec-path)
(require 'init-auto-save)
(require 'init-pair)
(require 'init-treemacs)
(require 'init-fonts)
(require 'init-git)
(require 'init-program)
(require 'init-lsp)
(require 'init-cc)
(require 'init-slime)
(require 'init-flycheck)
(require 'init-yasnippet)
(require 'init-elisp)
(require 'init-org)
(require 'init-org-agenda)
;;(require 'init-pdf)
(require 'init-music)
(require 'init-windows)
(require 'init-key-binding)

;; change custom file location
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Full screen display
(fullscreen)

(when (fboundp 'global-eldoc-mode)
  (add-hook 'after-init-hook 'global-eldoc-mode))

(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (auto-package-update-maybe))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
