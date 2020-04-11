;;; init.el --- Load the full configuration -*- lexical-binding: t -*-

;; Copyright (C) 2020 王北洛

;; Author: 王北洛 <wbeiluo@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Load configuration path
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

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
  (setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                           ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                           ("org"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))
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
;; (require 'init-hydra)
(require 'init-theme)
(require 'init-modeline)
(require 'init-exec-path)
(require 'init-eshell)
(require 'init-auto-save)
(require 'init-dired)
(require 'init-fonts)
(require 'init-recentf)
(require 'init-flycheck)
(require 'init-ibuffer)
(require 'init-edit)
(require 'init-ivy)
(require 'init-company)
(require 'init-awesome-pair)
(require 'init-highlight)
(require 'init-window)
(require 'init-treemacs)
(require 'init-git)
(require 'init-projectile)
;; (require 'init-session)

(require 'init-lsp)
(require 'init-elisp)
(require 'init-slime)
(require 'init-c)

;; change custom file location
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Full screen display
(fullscreen)
;; Restore session at last.
;; (emacs-session-restore)

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
