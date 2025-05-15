;;; init.el --- Load the full configuration. -*- lexical-binding: t -*-

;; Copyright (C) 2020~2025 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

(tool-bar-mode -1)    ; 禁用工具栏
(menu-bar-mode -1)    ; 禁用菜单栏
(scroll-bar-mode -1)  ; 禁用滚动条

;; Add configuration path
(setq default-directory "~/")
(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

;; Load extensions path
(require 'init-extensions)

(with-temp-message ""   ; 抹掉插件启动的输出
  (require 'init-general)
  (require 'init-lib)
  (require 'init-gcmh)
  (require 'init-fonts)
  (require 'init-time)
  (require 'init-theme)
  (require 'init-modeline)
  (require 'init-consult)
  (require 'init-vertico)
  (require 'init-embark)
  (require 'init-markdown)
  (require 'init-corfu)
  (require 'init-lsp)
  (require 'init-flycheck)
  (require 'init-git)
  (require 'init-treemacs)
  (require 'init-goto)
  (require 'init-window)
  (require 'init-org)
  (require 'init-program)
  (require 'init-key)
  (require 'init-screen)
  (require 'init-start-board)

  ;; 可以延后加载的
  (run-with-idle-timer
   1 nil
   #'(lambda ()
       (require 'init-undo)
       (require 'init-auto-save)
       (require 'init-shell)
       (require 'init-highlight)
       (require 'init-edit)
       (require 'init-cc)
       (require 'init-tempel)
       (require 'init-sdcv)
       (require 'init-music)
       (require 'init-windows)
       )))

;; change custom file location
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
