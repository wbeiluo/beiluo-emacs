;;; init.el --- Load the full configuration. -*- lexical-binding: t -*-

;; Copyright (C) 2020~2026 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Set the software name and version
(setq blemacs-name    "BLEmacs")
(setq blemacs-version "2.3.0")

;; Add configuration path
(setq default-directory "~/")
(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

;; Change custom file location
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(with-temp-message ""   ; 抹掉插件启动的输出
  (require 'init-general)
  (require 'init-lib)
  (require 'init-gcmh)
  (require 'init-func)
  (require 'init-goto)
  (require 'init-window)
  (require 'init-theme)
  (require 'init-modeline)
  (require 'init-fonts)
  (require 'init-consult)
  (require 'init-vertico)
  (require 'init-org)
  (require 'init-note)
  (require 'init-embark)
  (require 'init-markdown)
  (require 'init-shell)
  (require 'init-edit)
  (require 'init-git)
  (require 'init-treemacs)
  (require 'init-corfu)
  (require 'init-flycheck)
  (require 'init-lsp)
  (require 'init-tempel)
  (require 'init-program)
  (require 'init-cc)
  (require 'init-python)
  (require 'init-music)
  (require 'init-ai)
  (require 'init-windows)
  (require 'init-save-restore)
  (require 'init-screen)
  (require 'init-start-board))

;; Load custom file
(when (file-exists-p custom-file)
  (load custom-file nil t))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
