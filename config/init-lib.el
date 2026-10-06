;;; init-lib.el --- Library configurations. -*- lexical-binding: t -*-

;; Copyright (C) 2022~2026 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;;  通用库及组建
;;

;;; Code:

(require 'use-package)
(setq use-package-always-ensure nil)

;; A modern list library for Emacs
(use-package dash
  :ensure t
  :demand t)

;; The long lost Emacs string manipulation library.
(use-package s
  :ensure t)

;; Modern API for working with files and directories in Emacs
(use-package f
  :ensure t)

;; The missing hash table library for Emacs
(use-package ht
  :ensure t)

;; Simple way to manipulate overlay for Emacs
(use-package ov
  :ensure t)

;; Emacs timestamp and date-time library
(use-package ts
  :ensure t)

;; make Emacs bindings that stick around
(use-package hydra
  :ensure t)

;; COMPATibility Library for Emacs Lisp(built-in)
(use-package compat)

;; Child Frame Read String
(use-package cfrs
  :ensure t)

;; Pop a posframe (just a frame) at point
(use-package posframe
  :ensure t)

;; Display available keybindings in popup
(use-package which-key
  :config
  (which-key-mode))

(provide 'init-lib)

;;; init-lib.el ends here
