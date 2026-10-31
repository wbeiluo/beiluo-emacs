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

;; 以下通用库均不在本配置中直接调用，其他包（treemacs/lsp-mode/magit 等）
;; 需要时会自行 require，故统一延迟加载，避免启动时无谓加载
;; （hydra 实测约 0.5s，dash/s/f/ht/ov/ts/posframe/cfrs 各约 0.1s）

;; A modern list library for Emacs
(use-package dash
  :ensure t
  :defer t)

;; The long lost Emacs string manipulation library.
(use-package s
  :ensure t
  :defer t)

;; Modern API for working with files and directories in Emacs
(use-package f
  :ensure t
  :defer t)

;; The missing hash table library for Emacs
(use-package ht
  :ensure t
  :defer t)

;; Simple way to manipulate overlay for Emacs
(use-package ov
  :ensure t
  :defer t)

;; Emacs timestamp and date-time library
(use-package ts
  :ensure t
  :defer t)

;; make Emacs bindings that stick around
(use-package hydra
  :ensure t
  :defer t)

;; COMPATibility Library for Emacs Lisp(built-in)
(use-package compat
  :defer t)

;; Child Frame Read String
(use-package cfrs
  :ensure t
  :defer t)

;; Pop a posframe (just a frame) at point
(use-package posframe
  :ensure t
  :defer t)

;; Display available keybindings in popup
(use-package which-key
  :config
  (which-key-mode))

(provide 'init-lib)

;;; init-lib.el ends here
