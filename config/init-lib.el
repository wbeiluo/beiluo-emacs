;;; init-lib.el --- Library configurations. -*- lexical-binding: t -*-

;; Copyright (C) 2022~2025 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;;  通用库及组建
;;

;;; Code:

(require 'use-package)
(setq use-package-always-ensure nil)

(defconst extensions-dash-dir
  (expand-file-name "extensions/dash" user-emacs-directory))
(defconst extensions-s-dir
  (expand-file-name "extensions/s" user-emacs-directory))
(defconst extensions-f-dir
  (expand-file-name "extensions/f" user-emacs-directory))
(defconst extensions-ht-dir
  (expand-file-name "extensions/ht" user-emacs-directory))
(defconst extensions-hydra-dir
  (expand-file-name "extensions/hydra" user-emacs-directory))

(use-package dash
  :ensure nil
  :load-path extensions-dash-dir)

(use-package s
  :ensure nil
  :load-path extensions-s-dir)

(use-package f
  :ensure nil
  :load-path extensions-f-dir)

(use-package ht
  :ensure nil
  :load-path extensions-ht-dir)

(use-package hydra
  :ensure nil
  :load-path extensions-hydra-dir)

(require 'which-key)
(which-key-mode 1)

(provide 'init-lib)

;;; init-lib.el ends here
