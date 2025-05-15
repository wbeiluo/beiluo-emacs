;;; init-program.el --- Program config -*- lexical-binding: t -*-

;; Copyright (C) 2021~2025 王北洛

;; Author: 王北洛 <beiluo.wang@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;; Code:

(require 'smartparens)
(require 'origami)

;; Auto parentheses
(require 'smartparens-config) ; load default config

;; Always start smartparens mode in prog-mode text-mode markdown-mode.
(add-hook 'prog-mode-hook #'smartparens-mode)
(add-hook 'text-mode-hook #'smartparens-mode)
(add-hook 'markdown-mode-hook #'smartparens-mode)

;; Flexible text folding
(face-spec-reset-face 'origami-fold-header-face)
;; Start origami mode in prog-mode.
(add-hook 'prog-mode-hook #'origami-mode)

(provide 'init-program)

;;; init-program.el ends here
