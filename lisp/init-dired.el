;; init-dired.el --- Dired configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2020 王北洛

;; Author: 王北洛 <wbeiluo@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;; dired configurations.
;;

;;; Code:

(setq-default dired-dwim-target t)

;; Prefer g-prefixed coreutils version of standard utilities when available
(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls)))

;(when (maybe-require-package 'diredfl)
;  (after-load 'dired
;    (diredfl-global-mode)
;    (require 'dired-x)))

(use-package diredfl
  :after dired
  :bind ("C-x j" . dired-jump)
  :config
  (diredfl-global-mode)
  (require dired-x))


(provide 'init-dired)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dired.el ends here
