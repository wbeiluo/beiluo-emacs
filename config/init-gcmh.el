;;; init-gcmh.el --- gc configurations -*- lexical-binding: t -*-

;; Copyright (C) 2025~2025 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;; Code:

(require 'gcmh)

;; Adjust garbage collection threshold for early startup (see use of gcmh below)
(setq gc-cons-threshold (* 512 1024 1024))

;; Process performance tuning
(setq read-process-output-max (* 4 1024 1024))
(setq process-adaptive-read-buffering nil)

;; General performance tuning
(setq gcmh-high-cons-threshold (* 512 1024 1024))
(gcmh-mode 1)

(provide 'init-gcmh)

;;; init-gcmh.el ends here
