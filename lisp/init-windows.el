;;; init-windows.el --- Windows system configs -*- lexical-binding: t -*-

;; Copyright (C) 2020 王北洛

;; Author: 王北洛 <wbeiluo@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;; Code:

;; 设置
(when (eq system-type 'windows-nt)
    (setq shell-file-name (executable-find "zsh.exe")))

(provide 'init-windows)

;;; init-windows.el ends here
