;;; init-windows.el --- Windows system configs -*- lexical-binding: t -*-

;; Copyright (C) 2022~2025 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;;  Windows系统环境配置
;;
;;; Code:

(when (eq system-type 'windows-nt)

  ;; 设置Windows系统环境变量
  (setenv "PATH" (concat "C:\\Windows\\System32;C:\\Windows;C:\\Windows\\System32\\Wbem;C:\\Windows\\System32\\WindowsPowerShell\\v1.0\\;C:\\msys64\\usr\\bin;C:\\msys64\\mingw64\\bin;C:\\Program Files\\Git\\cmd;C:\\Program Files\\CMake\\bin;" (getenv "PATH")))
  
  ;; 指定shell程序为zsh
  (setq shell-file-name (executable-find "C:\\msys64\\usr\\bin\\zsh.exe"))

  ;; 解决粘贴中文出现乱码的问题
  (set-selection-coding-system 'utf-16le-dos)

  ;; 解决sdcv词典识别和乱码问题
  (add-to-list 'process-coding-system-alist '("sdcv" . (utf-8-dos . chinese-gbk-dos)))
  )

(provide 'init-windows)

;;; init-windows.el ends here
