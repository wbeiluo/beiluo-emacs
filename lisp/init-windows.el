;;; init-windows.el --- Windows system configs -*- lexical-binding: t -*-

;; Copyright (C) 2022 王北洛

;; Author: 王北洛 <wbeiluo@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;; Code:

;; 设置
(when (eq system-type 'windows-nt)
  (setenv "PATH" (concat "C:\\Windows\\System32;C:\\Windows;C:\\Windows\\System32\\Wbem;C:\\Windows\\System32\\WindowsPowerShell\\v1.0\\;C:\\msys64\\usr\\bin;C:\\msys64\\mingw64\\bin;C:\\Program Files\\Git\\cmd;C:\\Program Files\\CMake\\bin;C:\\Program Files (x86)\\GNU Arm Embedded Toolchain\\10 2020-q4-major\\bin;C:\\Program Files\\OpenOCD-20211118-0.11.0\\bin;" (getenv "PATH")))
  (setq shell-file-name (executable-find "C:\\msys64\\usr\\bin\\zsh.exe"))
  (setq gc-cons-threshold (* 512 1024 1024))
  (setq gc-cons-percentage 0.5)
  (run-with-idle-timer 5 t #'garbage-collect)
  )

(provide 'init-windows)

;;; init-windows.el ends here
