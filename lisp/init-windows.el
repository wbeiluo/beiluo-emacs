;;; init-windows.el --- Windows system configs -*- lexical-binding: t -*-

;; Copyright (C) 2020 王北洛

;; Author: 王北洛 <wbeiluo@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;; Code:

;; 设置
(when (eq system-type 'windows-nt)
  (setenv "PATH" "D:\\Home\\bin;D:\\Home\\opt\\msys64\\mingw64\\bin;D:\\Home\\opt\\msys64\\usr\\local\\bin;D:\\Home\\opt\\msys64\\usr\\bin;D:\\Home\\opt\\msys64\\usr\\bin;C:\\Windows\\System32;C:\\Windows;C:\\Windows\\System32\\Wbem;C:\\Windows\\System32\\WindowsPowerShell\\v1.0\\;D:\\Home\\opt\\msys64\\usr\\bin\\site_perl;D:\\Home\\opt\\msys64\\usr\\bin\\vendor_perl;D:\\Home\\opt\\msys64\\usr\\bin\\core_perl")
  (setq shell-file-name (executable-find "D:\\Home\\opt\\msys64\\usr\\bin\\zsh.exe"))
  (setq gc-cons-threshold (* 512 1024 1024))
  (setq gc-cons-percentage 0.5)
  (run-with-idle-timer 5 t #'garbage-collect)
  (setq garbage-collection-messages t))

(provide 'init-windows)

;;; init-windows.el ends here
