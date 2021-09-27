;;; init-windows.el --- Windows system configs -*- lexical-binding: t -*-

;; Copyright (C) 2020 王北洛

;; Author: 王北洛 <wbeiluo@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;; Code:

;; 设置
(when (eq system-type 'windows-nt)
  (setenv "PATH" "C:\\Windows\\System32;C:\\Windows;C:\\Windows\\System32\\Wbem;C:\\Windows\\System32\\WindowsPowerShell\\v1.0\\;C:\\msys64\\mingw64\\bin;C:\\msys64\\usr\\bin;C:\\msys64\\usr\\local\\bin;C:\\Qt\\Qt5.14.2\\5.14.2\\mingw73_64\\bin;")
  (setq shell-file-name (executable-find "C:\\msys64\\usr\\bin\\zsh.exe"))
  ;;(setq gc-cons-threshold (* 512 1024 1024))
  ;;(setq gc-cons-percentage 0.5)
  ;;(run-with-idle-timer 5 t #'garbage-collect)
  ;;(setq garbage-collection-messages t)
  )

(provide 'init-windows)

;;; init-windows.el ends here
