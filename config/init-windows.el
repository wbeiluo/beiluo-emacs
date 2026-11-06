;;; init-windows.el --- Windows system configs -*- lexical-binding: t -*-

;; Copyright (C) 2022~2026 王北洛

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
  
  ;; 加大子进程管道缓冲（默认仅 4KB）：clangd 等 LSP 大响应、git/magit 输出
  ;; 经小管道传输时反复等待，主循环被频繁打断，表现为光标移动发卡。
  ;; 需在拉起子进程前设置，故放在此处
  (when (boundp 'w32-pipe-buffer-size)
    (setq w32-pipe-buffer-size (* 128 1024)))

  ;; 指定shell程序为zsh
  (setq shell-file-name (executable-find "C:\\msys64\\usr\\bin\\zsh.exe"))

  ;; 解决粘贴中文出现乱码的问题
  (set-selection-coding-system 'utf-16le-dos))

(provide 'init-windows)

;;; init-windows.el ends here
