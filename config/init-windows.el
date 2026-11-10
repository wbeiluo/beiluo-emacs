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
  ;; 注意 PATH 顺序：Git for Windows 在前，MSYS2（C:\msys64）放最后——
  ;; 两套工具的同名程序（git、bash 等）优先用原生版，进程启动更快；
  ;; zsh 等 MSYS2 专有工具不受影响（shell-file-name 用绝对路径调用）
  (setenv "PATH" (concat "C:\\Windows\\System32;C:\\Windows;C:\\Windows\\System32\\Wbem;C:\\Windows\\System32\\WindowsPowerShell\\v1.0\\;C:\\Program Files\\Git\\cmd;C:\\Program Files\\Git\\mingw64\\bin;C:\\Program Files\\CMake\\bin;C:\\msys64\\usr\\bin;C:\\msys64\\mingw64\\bin;" (getenv "PATH")))

  ;; 为 magit/VC 固定 Git for Windows 的原生 git.exe（上面 PATH 中
  ;; Git\cmd\git.exe 只是启动器包装，直接用 mingw64\bin\git.exe 更快；
  ;; magit 一次刷新要串行调用十几次 git）。PATH 已含 Git\mingw64\bin，
  ;; 保证 git 拉起 git-remote-https 等辅助程序时能找到 DLL。
  ;; 找不到安装则保持默认
  (when-let* ((native-git (seq-find #'file-exists-p
                                    (list (expand-file-name "Programs/Git/mingw64/bin/git.exe"
                                                            (getenv "LOCALAPPDATA"))
                                          "C:/Program Files/Git/mingw64/bin/git.exe"
                                          "C:/Program Files/Git/bin/git.exe"))))
    (setq magit-git-executable native-git
          vc-git-program native-git))
  
  ;; 加大子进程管道缓冲（默认仅 4KB）：clangd 等 LSP 大响应、git/magit 输出
  ;; 经小管道传输时反复等待，主循环被频繁打断，表现为光标移动发卡。
  ;; 需在拉起子进程前设置，故放在此处
  (when (boundp 'w32-pipe-buffer-size)
    (setq w32-pipe-buffer-size (* 128 1024)))

  ;; 关闭子进程读缓冲自适应收缩：默认遇到输出慢的子进程会把读缓冲缩到很小，
  ;; git 这类一次吐大量输出的程序退化为大量小块读取。关闭后始终按
  ;; read-process-output-max（init-gcmh.el 设为 4MB）大块读取，吞吐更高
  (setq process-adaptive-read-buffering nil)

  ;; 默认会打开每个文件的句柄去取 NTFS 文件 ID（“真实”inode 信息），
  ;; dired/auto-revert/magit 遍历文件时空耗明显。关闭后 inode 号不再精确，
  ;; 本配置没有依赖它的功能
  (setq w32-get-true-file-attributes nil)

  ;; 指定shell程序为zsh
  (setq shell-file-name (executable-find "C:\\msys64\\usr\\bin\\zsh.exe"))

  ;; 解决粘贴中文出现乱码的问题
  (set-selection-coding-system 'utf-16le-dos))

(provide 'init-windows)

;;; init-windows.el ends here
