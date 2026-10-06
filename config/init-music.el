;;; init-music.el --- Music Configuration -*- lexical-binding: t -*-

;; Copyright (C) 2023~2026 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;;  音乐播放
;;
;;; Code:

(use-package emms
  :ensure t
  :bind (("C-c e e" .   emms)
         ("C-c e s" .   emms-start)
         ("C-c e x" .   emms-stop)
         ("C-c e SPC" . emms-pause)
         ("C-c e n" .   emms-next)
         ("C-c e p" .   emms-previous)
         ("C-c e a" .   emms-add-directory-tree)
         ("C-c e c" .   emms-playlist-clear)
         ("C-c e l" .   emms-playlist-mode-go))
  :config
  (require 'emms-setup)
  (emms-standard)        ;; 加载标准特性
  (emms-default-players) ;; 加载默认播放器
  (emms-history-load)
  
  (setq emms-player-list '(emms-player-mpv))
  (setq emms-player-mpv-parameters '("--quiet" "--really-quiet" "--no-audio-display"))

  (setq emms-source-file-default-directory "~/Music/")

  ;; 在modeline显示图标和歌曲名
  (setq emms-mode-line-format "    %s ")
  (setq emms-mode-line-mode-line-function #'emms-mode-line-playlist-current)
  ;; 播放列表界面美化
  (setq emms-playlist-buffer-name "*Music*")
  (setq emms-playlist-mode-center-when-go t)
  ;; 设置播放列表显示的格式
  (setq emms-last-played-format-alist
        '(((emms-last-played-time-days) . "%Y-%m-%d %H:%M")
          (t . "%H:%M")))

  ;; mimibuffer显示歌词
  (setq emms-lyrics-display-on-modeline nil)
  (setq emms-lyrics-display-on-minibuffer t)
  
  (emms-mode-line 1)
  (emms-playing-time 1))

(provide 'init-music)

;;; init-music.el ends here
