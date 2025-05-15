;;; init-music.el --- Music Configuration -*- lexical-binding: t -*-

;; Copyright (C) 2023~2025 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;;  音乐播放
;;
;;; Code:

(require 'emms)

(setq emms-player-list '(emms-player-mpv))
(setq emms-source-file-default-directory "~/Music/")
(setq emms-playlist-buffer-name "*Emms*")
(setq emms-repeat-playlist t)
(setq emms-lyrics-display-on-modeline nil)
(setq emms-lyrics-display-on-minibuffer t)

(require 'emms-setup)
(emms-standard)
(emms-history-load)
;; (emms-mode-line-disable)

;; Key map
(define-key emms-playlist-mode-map (kbd "s")   #'emms-stop)
(define-key emms-playlist-mode-map (kbd "SPC") #'emms-pause)
(define-key emms-playlist-mode-map (kbd "n")   #'emms-next)
(define-key emms-playlist-mode-map (kbd "p")   #'emms-previous)
(define-key emms-playlist-mode-map (kbd "d")   #'emms-play-directory-tree)
(define-key emms-playlist-mode-map (kbd "g")   #'emms-playlist-mode-go)
(define-key emms-playlist-mode-map (kbd "m")   #'emms-metaplaylist-mode-go)
(define-key emms-playlist-mode-map (kbd "i")   #'emms-mode-line-mode)

(provide 'init-music)

;;; init-music.el ends here
