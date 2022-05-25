;;; init-music.el --- 音乐配置 -*- lexical-binding: t -*-

;; Copyright (C) 2021 王北洛

;; Author: 王北洛 <beiluo.wang@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;; Code:

;; (use-package bongo
;;   :ensure t
;;   :bind ("<f9>" . bongo))

(use-package bongo
  :ensure t
  :bind (("<f9>" . bongo)
	 (:map bongo-playlist-mode-map
	       ("<return>" . bongo-dwim)
	       ("i" . bongo-insert-file)
	       ("p" . bongo-play-previous)
	       ("n" . bongo-play-next)
	       ("w" . bongo-pause/resume)
	       ("d" . bongo-dired-line)
	       ("e" . bongo-append-enqueue)
	       ("s" . bongo-seek)
	       ("r" . bongo-rename-line)
	       ("v" . volume)))
  :custom
  (bongo-enabled-backends '(mplayer))
  (bongo-default-directory "~/Music/")
  (bongo-insert-album-covers t)
  (bongo-album-cover-size 100)
  (bongo-mode-line-indicator-mode nil))

(provide 'init-music)

;;; init-music.el ends here
