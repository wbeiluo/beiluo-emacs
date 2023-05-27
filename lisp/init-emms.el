;;; init-emms.el --- Emms Configuration -*- lexical-binding: t -*-

;; Copyright (C) 2023 王北洛

;; Author: 王北洛 <beiluo.wang@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;; Code:

(use-package emms
  :ensure t
  :bind (("<f9>" . emms)
	 (:map emms-playlist-mode-map
	       ("s" . emms-stop)
	       ("SPC" . emms-pause)
	       ("n" . emms-next)
	       ("p" . emms-previous)
	       ("d" . emms-play-directory-tree)
	       ("g" . emms-playlist-mode-go)
	       ("m" . emms-metaplaylist-mode-go)
	       ("i" . emms-mode-line-toggle)
	       ))
  :custom
  (emms-player-list '(emms-player-mplayer emms-player-mpv))
  (emms-source-file-default-directory "~/Music/")
  (emms-playlist-buffer-name "*Emms*")
  (emms-repeat-playlist t)
  (emms-lyrics-display-on-modeline nil)
  (emms-lyrics-display-on-minibuffer t)
  :config
  (require 'emms-setup)
  (emms-standard)
  (emms-history-load)
  (emms-mode-line-disable)
  )

(provide 'init-emms)

;;; init-emms.el ends here
