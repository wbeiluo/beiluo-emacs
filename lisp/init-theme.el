;;; init-theme.el --- Config Theme  -*- lexical-binding: t -*-

;; Copyright (C) 2020 王北洛

;; Author: 王北洛 <wbeiluo@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;; Code:

(use-package doom-themes
  :custom
  (doom-themes-enable-italic t)
  (doom-themes-enable-bold t)
  :custom-face
  ;; (vertical-bar   (doom-darken base5 0.4))
  ;; (doom-darken bg 0.4)
  :config
  (load-theme 'doom-one t)
  (doom-themes-neotree-config)
  (doom-themes-org-config)
  )

;; spacemacs-theme
;; (use-package spacemacs-theme
;;   :ensure t
;;   :defer t
;;   :init (load-theme 'spacemacs-dark t))

;; solarized-theme
;; (use-package solarized-theme
;;   :ensure t
;;   :config
;;   (setq solarized-distinct-fringe-background t)
;;   (setq solarized-use-variable-pitch nil)
;;   (setq solarized-scale-org-headlines nil)
;;   (setq solarized-high-contrast-mode-line t)
;;   (load-theme 'solarized-dark t))

;;; zenburn-theme
;; (use-package zenburn-theme
;;   :ensure t
;;   :config
;; (load-theme 'zenburn t))

(provide 'init-theme)
;;; init-theme.el ends here
