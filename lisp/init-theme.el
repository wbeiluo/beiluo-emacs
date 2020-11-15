;;; init-theme.el --- Config Theme  -*- lexical-binding: t -*-

;; Copyright (C) 2020 王北洛

;; Author: 王北洛 <wbeiluo@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;; Code:

;; Install additinal themes from melpa
;; make sure to use :defer keyword
(use-package doom-themes
  :ensure
  :defer
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; Switching themes on time of day
(use-package circadian
  :ensure t
  :config
  (setq circadian-themes '(("8:00" . doom-solarized-light)
                           ("19:30" . doom-solarized-dark)))
  (circadian-setup))


(provide 'init-theme)
;;; init-theme.el ends here
