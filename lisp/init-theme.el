;;; init-theme.el --- Config Theme  -*- lexical-binding: t -*-

;; Copyright (C) 2020 王北洛

;; Author: 王北洛 <wbeiluo@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;; Code:

;; Install additinal themes from melpa
;; make sure to use :defer keyword
;; (use-package doom-themes
;;   :ensure
;;   :config
;;   ;; Global settings (defaults)
;;   (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;         doom-themes-enable-italic t) ; if nil, italics is universally disabled

;;   ;; (if (display-graphic-p)
;;   ;;     (load-theme 'doom-solarized-dark t)
;;   ;;   (load-theme 'doom-solarized-light t))

;;   ;; Enable flashing mode-line on errors
;;   (doom-themes-visual-bell-config)

;;   ;; Enable custom neotree theme (all-the-icons must be installed!)
;;   (doom-themes-neotree-config)
;;   ;; or for treemacs users
;;   (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
;;   (doom-themes-treemacs-config)

;;   ;; Corrects (and improves) org-mode's native fontification.
;;   (doom-themes-org-config))

;; Switching themes on time of day
;; (use-package circadian
;;   :ensure t
;;   :config
;;   (setq circadian-themes '(("8:00" . solarized-light)
;;                            ("19:30" . solarized-dark)))
;;   (circadian-setup))


(use-package solarized-theme
  :ensure t
  :init
  (setq x-underline-at-descent-line t)

  ;; make the fringe stand out from the background
  (setq solarized-distinct-fringe-background nil)

  ;; Don't change the font for some headings and titles
  (setq solarized-use-variable-pitch nil)

  ;; make the modeline high contrast
  (setq solarized-high-contrast-mode-line nil)

  ;; Use less bolding
  (setq solarized-use-less-bold t)

  ;; Use more italics
  (setq solarized-use-more-italic t)

  ;; Use less colors for indicators such as git:gutter, flycheck and similar
  (setq solarized-emphasize-indicators nil)

  ;; Don't change size of org-mode headlines (but keep other size-changes)
  (setq solarized-scale-org-headlines nil)

  ;; Avoid all font-size changes
  (setq solarized-height-minus-1 1.0)
  (setq solarized-height-plus-1 1.0)
  (setq solarized-height-plus-2 1.0)
  (setq solarized-height-plus-3 1.0)
  (setq solarized-height-plus-4 1.0)

  :config
  ;; 加载配置
  (if (display-graphic-p)
      (load-theme 'solarized-dark t)
    (load-theme 'tango t))

  ;; (set-face-attribute 'mode-line          nil :overline   nil)
  ;; (set-face-attribute 'mode-line-inactive nil :overline   nil)
  ;; (set-face-attribute 'mode-line          nil :underline  nil)
  ;; (set-face-attribute 'mode-line-inactive nil :underline  nil)
  ;; (set-face-attribute 'mode-line          nil :box        nil)
  ;; (set-face-attribute 'mode-line-inactive nil :box        nil)

  ;; 取消modeline边框及下划线
  (set-face-attribute 'mode-line nil
                      ;; :background "#073642"
                      ;; :foreground "#839496"
                      :box nil
                      :overline nil
                      :underline nil)
  (set-face-attribute 'mode-line-inactive nil
                      :box nil
                      :overline nil
                      :underline nil)

  ;; 取消header-line下划线
  (set-face-attribute 'header-line nil
                      :box nil
                      :overline nil
                      :underline nil)
  )

(provide 'init-theme)
;;; init-theme.el ends here
