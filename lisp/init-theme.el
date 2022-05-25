;;; init-theme.el --- Config Theme  -*- lexical-binding: t -*-

;; Copyright (C) 2022 王北洛

;; Author: 王北洛 <wbeiluo@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;; Code:

;; (use-package solarized-theme
;;   :ensure t
;;   :init
;;   (setq x-underline-at-descent-line t)

;;   make the fringe stand out from the background
;;   (setq solarized-distinct-fringe-background nil)

;;   Don't change the font for some headings and titles
;;   (setq solarized-use-variable-pitch nil)

;;   make the modeline high contrast
;;   (setq solarized-high-contrast-mode-line nil)

;;   Use less bolding
;;   (setq solarized-use-less-bold t)

;;   Use more italics
;;   (setq solarized-use-more-italic t)

;;   Use less colors for indicators such as git:gutter, flycheck and similar
;;   (setq solarized-emphasize-indicators t)

;;   Don't change size of org-mode headlines (but keep other size-changes)
;;   (setq solarized-scale-org-headlines t)

;;   ;; Avoid all font-size changes
;;   (setq solarized-height-minus-1 1.0)
;;   (setq solarized-height-plus-1 1.0)
;;   (setq solarized-height-plus-2 1.0)
;;   (setq solarized-height-plus-3 1.0)
;;   (setq solarized-height-plus-4 1.0)

;;   :config
;;   (load-theme 'solarized-dark t)

;;   (set-face-attribute 'mode-line nil
;;                       :box nil
;;                       :overline nil
;;                       :underline nil)

;;   (set-face-attribute 'mode-line-inactive nil
;;                       :box nil
;;                       :overline nil
;;                       :underline nil)

;;   (set-face-attribute 'header-line nil
;;                       :background "#103d49";;"#073642"
;;                       :foreground "#839496"
;;                       :box nil
;;                       :overline nil
;;                       :underline nil)
;;   )

;; ;; 取消modeline边框及下划线, 取消header-line下划线
;; (defun del-line-of-modeline()
;;   (set-face-attribute 'mode-line nil
;;                       ;; :background "#073642"
;;                       ;; :foreground "#839496"
;;                       :box nil
;;                       :overline nil
;;                       :underline nil)

;;   (set-face-attribute 'mode-line-inactive nil
;;                       :box nil
;;                       :overline nil
;;                       :underline nil)

;;   (set-face-attribute 'header-line t
;;                       :box nil
;;                       :overline nil
;;                       :underline nil))

(use-package modus-themes
  :ensure
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-region '(bg-only no-extend))

  ;; Load the theme files before enabling a theme
  (modus-themes-load-themes)
  :config
  ;; Load the theme of your choice:
  ;; (modus-themes-load-operandi) ;; OR
  ;; (modus-themes-load-vivendi)

  ;; :bind ("<f6>" . modus-themes-toggle)
  )

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;;(load-theme 'doom-vibrant t)
  ;;(load-theme 'doom-one-light t)
  ;;(load-theme 'doom-solarized-dark-high-contrast t)
  ;;(load-theme 'doom-solarized-dark t)
  ;;(load-theme 'doom-solarized-light t)
  ;;(load-theme 'doom-nord t)
  ;;(load-theme 'doom-tomorrow-night t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ;doom-atom; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; ;; Switching themes on time of day
;; (if (display-graphic-p)
;;     (use-package circadian
;;       :ensure t
;;       :config
;;       (setq circadian-themes '(("8:00" . doom-solarized-light)
;;                                ("21:00" . doom-solarized-dark-high-contrast)))
;;       (circadian-setup))
;;   (modus-themes-load-vivendi))

;; switch to light theme
(defun light ()
  "Activate a light color theme."
  (interactive)
  ;;(load-theme 'doom-one-light t)
  (modus-themes-load-operandi)
  ;; 取消Modeline边框
  (set-face-attribute 'mode-line nil
                      :box nil
                      :overline nil
                      :underline nil
		      :background "#eee9e9")

  (set-face-attribute 'mode-line-inactive nil
                      :box nil
                      :overline nil
                      :underline nil)
  
  (set-face-attribute 'header-line t
                      :box nil
                      :overline nil
                      :underline nil))

;; switch to dark theme
(defun dark ()
  "Activate a dark color theme."
  (interactive)
  ;;(load-theme 'doom-solarized-dark-high-contrast t)
  (modus-themes-load-vivendi)
  ;; 取消Modeline边框
  (set-face-attribute 'mode-line nil
                      :box nil
                      :overline nil
                      :underline nil
		      :background "#2a2a3a")

  (set-face-attribute 'mode-line-inactive nil
                      :box nil
                      :overline nil
                      :underline nil)

  (set-face-attribute 'header-line t
                      :box nil
                      :overline nil
                      :underline nil))

;; different theme for term and gui
(if (display-graphic-p)
    (light)
  (dark))

;; 终端下不设置背景色(需要终端背景色与主题匹配)
(defun set-background-for-terminal (&optional frame)
  (or frame (setq frame (selected-frame)))
 "unsets the background color in terminal mode"
  (unless (display-graphic-p frame)
    (set-face-background 'default"unspecified-bg" frame)))
(add-hook 'after-make-frame-functions 'set-background-for-terminal)
(add-hook 'window-setup-hook 'set-background-for-terminal)

(provide 'init-theme)
;;; init-theme.el ends here
