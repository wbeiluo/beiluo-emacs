;;; init-theme.el --- Config Theme  -*- lexical-binding: t -*-

;; Copyright (C) 2020 王北洛

;; Author: 王北洛 <wbeiluo@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;; Code:

(use-package solarized-theme
  :ensure t
  :init
  (setq x-underline-at-descent-line t)

  ;; make the fringe stand out from the background
  (setq solarized-distinct-fringe-background nil)

  ;; Don't change the font for some headings and titles
  (setq solarized-use-variable-pitch t)

  ;; make the modeline high contrast
  (setq solarized-high-contrast-mode-line nil)

  ;; Use less bolding
  (setq solarized-use-less-bold t)

  ;; Use more italics
  (setq solarized-use-more-italic t)

  ;; Use less colors for indicators such as git:gutter, flycheck and similar
  (setq solarized-emphasize-indicators t)

  ;; Don't change size of org-mode headlines (but keep other size-changes)
  (setq solarized-scale-org-headlines t)

  ;; Avoid all font-size changes
  (setq solarized-height-minus-1 1.0)
  (setq solarized-height-plus-1 1.0)
  (setq solarized-height-plus-2 1.0)
  (setq solarized-height-plus-3 1.0)
  (setq solarized-height-plus-4 1.0)
  :config
  (load-theme 'solarized-dark t)

  (set-face-attribute 'mode-line nil
                      :box nil
                      :overline nil
                      :underline nil)

  (set-face-attribute 'mode-line-inactive nil
                      :box nil
                      :overline nil
                      :underline nil)

  (set-face-attribute 'header-line nil
                      :background "#103d49";;"#073642"
                      :foreground "#839496"
                      :box nil
                      :overline nil
                      :underline nil)
  )

;; dsiable-theme before load-theme
;; (defcustom load-theme-before-hook nil
;;   "Functions to run before load theme."
;;   :type 'hook)

;; (defcustom load-theme-after-hook nil
;;   "Functions to run after load theme."
;;   :type 'hook)

;; (defun load-theme-hook-wrapper (origin-func theme &rest args)
;;   "A wrapper of hooks around `load-theme'."
;;   (mapc #'disable-theme custom-enabled-themes)
;;   (run-hook-with-args 'load-theme-before-hook theme)
;;   (apply origin-func theme args)
;;   (run-hook-with-args 'load-theme-after-hook theme))

;; (advice-add 'load-theme :around #'load-theme-hook-wrapper)

;; 取消modeline边框及下划线, 取消header-line下划线
(defun del-line-of-modeline()
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

  (set-face-attribute 'header-line t
                      :box nil
                      :overline nil
                      :underline nil))

;; switch to light theme
(defun light ()
  "Activate a light color theme."
  (interactive)
  (load-theme 'solarized-light t)
  (del-line-of-modeline))

;; switch to dark theme
(defun dark ()
  "Activate a dark color theme."
  (interactive)
  (load-theme 'solarized-dark t)
  (del-line-of-modeline))

;; (use-package nano-theme
;;   :load-path "extensions/nano-theme/"
;;   :config
;;   (load-theme 'nano t)
;;   (nano-dark))


;; Switching themes on time of day
;; (if (display-graphic-p)
;;     (use-package circadian
;;       :ensure t
;;       :config
;;       (setq circadian-themes '(("8:00" . solarized-light)
;;                                ("19:30" . solarized-dark)))
;;       (circadian-setup))
;;   (load-theme 'tango t))

;; default theme
;; (if (display-graphic-p)
;;    (dark)
;;  (load-theme 'tango t))

;; (use-package doom-themes
;;   :ensure t
;;   :config
;;   ;; Global settings (defaults)
;;   (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;         doom-themes-enable-italic t) ; if nil, italics is universally disabled
;;   (load-theme 'doom-vibrant t)

;;   ;; Enable flashing mode-line on errors
;;   (doom-themes-visual-bell-config)
;;   ;; Enable custom neotree theme (all-the-icons must be installed!)
;;   (doom-themes-neotree-config)
;;   ;; or for treemacs users
;;   (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
;;   (doom-themes-treemacs-config)
;;   ;; Corrects (and improves) org-mode's native fontification.
;;   (doom-themes-org-config))

;; 终端下不设置背景色
;; (defun set-background-for-terminal (&optional frame)
;;   (or frame (setq frame (selected-frame)))
;;  "unsets the background color in terminal mode"
;;   (unless (display-graphic-p frame)
;;     (set-face-background 'default"unspecified-bg" frame)))
;; (add-hook 'after-make-frame-functions 'set-background-for-terminal)
;; (add-hook 'window-setup-hook 'set-background-for-terminal)

(provide 'init-theme)
;;; init-theme.el ends here
