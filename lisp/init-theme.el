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
  )

;; dsiable-theme before load-theme
(defcustom load-theme-before-hook nil
  "Functions to run before load theme."
  :type 'hook)

(defcustom load-theme-after-hook nil
  "Functions to run after load theme."
  :type 'hook)

(defun load-theme-hook-wrapper (origin-func theme &rest args)
  "A wrapper of hooks around `load-theme'."
  (mapc #'disable-theme custom-enabled-themes)
  (run-hook-with-args 'load-theme-before-hook theme)
  (apply origin-func theme args)
  (run-hook-with-args 'load-theme-after-hook theme))

(advice-add 'load-theme :around #'load-theme-hook-wrapper)

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

  (set-face-attribute 'header-line nil
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
(if (display-graphic-p)
    (light)
  (load-theme 'tsdh-light t))

(provide 'init-theme)
;;; init-theme.el ends here
