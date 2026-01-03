;;; init-theme.el --- Config Theme  -*- lexical-binding: t -*-

;; Copyright (C) 2022~2025 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;; Code:

(defconst extensions-solarized-emacs-dir
  (expand-file-name "extensions/solarized-emacs" user-emacs-directory))
(defconst extensions-modus-themes-dir
  (expand-file-name "extensions/modus-themes" user-emacs-directory))

;;; Solarized-theme
(use-package solarized-theme
  :ensure nil
  :load-path extensions-solarized-emacs-dir
  :custom
  ;; make the fringe stand out from the background
  (solarized-distinct-fringe-background nil)

  ;; Don't change the font for some headings and titles
  (solarized-use-variable-pitch nil)

  ;; make the modeline high contrast
  (solarized-high-contrast-mode-line nil)

  ;; Use less bolding
  (solarized-use-less-bold t)

  ;; Use more italics
  (solarized-use-more-italic t)

  ;; Use less colors for indicators such as git:gutter, flycheck and similar
  (solarized-emphasize-indicators nil)

  ;; Don't change size of org-mode headlines (but keep other size-changes)
  (solarized-scale-org-headlines nil)

  ;; Avoid all font-size changes
  (solarized-height-minus-1 1.0)
  (solarized-height-plus-1 1.0)
  (solarized-height-plus-2 1.0)
  (solarized-height-plus-3 1.0)
  (solarized-height-plus-4 1.0)

  (x-underline-at-descent-line t))

;;; Modus-themes
(use-package modus-themes
  :ensure nil
  :load-path extensions-modus-themes-dir
  :custom
  (modus-themes-to-toggle '(modus-operandi modus-vivendi))
  (modus-themes-to-rotate modus-themes-items)
  (modus-themes-mixed-fonts t)
  (modus-themes-variable-pitch-ui t)
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-completions '((t . (bold))))
  (modus-themes-prompts '(bold))
  (modus-themes-common-palette-overrides nil)
  :config
  (modus-themes-include-derivatives-mode 1))

;; switch to light theme
(defun light-modus ()
  "Activate modus light color theme."
  (interactive)

  (mapc #'disable-theme custom-enabled-themes)

  ;; 修改部分背景颜色
  (setq modus-themes-common-palette-overrides
      '((bg-mode-line-active "#efefef")
        (fg-mode-line-active "#000000")
        (bg-mode-line-inactive "#ffffff")
        (fg-mode-line-inactive "#000000")
        (bg-line-number-active "#e9e9e9")
        (fg-line-number-active 'fg-main)
        (bg-line-number-inactive 'bg-dim)
        (fg-line-number-inactive 'fg-dim)))

  (modus-themes-load-theme 'modus-operandi))

;; switch to dark theme
(defun dark-modus ()
  "Activate modus dark color theme."
  (interactive)

  (mapc #'disable-theme custom-enabled-themes)

  ;; 修改部分背景颜色
  (setq modus-themes-common-palette-overrides
      '((bg-mode-line-active "#191919")
        (fg-mode-line-active "#ffffff")
        (bg-mode-line-inactive "#000000")
        (fg-mode-line-inactive "#ffffff")
        (bg-line-number-active "#353535")
        (fg-line-number-active "#ffffff")
        (bg-line-number-inactive "#000000")
        (fg-line-number-inactive "#ffffff")))

  (modus-themes-load-theme 'modus-vivendi))

;; switch to light theme
(defun light-solarized ()
  "Activate solarized light color theme."
  (interactive)

  (mapc #'disable-theme custom-enabled-themes)

  (load-theme 'solarized-light t)

  ;; 设置选中区域背景色
  ;; (set-face-attribute 'region nil
  ;;                     ;; :distant-foreground 'unspecified
  ;;                     :foreground "#5d5b53"
  ;;                     :background "#e6e0ce")
  )

;; switch to dark theme
(defun dark-solarized ()
  "Activate solarized dark color theme."
  (interactive)
  
  (mapc #'disable-theme custom-enabled-themes)

  (load-theme 'solarized-dark t)

  ;; 设置选中区域背景色
  ;; (set-face-attribute 'region nil
  ;;                     ;; :distant-foreground 'unspecified
  ;;                     :foreground "grey"
  ;;                     :background "#103d49")
  )

;; 根据时间自动切换主题
(defun switch-theme-based-on-time ()
  "Switch Emacs themes based on the current time."
  (let* ((current-hour (string-to-number (format-time-string "%H")))
         (is-day-time (<= 7 current-hour 18))) ; 白天时间为 7:00 到 18:00
    (if is-day-time
        (light-modus) ; 白天主题
      (dark-modus)))) ; 夜间主题

(if (display-graphic-p)
    (run-with-timer 0 3600 'switch-theme-based-on-time) ; 启动时立即切换主题，之后每1小时切换一次
  (dark-modus))

(provide 'init-theme)
;;; init-theme.el ends here
