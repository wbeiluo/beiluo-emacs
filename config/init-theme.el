;;; init-theme.el --- Config Theme  -*- lexical-binding: t -*-

;; Copyright (C) 2022~2025 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;; Code:

(require 'solarized-theme)
(require 'modus-themes)

;;; Solarized-theme setting
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

(setq x-underline-at-descent-line t)

;;; Modus-themes setting
;; Add all your customizations prior to loading the themes
(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs nil)

;; switch to light theme
(defun light-modus ()
  "Activate modus light color theme."
  (interactive)

  (mapc #'disable-theme custom-enabled-themes)

  (load-theme 'modus-operandi t)

  ;; 设置modeline颜色
  (set-face-attribute 'mode-line nil
                      :background "#e6e6e6")

  ;; 设置选中区域背景色
  ;; (set-face-attribute 'region nil
  ;;                     :distant-foreground 'unspecified
  ;;                     ;; :foreground "grey"
  ;;                     :background "#b6bfc5")

  ;; 取消Modeline边框
  ;;   (set-face-attribute 'mode-line nil
  ;;                       :box nil
  ;;                       :overline nil
  ;;                       :underline nil
  ;;                       :background "#eee9e9")
  ;;
  ;;   (set-face-attribute 'mode-line-inactive nil
  ;;                       :box nil
  ;;                       :overline nil
  ;;                       :underline nil)
  
  ;;   (set-face-attribute 'header-line t
  ;;                       :box nil
  ;;                       :overline nil
  ;;                       :underline nil)
  )

;; switch to dark theme
(defun dark-modus ()
  "Activate modus dark color theme."
  (interactive)

  (mapc #'disable-theme custom-enabled-themes)

  (load-theme 'modus-vivendi t)

  ;; 设置modeline背景颜色
  (set-face-attribute 'mode-line nil
                      :background "#000000")

  ;; 设置line-number背景颜色
  (set-face-attribute 'line-number nil
                      :background "#000000")

  ;; 设置选中区域背景色
  ;; (set-face-attribute 'region nil
  ;;                     :distant-foreground 'unspecified
  ;;                     ;; :foreground "grey"
  ;;                     :background "#45526b")

  ;; 取消Modeline边框
  ;;   (set-face-attribute 'mode-line nil
  ;;                       :box nil
  ;;                       :overline nil
  ;;                       :underline nil
  ;;                       :background "#242424")
  ;;
  ;;   (set-face-attribute 'mode-line-inactive nil
  ;;                       :box nil
  ;;                       :overline nil
  ;;                       :underline nil)
  ;;
  ;;   (set-face-attribute 'header-line t
  ;;                       :box nil
  ;;                       :overline nil
  ;;                       :underline nil)
  )

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
