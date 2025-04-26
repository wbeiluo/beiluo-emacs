;;; init-theme.el --- Config Theme  -*- lexical-binding: t -*-

;; Copyright (C) 2022~2025 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
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
  :config
  ;; (load-theme 'solarized-dark :no-confim)
  ;; (load-theme 'solarized-light :no-confim)
  )

(use-package modus-themes
  :ensure t
  :config
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil)

  ;; Load the theme of your choice.
  ;; (load-theme 'modus-vivendi :no-confim)
  ;; :bind ("<f5>" . modus-themes-toggle)
  )

;; switch to light theme
(defun light_modus ()
  "Activate modus light color theme."
  (interactive)
  (load-theme 'modus-operandi :no-confim)

  ;; 设置选中区域背景色
  ;; (set-face-attribute 'region nil
  ;;                     :distant-foreground 'unspecified
  ;;                     ;; :foreground "grey"
  ;;                     :background "#b6bfc5")
  
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
                      :underline nil)
  )

;; switch to dark theme
(defun dark_modus ()
  "Activate modus dark color theme."
  (interactive)
  
  (load-theme 'modus-vivendi :no-confim)

  ;; 设置选中区域背景色
  ;; (set-face-attribute 'region nil
  ;;                     :distant-foreground 'unspecified
  ;;                     ;; :foreground "grey"
  ;;                     :background "#45526b")

  ;; 取消Modeline边框
  (set-face-attribute 'mode-line nil
                      :box nil
                      :overline nil
                      :underline nil
		      :background "#242424")

  (set-face-attribute 'mode-line-inactive nil
                      :box nil
                      :overline nil
                      :underline nil)

  (set-face-attribute 'header-line t
                      :box nil
                      :overline nil
                      :underline nil))

;; switch to light theme
(defun light_solarized ()
  "Activate solarized light color theme."
  (interactive)

  (load-theme 'solarized-light :no-confim)

  ;; 设置选中区域背景色
  ;; (set-face-attribute 'region nil
  ;;                     ;; :distant-foreground 'unspecified
  ;;                     :foreground "#5d5b53"
  ;;                     :background "#e6e0ce")

  ;; 取消Modeline边框
  (set-face-attribute 'mode-line nil
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
                      :underline nil)
  )

;; switch to dark theme
(defun dark_solarized ()
  "Activate solarized dark color theme."
  (interactive)
  
  (load-theme 'solarized-dark :no-confim)

  ;; 设置选中区域背景色
  ;; (set-face-attribute 'region nil
  ;;                     ;; :distant-foreground 'unspecified
  ;;                     :foreground "grey"
  ;;                     :background "#103d49")

  ;; 取消Modeline边框
  (set-face-attribute 'mode-line nil
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
                      :underline nil)
  )

;; different theme for term and gui
(if (display-graphic-p)
    (dark_solarized) ; light_modus
  (dark_modus))

;; 终端下不设置背景色(需要终端背景色与主题匹配)
;; (defun set-background-for-terminal (&optional frame)
;;   (or frame (setq frame (selected-frame)))
;;   "Unsets the background color in terminal mode"
;;   (unless (display-graphic-p frame)
;;     (set-face-background 'default"unspecified-bg" frame)))
;; (add-hook 'after-make-frame-functions 'set-background-for-terminal)
;; (add-hook 'window-setup-hook 'set-background-for-terminal)

(provide 'init-theme)
;;; init-theme.el ends here
