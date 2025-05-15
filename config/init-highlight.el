;; init-highlight.el --- Highlight Configurations. -*- lexical-binding: t -*-

;; Copyright (C) 2025~2025 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;; 高亮显示标志、括号、缩进线
;;

;;; Code:

(require 'symbol-overlay)
(require 'highlight-indent-guides)
(require 'rainbow-delimiters)
(require 'colorful-mode)

;; Highlight symbols
(add-hook 'prog-mode-hook 'symbol-overlay-mode)

;; Highlight indent
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'character)
(setq highlight-indent-guides-character ?|)
(set-face-foreground 'highlight-indent-guides-character-face "LightGray")

;; Highlight parentheses
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Colorful mode
(setq colorful-use-prefix t)
(setq colorful-only-strings 'only-prog)
(setq css-fontify-colors nil)
(global-colorful-mode 1)
(add-to-list 'global-colorful-modes 'helpful-mode)

(provide 'init-highlight)
;;; init-highlight.el ends here
