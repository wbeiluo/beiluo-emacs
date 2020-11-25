;; init-highlight.el --- highlight configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2020 王北洛

;; Author: 王北洛 <wbeiluo@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;; highlight configurations.
;;

;;; Code:

;; Highlight Line
(use-package hl-line
  :ensure nil
  :hook
  (after-init . global-hl-line-mode))

(use-package highlight-parentheses
 :config
 (global-highlight-parentheses-mode 1))

;; Beacon 当滚动时高亮光标所在行
(use-package beacon
  :custom
  (beacon-color "#66ccff")
  :hook (after-init . beacon-mode))

;; Rainbow Mode
(use-package rainbow-mode
  :diminish
  :hook (emacs-lisp-mode . rainbow-mode))

;; Rainbow delimiters
(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))


(provide 'init-highlight)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-highlight.el ends here
