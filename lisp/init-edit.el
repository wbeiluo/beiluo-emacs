;; init-edit.el --- edit configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2020 王北洛

;; Author: 王北洛 <wbeiluo@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;; 光标移动及文本编辑操作
;;

;;; Code:

;; 移动至行首或行尾
(use-package mwim
  :ensure t
  :bind (("C-a" . mwim-beginning-of-code-or-line)
	 ("C-e" . mwim-end-of-code-or-line)))

;;(use-package ace-jump-mode :ensure t)  ; 任意

;; Jump to things in Emacs tree-style
(use-package avy
  :ensure t
  :bind ("M-j" . avy-goto-char-timer))

;; Goto last change
(use-package goto-chg
  :ensure t
  :bind ("C-M-," . goto-last-change))

;; Record and jump to the last point in the buffer
(use-package goto-last-point
  :diminish
  :hook (after-init . goto-last-point-mode)
  :bind ("C-," . goto-last-point))

;; Highlight Line
(use-package hl-line
  :ensure nil
  :hook
  (after-init . global-hl-line-mode))

(use-package highlight-symbol
  :ensure t
  :init (highlight-symbol-mode))

(use-package highlight-parentheses
  :config
  (global-highlight-parentheses-mode 1))

;; Rainbow Mode
(use-package rainbow-mode
  :diminish
  :hook (emacs-lisp-mode . rainbow-mode))

;; Rainbow delimiters
(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

;; 文本移动
;; Drag stuff (lines, words, region, etc...) around
;; <M-up> <M-down> <M-right> <M-left>
(use-package drag-stuff
  :diminish
  :commands drag-stuff-define-keys
  :hook (after-init . drag-stuff-global-mode)
  :config
  (add-to-list 'drag-stuff-except-modes 'org-mode)
  (drag-stuff-define-keys))

;; 撤销树
;; (use-package undo-tree
;;   :ensure t
;;   :bind ("C-c u" . hydra-undo-tree/body)
;;   :config
;;   (global-undo-tree-mode)
;;   ;; 将备份文件集中管理
;;   (setq undo-tree-history-directory-alist `(("." . "~/.emacs.d/.cache/")))
;;   (defhydra hydra-undo-tree (:hint nil)
;;   "
;;   _p_: undo  _n_: redo _s_: save _l_: load   "

;;   ("p"   undo-tree-undo)
;;   ("n"   undo-tree-redo)
;;   ("s"   undo-tree-save-history)
;;   ("l"   undo-tree-load-history)
;;   ("u"   undo-tree-visualize "visualize" :color "deep sky blue")
;;   ("q"   nil "quit" :color "deep sky blue")))

;; visual displays the undo history
(use-package vundo
  :ensure t
  :bind ("C-x u" . vundo))

;; 多行文本操作
(use-package multiple-cursors
  :ensure t
  :bind ("C-S-s" . mc/edit-lines))

(use-package imenu :ensure t)

(provide 'init-edit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-edit.el ends here
