;;; init-vertico.el --- Vertico Completion Configurations -*- lexical-binding: t -*-

;; Copyright (C) 2025~2025 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;;  Vertico
;;

;;; Code:

(require 'vertico)
(require 'vertico-sort)
(require 'vertico-directory)
(require 'vertico-repeat)

;; 未生成autoload时,需在此手动设置vertico-sort-function
;; (setq vertico-sort-function #'vertico-sort-history-length-alpha)
;; Grow and shrink the Vertico minibuffer
(setq vertico-resize t)
(vertico-mode 1)

;; Persist history over Emacs restarts. Vertico sorts by history position.
(savehist-mode 1)

;; A few more useful configurations...
;; Support opening new minibuffers from inside existing minibuffers.
(setq enable-recursive-minibuffers t)
;; Hide commands in M-x which do not work in the current mode.  Vertico
;; commands are hidden in normal buffers. This setting is useful beyond
;; Vertico.
(setq read-extended-command-predicate #'command-completion-default-include-p)
;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))

(require 'orderless)
;; Use the 'orderless' completion style.
(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides '((file (styles basic partial-completion))))

;; Support Pinyin
(require 'pinyinlib)
(defun orderless-regexp-pinyin (str)
  "Match COMPONENT STR as a pinyin regex."
  (orderless-regexp (pinyinlib-build-regexp-string str)))
(add-to-list 'orderless-matching-styles 'orderless-regexp-pinyin)

;; More convenient directory navigation commands
(define-key vertico-map (kbd "RET") #'vertico-directory-enter)
(define-key vertico-map (kbd "DEL") #'vertico-directory-delete-char)
(define-key vertico-map (kbd "M-DEL") #'vertico-directory-delete-word)
(add-hook 'rfn-eshadow-update-overlay-hook 'vertico-directory-tidy)

(require 'marginalia)
(define-key minibuffer-local-map (kbd "M-A") 'marginalia-cycle)
;; 定制注解内容
(setq marginalia-annotators '(marginalia-annotate-heavy))
;; Enable rich annotations using the Marginalia
(marginalia-mode 1)

;; Add icons to completion candidates
(require 'nerd-icons-completion)
(nerd-icons-completion-mode 1)
(add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)

(provide 'init-vertico)
;;; init-vertico.el ends here
