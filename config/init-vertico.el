;;; init-vertico.el --- Vertico Completion Configurations -*- lexical-binding: t -*-

;; Copyright (C) 2025~2026 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;;  Vertico
;;

;;; Code:

;; VERTical Interactive COmpletion
(use-package vertico
  :ensure t
  :custom
  ;; Grow and shrink the Vertico minibuffer
  (vertico-resize t)
  ;; Enable cycling for `vertico-next/previous'
  (vertico-cycle t)
  :init
  (vertico-mode))

;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-multiform
  :after vertico
  :config
  (vertico-multiform-mode)
  (setq vertico-multiform-categories
        '((file)
          (command)
          (buffer reverse)
          (consult-grep)
          (unicode-name grid)
          (embark-keybinding grid))))

(use-package vertico-sort
  :after vertico
  :config
  (setq vertico-sort-function #'vertico-sort-history-alpha))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :ensure nil
  :config
  (savehist-mode 1)
  (add-to-list 'savehist-additional-variables 'kill-ring))

;; Emacs minibuffer configurations.
(use-package emacs
  :custom
  ;; Enable context menu. `vertico-multiform-mode' adds a menu in the minibuffer
  ;; to switch display modes.
  (context-menu-mode t)
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))

;; Use the 'orderless' completion style.
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (Completion-category-overrides '((file (styles partial-completion))))
  ;; Disable defaults, use our settings
  (completion-category-defaults nil)
  ;; Emacs 31: partial-completion behaves like substring
  (completion-pcm-leading-wildcard t))

(use-package marginalia
  :ensure t
  :init
  (define-key minibuffer-local-map (kbd "M-A") 'marginalia-cycle)
  (setq marginalia-annotators '(marginalia-annotate-heavy))
  :config
  ;; Enable rich annotations using the Marginalia
  (marginalia-mode 1))

;; Support Pinyin
(use-package pinyinlib
  :ensure t
  :config
  (defun orderless-regexp-pinyin (str)
    "Match COMPONENT STR as a pinyin regex."
    (orderless-regexp (pinyinlib-build-regexp-string str)))
  (add-to-list 'orderless-matching-styles 'orderless-regexp-pinyin))

;; Add icons to completion candidates
(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :config
  (nerd-icons-completion-mode))

(provide 'init-vertico)
;;; init-vertico.el ends here
