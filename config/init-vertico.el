;;; init-vertico.el --- Vertico Completion Configurations -*- lexical-binding: t -*-

;; Copyright (C) 2025~2025 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;;  Vertico
;;

;;; Code:

(defconst extensions-vertico-dir
  (list (expand-file-name "extensions/vertico" user-emacs-directory)
        (expand-file-name "extensions/vertico/extensions" user-emacs-directory)))
(defconst extensions-orderless-dir
  (expand-file-name "extensions/orderless" user-emacs-directory))
(defconst extensions-pinyinlib-dir
  (expand-file-name "extensions/pinyinlib" user-emacs-directory))
(defconst extensions-marginalia-dir
  (expand-file-name "extensions/marginalia" user-emacs-directory))
(defconst extensions-nerd-icons-completion-dir
  (expand-file-name "extensions/nerd-icons-completion" user-emacs-directory))

(use-package vertico
  :ensure nil
  :load-path extensions-vertico-dir
  :custom
  ;; Grow and shrink the Vertico minibuffer
  (vertico-resize t)
  ;; Enable cycling for `vertico-next/previous'
  (vertico-cycle t)
  :config
  (vertico-mode 1)
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
  :ensure nil
  :load-path extensions-orderless-dir
  :custom
  (completion-styles '(orderless basic))
  (Completion-category-overrides '((file (styles partial-completion))))
  ;; Disable defaults, use our settings
  (completion-category-defaults nil)
  ;; Emacs 31: partial-completion behaves like substring
  (completion-pcm-leading-wildcard t))

;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  :ensure nil
  :load-path extensions-vertico-dir
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-sort
  :after vertico
  :ensure nil
  :load-path extensions-vertico-dir)

(use-package vertico-repeat
  :after vertico
  :ensure nil
  :load-path extensions-vertico-dir)

;; Vertico 显示样式和行为
(use-package vertico-multiform
  :after vertico
  :ensure nil
  :load-path extensions-vertico-dir
  :config
  (vertico-multiform-mode)

  (use-package vertico-flat
    :after vertico
    :ensure nil
    :load-path extensions-vertico-dir)
  (use-package vertico-reverse
    :after vertico
    :ensure nil
    :load-path extensions-vertico-dir)
  (use-package vertico-buffer
    :after vertico
    :ensure nil
    :load-path extensions-vertico-dir)
  (use-package vertico-grid
    :after vertico
    :ensure nil
    :load-path extensions-vertico-dir)

  (setq vertico-multiform-categories
        '((file)
          (command)
          (buffer reverse)
          (consult-grep)
          ;; (consult-grep buffer)
          (unicode-name grid)
	  (embark-keybinding grid))))

(use-package marginalia
  :ensure nil
  :load-path extensions-marginalia-dir
  :init
  (define-key minibuffer-local-map (kbd "M-A") 'marginalia-cycle)
  (setq marginalia-annotators '(marginalia-annotate-heavy))
  :config
  ;; Enable rich annotations using the Marginalia
  (marginalia-mode 1))

;; Support Pinyin
(use-package pinyinlib
  :ensure nil
  :load-path extensions-pinyinlib-dir
  :config
  (defun orderless-regexp-pinyin (str)
    "Match COMPONENT STR as a pinyin regex."
    (orderless-regexp (pinyinlib-build-regexp-string str)))
  (add-to-list 'orderless-matching-styles 'orderless-regexp-pinyin))

;; Add icons to completion candidates
(use-package nerd-icons-completion
  :ensure nil
  :load-path extensions-nerd-icons-completion-dir
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :config
  (nerd-icons-completion-mode 1))

(provide 'init-vertico)
;;; init-vertico.el ends here
