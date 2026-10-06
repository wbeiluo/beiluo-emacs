;;; init-git.el --- Git Configurations. -*- lexical-binding: t -*-

;; Copyright (C) 2025~2026 王北洛

;; Author: 王北洛 <beiluo.wang@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;;  Magit配置
;;
;;; Code:

;; Compact syntax for short lambda
(use-package llama
  :ensure t)

;; Additional and improved binding conditionals
(use-package cond-let
  :ensure t)

;; Transient command menus
(use-package transient
  :ensure t)

;; Use the Emacsclient as the $EDITOR of child processes
(use-package with-editor
  :ensure t)

;; A Git Porcelain inside Emacs
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-c M-g" . magit-file-popup))
  :config
  (add-to-list 'auto-mode-alist '("\\COMMIT_EDITMSG\\'" . text-mode))
  (add-to-list 'auto-mode-alist '("\\MERGE_MSG\\'" . text-mode))
  (with-eval-after-load 'info
    (info-initialize)
    (add-to-list 'Info-directory-list (expand-file-name "extensions/magit/docs" user-emacs-directory))))

;; highlighting uncommitted changes
(use-package diff-hl
  :ensure t
  :config
  ;; Highlighting uncommitted changes
  (global-diff-hl-mode 1)

  (require 'diff-hl-dired)
  (require 'diff-hl-margin)

  ;; When Emacs runs in terminal, show the indicators in margin instead.
  (unless (display-graphic-p)
    (diff-hl-margin-mode 1))
  ;; Add hook
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode-unless-remote)
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(provide 'init-git)

;;; init-git.el ends here
