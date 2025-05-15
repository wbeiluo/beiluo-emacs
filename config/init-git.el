;;; init-git.el --- Git Configurations. -*- lexical-binding: t -*-

;; Copyright (C) 2025~2025 王北洛

;; Author: 王北洛 <beiluo.wang@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;;  Magit配置
;;
;;; Code:

(require 'compat)
(require 'llama)
(require 'transient)
(require 'with-editor)
(require 'magit)
(require 'diff-hl)
(require 'diff-hl-margin)
(require 'diff-hl-amend)
(require 'diff-hl-dired)
(require 'diff-hl-flydiff)
(require 'diff-hl-inline-popup)
(require 'diff-hl-show-hunk)
(require 'diff-hl-show-hunk-posframe)

;; (use-package magit
;;   :ensure t)

;; mode
(add-to-list 'auto-mode-alist '("\\COMMIT_EDITMSG\\'" . text-mode))
(add-to-list 'auto-mode-alist '("\\MERGE_MSG\\'" . text-mode))

(with-eval-after-load 'info
  (info-initialize)
  (add-to-list 'Info-directory-list (expand-file-name "extensions/magit/docs" user-emacs-directory)))

;; Highlighting uncommitted changes
(global-diff-hl-mode)
;; When Emacs runs in terminal, show the indicators in margin instead.
(unless (display-graphic-p)
  (diff-hl-margin-mode))
;; Add hook
(add-hook 'dired-mode-hook 'diff-hl-dired-mode-unless-remote)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

(provide 'init-git)

;;; init-git.el ends here
