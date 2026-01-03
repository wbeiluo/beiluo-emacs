;;; init-git.el --- Git Configurations. -*- lexical-binding: t -*-

;; Copyright (C) 2025~2025 王北洛

;; Author: 王北洛 <beiluo.wang@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;;  Magit配置
;;
;;; Code:

(defconst extensions-llama-dir
  (expand-file-name "extensions/llama" user-emacs-directory))
(defconst extensions-cond-let-dir
  (expand-file-name "extensions/cond-let" user-emacs-directory))
(defconst extensions-transient-dir
  (expand-file-name "extensions/transient/lisp" user-emacs-directory))
(defconst extensions-with-editor-dir
  (expand-file-name "extensions/with-editor/lisp" user-emacs-directory))
(defconst extensions-pfuture-dir
  (expand-file-name "extensions/pfuture" user-emacs-directory))
(defconst extensions-posframe-dir
  (expand-file-name "extensions/posframe" user-emacs-directory))
(defconst extensions-cfrs-dir
  (expand-file-name "extensions/cfrs" user-emacs-directory))
(defconst extensions-magit-dir
  (expand-file-name "extensions/magit/lisp" user-emacs-directory))
(defconst extensions-diff-hl-dir
  (expand-file-name "extensions/diff-hl" user-emacs-directory))

(use-package llama
  :ensure nil
  :load-path extensions-llama-dir)

(use-package cond-let
  :ensure nil
  :load-path extensions-cond-let-dir)

(use-package transient
  :ensure nil
  :load-path extensions-transient-dir)

(use-package with-editor
  :ensure nil
  :load-path extensions-with-editor-dir)

(use-package posframe
  :ensure nil
  :load-path extensions-posframe-dir)

(use-package cfrs
  :ensure nil
  :load-path extensions-cfrs-dir)

(use-package magit
  :ensure nil
  :load-path extensions-magit-dir
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-c M-g" . magit-file-popup))
  :config
  (add-to-list 'auto-mode-alist '("\\COMMIT_EDITMSG\\'" . text-mode))
  (add-to-list 'auto-mode-alist '("\\MERGE_MSG\\'" . text-mode))
  (with-eval-after-load 'info
    (info-initialize)
    (add-to-list 'Info-directory-list (expand-file-name "extensions/magit/docs" user-emacs-directory))))

(use-package diff-hl
  :ensure nil
  :load-path extensions-diff-hl-dir
  :config
  ;; Highlighting uncommitted changes
  (global-diff-hl-mode 1)
  ;; When Emacs runs in terminal, show the indicators in margin instead.
  (unless (display-graphic-p)
    (diff-hl-margin-mode 1))
  ;; Add hook
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode-unless-remote)
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(provide 'init-git)

;;; init-git.el ends here
