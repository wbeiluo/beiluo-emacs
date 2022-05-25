;; init-ivy.el --- Ivy configurations.  -*- lexical-binding: t -*-

;; Copyright (C) 2020 王北洛

;; Author: 王北洛 <wbeiluo@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;; Ivy configurations.
;;

;;; Code:

(use-package ivy
  :diminish ivy-mode
  :hook (after-init . ivy-mode)
  :init
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t
        ;;ivy-height 12
        ivy-use-selectable-prompt t
        ivy-use-virtual-buffers t
        ivy-fixed-height-minibuffer t
        ivy-count-format "[%d/%d] "
        ivy-on-del-error-function #'ignore
        ivy-initial-inputs-alist nil)
  :config
  ;; Integrate yasnippet
  (use-package ivy-yasnippet :ensure t)
  ;; Select from xref candidates with Ivy
  (use-package ivy-xref
    :ensure t
    :init
    ;; xref initialization is different in Emacs 27 - there are two different
    ;; variables which can be set rather than just one
    (when (>= emacs-major-version 27)
      (setq xref-show-definitions-function #'ivy-xref-show-defs))
    ;; Necessary in Emacs <27. In Emacs 27 it will affect all xref-based
    ;; commands other than xref-find-definitions (e.g. project-find-regexp)
    ;; as well
    (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

  (use-package imenu-anywhere :ensure t)
  )

;;(use-package ivy-posframe
;;  :ensure t
;;  :config
;;  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display)))
;;  (ivy-posframe-mode 1))

(use-package counsel
  :diminish counsel-mode
  :hook (ivy-mode . counsel-mode)
  :init
  ;; Use the faster search tool: ripgrep (`rg')
  (when (executable-find "rg")
    (setq counsel-grep-base-command "rg -S --no-heading --line-number --color never %s %s"))

  :config
  ;; Enhance M-x
  (use-package amx
    :init (setq amx-history-length 20))

  ;; Better sorting and filtering
  (use-package prescient
    :commands prescient-persist-mode
    :init (prescient-persist-mode 1))

  ;; Ivy integration for Projectile
  (use-package counsel-projectile
    :hook (counsel-mode . counsel-projectile-mode)
    :init (setq counsel-projectile-grep-initial-input '(ivy-thing-at-point))))

;; More friendly display transformer for Ivy
(use-package ivy-rich
  :ensure t
  :init
  ;; Better experience with icons
  ;; Enable it before`ivy-rich-mode' for better performance
  (use-package all-the-icons-ivy-rich
    :ensure t
    :init
    ;; Whether display the icons
    (setq all-the-icons-ivy-rich-icon t)

    ;; Whether display the colorful icons.
    ;; It respects `all-the-icons-color-icons'.
    (setq all-the-icons-ivy-rich-color-icon t)

    ;; The icon size
    (setq all-the-icons-ivy-rich-icon-size 1.0)

    ;; Whether support project root
    (setq all-the-icons-ivy-rich-project t)

    ;; Maximum truncation width of annotation fields.
    ;; This value is adjusted depending on the `window-width'.
    (setq all-the-icons-ivy-rich-field-width 80)

    ;; Definitions for ivy-rich transformers.
    ;; See `ivy-rich-display-transformers-list' for details."
    ;; all-the-icons-ivy-rich-display-transformers-list

    ;; Slow Rendering
    ;; If you experience a slow down in performance when rendering multiple icons simultaneously,
    ;; you can try setting the following variable
    (setq inhibit-compacting-font-caches t)

    (all-the-icons-ivy-rich-mode 1))

  :config

  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (setq ivy-rich-path-style 'abbrev)

  (ivy-rich-mode 1))

(use-package swiper :ensure t)

(provide 'init-ivy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ivy.el ends here
