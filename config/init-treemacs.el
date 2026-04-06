;; init-treemacs.el --- treemacs configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2020~2025 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;; tree configurations.
;;

;;; Code:

(defconst extensions-cfrs-dir
  (expand-file-name "extensions/cfrs" user-emacs-directory))
(defconst extensions-pfuture-dir
  (expand-file-name "extensions/pfuture" user-emacs-directory))
(defconst extensions-treemacs-dir
  (list (expand-file-name "extensions/treemacs/src/elisp" user-emacs-directory)
        (expand-file-name "extensions/treemacs/src/extra" user-emacs-directory)))
(defconst extensions-treemacs-nerd-icons-dir
  (expand-file-name "extensions/treemacs-nerd-icons" user-emacs-directory))
(defconst extensions-lsp-treemacs-dir
  (expand-file-name "extensions/lsp-treemacs" user-emacs-directory))

(use-package cfrs
  :ensure nil
  :load-path extensions-cfrs-dir)

(use-package pfuture
  :ensure nil
  :load-path extensions-pfuture-dir)

(use-package treemacs
  :ensure nil
  :load-path extensions-treemacs-dir
  :defer t
  :bind ("M-0" . treemacs-select-window)
  ;; :init
  ;; (with-eval-after-load 'winum
  ;;   (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-buffer-name-function            #'treemacs-default-buffer-name
          treemacs-buffer-name-prefix              " *Treemacs-Buffer-"
          treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-hide-dot-jj-directory           t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-files-by-mouse-dragging    t
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

;; add hydras support
(use-package treemacs-hydras
  :after treemacs
  :ensure nil
  :load-path extensions-treemacs-dir)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure nil
  :load-path extensions-treemacs-dir)

(use-package treemacs-nerd-icons
  :after treemacs
  :ensure nil
  :load-path extensions-treemacs-nerd-icons-dir
  :config
  (treemacs-nerd-icons-config))

(use-package lsp-treemacs
  :ensure nil
  :load-path extensions-lsp-treemacs-dir
  :after (lsp-mode treemacs)
  :config
  (lsp-treemacs-sync-mode 1)
  :bind
  (:map lsp-mode-map
        ("M-l t e" . lsp-treemacs-errors-list)
        ("M-l t s" . lsp-treemacs-symbols)
        ("M-l t r" . lsp-treemacs-references)
        ("M-l t i" . lsp-treemacs-implementations)))

(provide 'init-treemacs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-treemacs.el ends here
