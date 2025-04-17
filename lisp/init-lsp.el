;; init-lsp.el --- lsp configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2022~2025 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;; lsp configurations.
;;

;;; Code:

;;(global-unset-key (kbd "M-l"))

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap
  (setq lsp-keymap-prefix "C-c l")
  :hook ((c-mode . lsp)
         (c++-mode . lsp)
         (python-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :custom
  (lsp-completion-provider :none)
  :init
  ;; Disable headerline
  (setq lsp-headerline-breadcrumb-enable nil)
  ;; Enable Highlight references of the symbol at point.
  (setq lsp-enable-symbol-highlighting t)
  ;; Disable Code lens
  (setq lsp-lens-enable t)
  ;; Don't show diagnostics on modeline.
  (setq lsp-modeline-diagnostics-enable nil)
  )

;; optionally
(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)

(use-package consult-lsp
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols))

(use-package lsp-ui
  :commands lsp-ui-mode
  :init
  ;; lsp-ui-sideline
  ;; show diagnostics messages in sideline
  (setq lsp-ui-sideline-show-diagnostics t)
  ;; show hover messages in sideline
  (setq lsp-ui-sideline-show-hover t)
  ;; show code actions in sideline
  (setq lsp-ui-sideline-show-code-actions t)
  ;; seconds to wait before showing sideline
  (setq lsp-ui-sideline-delay 0.5)

  ;; lsp-ui-peek
  (setq lsp-ui-peek-enable t)
  ;; show the directory of files
  (setq lsp-ui-peek-show-directory t)

  ;; lsp-ui-doc
  (setq lsp-ui-doc-enable t)
  ;; Disable show doc frame boeder
  (setq lsp-ui-doc-border nil)
  ;; Position if doc display
  (setq lsp-ui-doc-position 'at-point)
  ;; Number of seconds before showing the doc
  (setq lsp-ui-doc-delay 1)
  ;; When non-nil, move the cursor over a symbol to show the doc
  (setq lsp-ui-doc-show-with-cursor t)
  ;; When non-nil, move the mouse pointer over a symbol to show the doc
  (setq lsp-ui-doc-show-with-mouse t)
  )

;; Debug Adapter Protocol
(use-package dape
  :preface
  ;; By default dape shares the same keybinding prefix as `gud'
  ;; If you do not want to use any prefix, set it to nil.
  ;; (setq dape-key-prefix "\C-x\C-a")

  :hook
  ;; Save breakpoints on quit
  (kill-emacs . dape-breakpoint-save)
  ;; Load breakpoints on startup
  (after-init . dape-breakpoint-load)

  :config
  ;; Turn on global bindings for setting breakpoints with mouse
  (dape-breakpoint-global-mode)

  ;; Info buffers to the right
  (setq dape-buffer-window-arrangement 'right)

  ;; Info buffers like gud (gdb-mi)
  ;; (setq dape-buffer-window-arrangement 'gud)
  ;; (setq dape-info-hide-mode-line nil)

  ;; Pulse source line (performance hit)
  (add-hook 'dape-display-source-hook 'pulse-momentary-highlight-one-line)

  ;; Showing inlay hints
  (setq dape-inlay-hints t)

  ;; Save buffers on startup, useful for interpreted languages
  (add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))

  ;; Kill compile buffer on build success
  ;; (add-hook 'dape-compile-hook 'kill-buffer)
  )

;; Enable repeat mode for more ergonomic `dape' use
(use-package repeat
  :config
  (repeat-mode))

(provide 'init-lsp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp.el ends here
