;; init-lsp.el --- lsp configurations. -*- lexical-binding: t -*-

;; Copyright (C) 2022~2025 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;;  lsp-mode configurations.
;;

;;; Code:

;; Set prefix for lsp-command-keymap
(global-unset-key (kbd "M-l"))
(setq lsp-keymap-prefix "M-l")

(require 'lsp-mode)
(require 'lsp-modeline)
(require 'lsp-completion)
(require 'lsp-inline-completion)
(require 'lsp-diagnostics)
(require 'lsp-dired)
(require 'lsp-icons)
(require 'lsp-lens)
(require 'lsp-protocol)
(require 'lsp-semantic-tokens)
(require 'lsp-ui)
(require 'spinner)
(require 'dape)
(require 'markdown-mode)
(require 'repeat)

;;; Lsp mode setting
(setq lsp-completion-provider :none)
;; Disable headerline
(setq lsp-headerline-breadcrumb-enable nil)
;; Enable Highlight references of the symbol at point.
(setq lsp-enable-symbol-highlighting t)
;; Don't show diagnostics on modeline.
(setq lsp-modeline-diagnostics-enable nil)
;; Disable snippet support.
(setq lsp-enable-snippet nil)

;; Add hook
(add-hook 'c-mode-hook #'lsp)
(add-hook 'c++-mode-hook #'lsp)
;; if you want which-key integration
(add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)

;; lsp-ui-sideline
;; show diagnostics messages in sideline
(setq lsp-ui-sideline-show-diagnostics t)
;; show hover messages in sideline
(setq lsp-ui-sideline-show-hover nil)
;; show code actions in sideline
(setq lsp-ui-sideline-show-code-actions t)
;; seconds to wait before showing sideline
(setq lsp-ui-sideline-delay 0.5)

;; lsp-ui-peek
(setq lsp-ui-peek-enable t)
;; show the directory of files
(setq lsp-ui-peek-show-directory t)
(define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
(define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)

;; lsp-ui-doc
(setq lsp-ui-doc-enable t)
;; Disable show doc frame boeder
(setq lsp-ui-doc-border nil)
;; Position if doc display(top, bottom or at-point)
(setq lsp-ui-doc-position 'at-point)
;; Number of seconds before showing the doc
(setq lsp-ui-doc-delay 1)
;; When non-nil, move the cursor over a symbol to show the doc
(setq lsp-ui-doc-show-with-cursor t)
;; When non-nil, move the mouse pointer over a symbol to show the doc
(setq lsp-ui-doc-show-with-mouse t)

;; lsp-ui-imenu


;; Debug Adapter Protocol
;; By default dape shares the same keybinding prefix as `gud'
;; If you do not want to use any prefix, set it to nil.
;; (setq dape-key-prefix "\C-x\C-a")
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
(add-hook 'dape-compile-hook 'kill-buffer)

(when (file-exists-p "~/.emacs.d/dape-breakpoints")
  ;; Save breakpoints on quit
  (add-hook 'kill-emacs-hook 'dape-breakpoint-save)
  ;; Load breakpoints on startup
  (add-hook 'after-init-hook 'dape-breakpoint-load))

;; Enable repeat mode for more ergonomic `dape' use
(repeat-mode)

(provide 'init-lsp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp.el ends here
