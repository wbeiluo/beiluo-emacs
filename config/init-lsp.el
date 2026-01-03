;; init-lsp.el --- lsp configurations. -*- lexical-binding: t -*-

;; Copyright (C) 2022~2025 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;;  lsp-mode configurations.
;;

;;; Code:

(defconst extensions-lsp-mode-dir
  (list (expand-file-name "extensions/lsp-mode" user-emacs-directory)
	(expand-file-name "extensions/lsp-mode/clients" user-emacs-directory)))
(defconst extensions-lsp-ui-dir
  (expand-file-name "extensions/lsp-ui" user-emacs-directory))
(defconst extensions-spinner-dir
  (expand-file-name "extensions/spinner" user-emacs-directory))
(defconst extensions-dape-dir
  (expand-file-name "extensions/dape" user-emacs-directory))

(use-package spinner
  :ensure nil
  :load-path extensions-spinner-dir)

(use-package lsp-mode
  :ensure nil
  :load-path extensions-lsp-mode-dir
  :hook ((c-mode . lsp)
         (c++-mode . lsp)
	 (python-mode . lsp)
	 (lsp-mode . lsp-enable-which-key-integration))
  :init
  ;; Set prefix for lsp-command-keymap
  (global-unset-key (kbd "M-l"))
  (setq lsp-keymap-prefix "M-l")
  :custom
  (lsp-completion-provider :none)
  ;; Disable headerline
  (lsp-headerline-breadcrumb-enable nil)
  ;; Enable Highlight references of the symbol at point.
  (lsp-enable-symbol-highlighting t)
  ;; Don't show diagnostics on modeline.
  (lsp-modeline-diagnostics-enable nil)
  ;; Disable snippet support.
  (lsp-enable-snippet nil)
  ;; Disable on-type formatting，开启后可能出现回车后光标跳行首的情况
  (lsp-enable-on-type-formatting nil)
  :commands lsp)

(use-package lsp-modeline
  :ensure nil
  :load-path extensions-lsp-mode-dir)

(use-package lsp-lens
  :ensure nil
  :load-path extensions-lsp-mode-dir)

(use-package lsp-completion
  :ensure nil
  :load-path extensions-lsp-mode-dir)

(use-package lsp-diagnostics
  :ensure nil
  :load-path extensions-lsp-mode-dir
  :custom
  ;; 配置 lsp 使用 flycheck
  (lsp-diagnostics-provider :flycheck)
  :hook (lsp-mode . lsp-diagnostics-mode))

(use-package lsp-dired
  :ensure nil
  :load-path extensions-lsp-mode-dir)

(use-package lsp-icons
  :ensure nil
  :load-path extensions-lsp-mode-dir)

(use-package lsp-semantic-tokens
  :ensure nil
  :load-path extensions-lsp-mode-dir)

(use-package lsp-ui
  :ensure nil
  :load-path extensions-lsp-ui-dir
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references))
  :custom
  ;; lsp-ui-doc (文档浮窗)
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-border nil)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  ;; Position if doc display(top, bottom or at-point)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-delay 0.5)
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-doc-show-with-mouse t)

  ;; lsp-ui-sideline (侧边栏提示)
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-show-diagnostics t)

  ;; lsp-ui-peek (定义预览)
  (lsp-ui-peek-enable t)
  ;; show the directory of files
  (lsp-ui-peek-show-directory t)

  ;; lsp-ui-imenu (代码结构)
  (lsp-ui-imenu-enable t)
  (lsp-ui-imenu-kind-justify t))

;; Debug Adapter Protocol
(use-package dape
  :ensure nil
  :load-path extensions-dape-dir
  :preface
  (setq dape-buffer-window-arrangement 'right) ;; 调试窗口布局在右侧
  :hook ((dape-on-start . dape-info-setup)     ;; 启动时显示变量/堆栈面板
	 (dape-on-start . dape-repl-setup)     ;; 启动时显示 REPL
	 (dape-compile . kill-buffer)          ;; Kill compile buffer on build success
	 (dape-display-source . pulse-momentary-highlight-one-line))
  :config
  (setq dape-display-source-buffer-action '(display-buffer-reuse-window)) 
  
  (when (file-exists-p "~/.emacs.d/dape-breakpoints")
    ;; Save breakpoints on quit
    (add-hook 'kill-emacs-hook 'dape-breakpoint-save)
    ;; Load breakpoints on startup
    (add-hook 'after-init-hook 'dape-breakpoint-load)))

(use-package repeat
  :ensure nil
  :config
  ;; Enable repeat mode for more ergonomic `dape' use
  (repeat-mode 1))

(provide 'init-lsp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp.el ends here
