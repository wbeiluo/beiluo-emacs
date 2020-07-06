;; init-lsp.el --- lsp configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2020 王北洛

;; Author: 王北洛 <wbeiluo@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;; lsp configurations.
;;

;;; Code:

;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
(setq lsp-keymap-prefix "M-l")
;; use flycheck
(setq lsp-prefer-flymake nil)

(use-package lsp-mode
  :hook ((c-mode . lsp)
         (c++-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package company-lsp :commands company-lsp)

;; optionally
;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; C/C++
(use-package ccls
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp)))
  :init
  (when (eq system-type 'gnu/linux)
    (setq ccls-executable "/usr/bin/ccls"))
  (when (eq system-type 'windows-nt)
    (setq ccls-executable "~/ccls/Release/ccls.exe")))


(use-package lsp-mode :commands lsp)
;;(use-package lsp-ui :commands lsp-ui-mode)
(use-package company-lsp :commands company-lsp)


;; optionally if you want to use debugger
;(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language


(provide 'init-lsp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp.el ends here
