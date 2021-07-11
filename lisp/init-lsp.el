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
;(setq lsp-keymap-prefix "M-l")


(defhydra hydra-lsp (:exit t :hint nil)
  "
          ^^                  ^^                         ^^^^                  ╭───────────┐
    Buffer^^            Server^^                   Symbol^^^^                  │ LSP MODE  │
  ╭───────^^──────────────────^^─────────────────────────^^^^──────────────────┴───────────╯
    [_f_] format        [_M-r_] restart            [_D_] declaration    [_o_] documentation
    [_m_] imenu         [_S_]   shutdown           [_d_] definition     [_t_] type
    [_x_] exec action   [_M-s_] describe session   [_r_] references     [_s_] signature
          ^^                  ^^                   [_i_] implementation [_R_] rename
   ───────^^──────────────────^^─────────────────────────^^^^──────────────────────────────╯
        "
  ("D" lsp-find-declaration)
  ("d" lsp-ui-peek-find-definitions)
  ("r" lsp-ui-peek-find-references)
  ("i" lsp-ui-peek-find-implementation)
  ("t" lsp-find-type-definition)
  ("s" lsp-signature-help)
  ("o" lsp-describe-thing-at-point)
  ("R" lsp-rename)

  ("f" lsp-format-buffer)
  ("m" lsp-ui-imenu)
  ("x" lsp-execute-code-action)

  ("M-s" lsp-describe-session)
  ("M-r" lsp-restart-workspace)
  ("S" lsp-shutdown-workspace)
  ("q" nil "quit")
  )


(use-package lsp-mode
  :hook ((c-mode . lsp)
         (c++-mode . lsp)
         ;; if you want which-key integration
         ;; (lsp-mode . lsp-enable-which-key-integration)
         )
  :commands lsp
  :bind (("C-/" . 'hydra-lsp/body))
  :init
  ;; Disable headerline
  (setq lsp-headerline-breadcrumb-enable nil)
  :config
  ;; use flycheck
  (setq lsp-prefer-flymake nil)
  ;; ;; C/C++
  ;; (require 'lsp-clangd)
  )

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
  (setq lsp-ui-sideline-delay 1)

  ;; lsp-ui-peek
  (setq lsp-ui-peek-enable t)
  ;; show the directory of files
  (setq lsp-ui-peek-show-directory t)

  ;; lsp-ui-doc
  (setq lsp-ui-doc-enable t)
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  )

;; if you are helm user
;(use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; C/C++
;; (use-package ccls
;;   :hook ((c-mode c++-mode objc-mode cuda-mode) .
;;          (lambda () (require 'ccls) (lsp)))
;;   :init
;;   (when (eq system-type 'gnu/linux)
;;     (setq ccls-executable "/usr/bin/ccls"))
;;   (when (eq system-type 'windows-nt)
;;     (setq ccls-executable "~/opt/ccls/Release/ccls.exe")))

(provide 'init-lsp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp.el ends here
