;; init-lsp.el --- lsp configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2022 王北洛

;; Author: 王北洛 <wbeiluo@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;; lsp configurations.
;;

;;; Code:

(use-package lsp-mode
  :ensure t
  :hook ((c-mode . lsp)
         (c++-mode . lsp))
  :commands lsp
  :bind (("M-l" . hydra-lsp/body))
  :custom
  (lsp-completion-provider :none) ;; we use Corfu!

  :init
  ;; Disable headerline
  (setq lsp-headerline-breadcrumb-enable nil)
  ;; Disable Code lens
  ;; (setq lsp-lens-enable nil)
  ;; Don't show diagnostics on modeline.
  (setq lsp-modeline-diagnostics-enable nil)
  ;; Disable Highlight references of the symbol at point.
  (setq lsp-enable-symbol-highlighting nil)
  
  (defun my/orderless-dispatch-flex-first (_pattern index _total)
    (and (eq index 0) 'orderless-flex))

  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))

  ;; Optionally configure the first word as flex filtered.
  (add-hook 'orderless-style-dispatchers #'my/orderless-dispatch-flex-first nil 'local)

  ;; Optionally configure the cape-capf-buster.
  (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point)))

  :hook
  (lsp-completion-mode . my/lsp-mode-setup-completion)

  :config
  (defhydra hydra-lsp (:exit t :hint nil)
    "
    _d_: definition  _r_: references  _s_: symbols  _e_: treemacs error
    _f_: format      _m_: imenu       _R_: rename   _i_: implementation    "
    
    ("d" lsp-find-definition)
    ("r" lsp-find-references)
    ("s" consult-lsp-symbols)
    ("e" lsp-treemacs-errors-list)
    ("f" lsp-format-buffer)
    ("m" lsp-ui-imenu)
    ("R" lsp-rename)
    ("i" lsp-find-implementation)

    ("q" nil "quit" :color "deep sky blue"))
  
  )

;; C/C++ Lsp
;; (use-package ccls
;;   :hook ((c-mode c++-mode objc-mode cuda-mode) .
;;          (lambda () (require 'ccls) (lsp)))

;;   :init
;;   (defun +my/ccls-code-lens ()
;;     (when (member major-mode '(c-mode c++-mode))
;;       (ccls-code-lens-mode 1)))

;;   :config
;;   ;; Code lens
;;   ;;(add-hook 'lsp-after-open-hook #'+my/ccls-code-lens)
;;   ;; Documentation
;;   (setq lsp-ui-doc-include-signature nil)  ; don't include type signature in the child frame
;;   (setq lsp-ui-sideline-show-symbol t)  ; show symbol on the right of info

;;   ;; Highlight
;;   (setq ccls-sem-highlight-method 'font-lock)
;;   ;; alternatively, (setq ccls-sem-highlight-method 'overlay)
;;   )

(use-package lsp-ui
  :commands lsp-ui-mode
  :init

  ;; Disable show doc frame boeder
  (setq lsp-ui-doc-border nil)
  
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
  (setq lsp-ui-peek-enable nil)
  ;; show the directory of files
  (setq lsp-ui-peek-show-directory t)

  ;; lsp-ui-doc
  (setq lsp-ui-doc-enable t)
  ;; Number of seconds before showing the doc
  (setq lsp-ui-doc-delay 0.5)
  ;; When non-nil, move the cursor over a symbol to show the doc
  (setq lsp-ui-doc-show-with-cursor t)
  ;; When non-nil, move the mouse pointer over a symbol to show the doc
  (setq lsp-ui-doc-show-with-mouse t)
  
  :config
  ;(define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  ;(define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  )

(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; DAP Mode
(use-package dap-mode
  :ensure t
  :commands dap-debug
  :defer
  :custom
  (dap-auto-configure-mode t)  ;; Automatically configure dap
  (dap-auto-configure-features
   '(sessions locals breakpoints controls expressions tooltip))  ;; Remove the button panel in the top
  :config
  (dap-mode 1)
  ;; ;; The modes below are optional
  ;; (dap-ui-mode 1)
  ;; ;; enables mouse hover support
  ;; (dap-tooltip-mode 1)
  ;; ;; use tooltips for mouse hover
  ;; ;; if it is not enabled `dap-mode' will use the minibuffer.
  ;; (tooltip-mode 1)
  ;; ;; displays floating panel with debug buttons
  ;; ;; requies emacs 26+
  ;; (dap-ui-controls-mode 1)

  ;; C/C++ Debug
  ;;(require 'dap-gdb-lldb)
  ;;(dap-gdb-lldb-setup)
  (require 'dap-cpptools)
  (dap-cpptools-setup)

  ;; ask user for executable to debug if not specified explicitly (c++)
  ;;(setq dap-lldb-debugged-program-function (lambda () (read-file-name "Select file to debug.")))

  ;; default debug template for ARM GCC
  ;; (dap-register-debug-template
  ;;  "C++ LLDB dap"
  ;;  (list :type "lldb-vscode"
  ;;        :cwd nil
  ;;        :args nil
  ;;        :request "launch"
  ;;        :program nil))

  (defun dap-debug-create-or-edit-json-template ()
    "Edit the C++ debugging configuration or create + edit if none exists yet."
    (interactive)
    (let ((filename (concat (lsp-workspace-root) "/launch.json"))
	  (default "~/.emacs.d/resources/default-launch.json"))
      (unless (file-exists-p filename)
	(copy-file default filename))
      (find-file-existing filename)))

)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language


(provide 'init-lsp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp.el ends here
