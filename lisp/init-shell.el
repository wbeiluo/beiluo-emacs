;; init-shell.el --- shell configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2022 王北洛

;; Author: 王北洛 <wbeiluo@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;; shell configurations.
;;

;;; Code:

;;  Display extra information for prompt
;; (use-package eshell-prompt-extras
;;   :after esh-opt
;;   :defines eshell-highlight-prompt
;;   :commands (epe-theme-lambda epe-theme-dakrone epe-theme-pipeline)
;;   :init (setq eshell-highlight-prompt nil
;;               eshell-prompt-function #'epe-theme-lambda))

;; Enhanced shell command completion
(use-package pcmpl-args
  :ensure t)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

;; `eldoc' support
(use-package esh-help
  :init (setup-esh-help-eldoc))

;; `cd' to frequent directory in `eshell'
(use-package eshell-z
  :hook (eshell-mode . (lambda () (require 'eshell-z))))

(provide 'init-shell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-shell.el ends here
