;; init-shell.el --- shell configurations. -*- lexical-binding: t -*-

;; Copyright (C) 2022~2026 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;; shell configurations.
;;

;;; Code:

;; 快捷键
(global-set-key (kbd "<f5>") 'eshell)
(global-set-key (kbd "<f6>") 'shell)

(use-package pcmpl-args
  :ensure t
  :custom
  ;; 开启缓存，避免重复解析同一命令
  (pcmpl-args-cache-shell-commands t))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x pgtk))
    (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH"))
      (add-to-list 'exec-path-from-shell-variables var))
    (exec-path-from-shell-initialize)))

(use-package esh-help
  :ensure t
  :after eshell
  :config
  (setup-esh-help-eldoc)  
  :bind (:map eshell-mode-map
              ("C-c C-h" . esh-help-run-help)))

(provide 'init-shell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-shell.el ends here
