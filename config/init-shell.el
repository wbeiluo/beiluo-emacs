;; init-shell.el --- shell configurations. -*- lexical-binding: t -*-

;; Copyright (C) 2022~2025 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;; shell configurations.
;;

;;; Code:

(defconst extensions-pcmpl-args-dir
  (expand-file-name "extensions/pcmpl-args" user-emacs-directory))
(defconst extensions-exec-path-from-shell-dir
  (expand-file-name "extensions/exec-path-from-shell" user-emacs-directory))
(defconst extensions-esh-help-dir
  (expand-file-name "extensions/esh-help" user-emacs-directory))

(use-package pcmpl-args
  :ensure nil
  :load-path extensions-pcmpl-args-dir
  :custom
  ;; 开启缓存，避免重复解析同一命令
  (pcmpl-args-cache-shell-commands t))

(use-package exec-path-from-shell
  :ensure nil
  :load-path extensions-exec-path-from-shell-dir
  :config
  (when (memq window-system '(mac ns x))
    (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH"))
      (add-to-list 'exec-path-from-shell-variables var))
    (exec-path-from-shell-initialize)))

(use-package esh-help
  :ensure nil
  :load-path extensions-esh-help-dir
  :after eshell
  :config
  (setup-esh-help-eldoc)  
  :bind (:map eshell-mode-map
              ("C-c C-h" . esh-help-run-help)))

(provide 'init-shell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-shell.el ends here
