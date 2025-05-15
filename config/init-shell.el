;; init-shell.el --- shell configurations. -*- lexical-binding: t -*-

;; Copyright (C) 2022~2025 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;; shell configurations.
;;

;;; Code:

(require 'pcmpl-args)
(require 'exec-path-from-shell)
(require 'esh-help)

(when (memq window-system '(mac ns x))
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

;; eldoc support
(setup-esh-help-eldoc)

(provide 'init-shell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-shell.el ends here
