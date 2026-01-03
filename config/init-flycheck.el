;; init-flycheck.el --- flycheck configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2022~2025 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;; flycheck configurations.
;;

;;; Code:

(defconst extensions-flycheck-dir
  (expand-file-name "extensions/flycheck" user-emacs-directory))
(defconst extensions-consult-flycheck-dir
  (expand-file-name "extensions/consult-flycheck" user-emacs-directory))

(use-package flycheck
  :ensure nil
  :load-path extensions-flycheck-dir
  :bind (("M-g n" . flycheck-next-error)      ;; 跳转到下一个错误
         ("M-g p" . flycheck-previous-error))  ;; 跳转到上一个错误
  :hook (after-init. global-flycheck-mode)
  :config
  (define-key flycheck-mode-map flycheck-keymap-prefix nil)
  (setq flycheck-keymap-prefix (kbd "C-c !"))
  (define-key flycheck-mode-map flycheck-keymap-prefix
              flycheck-command-map))

(use-package consult-flycheck
  :ensure nil
  :load-path extensions-consult-flycheck-dir
  :bind ("M-g f" . consult-flycheck))

(provide 'init-flycheck)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-flycheck.el ends here
