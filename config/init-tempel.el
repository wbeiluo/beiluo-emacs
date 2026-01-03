;;; init-tempel.el --- Tempel configurations. -*- lexical-binding: t -*-

;; Copyright (C) 2025~2025 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;;  Tempel configurations
;;

;;; Code:

(defconst extensions-tempel-dir
  (expand-file-name "extensions/tempel" user-emacs-directory))
(defconst extensions-tempel-collection-dir
  (expand-file-name "extensions/tempel-collection" user-emacs-directory))

(use-package tempel
  :ensure nil
  :load-path extensions-tempel-dir
  :bind (("M-+" . tempel-complete)
         ("M-*" . tempel-insert)
         :map tempel-map
         ("TAB" . tempel-next)
         ("S-TAB" . tempel-previous))
  :init
  ;; 将 Tempel 挂载到补全列表
  (defun tempel-setup-capf ()
    (require 'tempel)
    ;; 将 tempf-expand 放入补全函数中
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)
  :config
  (setq tempel-trigger-prefix "<"))

(use-package tempel-collection
  :ensure nil
  :load-path extensions-tempel-collection-dir
  :after tempel)

(provide 'init-tempel)
;;; init-tempel.el ends here

