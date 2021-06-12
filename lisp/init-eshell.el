;; init-eshell.el --- eshell configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2020 王北洛

;; Author: 王北洛 <wbeiluo@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;; eshell configurations.
;;

;;; Code:

(add-hook 'shell-mode-hook 'yas-minor-mode)
(add-hook 'shell-mode-hook 'flycheck-mode)
(add-hook 'shell-mode-hook 'company-mode)

(defun shell-mode-company-init ()
  (setq-local company-backends '((company-shell
                                  company-shell-env
                                  company-etags
                                  company-dabbrev-code))))

(use-package company-shell
  :ensure t
  :config
    (require 'company)
    (add-hook 'shell-mode-hook 'shell-mode-company-init))

;;  Display extra information for prompt
(use-package eshell-prompt-extras
  :after esh-opt
  :defines eshell-highlight-prompt
  :commands (epe-theme-lambda epe-theme-dakrone epe-theme-pipeline)
  :init (setq eshell-highlight-prompt nil
              eshell-prompt-function #'epe-theme-lambda))

;; Fish-like history autosuggestions
(use-package esh-autosuggest
  :defines ivy-display-functions-alist
  :bind (:map eshell-mode-map
              ([remap eshell-pcomplete] . completion-at-point))
  :hook ((eshell-mode . esh-autosuggest-mode)
         (eshell-mode . eshell-setup-ivy-completion))
  :init (defun eshell-setup-ivy-completion ()
          "Setup `ivy' completion in `eshell'."
          (setq-local ivy-display-functions-alist
                      (remq (assoc 'ivy-completion-in-region
                                   ivy-display-functions-alist)
                            ivy-display-functions-alist))))

;; `eldoc' support
(use-package esh-help
  :init (setup-esh-help-eldoc))

;; `cd' to frequent directory in `eshell'
(use-package eshell-z
  :hook (eshell-mode . (lambda () (require 'eshell-z))))


(provide 'init-eshell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-eshell.el ends here
