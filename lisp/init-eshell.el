;; init-eshell.el --- eshell configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2022 王北洛

;; Author: 王北洛 <wbeiluo@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;; eshell configurations.
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

;; `eldoc' support
(use-package esh-help
  :init (setup-esh-help-eldoc))

;; `cd' to frequent directory in `eshell'
(use-package eshell-z
  :hook (eshell-mode . (lambda () (require 'eshell-z))))


(provide 'init-eshell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-eshell.el ends here
