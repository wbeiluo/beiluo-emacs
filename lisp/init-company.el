;; init-company.el --- company configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2020 王北洛

;; Author: 王北洛 <wbeiluo@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;; company configurations.
;;

;;; Code:

(use-package company
  :diminish
  :hook (after-init . global-company-mode)
  :bind (:map company-mode-map
         ("M-/" . company-complete)
         :map company-active-map
         ("<tab>" . company-complete-selection)
         ("M-/" . company-other-backend)
         ("M-n" . company-select-next)
         ("M-p" . company-select-previous))
  :config
  (require 'company-dabbrev)
  (require 'company-files)
  (use-package company-c-headers :ensure t)
  (use-package company-lsp :ensure t)

  (setq-default company-dabbrev-other-buffers 'all
                company-tooltip-align-annotations t)
  ;; set the completion menu pop-up delay
  (setq company-idle-delay 0)
  ;; pop up a completion menu by tapping a character
  (setq company-minimum-prefix-length 1)
  ;; do not display numbers on the left
  (setq company-show-numbers t)
  ;; allow input string that do not match candidate words
  (setq company-require-match nil)
  ;; Don't downcase the returned candidates.
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-ignore-case t)

  (setq company-global-modes '(not erc-mode message-mode help-mode
                                   gud-mode eshell-mode shell-mode))

  (setq company-frontends '(company-tng-frontend
                            company-pseudo-tooltip-frontend
                            company-echo-metadata-frontend))

  ;; Allows TAB to select and complete at the same time.
  (company-tng-configure-default)

  ;; Customize company backends.
  (setq company-backends (delete 'company-xcode company-backends))
  (setq company-backends (delete 'company-bbdb company-backends))
  (setq company-backends (delete 'company-eclim company-backends))
  (setq company-backends (delete 'company-gtags company-backends))
  (setq company-backends (delete 'company-etags company-backends))
  (setq company-backends (delete 'company-oddmuse company-backends))


  (add-to-list 'company-backends #'company-files)
  (add-to-list 'company-backends #'company-c-headers)
  (add-to-list 'company-backends #'company-lsp)

  ;; Add `company-elisp' backend for elisp.
  (add-hook 'emacs-lisp-mode-hook
            '(lambda ()
              (require 'company-elisp)
              (push 'company-elisp company-backends)))

  (use-package company-quickhelp
    :hook (after-init . company-quickhelp-mode))

  ;; Better sorting and filtering
  (use-package company-prescient
    :init (company-prescient-mode 1))
  )


(provide 'init-company)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-company.el ends here
