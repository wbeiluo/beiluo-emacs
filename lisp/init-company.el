;; init-company.el --- company configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2020 王北洛

;; Author: 王北洛 <wbeiluo@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;; company configurations.
;;

;;; Code:

(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)

(use-package company-tabnine :ensure t)
(use-package company-yasnippet :ensure nil)
(use-package company-dabbrev :ensure nil)
(use-package company-files :ensure nil)
(use-package company-tng :ensure nil)
(use-package company-elisp :ensure nil)

(use-package company
  :diminish
  :hook (after-init . global-company-mode)
  :bind (("M-/" . company-complete)
         :map company-active-map
         ("M-/" . company-other-backend)
         ("M-n" . company-select-next)
         ("M-p" . company-select-previous))
  :config
  (setq-default company-dabbrev-other-buffers 'all
                company-tooltip-align-annotations t)
  ;; set the completion menu pop-up delay
  (setq company-idle-delay 0.2)
  ;; pop up a completion menu by tapping a character
  (setq company-minimum-prefix-length 1)
  ;; do not display numbers on the left
  (setq company-show-numbers nil)
  ;; allow input string that do not match candidate words
  (setq company-require-match nil)
  ;; Don't downcase the returned candidates.
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-ignore-case t)

  ;; Customize company backends.
  (setq company-backends (delete 'company-xcode company-backends))
  (setq company-backends (delete 'company-bbdb company-backends))
  (setq company-backends (delete 'company-eclim company-backends))
  (setq company-backends (delete 'company-gtags company-backends))
  (setq company-backends (delete 'company-etags company-backends))
  (setq company-backends (delete 'company-oddmuse company-backends))

  (add-to-list 'company-backends 'company-files)
  (add-to-list 'company-backends #'company-tabnine)

  ;; Use the tab-and-go frontend.
  ;; Allows TAB to select and complete at the same time.
  (company-tng-configure-default)
  (setq company-frontends
        '(company-tng-frontend
          company-pseudo-tooltip-frontend
          company-echo-metadata-frontend))

  ;; Add `company-elisp' backend for elisp.
  (add-hook 'emacs-lisp-mode-hook
  '(lambda ()
     (require 'company-elisp)
     (push 'company-elisp company-backends))))


(provide 'init-company)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-company.el ends here
