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
  :bind (:map company-active-map
         ("C-<tab>" . company-complete-common-or-cycle)
         ;("M-/" . company-complete-selection)
         ("M-/" . company-other-backend)
         ("M-n" . company-select-next)
         ("M-p" . company-select-previous))
  :config
  (require 'company-yasnippet)
  (require 'company-dabbrev)
  (require 'company-files)

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

  ;; Customize company backends.
  (setq company-backends (delete 'company-xcode company-backends))
  (setq company-backends (delete 'company-bbdb company-backends))
  (setq company-backends (delete 'company-eclim company-backends))
  (setq company-backends (delete 'company-gtags company-backends))
  (setq company-backends (delete 'company-etags company-backends))
  (setq company-backends (delete 'company-oddmuse company-backends))
  (add-to-list 'company-backends #'company-files)

  ;; Add `company-elisp' backend for elisp.
  (add-hook 'emacs-lisp-mode-hook
            '(lambda ()
              (require 'company-elisp)
              (push 'company-elisp company-backends)))

  ;; Add yasnippet support for all company backends.
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")

  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))

  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
  )


(provide 'init-company)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-company.el ends here
