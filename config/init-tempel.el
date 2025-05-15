;;; init-tempel.el --- Tempel configurations. -*- lexical-binding: t -*-

;; Copyright (C) 2025~2025 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;;  Tempel configurations
;;

;;; Code:

;; Setup completion at point
(defun tempel-setup-capf ()
  ;; Add the Tempel Capf to `completion-at-point-functions'.
  ;; `tempel-expand' only triggers on exact matches. Alternatively use
  ;; `tempel-complete' if you want to see all matches, but then you
  ;; should also configure `tempel-trigger-prefix', such that Tempel
  ;; does not trigger too often when you don't expect it. NOTE: We add
  ;; `tempel-expand' *before* the main programming mode Capf, such
  ;; that it will be tried first.
  (setq-local completion-at-point-functions
              (cons #'tempel-expand
                    completion-at-point-functions)))

(add-hook 'conf-mode-hook 'tempel-setup-capf)
(add-hook 'prog-mode-hook 'tempel-setup-capf)
(add-hook 'text-mode-hook 'tempel-setup-capf)

;; Optionally make the Tempel templates available to Abbrev,
;; either locally or globally. `expand-abbrev' is bound to C-x '.
;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)

(require 'tempel)
(require 'tempel-collection)

;; Require trigger prefix before template name when completing.
(setq tempel-trigger-prefix "<")

(provide 'init-tempel)
;;; init-tempel.el ends here

