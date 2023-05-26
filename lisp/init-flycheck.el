;; init-flycheck.el --- flycheck configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2022~2023 王北洛

;; Author: 王北洛 <wbeiluo@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;; flycheck configurations.
;;

;;; Code:


(use-package flycheck
  :ensure t
  :diminish
  :bind (("C-c e l". flycheck-list-errors)
         ("C-c e n". flycheck-next-error)
         ("C-c e p". flycheck-previous-error))
  :hook (prog-mode . flycheck-mode)
  :config
  (flycheck-mode 1)
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc)))

;; (use-package flycheck-pos-tip
;;   :after flycheck
;;   :hook
;;   (flycheck-mode . flycheck-pos-tip-mode))

;; (use-package flycheck-posframe
;;   :ensure
;;   :hook (flycheck-mode-hook . flycheck-posframe-mode))

(use-package flycheck-color-mode-line
  :hook (flycheck-mode-hook . flycheck-color-mode-line-mode))


(provide 'init-flycheck)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-flycheck.el ends here
