;; init-flycheck.el --- flycheck configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2020 王北洛

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
  :hook ((prog-mode python-mode) . flycheck-mode)
  :config
  (use-package flycheck-posframe
    :ensure t
    :hook (flycheck-mode-hook . flycheck-posframe-mode))

  (use-package flycheck-color-mode-line
    :hook (flycheck-mode-hook . flycheck-color-mode-line-mode))

  (flycheck-mode 1))


(provide 'init-flycheck)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-flycheck.el ends here
