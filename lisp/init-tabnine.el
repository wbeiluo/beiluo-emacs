;;; init-tabnine.el --- Configure for TabNine -*- lexical-binding: t -*-

;; Copyright (C) 2020 王北洛

;; Author: 王北洛 <wbeiluo@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;; Code:

(use-package company-tabnine
  :ensure t
  :config
  (add-to-list 'company-backends #'company-tabnine))

(provide 'init-tabnine)

;;; init-tabnine.el ends here
