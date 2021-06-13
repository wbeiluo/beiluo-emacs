;;; init-auto-save.el --- Init for auto-save.el -*- lexical-binding: t -*-

;; Copyright (C) 2020 王北洛

;; Author: 王北洛 <wbeiluo@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs


;;; Commentary:
;;; Code:

(use-package auto-save
  :load-path "extensions/auto-save/"
  :config
  (auto-save-enable)
  (setq auto-save-silent t)
  (setq auto-save-delete-trailing-whitespace t)
  
  ;; custom predicates if you don't want auto save.
  ;; disable auto save mode when current filetype is an gpg file.
  (setq auto-save-disable-predicates
        '((lambda ()
            (string-suffix-p
             "gpg"
             (file-name-extension (buffer-name)) t))))
  )

(provide 'init-auto-save)

;;; init-auto-save.el ends here
