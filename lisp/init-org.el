;;; init-org.el --- Org Mode Configurations -*- lexical-binding: t -*-

;; Copyright (C) 2020 王北洛

;; Author: 王北洛 <wbeiluo@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;; Code:

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)

;; org font
(with-eval-after-load 'org
  (defun org-buffer-face-mode-variable ()
    (interactive)
    (make-face 'width-font-face)
    (set-face-attribute 'width-font-face nil :font "WenQuanYi Zen Hei Mono 12")
    (setq buffer-face-mode 'width-font-face)
    (buffer-face-mode))
  (add-hook 'org-mode-hook 'org-buffer-face-mode-variable))

;; org-indent mode
(setq org-startup-indented t)

;; 自动折行
(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))

;; bullet list
(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '("☯" "☰" "☷" "❁")))


(provide 'init-org)

;;; init-org.el ends here
