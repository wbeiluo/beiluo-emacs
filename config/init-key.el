;; init-key.el --- Key binding configurations. -*- lexical-binding: t -*-

;; Copyright (C) 2022~2025 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;;  全局快捷键
;;

;;; Code:

;;; 功能键
;; Shell
(global-set-key (kbd "<f5>") 'eshell)
(global-set-key (kbd "<f6>") 'shell)
;; Emms
(global-set-key (kbd "<f9>") 'emms)

;;; 项目
(global-set-key (kbd "M-p") project-prefix-map)

;;; Org Mode
;; Org-agenda
(global-set-key (kbd "\e\e a") #'org-agenda)
(define-key org-agenda-mode-map (kbd "i") #'(lambda () (interactive) (org-capture nil "d")))
(define-key org-agenda-mode-map (kbd "J") #'consult-org-agenda)

;; Org-capture
(global-set-key (kbd "\e\e c") #'org-capture)

;; 时间
(global-set-key (kbd "C-c t t") 'insert-current-time)
(global-set-key (kbd "C-c t d") 'insert-current-data-time)
(global-set-key (kbd "C-c t w") 'insert-current-data-week-time)

(provide 'init-key)

;;; init-key.el ends here
