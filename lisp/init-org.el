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


(setq
  ;; Edit settings
  org-indent-mode nil
  org-startup-indented t
  org-auto-align-tags nil
  org-tags-column 0
  org-catch-invisible-edits 'show-and-error
  org-special-ctrl-a/e t
  org-insert-heading-respect-content t

  ;; Org styling, hide markup etc.
  org-hide-emphasis-markers t
  org-pretty-entities t
  org-ellipsis "…"

  ;; Agenda styling
  org-agenda-block-separator ?─
  org-agenda-time-grid
  '((daily today require-timed)
    (800 1000 1200 1400 1600 1800 2000)
    " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
  org-agenda-current-time-string
  "⭠ now ─────────────────────────────────────────────────")

;; org font
;; (with-eval-after-load 'org
;;   (defun org-buffer-face-mode-variable ()
;;     (interactive)
;;     (make-face 'width-font-face)
;;     (set-face-attribute 'width-font-face nil :font "WenQuanYi Zen Hei Mono 12")
;;     (setq buffer-face-mode 'width-font-face)
;;     (buffer-face-mode))
;;   (add-hook 'org-mode-hook 'org-buffer-face-mode-variable))


;; 自动折行
(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))

;; Modern style for org buffers
(use-package org-modern
  :after org
  :custom (org-modern-star ["✿""☰""☷""✸"])
  :config
  (global-org-modern-mode))

;; (use-package svg-tag-mode
;;   :config
;;   (svg-tag-mode))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/Org/"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

;; 表格对齐
(use-package valign
  :config
  (add-hook 'org-mode-hook #'valign-mode))

;; bullet list
;; (use-package org-bullets
;;   :ensure t
;;   :hook (org-mode . org-bullets-mode)
;;   :config
;;   (setq org-bullets-bullet-list '("☯" "☰" "☷" "❁")))


(provide 'init-org)

;;; init-org.el ends here
