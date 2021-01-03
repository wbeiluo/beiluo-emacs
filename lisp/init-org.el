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
    (setq buffer-face-mode-face 'width-font-face)
    (buffer-face-mode))
  (add-hook 'org-mode-hook 'org-buffer-face-mode-variable))

;; org-indent mode
(setq org-startup-indented t)

;; 自动折行
(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))

;; bullet list
;(use-package org-bullets
;  :ensure t
;  :hook (org-mode . org-bullets-mode)
;  :config
;  (setq org-bullets-bullet-list '("✿" "❁" "☸" "❃" "❉" "✲")))

;; Pomodoro
(use-package org-pomodoro
  :bind (:map org-agenda-mode-map
              ("P" . org-pomodoro))
  :config
  (setq org-pomodoro-keep-killed-pomodoro-time t))

(setq org-agenda-files (list "~/Org/Life.org"
                             "~/Org/Learning.org"
                             "~/Org/Reading.org"
                             "~/Org/Project.org"
                             "~/Org/Notes.org"
                             ))

;; org todo keywords
(setq org-todo-keywords (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
                                (sequence "WAITING(w@/!)" "HOLD(h@/!)" "SOMEDAY(s)" "|" "CANCELLED(c@/!)"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "green" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold))))

(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("SOMEDAY" ("WAITING") ("SOMEDAY" . t))
              (done ("WAITING") ("SOMEDAY"))
              ("TODO" ("WAITING") ("CANCELLED") ("SOMEDAY"))
              ("NEXT" ("WAITING") ("CANCELLED") ("SOMEDAY"))
              ("DONE" ("WAITING") ("CANCELLED") ("SOMEDAY")))))

(setq org-capture-templates
      `(("t" "todo" entry (file "~/Org/Life-2020.org")
         "* TODO %?\n%U\n" :clock-resume t)
        ("p" "proj" entry (file "~/Org/Project.org")
         "* TODO %?\n%U\n" :clock-resume t)
        ("n" "note" entry (file "~/Org/Notes.org")
         "* %? :NOTE:\n%U\n%a\n" :clock-resume t)
        ))

;; tags
(setq org-tag-alist '(("@work" . ?w) ("@read" . ?r) ("@learn" . ?l)
                      ("@prog" . ?p) ("@play" . ?y)))


(provide 'init-org)

;;; init-org.el ends here
