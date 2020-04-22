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

;; org-indent mode
(setq org-startup-indented t)

;; bullet list
(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '("✿" "❁" "❃" "☸" "❉" "✲")))

(setq org-agenda-files (list "~/Org/Life-2020.org"
                             "~/Org/Notes.org"
                             "~/Org/Project.org"
                             "~/Org/Reading.org"
                             "~/Org/Work.org"
                             ))

;; org todo keywords
(setq org-todo-keywords (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
                                (sequence "WAITING(w@/!)" "HOLD(h@/!)" "SOMEDAY(s)" "|" "CANCELLED(c@/!)"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ;;("DOING" :foreground "Black" :weight bold)
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
        ("n" "note" entry (file "~/Org/Notes.org")
         "* %? :NOTE:\n%U\n%a\n" :clock-resume t)
        ("p" "proj" entry (file "~/Org/Project.org")
         "* TODO %?\n%U\n" :clock-resume t)
        ("r" "read" entry (file "~/Org/Reading.org")
         "* TODO %?\n%U\n" :clock-resume t)
        ))

;; Pomodoro
(use-package org-pomodoro
  :bind (:map org-agenda-mode-map
              ("P" . org-pomodoro))
  :config
  (setq org-pomodoro-keep-killed-pomodoro-time t))


(provide 'init-org)

;;; init-org.el ends here
