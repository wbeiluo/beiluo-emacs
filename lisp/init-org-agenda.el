;;; init-org-agenda.el --- init for Org Agenda
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(use-package org-agenda
  :ensure nil
  :defer t
  :commands (org-agenda)
  :custom ((org-agenda-window-setup 'current-window)
           (org-agenda-restore-windows-after-quit t)
           ;; (org-agenda-sticky t)        ; don't kill *Org Agenda* buffer by [q].
           (org-agenda-remove-tags t)   ; PERFORMANCE don't display tags in Org Agenda
           )
  :config
  ;; `org-agenda-files'
  (setq org-agenda-files
        '("~/Documents/Agenda/agenda.org"
          "~/Documents/Agenda/project.org"
          "~/Documents/Agenda/books.org"
          "~/Documents/Agenda/learning.org"
          "~/Documents/Agenda/habit.org"
          "~/Documents/Agenda/shopping.org"
          ))


  ;; Agenda Views
  (setq org-agenda-span 'day)

  (setq org-agenda-align-tags-to-column (- (- (/ (/ (display-pixel-width) 2) 10) 3))
        org-agenda-tags-column (- (- (/ (/ (display-pixel-width) 2) 10) 3)))
  (setq org-agenda-prefix-format
        '((agenda . " %i %-12c %? e %?-12t % s")
          (timeline . " % s")
          (effort . " %e %(or (org-entry-get (point) \"Effort\") \"0:00\")")
          (clock . " %i %-12c %? e %?-12t")
          (todo . " %i %-12c")
          (search . " %i %-12c")
          (tags . " %i %-12c")))
  (setq org-agenda-scheduled-leaders '("Scheduled: " "%3d days | "))

  ;; speedup Org Agenda
  (setq org-agenda-inhibit-startup t
        org-agenda-dim-blocked-tasks t)

  ;; toggle log mode in agenda buffer. Press [l] in org-agenda buffer.
  (setq org-agenda-start-with-log-mode '(closed clock)
        org-agenda-log-mode-items '(closed clock))


  (setq org-agenda-prefer-last-repeat t)

  ;; sorting strategy
  (setq org-agenda-sorting-strategy
        '((agenda time-up deadline-up scheduled-down ts-up habit-down priority-down category-keep)
          (todo priority-down category-keep)
          (tags priority-down category-keep)
          (search category-keep))
        org-agenda-sorting-strategy-selected
        '(time-up priority-down deadline-up scheduled-down ts-up habit-down category-keep))


  (setq org-agenda-timegrid-use-ampm t)

  ;; specify different color for days
  (defun my-org-agenda-get-day-face-fn (date)
    "Return the face DATE should be displayed with."
    (let ((day-of-week (calendar-day-of-week date)))
      (cond
       ((or (= day-of-week 1) (= day-of-week 5))
        '(:foreground "forest green" :box (:color "dim gray" :line-width 3)))
       ((org-agenda-todayp date)
        'org-agenda-date-today)
       ((member day-of-week org-agenda-weekend-days)
        'org-agenda-date-weekend)
       (t 'org-agenda-date))))

  (setq org-agenda-day-face-function 'my-org-agenda-get-day-face-fn)

  (setq org-agenda-skip-timestamp-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-deadline-prewarning-if-scheduled t
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-scheduled-delay-if-deadline 'post-deadline
        org-agenda-skip-scheduled-if-deadline-is-shown t
        org-deadline-warning-days 14
        org-scheduled-delay-days 0 ; XXX: if change default 0, will invalid `org-habit'.
        org-agenda-tags-todo-honor-ignore-options t)

  ;; Org-Agenda All Todo List
  (setq org-agenda-todo-ignore-timestamp 'all
        org-agenda-todo-ignore-with-date nil
        org-agenda-todo-ignore-scheduled 'future)

  ;; entry text mode
  ;; (setq org-agenda-start-with-entry-text-mode t)
  ;; follow mode
  ;; (setq org-agenda-start-with-follow-mode t)

  :config
  ;; show context details when jump from agenda.
  (add-to-list 'org-show-context-detail '(agenda . tree))

  ;; [ Composite Agenda View ]
  ;; Usage: `(org-agenda nil "C")'
  (add-to-list
   'org-agenda-custom-commands
   '("C" "Custom Agenda with in progress tasks, priority tasks (and all tasks)."
     ((todo "STARTED") ; from `org-clock' forced state keyword.
      (todo "INPROGRESS")
      (tags "PRIORITY=\"A\""
            ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
             (org-agenda-overriding-header "High-priority unfinished tasks:")))
      (agenda ""
              ((org-agenda-ndays 1)
               (org-agenda-span 1)
               (org-agenda-use-time-grid t))))
     ((org-agenda-compact-blocks t))))

  (add-to-list
   'org-agenda-custom-commands
   '("c" "Tody [c]locked tasks."
     ((agenda ""
              ((org-agenda-ndays 1)
               (org-agenda-span-1)
               (org-agenda-use-time-grid t)
               (org-agenda-include-diary nil)
               (org-agenda-show-log (quote clockcheck))
               (org-agenda-clockreport t))))))

  (add-to-list 'org-agenda-custom-commands
               '("p" "[p]rogramming - BUG, ISSUE, FEATURE etc."
                 ((todo "BUG")
                  (todo "ISSUE")
                  (todo "FEATURE"))))

  (add-to-list 'org-agenda-custom-commands
               '("w" "[W]ork"
                 todo "WORK"
                 ((org-agenda-overriding-header "Work"))))

  ;; used to filter out fragment time tasks.
  (add-to-list 'org-agenda-custom-commands
               '("f" "[f]ragment time tasks"
                 tags "fragment"
                 ((org-agenda-overriding-header "Fragment Tasks"))))

  ;; Show Org Agenda tasks with heigh spacing based on clock time with `org-agenda-log-mode'.
  ;; https://emacs-china.org/t/org-agenda/8679
  ;; work with org-agenda dispatcher [c] "Today Clocked Tasks" to view today's clocked tasks.
  (defun org-agenda-clock-colorize-block ()
    "Set different line spacing based on clock time duration."
    (save-excursion
      (let* ((colors (cl-case (alist-get 'background-mode (frame-parameters))
		               ('light
		                (list "#aa557f" "#FFFF9D" "#BEEB9F" "#ADD5F7"))
		               ('dark
		                (list "orange" "DarkGreen" "DarkSlateBlue" "DarkSlateGray"))))
             pos
             duration)
        (nconc colors colors)
        (goto-char (point-min))
        (while (setq pos (next-single-property-change (point) 'duration))
          (goto-char pos)
          (when (and (not (equal pos (point-at-eol)))
                     (setq duration (org-get-at-bol 'duration)))
            ;; larger duration bar height
            (let ((line-height (if (< duration 15) 1.0 (+ 0.5 (/ duration 30))))
                  (ov (make-overlay (point-at-bol) (1+ (point-at-eol)))))
              (overlay-put ov 'face `(:background ,(car colors) :foreground "black" :extend t))
              (setq colors (cdr colors))
              (overlay-put ov 'line-height line-height)
              (overlay-put ov 'line-spacing (1- line-height))))))))

  ;; use `org-agenda-window-setup' `only-window' value to avoid ol 'line-height
  ;; property failed on long line in "colorized blocks on agenda" hook.
  (when (< (display-mm-width) 800)
    (setq org-agenda-window-setup 'only-window))

  (add-hook 'org-agenda-finalize-hook #'org-agenda-clock-colorize-block)

  ;; (define-key org-agenda-mode-map (kbd "M-s") 'org-search-view)

  ;; auto refresh update `*Org Agenda*' buffer.
  (when (eq system-type 'gnu/linux)
    (defun my/org-agenda-auto-refresh ()
      "Rebuild all agenda views buffers."
      (org-agenda-redo-all t))
    (run-with-idle-timer (* 60 20) t #'my/org-agenda-auto-refresh))
  )

;; org todo keywords
(setq org-todo-keywords (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
                                (sequence "WAITING(w@/!)" "HOLD(h@/!)" "SOMEDAY(s)" "|" "CANCELLED(c@/!)"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "dark red" :weight bold)
              ("NEXT" :foreground "green" :weight bold)
              ("DONE" :foreground "dim gray" :weight bold)
              ("WAITING" :foreground "forest green" :weight bold)
              ("HOLD" :foreground "forest green" :weight bold)
              ("CANCELLED" :foreground "dim gray" :weight bold))))

(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("SOMEDAY" ("WAITING") ("SOMEDAY" . t))
              (done ("WAITING") ("SOMEDAY"))
              ("TODO" ("WAITING") ("CANCELLED") ("SOMEDAY"))
              ("NEXT" ("WAITING") ("CANCELLED") ("SOMEDAY"))
              ("DONE" ("WAITING") ("CANCELLED") ("SOMEDAY")))))

(setq org-capture-templates
      `(("t" "todo" entry (file "~/Documents/Agenda/agenda.org")
         "* TODO %?\n%U\n" :clock-resume t)
        ("p" "proj" entry (file "~/Documents/Agenda/project.org")
         "* TODO %?\n%U\n" :clock-resume t)
        ;; ("n" "note" entry (file "~/Org/Notes.org")
        ;;  "* %? :NOTE:\n%U\n%a\n" :clock-resume t)
        ))

;; tags
(setq org-tag-alist '(("@work" . ?w) ("@read" . ?r) ("@learn" . ?l)
                      ("@prog" . ?p) ("@play" . ?y)))


;;; display icon for Org Agenda category
;; (use-package all-the-icons
;;   :ensure t
;;   :defer t
;;   :after org-agenda
;;   :config
;;   (setq org-agenda-category-icon-alist
;;         `(("agenda" ,(list (all-the-icons-faicon "calendar-o" :height 1)) nil nil :ascent center)  ;;question-circle-o
;;           ("habit" ,(list (all-the-icons-faicon "refresh" 'display '(raise 0.1))) nil nil :ascent center);;refresh
;;           ("project" ,(list (all-the-icons-faicon "product-hunt")) nil nil :ascent center)
;;           ("shopping" ,(list (all-the-icons-faicon "shopping-cart")) nil nil :ascent center)
;;           ("books" ,(list (all-the-icons-faicon "book")) nil nil :ascent center)
;;           ("learning" ,(list (all-the-icons-faicon "leanpub")) nil nil :ascent center)

;;           ;; (".*" ,(list (all-the-icons-faicon "calendar")) nil nil :ascent center)
;;           (".*" '(space . (:width (16))))
;;           )))

;; Pomodoro
(use-package org-pomodoro
  :bind (:map org-agenda-mode-map
              ("P" . org-pomodoro))
  :config
  (setq org-pomodoro-keep-killed-pomodoro-time t))

(provide 'init-org-agenda)

;;; init-org-agenda.el ends here
