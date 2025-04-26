;; init-edit.el --- edit configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2020~2025 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;; 光标移动及文本编辑操作
;;

;;; Code:

(require 'ring)

;; 移动至行首或行尾
(use-package mwim
  :ensure t
  :bind (("C-a" . mwim-beginning-of-code-or-line)
	 ("C-e" . mwim-end-of-code-or-line)))

;; Jump to things in Emacs tree-style
(use-package avy
  :ensure t
  :bind (("C-'" . avy-goto-char-timer)
         :map isearch-mode-map
         ("C-'" . avy-isearch))

  :custom
  (avy-timeout-seconds 0.6)
  ;; (avy-keys '(?a ?s ?d ?f ?g ?h ?j ?l ?q ?e ?r ?u ?i ?p ?n))
  
  :config
  (defun avy-action-kill-whole-line (pt)
    "avy action: kill the whole line where avy selection is"
    (save-excursion
      (goto-char pt)
      (kill-whole-line))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (defun avy-action-copy-whole-line (pt)
    "avy action: copy the whole line where avy selection is"
    (save-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end)
          (bounds-of-thing-at-point 'line)
        (copy-region-as-kill start end)))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (defun avy-action-yank-whole-line (pt)
    "avy action: copy the line where avy selection is and paste to current point"
    (avy-action-copy-whole-line pt)
    (save-excursion (yank))
    t)

  (defun avy-action-teleport-whole-line (pt)
    "avy action: kill the line where avy selection is and paste to current point"
    (avy-action-kill-whole-line pt)
    (save-excursion (yank)) t)

  (defun avy-action-mark-to-char (pt)
    "avy action: mark from current point to avy selection"
    (activate-mark)
    (goto-char pt))

  (defun avy-action-embark (pt)
    "avy action: embark where avy selection is"
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  (setf (alist-get ?k avy-dispatch-alist) 'avy-action-kill-stay
        (alist-get ?K avy-dispatch-alist) 'avy-action-kill-whole-line
        (alist-get ?w avy-dispatch-alist) 'avy-action-copy
        (alist-get ?W avy-dispatch-alist) 'avy-action-copy-whole-line
        (alist-get ?y avy-dispatch-alist) 'avy-action-yank
        (alist-get ?Y avy-dispatch-alist) 'avy-action-yank-whole-line
        (alist-get ?t avy-dispatch-alist) 'avy-action-teleport
        (alist-get ?T avy-dispatch-alist) 'avy-action-teleport-whole-line
        (alist-get ?  avy-dispatch-alist) 'avy-action-mark-to-char
        (alist-get ?o avy-dispatch-alist) 'avy-action-embark
        )
  )

;; Goto last change
(use-package goto-chg
  :ensure t
  :bind ("C-M-," . goto-last-change))

;; Record and jump to the last point in the buffer
(use-package goto-last-point
  :diminish
  :hook (after-init . goto-last-point-mode)
  :bind ("C-," . goto-last-point))

(use-package highlight-symbol
  :ensure t
  :bind ("C-x l" . highlight-symbol)
  :init (highlight-symbol-mode))

;; 文本移动
;; Drag stuff (lines, words, region, etc...) around
;; <M-up> <M-down> <M-right> <M-left>
(use-package drag-stuff
  :diminish
  :commands drag-stuff-define-keys
  :hook (after-init . drag-stuff-global-mode)
  :config
  (add-to-list 'drag-stuff-except-modes 'org-mode)
  (drag-stuff-define-keys))

;; visual displays the undo history
(use-package vundo
  :ensure t
  :bind ("C-x u" . vundo))

;; 多行文本操作
(use-package multiple-cursors
  :ensure t
  :bind (("C-c C-c SPC" . mc/edit-lines)
         ("C-c C-c n"   . mc/mark-next-like-this)
         ("C-c C-c p"   . mc/mark-previous-like-this)
         ("C-c C-c a"   . mc/mark-all-like-this)))

(use-package imenu
  :ensure t
  :bind ("M-g i" . imenu))

;; 插入时间
(defun insert-current-time ()
  "Insert current time."
  (interactive)
  (insert (format-time-string "%H:%M")))

;; 插入日期时间
(defun insert-current-data-time ()
  "Insert current data and time."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M")))

;; 插入日期星期时间
(defun insert-current-data-week-time ()
  "Insert current data, week and time."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %A %H:%M")))

(provide 'init-edit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-edit.el ends here
