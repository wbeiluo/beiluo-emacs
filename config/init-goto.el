;; init-goto.el --- Goto Anywhere Configurations. -*- lexical-binding: t -*-

;; Copyright (C) 2025~2025 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;; 光标移动及跳转操作
;;

;;; Code:

(require 'avy)
(require 'mwim)
(require 'goto-chg)
(require 'goto-last-point)

;; Jump to things in Emacs tree-style

(setq avy-timeout-seconds 0.6)
;; (setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?l ?q ?e ?r ?u ?i ?p ?n))

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
      (alist-get ?o avy-dispatch-alist) 'avy-action-embark)

;; Record and jump to the last point in the buffer
(goto-last-point-mode 1)

(provide 'init-goto)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-goto.el ends here
