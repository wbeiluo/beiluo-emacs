;;; init-window.el --- Window configurations -*- lexical-binding: t -*-

;; Copyright (C) 2020~2026 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs


;;; Commentary:
;;
;;  窗口操作
;;

;;; Code:

;; Quickly switch windows
(use-package ace-window
  :ensure t
  :bind (("M-o" . ace-window))
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-dispatch-always t)
  (aw-background t)
  :config
  (defun toggle-window-split ()
    (interactive)
    (if (= (count-windows) 2)
	(let* ((this-win-buffer (window-buffer))
               (next-win-buffer (window-buffer (next-window)))
               (this-win-edges (window-edges (selected-window)))
               (next-win-edges (window-edges (next-window)))
               (this-win-2nd (not (and (<= (car this-win-edges)
                                           (car next-win-edges))
                                       (<= (cadr this-win-edges)
                                           (cadr next-win-edges)))))
               (splitter
		(if (= (car this-win-edges)
                       (car (window-edges (next-window))))
                    'split-window-horizontally
                  'split-window-vertically)))
          (delete-other-windows)
          (let ((first-win (selected-window)))
            (funcall splitter)
            (if this-win-2nd (other-window 1))
            (set-window-buffer (selected-window) this-win-buffer)
            (set-window-buffer (next-window) next-win-buffer)
            (select-window first-win)
            (if this-win-2nd (other-window 1))))))

  (setq aw-dispatch-alist
	'((?0 delete-window)
          (?1 delete-other-windows)
          (?2 split-window-below)
          (?3 split-window-right)
          (?4 consult-buffer-other-window)
          (?| (lambda ()
		(interactive)
		(split-window-right)
		(windmove-right)))
          (?_ (lambda ()
		(interactive)
		(split-window-below)
		(windmove-down)))))

  (ace-window-display-mode 1))

(provide 'init-window)

;;; init-window.el ends here
