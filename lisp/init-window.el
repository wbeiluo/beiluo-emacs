;;; init-window.el --- Window configurations -*- lexical-binding: t -*-

;; Copyright (C) 2020 王北洛

;; Author: 王北洛 <wbeiluo@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs


;;; Commentary:
;;; Code:

(use-package switch-window
  :ensure t
  :config
  (setq-default switch-window-shortcut-style 'alphabet)
  (setq-default switch-window-timeout nil)
  :bind ("C-x o" . 'switch-window))

;; 全屏显示
(defun fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
                       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))

;; @purcell 调整窗口透明度
(defun sanityinc/adjust-opacity (frame incr)
  "Adjust the background opacity of FRAME by increment INCR."
  (unless (display-graphic-p frame)
    (error "Cannot adjust opacity of this frame"))
  (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
         (oldalpha (if (listp oldalpha) (car oldalpha) oldalpha))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha newalpha))))))

(sanityinc/adjust-opacity nil -4)

(global-set-key (kbd "M-C-8") (lambda () (interactive) (sanityinc/adjust-opacity nil -2)))
(global-set-key (kbd "M-C-9") (lambda () (interactive) (sanityinc/adjust-opacity nil 2)))
(global-set-key (kbd "M-C-7") (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))

(provide 'init-window)

;;; init-window.el ends here
