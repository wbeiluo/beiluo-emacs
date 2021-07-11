;;; init-window.el --- Window configurations -*- lexical-binding: t -*-

;; Copyright (C) 2020 王北洛

;; Author: 王北洛 <wbeiluo@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs


;;; Commentary:
;;; Code:

;; (use-package switch-window
;;   :ensure t
;;   :config
;;   (setq-default switch-window-shortcut-style 'alphabet)
;;   (setq-default switch-window-timeout nil)
;;   :bind ("C-x o" . 'switch-window))

(defun joe-scroll-other-window()
  (interactive)
  (scroll-other-window 1))
(defun joe-scroll-other-window-down ()
  (interactive)
  (scroll-other-window-down 1))
(use-package ace-window
    :ensure t
    :defer 1
    :bind (("M-o" . 'ace-window))
    :config
    (set-face-attribute
     'aw-leading-char-face nil
     :foreground "deep sky blue"
     :weight 'bold
     :height 2.0)
    (set-face-attribute
     'aw-mode-line-face nil
     :inherit 'mode-line-buffer-id
     :foreground "lawn green")
    (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l)
          aw-dispatch-always t
          aw-dispatch-alist
          '((?f hydra-frame-window/body)
            (?0 delete-window)
            (?1 delete-other-windows)
            (?2 split-window-below)
            (?3 split-window-right)
            (?| (lambda ()
                  (interactive)
                  (split-window-right)
                  (windmove-right)))
            (?_ (lambda ()
                  (interactive)
                  (split-window-below)
                  (windmove-down)))
            (?b balance-windows)
            (?w ace-delete-window)
            (?S ace-swap-window)
            (?F toggle-frame-fullscreen)
            (?t toggle-window-spilt)
            (?u (lambda ()
                  (progn
                    (winner-undo)
                    (setq this-command 'winner-undo))))
            (?r winner-redo)))

    (defhydra hydra-frame-window (:hint nil :color "deep sky blue")
      "
^Delete^                       ^Frame resize^             ^Window^                Window Size^^^^^^   ^Text^
_0_: delete-frame              _g_: resize-frame-right    _t_: toggle               ^ ^ _k_ ^ ^        _K_
_1_: delete-other-frames       _H_: resize-frame-left     _e_: ace-swap-win         _h_ ^+^ _l_        ^+^
_2_: make-frame                _F_: fullscreen            ^ ^                       ^ ^ _j_ ^ ^        _J_
_d_: kill-and-delete-frame     _n_: new-frame-right       _w_: ace-delete-window    _b_alance^^^^      ^ ^
"
      ("0" delete-frame :exit t)
      ("1" delete-other-frames :exit t)
      ("2" make-frame  :exit t)
      ("b" balance-windows)
      ("d" kill-and-delete-frame :exit t)
      ("e" ace-swap-window)
      ("F" toggle-frame-fullscreen)    ;; is <f11>
      ("g" resize-frame-right :exit t)
      ("H" resize-frame-left :exit t)  ;; aw-dispatch-alist uses h, I rebind here so hjkl can be used for size
      ("n" new-frame-right :exit t)
      ;; ("r" reverse-windows)
      ("t" toggle-window-spilt)
      ("w" ace-delete-window :exit t)
      ("x" delete-frame :exit t)
      ("K" text-scale-decrease)
      ("J" text-scale-increase)
      ("h" shrink-window-horizontally)
      ("k" shrink-window)
      ("j" enlarge-window)
      ("l" enlarge-window-horizontally)
      ("q" nil "quit"))

    (ace-window-display-mode t))

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

(sanityinc/adjust-opacity nil -3)

(global-set-key (kbd "M-C-8") (lambda () (interactive) (sanityinc/adjust-opacity nil -2)))
(global-set-key (kbd "M-C-9") (lambda () (interactive) (sanityinc/adjust-opacity nil 2)))
(global-set-key (kbd "M-C-7") (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))

(provide 'init-window)

;;; init-window.el ends here
