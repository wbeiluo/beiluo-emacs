;;; init-window.el --- Window configurations -*- lexical-binding: t -*-

;; Copyright (C) 2020~2025 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs


;;; Commentary:
;;
;;  窗口操作
;;
;;; Code:

(require 'ace-window)

;; Quickly switch windows

(custom-set-faces
 '(aw-leading-char-face ((t (:inherit 'error :bold t :height 1.2))))
 '(aw-mode-line-face ((t (:inherit 'mode-line-emphasis :bold t)))))

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

(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
      aw-dispatch-always t
      aw-dispatch-alist
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

;; 添加hydra支持
(with-eval-after-load 'hydra
  ;; https://github.com/abo-abo/ace-window/wiki/Hydra
  ;; hydra-frame-window is designed from `ace-window' and
  ;; matches aw-dispatch-alist with a few extra
  (defhydra hydra-frame-window (:color red :hint nil)
    "
^Frame^                 ^Window^      Window Size^^^^^^    ^Text Zoom^               (__)
_0_: delete             _t_oggle        ^ ^ _k_ ^ ^            _=_                   (oo)
_1_: delete others      _s_wap          _h_ ^+^ _l_            ^+^             /------\\/
_2_: new                _d_elete        ^ ^ _j_ ^ ^            _-_            / |    ||
_F_ullscreen            ^ ^             _b_alance^^^^          ^ ^        *  /\\---/\\  ~~  M-o w
"
    ("0" delete-frame :exit t)
    ("1" delete-other-frames :exit t)
    ("2" make-frame  :exit t)
    ("b" balance-windows)
    ("s" ace-swap-window)
    ("F" toggle-frame-fullscreen)
    ("t" toggle-window-split)
    ("d" ace-delete-window :exit t)
    ("-" text-scale-decrease)
    ("=" text-scale-increase)
    ("h" shrink-window-horizontally)
    ("k" shrink-window)
    ("j" enlarge-window)
    ("l" enlarge-window-horizontally)
    ("q" nil "quit"))
  (add-to-list 'aw-dispatch-alist '(?w hydra-frame-window/body) t))

(ace-window-display-mode 1)

(provide 'init-window)

;;; init-window.el ends here
