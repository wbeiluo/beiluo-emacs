;;; init-rectangle.el --- Rectangle Operations -*- lexical-binding: t -*-

;; Copyright (C) 2021 王北洛

;; Author: 王北洛 <beiluo.wang@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;; Code:

(defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
                                     :color "deep sky blue"
                                     :hint nil
                                     :post (deactivate-mark))
  "
  Rectangle^^
  ---------^^----------------------------------------------------------
    ^_k_^       _c_: copy      _o_: open       _n_: number-lines
  _h_   _l_     _y_: yank      _t_: type       _e_: exchange-point
    ^_j_^       _d_: kill      _C_: clear      _r_: reset-region-mark
  ^^^^          _u_: undo      ^^              ^^
    "
  ("k" rectangle-previous-line)
  ("j" rectangle-next-line)
  ("h" rectangle-backward-char)
  ("l" rectangle-forward-char)
  ("d" kill-rectangle)                    ;; C-x r k
  ("y" yank-rectangle)                    ;; C-x r y
  ("c" copy-rectangle-as-kill)            ;; C-x r M-w
  ("o" open-rectangle)                    ;; C-x r o
  ("t" string-rectangle)                  ;; C-x r t
  ("C" clear-rectangle)                   ;; C-x r c
  ("e" rectangle-exchange-point-and-mark) ;; C-x C-x
  ("n" rectangle-number-lines)            ;; C-x r N
  ("r" (if (region-active-p)
           (deactivate-mark)
         (rectangle-mark-mode 1)))
  ("u" undo nil)
  ("q" nil "quit"))

;; (global-set-key (kbd "C-x SPC") 'hydra-rectangle/body)

(provide 'init-rectangle)

;;; init-rectangle.el ends here
