;;; init-program.el --- Program config -*- lexical-binding: t -*-

;; Copyright (C) 2021 王北洛

;; Author: 王北洛 <beiluo.wang@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;; Code:

(defhydra origami-hydra (:hint nil :color "#66ccff")
  "
    Node^^^^                               Actions^^^^
  ------^^^^--------------------------------------^^^^---------------------
    _o_: open node    _a_: toggle all      _n_: next fold      _r_: redo
    _c_: close node   _t_: toggle current  _p_: previous fold
    _s_: show current _f_: toggle forword  _u_: undo
        "
  ("o" origami-open-node)
  ("c" origami-close-node)
  ("n" origami-next-fold)
  ("p" origami-previous-fold)
  ("f" origami-forward-toggle-node)
  ("a" origami-toggle-all-nodes)
  ("t" origami-toggle-node)
  ("s" origami-show-only-node)
  ("u" origami-undo)
  ("r" origami-redo)
  ("q" nil "quit"))


;; Flexible text folding
(use-package origami
  ;;:bind (("C-`" . origami-hydra/body))
  :hook (prog-mode . origami-mode)
  :config
  (face-spec-reset-face 'origami-fold-header-face))


;; 显示缩进线
(use-package highlight-indent-guides
  ;;:ensure t
  :hook (prog-mode . highlight-indent-guides-mode)
  :init
  (setq highlight-indent-guides-method 'character))

(provide 'init-program)

;;; init-program.el ends here
