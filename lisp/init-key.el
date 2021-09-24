;;; init-key.el --- misc configurations -*- lexical-binding: t -*-

;; Copyright (C) 2020 王北洛

;; Author: 王北洛 <wbeiluo@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;; misc
;;

;;; Code:

(global-set-key (kbd "C-j") 'forward-char)

(global-set-key (kbd "<f8>") 'ivy-resume)

(global-set-key (kbd "C-;") 'avy-goto-char)
(global-set-key (kbd "C-'") 'avy-goto-line)

(global-set-key (kbd "C-,") 'goto-last-change)
(global-set-key (kbd "C-.") 'goto-last-point)


(defhydra hydra-navigation (:exit t :hint nil)
  "
    Buffer^^              Goto^^             Edit^^             Program^^             Others^^
  --------^^------------------^^-----------------^^--------------------^^-------------------^^-----------
    _f_: find file        _a_: avy           _S_: multi cursor     _p_: projectile    _T_: shell
    _r_: recent           _s_: counsel-rg    _R_: rectangle        _l_: lsp mode      _E_: eshell
    _b_: buffer           _w_: frame window  _c_: flycheck         _m_: magit         _F_: fullscreen
    _B_: ibuffer            ^^.: last point   ^^                   _y_: yasnippet
    _t_: tab                ^^                ^^                   _o_: origami
        "
  ("f" counsel-find-file)
  ("r" recentf-open-files)
  ("b" ivy-switch-buffer)
  ("B" ibuffer)
  ("t" awesome-fast-switch/body)

  ("a" hydra-avy/body)
  ("s" counsel-rg)
  ("w" hydra-frame-window/body)

  ("R" hydra-rectangle/body)
  ("S" hydra-multiple-cursors/body)
  ("c" hydra-flycheck/body)

  ("p" hydra-projectile/body)
  ("l" hydra-lsp/body)
  ("m" magit-status)
  ("y" hydra-yasnippet/body)
  ("o" origami-hydra/body)

  ("T" shell)
  ("E" eshell)
  ("F" fullscreen)

  ("q" nil "quit")
  )

(global-set-key (kbd "M-SPC") 'hydra-navigation/body)



;; 快捷键提示
(use-package which-key
  :config
  (which-key-mode))

(provide 'init-key)
;;; init-key.el ends here
