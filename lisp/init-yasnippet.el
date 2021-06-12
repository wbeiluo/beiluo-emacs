;;; init-yasnippet.el --- Yasnippet configurations -*- lexical-binding: t -*-

;; Copyright (C) 2020 王北洛

;; Author: 王北洛 <wbeiluo@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;; yasnippet configurations
;;

;;; Code:


(use-package yasnippet-snippets)
(use-package yasnippet
  :ensure t
  :hook (prog-mode . yas-global-mode)
  :bind (("C-x y" . 'hydra-yasnippet/body))
  :config
  (defhydra hydra-yasnippet (:color "deep sky blue" :hint nil)
    "
              ^YASnippets^
--------------------------------------------
  Modes:    Load/Visit:    Actions:

 _g_lobal  _d_irectory    _i_nsert
 _m_inor   _f_ile         _t_ryout
 _e_xtra   _l_ist         _n_ew
         _a_ll
"
    ("d" yas-load-directory)
    ("e" yas-activate-extra-mode)
    ("i" yas-insert-snippet)
    ("f" yas-visit-snippet-file :color blue)
    ("n" yas-new-snippet)
    ("t" yas-tryout-snippet)
    ("l" yas-describe-tables)
    ("g" yas-global-mode)
    ("m" yas-minor-mode)
    ("a" yas-reload-all)
    ("q"   nil "cancel" :color blue))
  )


(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
