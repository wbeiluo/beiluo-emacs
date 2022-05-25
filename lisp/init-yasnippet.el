;;; init-yasnippet.el --- Yasnippet configurations -*- lexical-binding: t -*-

;; Copyright (C) 2020 王北洛

;; Author: 王北洛 <wbeiluo@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;; yasnippet configurations
;;

;;; Code:


(use-package yasnippet-snippets
  :ensure t)

(use-package yasnippet
  :ensure t
  :hook (prog-mode . yas-global-mode)
  :bind (("C-x y" . 'hydra-yasnippet/body))
  :config
  
  (defhydra hydra-yasnippet (:hint nil)
    "
    _d_: directory        _i_: ivy insert
    _f_: file             _s_: insert
    _l_: list             _t_: tryout
    _a_: all              _n_: new
        "
    ("d" yas-load-directory)
    ("f" yas-visit-snippet-file)
    ("l" yas-describe-tables)
    ("a" yas-reload-all)

    ("i" ivy-yasnippet)
    ("s" yas-insert-snippet)
    ("t" yas-tryout-snippet)
    ("n" yas-new-snippet)

    ("q"   nil "cancel" :color "deep sky blue"))
  )


(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
