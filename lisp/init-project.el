;;; init-project.el --- Project configuration -*- lexical-binding: t -*-

;; Copyright (C) 2020 王北洛

;; Author: 王北洛 <wbeiluo@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;; 项目管理配置

;;; Code:

(defhydra hydra-project (:exit t :hint nil)
  "
  _p_: switch project  _f_: find file  _g_: find regexp    _r_: replace  _c_: compile
  _b_: switch buffer   _d_: find dir   _D_: project dired  _e_: eshell   "

  ("p"   project-switch-project)
  ("b"   project-switch-to-buffer)
  ("f"   project-find-file)
  ("d"   project-find-dir)
  ("D"   project-dired)
  ("e"   project-eshell)
  ("r"   project-query-replace-regexp)
  ("c"   project-compile)
  ("g"   project-find-regexp)

  ("q"   nil "quit" :color "deep sky blue"))

(global-set-key (kbd "M-p") 'hydra-project/body)

(provide 'init-project)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-project.el ends here
