;;; init-projectile.el --- projectile configurations. -*- lexical-binding: t -*-

;; Copyright (C) 2020 王北洛

;; Author: 王北洛 <wbeiluo@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;; Code:


;; Manage and navigate projects
(use-package projectile
  :diminish
  :bind (("M-p" . hydra-projectile/body))
  :hook (after-init . projectile-mode)
  :init
  (setq projectile-mode-line-prefix ""
        projectile-sort-order 'recentf
        projectile-use-git-grep t)

  :config

  ;; Use the faster searcher to handle project files: ripgrep `rg'.
  (when (and (not (executable-find "fd"))
             (executable-find "rg"))
    (setq projectile-generic-command
          (let ((rg-cmd ""))
            (dolist (dir projectile-globally-ignored-directories)
              (setq rg-cmd (format "%s --glob '!%s'" rg-cmd dir)))
            (concat "rg -0 --files --color=never --hidden" rg-cmd))))

  ;; Hydra config
  (defhydra hydra-projectile-other-window (:color teal)
    "projectile-other-window"
    ("f"  projectile-find-file-other-window        "file")
    ("g"  projectile-find-file-dwim-other-window   "file dwim")
    ("d"  projectile-find-dir-other-window         "dir")
    ("b"  projectile-switch-to-buffer-other-window "buffer")
    ("q"  nil                                      "cancel" :color "deep sky blue"))

  (defhydra hydra-projectile (:color teal
                              :hint nil)
    "
     PROJECTILE: %(projectile-project-root)

  Project            Find File            Search/Edit          Buffers              Cache
  ^^^^^^^^------------------------------------------------------------------------------------------
  _p_: switch prj    _f_: file            _s_: search          _i_: Ibuffer         _c_: cache clear
  _q_: switch open   _g_: file dwim       _o_: multi-occur     _b_: switch to buf   _x_: remove known project
  _d_: find dir      _a_: find other file _r_: replace         _k_: Kill all bufs   _X_: cleanup non-existing
  ^ ^                _e_: recent file       ^ ^                  ^ ^                _z_: cache current

    "
    ("a"   projectile-find-other-file)
    ("b"   projectile-switch-to-buffer)
    ("c"   projectile-invalidate-cache)
    ("d"   projectile-find-dir)
    ("f"   projectile-find-file)
    ("g"   projectile-find-file-dwim)
    ("l"   projectile-find-file-in-directory)
    ;("s-g" ggtags-update-tags)
    ("i"   projectile-ibuffer)
    ("k"   projectile-kill-buffers)
    ("m"   projectile-multi-occur)
    ("o"   projectile-multi-occur)
    ("p"   projectile-switch-project)
    ("q"   projectile-switch-open-project)
    ("s"   counsel-projectile-rg)
    ("e"   projectile-recentf)
    ("r"   projectile-replace)
    ("x"   projectile-remove-known-project)
    ("X"   projectile-cleanup-known-projects)
    ("z"   projectile-cache-current-file)
    ("`"   hydra-projectile-other-window/body "other window")
    ("q"   nil "cancel" :color "deep sky blue"))

  )

(provide 'init-projectile)

;;; init-projectile.el ends here
