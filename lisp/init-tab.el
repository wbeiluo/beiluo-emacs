;;; init-tab.el --- Insert description here -*- lexical-binding: t -*-

;; Copyright (C) 2021 王北洛

;; Author: 王北洛 <beiluo.wang@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;; Code:

(defhydra awesome-fast-switch (:hint nil :color "deep sky blue")
    "
 ^^^^Fast Move             ^^^^Tab                    ^^Search            ^^Misc
-^^^^--------------------+-^^^^---------------------+-^^----------------+-^^---------------------------
   ^_k_^   prev group    | _C-a_^^     select first | _b_ search buffer | _C-k_   kill buffer
 _[_   _]_  switch tab   | _C-e_^^     select last  | _g_ search group  | _C-S-k_ kill others in group
   ^_j_^   next group    | _C-j_^^     ace jump     | ^^                | ^^
 ^^0 ~ 9^^ select window | _C-h_/_C-l_ move current | ^^                | ^^
-^^^^--------------------+-^^^^---------------------+-^^----------------+-^^---------------------------
"
    ("[" awesome-tab-backward-tab)
    ("j" awesome-tab-forward-group)
    ("k" awesome-tab-backward-group)
    ("]" awesome-tab-forward-tab)
    ("1" awesome-tab-select-visible-tab)
    ("2" awesome-tab-select-visible-tab)
    ("3" awesome-tab-select-visible-tab)
    ("4" awesome-tab-select-visible-tab)
    ("5" awesome-tab-select-visible-tab)
    ("6" awesome-tab-select-visible-tab)
    ("7" awesome-tab-select-visible-tab)
    ("8" awesome-tab-select-visible-tab)
    ("9" awesome-tab-select-visible-tab)
    ("0" awesome-tab-select-visible-tab)
    ("C-a" awesome-tab-select-beg-tab)
    ("C-e" awesome-tab-select-end-tab)
    ("C-j" awesome-tab-ace-jump)
    ("C-h" awesome-tab-move-current-tab-to-left)
    ("C-l" awesome-tab-move-current-tab-to-right)
    ("b" ivy-switch-buffer)
    ("g" awesome-tab-counsel-switch-group)
    ("C-k" kill-current-buffer)
    ("C-S-k" awesome-tab-kill-other-buffers-in-current-group)
    ("q" nil "quit"))

(use-package awesome-tab
  :load-path "extensions/awesome-tab"
  :bind(("C-=" . 'awesome-fast-switch/body))
  :config
  (awesome-tab-mode t))

(provide 'init-tab)
;;; init-tab.el ends here
