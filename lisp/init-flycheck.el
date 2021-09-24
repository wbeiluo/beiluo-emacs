;; init-flycheck.el --- flycheck configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2020 王北洛

;; Author: 王北洛 <wbeiluo@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;; flycheck configurations.
;;

;;; Code:


(use-package flycheck
  :ensure t
  :diminish
  ;;:bind ("C-c e". hydra-flycheck/body)
  :hook (prog-mode . flycheck-mode)
  :config
  (flycheck-mode 1))

;; (use-package flycheck-pos-tip
;;   :after flycheck
;;   :hook
;;   (flycheck-mode . flycheck-pos-tip-mode))

(use-package flycheck-posframe
  :ensure
  :hook (flycheck-mode-hook . flycheck-posframe-mode))

(use-package flycheck-color-mode-line
  :hook (flycheck-mode-hook . flycheck-color-mode-line-mode))

(defhydra hydra-flycheck (:color: "deep sky blue" :hint nil)
  "
    Errors^^              Action^^
  --------^^------------------^^---------------
    _a_: list errors        _c_: check buffer
    _p_: previous error     _C_: clear errors
    _n_: next error         _w_: copy message
      "
  ("a" flycheck-list-errors)
  ("n" flycheck-next-error)
  ("p" flycheck-previous-error)

  ("c" flycheck-buffer)
  ("C" flycheck-clear)
  ("w" flycheck-copy-errors-as-kill)

  ("q" nil "quit")
  )

(provide 'init-flycheck)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-flycheck.el ends here
