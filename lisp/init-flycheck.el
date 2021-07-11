;; init-flycheck.el --- flycheck configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2020 王北洛

;; Author: 王北洛 <wbeiluo@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;; flycheck configurations.
;;

;;; Code:

(defhydra hydra-flycheck
  (:pre (flycheck-list-errors)
        :post (quit-windows-on "*Flycheck errors*")
        :hint nil)
  "Errors"
  ;; "
  ;;         ^^                  ^^                         ^^^^                  ╭───────────┐
  ;;   Buffer^^            Server^^                   Symbol^^^^                  │ Flycheck  │
  ;; ╭───────^^──────────────────^^─────────────────────────^^^^──────────────────┴───────────╯
  ;;   [_f_] format        [_M-r_] restart            [_d_] declaration    [_o_] documentation
  ;;   [_m_] imenu         [_S_]   shutdown           [_D_] definition     [_t_] type
  ;;   [_x_] exec action   [_M-s_] describe session   [_R_] references     [_s_] signature
  ;;         ^^                  ^^                   [_i_] implementation [_r_] rename
  ;;  ───────^^──────────────────^^─────────────────────────^^^^──────────────────────────────╯
  ;;       "
  ("f" flycheck-error-list-set-filter "Filter")
  ("j" flycheck-next-error "Next")
  ("k" flycheck-previous-error "Previous")
  ("gg" flycheck-first-error "First")
  ("G" (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
  ("q" nil))


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

(provide 'init-flycheck)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-flycheck.el ends here
