;; init-ivy.el --- Ivy configurations.  -*- lexical-binding: t -*-

;; Copyright (C) 2020 王北洛

;; Author: 王北洛 <wbeiluo@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;; Ivy configurations.
;;

;;; Code:

(use-package ivy
  :diminish ivy-mode
  :hook (after-init . ivy-mode)
  :init
  (setq-default ivy-use-virtual-buffers t
                ivy-virtual-abbreviate 'fullpath
                ivy-count-format ""
                projectile-completion-system 'ivy
                ivy-magic-tilde nil
                ivy-dynamic-exhibit-delay-ms 150
                ivy-use-selectable-prompt t)

  ;; Additional key bindings for Ivy
  (use-package ivy-hydra
    :commands ivy-hydra-read-action
    :init (setq ivy-read-action-function #'ivy-hydra-read-action))

  ;; Integrate yasnippet
  (use-package ivy-yasnippet
    :bind ("C-c C-y" . ivy-yasnippet)))


(use-package counsel
  :diminish counsel-mode
  :hook (ivy-mode . counsel-mode)
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)

         ("C-c C-r" . ivy-resume)
         ("C-c r" . counsel-rg)
         ("C-c g" . counsel-grep)
         ("C-c f" . counsel-describe-function)
         ("C-c v" . counsel-describe-variable)

         :map counsel-mode-map
;;         ([remap dired] . counsel-dired)
         ([remap set-variable] . counsel-set-variable)
         ([remap insert-char] . counsel-unicode-char)
         ([remap recentf-open-files] . counsel-recentf))

  :init
  (setq-default ivy-initial-inputs-alist
                '((Man-completion-table . "^")
                  (woman . "^")))

  (setq enable-recursive-minibuffers t) ; Allow commands in minibuffers
  (setq swiper-action-recenter t)

  ;; Use the faster search tool: ripgrep (`rg')
  (when (executable-find "rg")
    (setq counsel-grep-base-command "rg -S --no-heading --line-number --color never %s %s"))

  :config
  (with-no-warnings
    ;; Integration with `projectile'
    (with-eval-after-load 'projectile
      (setq projectile-completion-system 'ivy))

    ;; Integration with `magit'
    (with-eval-after-load 'magit
      (setq magit-completing-read-function 'ivy-completing-read)))

  ;; Ivy integration for Projectile
  (use-package counsel-projectile
    :hook (counsel-mode . counsel-projectile-mode)
    :init (setq counsel-projectile-grep-initial-input '(ivy-thing-at-point))))

(use-package swiper
  :bind (("C-s" . swiper)
         ;;:map ivy-mode-map
         ("M-s /" . swiper-thing-at-point)))

(use-package ivy-xref
  :config
  (setq xref-show-xrefs-function 'ivy-xref-show-xrefs))

(provide 'init-ivy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ivy.el ends here
