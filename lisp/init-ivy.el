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

  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t) ; Allow commands in minibuffers

  (setq ivy-height 12
        ivy-use-selectable-prompt t
        ivy-use-virtual-buffers t    ; Enable bookmarks and recentf
        ivy-fixed-height-minibuffer t
        ;;ivy-count-format "[%d/%d] "
        ivy-on-del-error-function #'ignore
        ivy-initial-inputs-alist nil)
  ;; Integrate yasnippet
  (use-package ivy-yasnippet)

  ;; Select from xref candidates with Ivy
  (use-package ivy-xref
    :init
    (when (boundp 'xref-show-definitions-function)
      (setq xref-show-definitions-function #'ivy-xref-show-defs))
    (setq xref-show-xrefs-function #'ivy-xref-show-xrefs)))

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
         ([remap swiper] . counsel-grep-or-swiper)
         ([remap swiper-backward] . counsel-grep-or-swiper-backward)
         ([remap dired] . counsel-dired)
         ([remap set-variable] . counsel-set-variable)
         ([remap insert-char] . counsel-unicode-char)
         ([remap recentf-open-files] . counsel-recentf))

  :init
  ;; Use the faster search tool: ripgrep (`rg')
  (when (executable-find "rg")
    (setq counsel-grep-base-command "rg -S --no-heading --line-number --color never %s %s"))

  :config
  ;; Enhance M-x
  (use-package amx
    :init (setq amx-history-length 20))

  ;; Better sorting and filtering
  (use-package prescient
    :commands prescient-persist-mode
    :init (prescient-persist-mode 1))

  ;; (use-package ivy-prescient
;;     :commands ivy-prescient-re-builder
;;     :custom-face
;;     (ivy-minibuffer-match-face-1 ((t (:foreground ,(face-foreground 'font-lock-doc-face nil t)))))
;;     :init
;;     (defun ivy-prescient-non-fuzzy (str)
;;       "Generate an Ivy-formatted non-fuzzy regexp list for the given STR.
;; This is for use in `ivy-re-builders-alist'."
;;            (let ((prescient-filter-method '(literal regexp)))
;;              (ivy-prescient-re-builder str)))

;;     (setq ivy-prescient-retain-classic-highlighting t
;;           ivy-re-builders-alist
;;           '((counsel-ag . ivy-prescient-non-fuzzy)
;;             (counsel-rg . ivy-prescient-non-fuzzy)
;;             (counsel-pt . ivy-prescient-non-fuzzy)
;;             (counsel-grep . ivy-prescient-non-fuzzy)
;;             (counsel-imenu . ivy-prescient-non-fuzzy)
;;             (counsel-yank-pop . ivy-prescient-non-fuzzy)
;;             (swiper . ivy-prescient-non-fuzzy)
;;             (swiper-isearch . ivy-prescient-non-fuzzy)
;;             (swiper-all . ivy-prescient-non-fuzzy)
;;             (lsp-ivy-workspace-symbol . ivy-prescient-non-fuzzy)
;;             (lsp-ivy-global-workspace-symbol . ivy-prescient-non-fuzzy)
;;             (insert-char . ivy-prescient-non-fuzzy)
;;             (counsel-unicode-char . ivy-prescient-non-fuzzy)
;;             (t . ivy-prescient-re-builder))
;;           ivy-prescient-sort-commands
;;           '(:not swiper swiper-isearch ivy-switch-buffer
;;                  lsp-ivy-workspace-symbol ivy-resume ivy--restore-session
;;                  counsel-grep counsel-git-grep counsel-rg counsel-ag
;;                  counsel-ack counsel-fzf counsel-pt counsel-imenu
;;                  counsel-org-capture counsel-outline counsel-org-goto
;;                  counsel-load-theme counsel-yank-pop
;;                  counsel-recentf counsel-buffer-or-recentf
;;                  centaur-load-theme))

;;     (ivy-prescient-mode 1))

  ;; Ivy integration for Projectile
  (use-package counsel-projectile
    :hook (counsel-mode . counsel-projectile-mode)
    :init (setq counsel-projectile-grep-initial-input '(ivy-thing-at-point))))

  ;; Display world clock using Ivy
  ;; (use-package counsel-world-clock
  ;;   :bind (:map counsel-mode-map
  ;;               ("C-c c k" . counsel-world-clock))))


;; Better experience with icons
;; Enable it before`ivy-rich-mode' for better performance
(use-package all-the-icons-ivy-rich
  :hook (ivy-mode . all-the-icons-ivy-rich-mode)
  :config
  (plist-put all-the-icons-ivy-rich-display-transformers-list
             'centaur-load-theme
             '(:columns
               ((all-the-icons-ivy-rich-theme-icon)
                (ivy-rich-candidate))
               :delimiter "\t"))
  (all-the-icons-ivy-rich-reload))

;; More friendly display transformer for Ivy
(use-package ivy-rich
  :hook ((counsel-projectile-mode . ivy-rich-mode) ; MUST after `counsel-projectile'
         (ivy-rich-mode . (lambda ()
                            "Use abbreviate in `ivy-rich-mode'."
                            (setq ivy-virtual-abbreviate
                                  (or (and ivy-rich-mode 'abbreviate) 'name)))))
  :init
  ;; For better performance
  (setq ivy-rich-parse-remote-buffer nil))

(use-package swiper
  :bind (("C-s" . swiper)
         ;;:map ivy-mode-map
         ("M-s" . swiper-thing-at-point)))

(provide 'init-ivy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ivy.el ends here
