;; init-edit.el --- edit configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2020 王北洛

;; Author: 王北洛 <wbeiluo@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;; edit configurations.
;;

;;; Code:

;; Jump to things in Emacs tree-style
(use-package avy
  :bind (("C-:" . avy-goto-char)
         ("C-'" . avy-goto-char-2)
         ("M-g f" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)
         ("M-g e" . avy-goto-word-0))
  :hook (after-init . avy-setup-default)
  :config (setq avy-all-windows nil
                avy-all-windows-alt t
                avy-background t
                avy-style 'pre))

;; Drag stuff (lines, words, region, etc...) around
;; <M-up> <M-down> <M-right> <M-left>
(use-package drag-stuff
  :diminish
  :commands drag-stuff-define-keys
  :hook (after-init . drag-stuff-global-mode)
  :config
  (add-to-list 'drag-stuff-except-modes 'org-mode)
  (drag-stuff-define-keys))

;; Multiple cursors
(use-package multiple-cursors
  :bind (("C-S-c C-S-c"   . mc/edit-lines)
         ("C->"           . mc/mark-next-like-this)
         ("C-<"           . mc/mark-previous-like-this)
         ("C-c C-<"       . mc/mark-all-like-this)
         ("C-M->"         . mc/skip-to-next-like-this)
         ("C-M-<"         . mc/skip-to-previous-like-this)
         ("s-<mouse-1>"   . mc/add-cursor-on-click)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)
         :map mc/keymap
         ("C-|" . mc/vertical-align-with-space)))

;; Goto last change
(use-package goto-chg
  :bind ("C-," . goto-last-change))

;; Record and jump to the last point in the buffer
(use-package goto-last-point
  :diminish
  :bind ("C-M-," . goto-last-point)
  :hook (after-init . goto-last-point-mode))

;; Flexible text folding
;; (use-package origami
;;   :pretty-hydra
;;   ((:title (pretty-hydra-title "Origami" 'octicon "fold")
;;     :color amaranth :quit-key "q")
;;    ("Node"
;;     ((":" origami-recursively-toggle-node "toggle recursively")
;;      ("a" origami-toggle-all-nodes "toggle all")
;;      ("t" origami-toggle-node "toggle current")
;;      ("o" origami-show-only-node "only show current"))
;;     "Actions"
;;     (("u" origami-undo "undo")
;;      ("d" origami-redo "redo")
;;      ("r" origami-reset "reset"))))
;;   :bind (:map origami-mode-map
;;          ("C-`" . origami-hydra/body))
;;   :hook (prog-mode . origami-mode)
;;   :init (setq origami-show-fold-header t)
;;   :config (face-spec-reset-face 'origami-fold-header-face))

(provide 'init-edit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-edit.el ends here
