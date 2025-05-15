;;; init-consult.el --- Consult Completion Configurations -*- lexical-binding: t -*-

;; Copyright (C) 2025~2025 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;; Code:

;; Tweak the register preview for `consult-register-load',
;; `consult-register-store' and the built-in commands.  This improves the
;; register formatting, adds thin separator lines, register sorting and hides
;; the window mode line.
(advice-add #'register-preview :override #'consult-register-window)
(setq register-preview-delay 0.5)

;; Use Consult to select xref locations with preview
(setq xref-show-xrefs-function #'consult-xref
      xref-show-definitions-function #'consult-xref)

(require 'consult)
(require 'consult-xref)
(require 'consult-org)
(require 'consult-imenu)
(require 'consult-compile)
(require 'consult-info)
(require 'consult-kmacro)
(require 'consult-register)

;; Optionally configure preview. The default value
;; is 'any, such that any key triggers the preview.
;; (setq consult-preview-key 'any)
;; (setq consult-preview-key "M-.")
;; (setq consult-preview-key '("S-<down>" "S-<up>"))
;; For some commands and buffer sources it is useful to configure the
;; :preview-key on a per-command basis using the `consult-customize' macro.
(consult-customize
 consult-theme :preview-key '(:debounce 0.2 any)
 consult-ripgrep consult-git-grep consult-grep consult-man
 consult-bookmark consult-recent-file consult-xref
 consult--source-bookmark consult--source-file-register
 consult--source-recent-file consult--source-project-recent-file
 ;; :preview-key "M-."
 :preview-key '(:debounce 0.4 any))

;; Optionally configure the narrowing key.
;; Both < and C-+ work reasonably well.
(setq consult-narrow-key "<")
;; Enable automatic preview at point in the *Completions* buffer. This is
;; relevant when you use the default completion UI.
(add-hook 'completion-list-mode-hook 'consult-preview-at-point-mode)

(provide 'init-consult)

;;; init-consult.el ends here
