;;; init-corfu.el --- Corfu Completion Configurations -*- lexical-binding: t -*-

;; Copyright (C) 2025~2025 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;; Code:

(require 'corfu)
(require 'corfu-popupinfo)
(require 'corfu-history)
(require 'cape)
(require 'cape-char)
(require 'cape-keyword)
(require 'nerd-icons-corfu)

;; Enable Corfu
(setq corfu-auto t)
(setq corfu-cycle t)  ;; Enable cycling for `corfu-next/previous'
(global-corfu-mode 1)
(add-hook 'eshell-mode-hook (lambda ()
                              (setq-local corfu-auto nil)
                              (corfu-mode)))

;; Corfu popupinfo
(corfu-popupinfo-mode 1)               ;; 启用 corfu-popupinfo
(setq corfu-popupinfo-delay 0.25)      ;; 延迟显示(秒)
(setq corfu-popupinfo-position 'right) ;; 信息显示位置（可选 'right, 'left, 'top, 'bottom, 'posframe）
;; 使用 ElDoc 提供信息
(setq corfu-popupinfo-documentation-function #'eldoc-documentation-function)
;; 使用 LSP 模式提供信息
;; (setq corfu-popupinfo-documentation-function
;;       (lambda ()
;;         (when (fboundp 'lsp-eldoc-render-all)
;;           (lsp-eldoc-render-all))))
(define-key corfu-popupinfo-map (kbd "M-t") 'corfu-popupinfo-toggle)
(define-key corfu-popupinfo-map (kbd "M-n") 'corfu-popupinfo-scroll-up)
(define-key corfu-popupinfo-map (kbd "M-p") 'corfu-popupinfo-scroll-down)

;; Corfu history
(corfu-history-mode 1)              ;; 启用 corfu-history
(setq corfu-history-max-entries 20) ;; 最多保留 20 条历史记录

;; A few more useful configurations...
;; TAB cycle if there are only few candidates
(setq completion-cycle-threshold 3)

;; Enable indentation+completion using the TAB key.
;; `completion-at-point' is often bound to M-TAB.
(setq tab-always-indent 'complete)

;; Emacs 30 and newer: Disable Ispell completion function.
;; Try `cape-dict' as an alternative.
(setq text-mode-ispell-word-completion nil)

;; Hide commands in M-x which do not apply to the current mode.  Corfu
;; commands are hidden, since they are not used via M-x. This setting is
;; useful beyond Corfu.
(setq read-extended-command-predicate #'command-completion-default-include-p)

;; Cape
;; Add to the global default value of `completion-at-point-functions' which is
;; used by `completion-at-point'.  The order of the functions matters, the
;; first function returning a result wins.  Note that the list of buffer-local
;; completion functions takes precedence over the global list.
(add-hook 'completion-at-point-functions #'cape-dabbrev)
(add-hook 'completion-at-point-functions #'cape-file)
(add-hook 'completion-at-point-functions #'cape-history)
(add-hook 'completion-at-point-functions #'cape-elisp-block)

;; Nerd icon for Corfu
(add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)

(provide 'init-corfu)

;;; init-corfu.el ends here
