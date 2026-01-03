;;; init-corfu.el --- Corfu Completion Configurations -*- lexical-binding: t -*-

;; Copyright (C) 2025~2025 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;; Code:

(defconst extensions-corfu-dir
  (list (expand-file-name "extensions/corfu" user-emacs-directory)
	(expand-file-name "extensions/corfu/extensions" user-emacs-directory)))
(defconst extensions-cape-dir
  (expand-file-name "extensions/cape" user-emacs-directory))
(defconst extensions-nerd-icons-corfu-dir
  (expand-file-name "extensions/nerd-icons-corfu" user-emacs-directory))

(use-package corfu
  :ensure nil
  :load-path extensions-corfu-dir
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  :config
  (global-corfu-mode 1)
  (add-hook 'eshell-mode-hook (lambda ()
				(setq-local corfu-auto nil)
				(corfu-mode))))

;; A few more useful configurations...
(use-package emacs
  :custom
  ;; TAB cycle if there are only few candidates
  (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p))

;; Corfu popupinfo
(use-package corfu-popupinfo
  :ensure nil
  :load-path extensions-corfu-dir
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode) ;; 随 corfu 模式自动启动
  :bind
  (:map corfu-map
        ("M-p" . corfu-popupinfo-scroll-down)
        ("M-n" . corfu-popupinfo-scroll-up)
        ("M-t" . corfu-popupinfo-toggle))
  :custom
  (corfu-popupinfo-delay '(0.5 . 0.2)) ;; 设置文档弹窗的延迟（不建议设为 0，否则由于查询文档会有卡顿感）
  (corfu-popupinfo-position 'right)    ;; 显示位置（可选 'right, 'left, 'top, 'bottom, 'posframe）
  :init
  ;; 使用 ElDoc 提供信息
  (setq corfu-popupinfo-documentation-function #'eldoc-documentation-function))

;; Corfu history
(use-package corfu-history
  :ensure nil
  :load-path extensions-corfu-dir
  :after corfu
  :hook (corfu-mode . corfu-history-mode) ;; 随 corfu 模式自动启动
  :config
  (setq corfu-history-max-entries 20) ;; 最多保留 20 条历史记录
  (setq corfu-sort-function #'corfu-sort-history))

;; Cape
(use-package cape
  :ensure nil
  :load-path extensions-cape-dir
  :init
  ;; 将 Cape 提供的补全函数添加到全局 Capf 列表中
  (add-to-list 'completion-at-point-functions #'cape-file)        ;; 文件路径补全
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)     ;; 当前 Buffer 关键词补全
  (add-to-list 'completion-at-point-functions #'cape-history)     ;; Eshell, Comint or minibuffer history 补全
  (add-to-list 'completion-at-point-functions #'cape-elisp-block) ;; Org/Markdown 里的代码块补全
  )

;; Nerd icon for Corfu
(use-package nerd-icons-corfu
  :ensure nil
  :load-path extensions-nerd-icons-corfu-dir
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(provide 'init-corfu)

;;; init-corfu.el ends here
