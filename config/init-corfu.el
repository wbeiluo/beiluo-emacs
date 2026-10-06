;;; init-corfu.el --- Corfu Completion Configurations -*- lexical-binding: t -*-

;; Copyright (C) 2025~2026 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;; Code:

(use-package corfu
  :ensure t
  :bind
  (:map corfu-map
        ("M-p" . corfu-popupinfo-scroll-down)
        ("M-n" . corfu-popupinfo-scroll-up)
        ("M-t" . corfu-popupinfo-toggle))
  :custom
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  (corfu-quit-no-match t)        ;; quit, even if there is no match
  (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect 'prompt)      ;; Preselect the prompt
  (corfu-on-exact-match 'insert) ;; Configure handling of exact matches

  (corfu-auto-delay 0.2)
  (corfu-popupinfo-delay '(0.5 . 0.2)) ;; 设置文档弹窗的延迟（不建议设为 0，否则由于查询文档会有卡顿感）
  (corfu-popupinfo-direction '(right)) ;; 弹窗优先显示在右侧（列表按优先级排列，可选 left/right/vertical/force-*）

  (corfu-history-max-entries 20)             ;; 最多保留 20 条历史记录
  (corfu-sort-function #'corfu-sort-length-alpha) ;; 基础排序；历史排序由 corfu-history-mode 自动接管
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  (corfu-history-mode)
  :config
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

;; Completion At Point Extensions
(use-package cape
  :ensure t
  :init
  ;; 将 Cape 提供的补全函数添加到全局 Capf 列表中
  (add-to-list 'completion-at-point-functions #'cape-file)         ;; 文件路径补全
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)      ;; 当前 Buffer 关键词补全
  (add-to-list 'completion-at-point-functions #'cape-history)      ;; Eshell, Comint or minibuffer history 补全
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)) ;; Org/Markdown 里的代码块补全

;; Nerd icon for Corfu
(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(provide 'init-corfu)

;;; init-corfu.el ends here
