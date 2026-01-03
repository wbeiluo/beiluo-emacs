;;; init-embark.el --- Embark Configurations -*- lexical-binding: t -*-

;; Copyright (C) 2020~2025 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;; Code:

(defconst extensions-embark-dir
  (expand-file-name "extensions/embark" user-emacs-directory))

(use-package embark
  :ensure nil
  :load-path extensions-embark-dir
  :demand t  ;; 立即加载，不使用延迟加载
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("M-." . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)  ;; alternative for `describe-bindings'
   ("C-c C-e" . embark-export))
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  ;; Add Embark to the mouse context menu. Also enable `context-menu-mode'.
  ;; (context-menu-mode 1)
  ;; (add-hook 'context-menu-functions #'embark-context-menu 100)

  ;; 简化动作提示
  (setq embark-indicators
	'(embark-minimal-indicator             ; 在迷你缓冲区中显示简化的动作提示，C-h显示动作快捷键
          embark-highlight-indicator           ; 高亮显示当前选中的动作
          embark-isearch-highlight-indicator)) ; 在 isearch 模式下高亮显示与当前输入匹配的动作

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure nil
  :load-path extensions-embark-dir
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(provide 'init-embark)

;;; init-embark.el ends here
