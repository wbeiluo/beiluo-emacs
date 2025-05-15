;;; init-embark.el --- Embark Configurations -*- lexical-binding: t -*-

;; Copyright (C) 2020~2025 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;; Code:

(require 'embark)
(require 'embark-consult)

(require 'vertico-multiform)
(require 'vertico-grid)

;; Optionally replace the key help with a completing-read interface
(setq prefix-help-command #'embark-prefix-help-command)

;; Hide the mode line of the Embark live/completions buffers
(add-to-list 'display-buffer-alist
             '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
               nil
               (window-parameters (mode-line-format . none))))

;; Consult support
(add-hook 'embark-collect-mode-hook 'consult-preview-at-point-mode)

;; 简化动作提示
(setq embark-indicators
      '(embark-minimal-indicator             ; 在迷你缓冲区中显示简化的动作提示，C-h显示动作快捷键
        embark-highlight-indicator           ; 高亮显示当前选中的动作
        embark-isearch-highlight-indicator)) ; 在 isearch 模式下高亮显示与当前输入匹配的动作

;; vertico support for the grid display
(add-to-list 'vertico-multiform-categories '(embark-keybinding grid))
(vertico-multiform-mode 1)

(provide 'init-embark)

;;; init-embark.el ends here
