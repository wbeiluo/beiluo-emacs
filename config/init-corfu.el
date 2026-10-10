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
        ;; TAB-and-Go：TAB/S-TAB 循环选择候选（配合 corfu-cycle 与 corfu-preselect 'prompt）；
        ;; 需要展开公共前缀时用 M-TAB（corfu-expand）
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))
  ;; 文档弹窗（corfu-popupinfo-map，弹窗出现时生效）直接用内置键位：
  ;;   M-t 开关弹窗，M-h/M-g 查看文档/跳到定义，
  ;;   C-M-v / C-M-S-v（或 M-<next>/M-<prior>）滚动，M-<home>/M-<end> 到开头/结尾；
  ;;   M-p/M-n 保留给 corfu-previous/next（corfu 默认绑定）
  :custom
  (corfu-auto t)                       ;; Enable auto completion
  (corfu-cycle t)                      ;; Enable cycling for `corfu-next/previous'
  (corfu-quit-at-boundary 'separator)  ;; 越过补全边界即退出；orderless 多段过滤用 M-SPC 插入分隔符
  (corfu-quit-no-match 'separator)     ;; 无匹配时退出（插入分隔符后才保持弹窗，便于 orderless 纠错）
  (corfu-preview-current nil)          ;; Disable current candidate preview
  (corfu-preselect 'prompt)            ;; Preselect the prompt
  (corfu-on-exact-match 'insert)       ;; Configure handling of exact matches

  (corfu-auto-delay 0.5)
  (corfu-popupinfo-delay '(1.0 . 0.5))               ;; 文档弹窗延迟（lsp 需 completionItem/resolve，别太快）
  (corfu-popupinfo-direction '(right left vertical)) ;; 右侧放不下时按序回退

  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  (corfu-history-mode)
  :config
  ;; 历史长度：corfu-history 并没有 corfu-history-max-entries 这个变量，
  ;; 长度由 `history-length' 或该符号的 history-length 属性决定
  (put 'corfu-history 'history-length 20)

  ;; C/C++：. -> :: 后立即触发补全（跳过 delay 与前缀长度限制），顺带预热 clangd
  (dolist (hook '(c-mode-hook c++-mode-hook))
    (add-hook hook (lambda () (setq-local corfu-auto-trigger ".:>"))))

  ;; Eshell：不做自动补全（RET 在 shell 里的行为见下方 keymap-set）
  (add-hook 'eshell-mode-hook
            (lambda ()
              (setq-local corfu-auto nil)
              (corfu-mode 1)))
  ;; RET：普通 buffer 插入选中候选（corfu-insert）；
  ;; eshell/comint 里改用 corfu-send（插入并发送给进程，省掉按两次回车）
  (keymap-set corfu-map "RET"
              `(menu-item "" nil :filter
                          ,(lambda (&optional _)
                             (if (derived-mode-p 'eshell-mode 'comint-mode)
                                 #'corfu-send
                               #'corfu-insert)))))

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
  ;; 注意：必须用 add-hook 写入"默认值"。init 加载时 *scratch* 里该变量已是
  ;; buffer-local，add-to-list 只改那个 buffer 的局部副本，其它 buffer 看不到。
  (add-hook 'completion-at-point-functions #'cape-file)         ;; 文件路径补全
  (add-hook 'completion-at-point-functions #'cape-dabbrev)      ;; 当前 Buffer 关键词补全
  (add-hook 'completion-at-point-functions #'cape-history)      ;; Eshell, Comint or minibuffer history 补全
  (add-hook 'completion-at-point-functions #'cape-elisp-block)  ;; Org/Markdown 里的代码块补全
  ;; 上游建议把 cape 命令绑到 C-c p（cape-prefix-map），但本配置的 C-c p
  ;; 已给 project-prefix-map（见 init-program.el），这里改用空闲的 C-c c；
  ;; 按 C-c c C-h（或 which-key）可查看全部 cape 命令
  :bind ("C-c c" . cape-prefix-map)
  :config
  ;; 自动补全下避免每次触发都扫描所有同 major-mode 的 buffer（cape 对此有性能提示）
  (setq cape-dabbrev-buffer-function #'current-buffer))

;; Nerd icon for Corfu
(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(provide 'init-corfu)

;;; init-corfu.el ends here
