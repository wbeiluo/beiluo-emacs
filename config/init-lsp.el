;;; init-lsp.el --- lsp configurations. -*- lexical-binding: t -*-

;; Copyright (C) 2022~2026 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;;  lsp-mode 配置，按职责分层组织：
;;
;;  1. 核心        lsp-mode：server 启动、补全、行为与 which-key 集成
;;  2. 代码智能    lens / inlay hints / 语义高亮 / modeline 提示
;;  3. 诊断        lsp-diagnostics 接入 flycheck（快捷键见 init-flycheck.el）
;;  4. 界面        lsp-ui：文档浮窗、sideline、peek、imenu
;;  5. 视图检索    lsp-treemacs（错误列表/调用层次）、consult-lsp（符号/诊断选单）
;;  6. 调试        dape + repeat-mode
;;
;;  语言 server：C/C++ 用 clangd，Python 用 lsp-pyright（配置见 init-python.el）。
;;  语言钩子统一在下面 lsp-mode 的 :hook 注册，用 lsp-deferred 延迟启动。
;;

;;; Code:

;;; 依赖：lsp-mode 的进度指示器
(use-package spinner
  :ensure t)

;;; 1. lsp-mode 核心
(use-package lsp-mode
  :ensure t
  ;; lsp-deferred：等 buffer 可见后再启动 server，
  ;; 恢复 session 或批量打开文件时不会一次性拉起所有 server
  :hook (((c-mode c++-mode python-mode) . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :init
  ;; lsp 命令前缀：C-c l（which-key 会按 w/= /T/g/G/h/r/a 分组提示）
  (setq lsp-keymap-prefix "C-c l")
  :custom
  ;; ---- 补全：lsp 只提供 capf，弹窗由 corfu + cape 负责（见 init-corfu.el）----
  (lsp-completion-provider :none)
  (lsp-completion-show-kind t)                ;; 候选标注类型，配合 nerd-icons-corfu 显示图标
  (lsp-completion-show-detail t)
  ;; ---- 响应与性能 ----
  (lsp-idle-delay 0.5)                        ;; 输入停顿后才向 server 请求，减少卡顿
  ;; 不用 server 的 snippet（模板补全由 tempel 提供，见 init-tempel.el）
  (lsp-enable-snippet nil)
  ;; 关闭随输入格式化，开启后可能出现回车后光标跳行首的情况
  (lsp-enable-on-type-formatting nil)
  ;; ---- 代码智能 ----
  (lsp-enable-symbol-highlighting t)          ;; 高亮光标处符号的所有引用
  (lsp-inlay-hint-enable t)                   ;; 类型/参数名内联提示（clangd、pyright 均支持）
  (lsp-signature-auto-activate t)             ;; 边输入边提示函数签名
  (lsp-signature-render-documentation t)      ;; 签名提示附带文档
  ;; ---- 界面 ----
  ;; headerline 面包屑：项目 / 文件 / 符号 三级导航
  (lsp-headerline-breadcrumb-enable t)
  (lsp-headerline-breadcrumb-enable-diagnostics t)
  ;; 不显示图标，纯文本路径更干净
  (lsp-headerline-breadcrumb-icons-enable nil)
  (lsp-headerline-breadcrumb-segments '(project file symbols))
  ;; ---- 其它 ----
  ;; 调试用 dape（见下文），不自动配置 dap-mode
  (lsp-enable-dap-auto-configure nil)
  :commands lsp
  :config
  ;; lsp-mode 的候选过滤区分大小写（跟随 `completion-ignore-case'，默认 nil），
  ;; 小写输入匹配不到 Lan_Data_Handler / PRODUCT_INFO 这类标识符，候选被过滤光
  ;; 就不弹窗（现象是"输入不弹、重新输入又好了"）。设为 t 让过滤忽略大小写。
  (add-hook 'lsp-mode-hook (lambda () (setq-local completion-ignore-case t))))

;;; 2. 代码智能显示

;; lens：行末显示引用计数等（clangd codeLens / pyright）
(use-package lsp-lens
  :custom
  (lsp-lens-place-position 'end-of-line))     ;; 贴在行末，不额外占行

;; 语义高亮：按 server 返回的 token 类型着色，比 font-lock 关键字匹配更准确
(use-package lsp-semantic-tokens
  :custom
  (lsp-semantic-tokens-enable t))

;; modeline 上的 LSP 状态与 code action 提示（诊断由 flycheck 显示）
(use-package lsp-modeline
  :custom
  ;; 诊断不重复显示在 modeline，doom-modeline 的 check 段已经展示
  (lsp-modeline-diagnostics-enable nil)
  (lsp-modeline-code-actions-enable t)
  ;; 不显示图标，只用文字提示
  (lsp-modeline-code-action-icons-enable nil))

;;; 3. 诊断

(use-package lsp-diagnostics
  :custom
  ;; 配置 lsp 使用 flycheck
  (lsp-diagnostics-provider :flycheck)
  :hook (lsp-mode . lsp-diagnostics-mode))

;;; 4. lsp-ui：文档浮窗 / sideline / peek / imenu

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references))
  :custom
  ;; ---- lsp-ui-doc：文档浮窗（childframe 卡片）----
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-delay 0.3)
  (lsp-ui-doc-use-childframe t)
  ;; webkit 渲染已停止维护，统一用内置 markdown 渲染
  (lsp-ui-doc-use-webkit nil)
  (lsp-ui-doc-enhanced-markdown t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-max-width 80)                   ;; 太宽会遮住代码
  (lsp-ui-doc-max-height 15)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-doc-show-with-mouse t)
  ;; 无边框卡片，用主题底色轻微区分
  (lsp-ui-doc-border nil)

  ;; ---- lsp-ui-sideline：行尾诊断与 code action ----
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-code-actions t)
  ;; 悬浮文档由 lsp-ui-doc 负责，sideline 不重复显示
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-update-mode 'line)         ;; 行内移动不重绘，减少闪烁
  (lsp-ui-sideline-diagnostic-max-line-length 120)

  ;; ---- lsp-ui-peek：定义/引用内嵌预览 ----
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-show-directory t)
  (lsp-ui-peek-peek-height 20)
  (lsp-ui-peek-list-width 50)

  ;; ---- lsp-ui-imenu：代码结构侧栏 ----
  (lsp-ui-imenu-enable t)
  (lsp-ui-imenu-kind-position 'top))

;;; 5. 视图与检索增强

;; lsp-treemacs：错误列表、调用层次、符号树
;; lsp-command-map 内置入口：C-c l ge（错误列表）、C-c l gh（调用层次）、C-c l TT（联动开关）
(use-package lsp-treemacs
  :ensure t
  :commands (lsp-treemacs-errors-list lsp-treemacs-call-hierarchy
             lsp-treemacs-symbols lsp-treemacs-type-hierarchy))

;; consult-lsp：LSP 符号/诊断接入 consult 选单（配合 vertico，见 init-vertico.el）
(use-package consult-lsp
  :ensure t
  :commands (consult-lsp-symbols consult-lsp-file-symbols consult-lsp-diagnostics)
  :init
  ;; 绑进 lsp-command-map（C-c l 前缀），与其它 lsp 命令放在一起
  (with-eval-after-load 'lsp-mode
    (define-key lsp-command-map (kbd "s") #'consult-lsp-symbols)       ;; 工作区符号
    (define-key lsp-command-map (kbd "S") #'consult-lsp-file-symbols)  ;; 当前文件符号
    (define-key lsp-command-map (kbd "d") #'consult-lsp-diagnostics))) ;; 工作区诊断

;; 附带功能：lsp-dired（dired 里显示诊断）、lsp-icons（图标辅助）
(use-package lsp-dired)

(use-package lsp-icons)

;;; 6. 调试

;; Debug Adapter Protocol
(use-package dape
  :ensure t
  :preface
  (setq dape-buffer-window-arrangement 'right) ;; 调试窗口布局在右侧
  :hook ((dape-on-start . dape-info-setup)     ;; 启动时显示变量/堆栈面板
	 (dape-on-start . dape-repl-setup)     ;; 启动时显示 REPL
	 (dape-compile . kill-buffer)          ;; Kill compile buffer on build success
	 (dape-display-source . pulse-momentary-highlight-one-line))
  :config
  (setq dape-display-source-buffer-action '(display-buffer-reuse-window))

  (when (file-exists-p "~/.emacs.d/dape-breakpoints")
    ;; Save breakpoints on quit
    (add-hook 'kill-emacs-hook 'dape-breakpoint-save)
    ;; Load breakpoints on startup
    (add-hook 'after-init-hook 'dape-breakpoint-load)))

(use-package repeat
  :config
  ;; Enable repeat mode for more ergonomic `dape' use
  (repeat-mode 1))

(provide 'init-lsp)

;;; init-lsp.el ends here
