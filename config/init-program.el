;;; init-program.el --- Program config -*- lexical-binding: t -*-

;; Copyright (C) 2021~2025 王北洛

;; Author: 王北洛 <beiluo.wang@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;; Code:


(defconst extensions-symbol-overlay-dir
  (expand-file-name "extensions/symbol-overlay" user-emacs-directory))
(defconst extensions-highlight-indent-guides-dir
  (expand-file-name "extensions/highlight-indent-guides" user-emacs-directory))
(defconst extensions-rainbow-delimiters-dir
  (expand-file-name "extensions/rainbow-delimiters" user-emacs-directory))
(defconst extensions-colorful-mode-dir
  (expand-file-name "extensions/colorful-mode" user-emacs-directory))
(defconst extensions-smartparens-dir
  (expand-file-name "extensions/smartparens" user-emacs-directory))
(defconst extensions-origami-dir
  (expand-file-name "extensions/origami" user-emacs-directory))
(defconst extensions-lsp-origami-dir
  (expand-file-name "extensions/lsp-origami" user-emacs-directory))

;; Highlight symbols
(use-package symbol-overlay
  :ensure nil
  :load-path extensions-symbol-overlay-dir
  :bind (("M-i" . symbol-overlay-put)
         ("M-n" . symbol-overlay-jump-next)
         ("M-p" . symbol-overlay-jump-prev)
         ("M-I" . symbol-overlay-remove-all))
  :hook ((prog-mode . symbol-overlay-mode)
         (iedit-mode . (lambda () (symbol-overlay-mode -1))))
  :config
  ;; 设置高亮延迟，防止光标移动时频繁闪烁
  (setq symbol-overlay-idle-time 0.1))

;; Highlight indent
(use-package highlight-indent-guides
  :ensure nil
  :load-path extensions-highlight-indent-guides-dir
  :hook (prog-mode . highlight-indent-guides-mode)
  :custom
  ;; 使用字符描绘线条
  (highlight-indent-guides-method 'character)
  ;; 设置指引线字符
  (highlight-indent-guides-char ?┆)
  ;; 仅高亮当前光标所在的缩进层级
  (highlight-indent-guides-responsive 'stack)
  ;; 延迟更新以提升性能
  (highlight-indent-guides-delay 0.1)
  :config
  ;; 确保在切换主题后线条颜色能自动适配
  (setq highlight-indent-guides-auto-enabled t))

;; Highlight parentheses
(use-package rainbow-delimiters
  :ensure nil
  :load-path extensions-rainbow-delimiters-dir
  :hook (prog-mode . rainbow-delimiters-mode))

;; Colorful mode
(use-package colorful-mode
  :ensure nil
  :load-path extensions-colorful-mode-dir
  :custom
  (colorful-use-prefix t)
  (colorful-only-strings 'only-prog)
  (css-fontify-colors nil)
  :config
  ;; 支持 Hex, HSL 等颜色显示
  (setq colorful-extra-color-keyword-functions
        '(colorful-add-hex-colors
          colorful-add-color-names
          colorful-add-hsl-colors))
  ;; 全局开启
  (global-colorful-mode 1)
  (add-to-list 'global-colorful-modes 'helpful-mode))

;; Auto parentheses
(use-package smartparens
  :ensure nil
  :load-path extensions-smartparens-dir
  :hook ((prog-mode . smartparens-mode)
	 ;;(emacs-lisp-mode . smartparens-strict-mode)
	 )
  :bind
  (:map sp-keymap
        ("M-<right>" . sp-forward-slurp-sexp) ;; 向右吞噬：将后面的符号吸进括号
        ("M-<left>"  . sp-forward-barf-sexp)  ;; 向右吐出：将内部符号挤出括号
        ("M-D"       . sp-splice-sexp)        ;; 解开括号：保留内容，删除外层括号
        ("C-M-k"     . sp-kill-sexp))         ;; 杀掉整个括号
  :config
  ;; 加载默认的配置
  (require 'smartparens-config)
  
  ;; 在 Org-mode 中不要自动配对单引号
  (sp-local-pair 'org-mode "'" nil :actions nil)

  ;; 增强：支持“跳出”括号
  ;; 如果你输入了 ) 而光标后面已经是 )，它会直接跳过而不是重复输入
  (setq sp-autoskip-closing-pair 'always)

  (smartparens-global-mode t))

;; 代码折叠
(use-package origami
  :ensure nil
  :load-path extensions-origami-dir
  :hook (prog-mode . origami-mode)
  :bind (:map origami-mode-map
	      ("C-<tab> <tab>" . origami-toggle-node)
	      ("C-<tab> C-<tab>" . origami-toggle-all-nodes)
	      ("C-<tab> o" . origami-open-node)
	      ("C-<tab> c" . origami-close-node)
	      ("C-<tab> n" . origami-next-fold)
	      ("C-<tab> p" . origami-next-fold)
	      ("C-<tab> t" . origami-recursively-toggle-node)))

(use-package lsp-origami
  :ensure nil
  :load-path extensions-lsp-origami-dir
  :after (lsp-mode origami)
  :hook (lsp-configure . lsp-origami-try-enable))

(provide 'init-program)

;;; init-program.el ends here
