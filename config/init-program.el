;;; init-program.el --- Program config -*- lexical-binding: t -*-

;; Copyright (C) 2021~2026 王北洛

;; Author: 王北洛 <beiluo.wang@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;; Code:

;;; 快捷键(默认C-x p)，增加C-c p
(global-set-key (kbd "C-c p") project-prefix-map)

;; Highlight symbols
(use-package symbol-overlay
  :ensure t
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
  :ensure t
  :hook (prog-mode . highlight-indent-guides-mode)
  :custom
  ;; 使用字符描绘线条
  (highlight-indent-guides-method 'character)
  ;; 设置指引线字符
  (highlight-indent-guides-character ?|)
  ;; 仅高亮当前光标所在的缩进层级
  (highlight-indent-guides-responsive 'stack)
  ;; 延迟更新以提升性能
  (highlight-indent-guides-delay 0.1)
  :config
  ;; 确保在切换主题后线条颜色能自动适配
  (setq highlight-indent-guides-auto-enabled t))

;; Highlight parentheses
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Colorful mode
(use-package colorful-mode
  :ensure t
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
  :ensure t
  :hook (prog-mode . smartparens-mode)
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
  :ensure t
  :hook (prog-mode . origami-mode)
  :bind (:map origami-mode-map
	      ("C-<tab> <tab>" . origami-toggle-node)
	      ("C-<tab> C-<tab>" . origami-toggle-all-nodes)
	      ("C-<tab> o" . origami-open-node)
	      ("C-<tab> c" . origami-close-node)
	      ("C-<tab> n" . origami-next-fold)
	      ("C-<tab> p" . origami-previous-fold)
	      ("C-<tab> t" . origami-recursively-toggle-node)))

(use-package lsp-origami
  :ensure t
  :after (lsp-mode origami)
  :hook (lsp-configure . lsp-origami-try-enable))

(provide 'init-program)

;;; init-program.el ends here
