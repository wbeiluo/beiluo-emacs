;; init-key.el --- Key binding configurations. -*- lexical-binding: t -*-

;; Copyright (C) 2022~2025 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;;  绑定常用快捷键
;;

;;; Code:

;;; 显示与窗口
;; ACE
(global-set-key (kbd "M-o") #'ace-window)

;; 窗口透明度切换
(global-set-key (kbd "C-M-8") #'transwin-toggle)

;;; 搜索
;; C-c bindings in `mode-specific-map'
(global-set-key (kbd "C-c M-x") #'consult-mode-command)
(global-set-key (kbd "C-c h") #'consult-history)
(global-set-key (kbd "C-c k") #'consult-kmacro)
(global-set-key (kbd "C-c m") #'consult-man)
(global-set-key (kbd "C-c i") #'consult-info)
(global-set-key (kbd "C-c r") #'consult-recent-file)
(define-key global-map [remap Info-search] #'consult-info)
;; C-x bindings in `ctl-x-map'
(global-set-key (kbd "C-x M-:") #'consult-complex-command)     ; orig. repeat-complex-command
(global-set-key (kbd "C-x b") #'consult-buffer)                ; orig. switch-to-buffer
(global-set-key (kbd "C-x 4 b") #'consult-buffer-other-window) ; orig. switch-to-buffer-other-window
(global-set-key (kbd "C-x 5 b") #'consult-buffer-other-frame)  ; orig. switch-to-buffer-other-frame
(global-set-key (kbd "C-x t b") #'consult-buffer-other-tab)    ; orig. switch-to-buffer-other-tab
(global-set-key (kbd "C-x r b") #'consult-bookmark)            ; orig. bookmark-jump
(global-set-key (kbd "C-x p b") #'consult-project-buffer)      ; orig. project-switch-to-buffer
;; Custom M-# bindings for fast register access
(global-set-key (kbd "M-#") #'consult-register-load)
(global-set-key (kbd "M-'") #'consult-register-store)          ; orig. abbrev-prefix-mark (unrelated)
(global-set-key (kbd "C-M-#") #'consult-register)
;; Other custom bindings
(global-set-key (kbd "M-y") #'consult-yank-pop)                ; orig. yank-pop
;; M-g bindings in `goto-map'
(global-set-key (kbd "M-g e") #'consult-compile-error)
(global-set-key (kbd "M-g f") #'consult-flycheck)              ; Alternative: consult-flymake
(global-set-key (kbd "M-g g") #'consult-goto-line)             ; orig. goto-line
(global-set-key (kbd "M-g M-g") #'consult-goto-line)           ; orig. goto-line
(global-set-key (kbd "M-g o") #'consult-outline)               ; Alternative: consult-org-heading
(global-set-key (kbd "M-g m") #'consult-mark)
(global-set-key (kbd "M-g k") #'consult-global-mark)
(global-set-key (kbd "M-g i") #'consult-imenu)
(global-set-key (kbd "M-g I") #'consult-imenu-multi)
;; M-s bindings in `search-map'
(global-set-key (kbd "M-s d") #'consult-find)                  ; Alternative: consult-fd
(global-set-key (kbd "M-s c") #'consult-locate)
(global-set-key (kbd "M-s g") #'consult-grep)
(global-set-key (kbd "M-s G") #'consult-git-grep)
(global-set-key (kbd "M-s r") #'consult-ripgrep)
(global-set-key (kbd "M-s l") #'consult-line)
(global-set-key (kbd "M-s L") #'consult-line-multi)
(global-set-key (kbd "M-s k") #'consult-keep-lines)
(global-set-key (kbd "M-s u") #'consult-focus-lines)
(global-set-key (kbd "M-s f") #'sdcv-search-pointer)           ; sdcv
(global-set-key (kbd "M-s F") #'sdcv-search-input)             ; sdcv
;; Isearch integration
(global-set-key (kbd "C-s") #'consult-line)
(global-set-key (kbd "M-s e") #'consult-isearch-history)
(define-key isearch-mode-map (kbd "M-e") #'consult-isearch-history)   ; orig. isearch-edit-string
(define-key isearch-mode-map (kbd "M-s e") #'consult-isearch-history) ; orig. isearch-edit-string
(define-key isearch-mode-map (kbd "M-s l") #'consult-line)            ; needed by consult-line to detect isearch
(define-key isearch-mode-map (kbd "M-s L") #'consult-line-multi)      ; needed by consult-line to detect isearch
(define-key minibuffer-local-map (kbd "M-s") #'consult-history)       ; orig. next-matching-history-element
(define-key minibuffer-local-map (kbd "M-r") #'vertico-repeat)        ; orig. previous-matching-history-element
;; Embark
(global-set-key (kbd "C-.") #'embark-act)
(global-set-key (kbd "M-.") #'embark-dwim)
(global-set-key (kbd "C-h B") #'embark-bindings)
(global-set-key (kbd "C-c C-e") #'embark-export)

;;; 光标移动及跳转
;; Avy
(global-set-key (kbd "C-'") #'avy-goto-char-timer)
(define-key isearch-mode-map (kbd "C-'") #'avy-isearch)

;; 移动至行首或行尾
(global-set-key (kbd "C-a") #'mwim-beginning-of-code-or-line)
(global-set-key (kbd "C-e") #'mwim-end-of-code-or-line)

;; Goto last change
(global-set-key (kbd "C-M-,") #'goto-last-change)

;; Record and jump to the last point in the buffer
(global-set-key (kbd "C-,") #'goto-last-point)

;;; Shell
(global-set-key (kbd "<f5>") 'eshell)
(global-set-key (kbd "<f6>") 'shell)

;;; 文件、目录操作
;; Treemacs
(define-key global-map (kbd "M-0")       'treemacs-select-window)
(define-key global-map (kbd "C-x t 1")   'treemacs-delete-other-windows)
(define-key global-map (kbd "C-x t t")   'treemacs)
(define-key global-map (kbd "C-x t d")   'treemacs-select-directory)
(define-key global-map (kbd "C-x t B")   'treemacs-bookmark)
(define-key global-map (kbd "C-x t C-t") 'treemacs-find-file)
(define-key global-map (kbd "C-x t M-t") 'treemacs-find-tag)

;; Undo
(global-set-key (kbd "C-x u") #'vundo)

;; 多行文本操作
(global-set-key (kbd "C-c C-c SPC") #'mc/edit-lines)
(global-set-key (kbd "C-c C-c n")   #'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-c p")   #'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-c a")   #'mc/mark-all-like-this)

;; Markdown
(with-eval-after-load 'markdown-mode
  (define-key markdown-mode-map (kbd "C-c C-e") #'markdown-do))

;;; 项目
(global-set-key (kbd "M-p") project-prefix-map)

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch)
(global-set-key (kbd "C-c M-g") 'magit-file-popup)

;; Flycheck
(define-key flycheck-mode-map flycheck-keymap-prefix nil)
(setq flycheck-keymap-prefix (kbd "C-c e"))
(define-key flycheck-mode-map flycheck-keymap-prefix
            flycheck-command-map)
;; (global-set-key (kbd "C-c e l") #'flycheck-list-errors)
;; (global-set-key (kbd "C-c e n") #'flycheck-next-error)
;; (global-set-key (kbd "C-c e p") #'flycheck-previous-error)

;; 代码折叠
(global-set-key (kbd "C-<tab> <tab>")   #'origami-toggle-node)
(global-set-key (kbd "C-<tab> C-<tab>") #'origami-toggle-all-nodes)
(global-set-key (kbd "C-<tab> o")       #'origami-open-node)
(global-set-key (kbd "C-<tab> c")       #'origami-close-node)
(global-set-key (kbd "C-<tab> n")       #'origami-next-fold)
(global-set-key (kbd "C-<tab> p")       #'origami-previous-fold)

;;; Highlight
;; Symbol-overlay
;; "i" -> symbol-overlay-put
;; "n" -> symbol-overlay-jump-next
;; "p" -> symbol-overlay-jump-prev
;; "w" -> symbol-overlay-save-symbol
;; "t" -> symbol-overlay-toggle-in-scope
;; "e" -> symbol-overlay-echo-mark
;; "d" -> symbol-overlay-jump-to-definition
;; "s" -> symbol-overlay-isearch-literally
;; "q" -> symbol-overlay-query-replace
;; "r" -> symbol-overlay-rename
(global-set-key (kbd "M-i") 'symbol-overlay-put)
;; (global-set-key (kbd "M-n") 'symbol-overlay-switch-forward)
;; (global-set-key (kbd "M-p") 'symbol-overlay-switch-backward)
;; (global-set-key (kbd "<f7>") 'symbol-overlay-mode)
;; (global-set-key (kbd "<f8>") 'symbol-overlay-remove-all)

;; Templates
(global-set-key (kbd "M-+") #'tempel-complete)
(global-set-key (kbd "M-*") #'tempel-insert)

;;; Org Mode
;; Org-agenda
(global-set-key (kbd "\e\e a") #'org-agenda)
(define-key org-agenda-mode-map (kbd "i") #'(lambda () (interactive) (org-capture nil "d")))
(define-key org-agenda-mode-map (kbd "J") #'consult-org-agenda)

;; Org-capture
(global-set-key (kbd "\e\e c") #'org-capture)

;; Denote
(global-set-key (kbd "C-c n n") #'denote)
(global-set-key (kbd "C-c n r") #'denote-rename-file)
(global-set-key (kbd "C-c n l") #'denote-link)
(global-set-key (kbd "C-c n b") #'denote-backlinks)
(global-set-key (kbd "C-c n d") #'denote-dired)
(global-set-key (kbd "C-c n f") #'consult-notes)
(global-set-key (kbd "C-c n c") #'consult-notes-search-in-all-notes)

;; Org links
(global-set-key (kbd "C-c s s")   #'org-super-links-link)
(global-set-key (kbd "C-c s l")   #'org-super-links-store-link)
(global-set-key (kbd "C-c s C-l") #'org-super-links-insert-link)
(global-set-key (kbd "C-c s d")   #'org-super-links-quick-insert-drawer-link)
(global-set-key (kbd "C-c s i")   #'org-super-links-quick-insert-inline-link)
(global-set-key (kbd "C-c s C-d") #'org-super-links-delete-link)

;;; 其他快捷键
;; 时间
(global-set-key (kbd "C-c t t") 'insert-current-time)
(global-set-key (kbd "C-c t d") 'insert-current-data-time)
(global-set-key (kbd "C-c t w") 'insert-current-data-week-time)

;; Emms
(global-set-key (kbd "<f9>") 'emms)

(provide 'init-key)

;;; init-key.el ends here
