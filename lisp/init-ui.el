;;; init-ui.el --- Emacs UI configurations -*- lexical-binding: t -*-

;; Copyright (C) 2023 王北洛

;; Author: 王北洛 <wbeiluo@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs


;;; Commentary:
;;; Code:

;; 界面设置
;; (tool-bar-mode -1)                      ; 禁用工具栏
;; (menu-bar-mode -1)                      ; 禁用菜单栏
;; (scroll-bar-mode -1)                    ; 禁用滚动条
;; (show-paren-mode t)                     ; 高亮另一个括号
;; (global-auto-revert-mode t)             ; 自动刷新buffer
;; (display-time-mode t)                   ; 打开时间显示
;; (setq display-time-format "%H:%M")      ; 设置时间显示格式
;; (display-battery-mode t)                ; 打开电池显示

;; (setq use-dialog-box nil)               ; 鼠标操作不使用对话框
;; (setq inhibit-default-init t)           ; 不加载 `default' 库
;; (setq inhibit-startup-screen t)         ; 不加载启动画面
;; (setq inhibit-startup-message t)        ; 不加载启动消息
;; (setq inhibit-startup-buffer-menu t)    ; 不显示缓冲区列表

;; 隐藏 title bar
;; (setq default-frame-alist '((undecorated . t)))
;; (add-to-list 'default-frame-alist '(drag-internal-border . 1))
;; (add-to-list 'default-frame-alist '(internal-border-width . 1))

;; 在modeline上显示列号
;; (column-number-mode t)

;; pop buffer
;; (use-package shackle
;;   :ensure t
;;   :hook (after-init . shackle-mode)
;;   :init
;;   (setq shackle-lighter "")
;;   (setq shackle-select-reused-windows nil) ; default nil
;;   (setq shackle-default-alignment 'below)  ; default below
;;   (setq shackle-default-size 0.3)          ; default 0.5
;;   (setq shackle-rules
;;         ;; CONDITION(:regexp)            :select     :inhibit-window-quit   :size+:align|:other     :same|:popup
;;         '((compilation-mode              :ignore t)
;;           ("\\*Async Shell.*\\*" :regexp t :ignore t)
;;           ("\\*corfu.*\\*"       :regexp t :ignore t)
;;           ("*eshell*"                    :select t                          :size 0.4  :align t     :popup t)
;;           (helpful-mode                  :select t                          :size 0.6  :align right :popup t)
;;           ;; ("*Messages*"                  :select t                          :size 0.4  :align t     :popup t)
;;           ("*Calendar*"                  :select t                          :size 0.3  :align t     :popup t)
;;           ;; ("*info*"                      :select t                                                  :same t)
;;           ;; (magit-status-mode             :select t   :inhibit-window-quit t                         :same t)
;;           ;; (magit-log-mode                :select t   :inhibit-window-quit t                         :same t)
;;           ))
;;   )

;; (use-package popper
;;   :ensure t
;;   :bind (("M-`"     . popper-toggle-latest)
;;          ("M-<tab>" . popper-cycle)
;;          ("M-\\"    . popper-toggle-type)
;;          )
;;   :init
;;   (setq popper-reference-buffers
;;         '("\\*Messages\\*"
;;           "\\*Async Shell Command\\*"
;;           help-mode
;;           helpful-mode
;;           occur-mode
;;           pass-view-mode
;;           "^\\*eshell.*\\*$" eshell-mode ;; eshell as a popup
;;           "^\\*shell.*\\*$"  shell-mode  ;; shell as a popup
;;           ("\\*corfu\\*" . hide)
;;           (compilation-mode . hide)
;;           ;; derived from `fundamental-mode' and fewer than 10 lines will be considered a popup
;;           (lambda (buf) (with-current-buffer buf
;;                           (and (derived-mode-p 'fundamental-mode)
;;                                (< (count-lines (point-min) (point-max))
;;                                   10))))
;;           )
;;         )
;;   (popper-mode +1)
;;   (popper-echo-mode +1)
;;   :config
;;   ;; group by project.el, projectile, directory or perspective
;;   (setq popper-group-function nil)
  




(provide 'init-ui)

;;; init-ui.el ends here
