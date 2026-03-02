;;; init-fonts.el --- Set Fonts -*- lexical-binding: t -*-

;; Copyright (C) 2020~2026 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;; 中英文及图标字体设置
;;
;;; Code:

(defconst extensions-nerd-icons-dir
  (expand-file-name "extensions/nerd-icons" user-emacs-directory))

(use-package nerd-icons
  :ensure nil
  :load-path extensions-nerd-icons-dir)

;; (defun set-font (english chinese english-size chinese-size &optional english-weight chinese-weight)
;;   "Set ENGLISH, CHINESE font and ENGLISH-SIZE, CHINESE-SIZE."
;;   (let ((ew (or english-weight 'regular))
;;         (cw (or chinese-weight 'regular)))
;;     ;; 默认字体
;;     (set-face-attribute 'default nil
;;                         :font (format "%s:pixelsize=%d" english english-size)
;;                         :weight ew)
;;     ;; 比例字体
;;     (set-face-attribute 'variable-pitch nil
;;                         :font (format "%s:pixelsize=%d" english english-size)
;;                         :weight ew)
;;     ;; 等宽字体
;;     (set-face-attribute 'fixed-pitch nil
;;                         :font (format "%s:pixelsize=%d" english english-size)
;;                         :weight ew)
;;     ;; 中文字体
;;     (dolist (charset '(kana han symbol cjk-misc bopomofo))
;;       (set-fontset-font (frame-parameter nil 'font) charset
;;                         (font-spec :family chinese :size chinese-size :weight cw)))))

(defun set-font (english chinese english-size chinese-scale &optional english-weight chinese-weight)
  "Set ENGLISH, CHINESE font and ENGLISH-SIZE, CHINESE-SCALE."
  (let ((ew (or english-weight 'regular))
        (cw (or chinese-weight 'regular)))
    ;; 默认字体
    (set-face-attribute 'default nil
                        :family english
                        :height (round (* english-size 10))
                        :weight ew)
    ;; 等宽、比例字体
    (dolist (face '(fixed-pitch variable-pitch))
      (set-face-attribute face nil
                          :family english
                          :height (round (* english-size 10))
                          :weight ew))
    
    ;; 中文字体
    (dolist (charset '(kana han cjk-misc bopomofo symbol))
      (set-fontset-font (frame-parameter nil 'font) charset
                        (font-spec :family chinese :weight cw)
                        nil 'append))
    ;; 中文字体比例
    (setq face-font-rescale-alist
          `((,chinese . ,chinese-scale)))))

;; 设置中英文字体
;; Chinese fonts: Sarasa Mono SC, WenQuanYi Micro Hei Mono, Hack Nerd Font Mono, 霞鹜文楷等宽
;; English fonts: JetBrains Mono, Sarasa Mono SC, Maple Mono, Cascadia Code

(when (display-graphic-p)
  (if (eq system-type 'windows-nt)
      (set-font "LXGW WenKai Mono Medium" "霞鹜文楷等宽 Medium" 12 1.00)
    (set-font "LXGW WenKai Mono" "霞鹜文楷等宽" 12 1.00 'medium 'regular)))

;; (when (display-graphic-p)
;;   (set-font "Maple Mono" "霞鹜文楷等宽" 11 1.06))

;; (when (display-graphic-p)
;;   (set-font "Cascadia Code" "霞鹜文楷等宽" 11.5 1.0))

(add-to-list 'after-make-frame-functions
             (lambda (new-frame)
               (select-frame new-frame)
               (when (display-graphic-p)
                 (if (eq system-type 'windows-nt)
                     (set-font "LXGW WenKai Mono Medium" "霞鹜文楷等宽 Medium" 12 1.00)
                   (set-font "LXGW WenKai Mono" "霞鹜文楷等宽" 12 1.00 'medium 'regular)))))

;; 设置图标字体
;; FiraCode Nerd Font, Symbols Nerd Font Mono
(setq nerd-icons-font-family "Symbols Nerd Font Mono")
;; (setq nerd-icons-scale-factor 1.0)

;; 设置Uniclode字体
(set-fontset-font "fontset-default" 'unicode "Symbols Nerd Font Mono")

(provide 'init-fonts)

;;; init-fonts.el ends here
