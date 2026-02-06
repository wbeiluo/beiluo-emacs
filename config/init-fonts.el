;;; init-fonts.el --- Set Fonts -*- lexical-binding: t -*-

;; Copyright (C) 2020~2025 王北洛

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

(defun set-font (english chinese english-size chinese-size &optional english-weight chinese-weight)
  "Set ENGLISH, CHINESE font and ENGLISH-SIZE, CHINESE-SIZE."
  (let ((ew (or english-weight 'regular))
        (cw (or chinese-weight 'regular)))
    ;; 默认字体
    (set-face-attribute 'default nil
                        :font (format "%s:pixelsize=%d" english english-size)
                        :weight ew)
    ;; 比例字体
    (set-face-attribute 'variable-pitch nil
                        :font (format "%s:pixelsize=%d" english english-size)
                        :weight ew)
    ;; 等宽字体
    (set-face-attribute 'fixed-pitch nil
                        :font (format "%s:pixelsize=%d" english english-size)
                        :weight ew)
    ;; 中文字体
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font) charset
                        (font-spec :family chinese :size chinese-size :weight cw)))))

;; Chinese fonts: Sarasa Mono SC, WenQuanYi Micro Hei Mono, Hack Nerd Font Mono, LXGW WenKai Mono
;; English fonts: JetBrains Mono, Sarasa Mono SC, Maple Mono, Cascadia Code

(when (display-graphic-p)
  (set-font "LXGW WenKai Mono" "LXGW WenKai Mono" 26 26 'medium 'regular))

;; (when (display-graphic-p)
;;   (set-font "Maple Mono" "LXGW WenKai Mono" 24 26))

;; (when (display-graphic-p)
;;   (set-font "Cascadia Code" "LXGW WenKai Mono" 24 24))

(add-to-list 'after-make-frame-functions
             (lambda (new-frame)
               (select-frame new-frame)
               (when (display-graphic-p)
                 (set-font "LXGW WenKai Mono" "LXGW WenKai Mono" 26 26 'medium 'regular))))

;; 设置org字体
(custom-set-faces
 '(org-table ((t (:font "LXGW WenKai Mono:pixelsize=26"))))
 '(org-date ((t (:font "LXGW WenKai Mono:pixelsize=26")))))

;; 设置图标字体: FiraCode Nerd Font, Symbols Nerd Font Mono
(setq nerd-icons-font-family "Symbols Nerd Font Mono")
;; (setq nerd-icons-scale-factor 1.0)

;; 设置Uniclode字体
(set-fontset-font "fontset-default" 'unicode "Symbols Nerd Font Mono")

(provide 'init-fonts)

;;; init-fonts.el ends here
