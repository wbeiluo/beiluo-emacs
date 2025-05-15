;;; init-fonts.el --- Set Fonts -*- lexical-binding: t -*-

;; Copyright (C) 2020~2025 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;; 中英文及图标字体设置
;;
;;; Code:

(require 'nerd-icons)

(defun set-font (english chinese english-size chinese-size)
  "Set ENGLISH, CHINESE font and ENGLISH-SIZE, CHINESE-SIZE."
  (set-face-attribute 'default nil :font
                      (format "%s:pixelsize=%d" english english-size))
  (set-face-attribute 'variable-pitch nil :font
                      (format "%s:pixelsize=%d" english english-size))
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
                      (font-spec :family chinese :size chinese-size))))

;; Chinese fonts: Sarasa Mono SC, WenQuanYi Micro Hei Mono, Hack Nerd Font Mono, LXGW WenKai Mono
;; English fonts: jetbrains mono, Sarasa Mono SC, Maple Mono

;; (when (display-graphic-p)
;;   (set-font "LXGW WenKai Mono" "LXGW WenKai Mono" 26 26))

(when (display-graphic-p)
  (set-font "Maple Mono" "LXGW WenKai Mono" 24 26))

;; (when (display-graphic-p)
;;   (set-font "Sarasa Mono SC" "Sarasa Mono SC" 24 24))

(add-to-list 'after-make-frame-functions
             (lambda (new-frame)
               (select-frame new-frame)
               (when (display-graphic-p)
                 (set-font "Maple Mono" "LXGW WenKai Mono" 24 26))))

;; 设置图标字体: FiraCode Nerd Font, Symbols Nerd Font Mono                                      ;
(setq nerd-icons-font-family "Symbols Nerd Font Mono")
;; (setq nerd-icons-scale-factor 1.0)

;; 设置Uniclode字体
(set-fontset-font "fontset-default" 'unicode "Symbols Nerd Font Mono")

(provide 'init-fonts)

;;; init-fonts.el ends here
