;;; init-fonts.el --- Set Fonts -*- lexical-binding: t -*-

;; Copyright (C) 2020~2023 王北洛

;; Author: 王北洛 <wbeiluo@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;; Code:

;; (let ((emacs-font-size 14)
;;       (emacs-font-name "Sarasa Mono SC"))
;;   (when (display-grayscale-p)
;;   (set-frame-font (format "%s-%s" (eval emacs-font-name) (eval emacs-font-size)))
;;   (set-fontset-font (frame-parameter nil 'font) 'unicode (eval emacs-font-name))))

(defun set-font (english chinese english-size chinese-size)
  "Set english, chinese font and size"
  (set-face-attribute 'default nil :font

                      (format "%s:pixelsize=%d"  english english-size))
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
                      (font-spec :family chinese :size chinese-size))))

(add-to-list 'after-make-frame-functions
             (lambda (new-frame)
               (select-frame new-frame)
               (when (display-graphic-p)
                   (set-font "Sarasa Mono SC" "Sarasa Mono SC" 18 18))))

;; (when (display-graphic-p)
;;   (set-font "jetbrains mono" "WenQuanYi Micro Hei Mono" 18 17))

(when (display-graphic-p)
  (set-font "Sarasa Mono SC" "Sarasa Mono SC" 18 18))

(provide 'init-fonts)

;;; init-fonts.el ends here
