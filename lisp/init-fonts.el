;;; init-fonts.el --- Set Fonts -*- lexical-binding: t -*-

;; Copyright (C) 2020 王北洛

;; Author: 王北洛 <wbeiluo@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;; Code:

(let ((emacs-font-size 10)
      (emacs-font-name "WenQuanYi Micro Hei Mono"))
  (when (display-grayscale-p)
  (set-frame-font (format "%s-%s" (eval emacs-font-name) (eval emacs-font-size)))
  (set-fontset-font (frame-parameter nil 'font) 'unicode (eval emacs-font-name))))

(provide 'init-fonts)

;;; init-fonts.el ends here
