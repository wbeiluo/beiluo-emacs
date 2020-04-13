;;; init-fonts.el --- Set Fonts -*- lexical-binding: t -*-

;; Copyright (C) 2020 王北洛

;; Author: 王北洛 <wbeiluo@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;; Code:

(let ((emacs-font-size 14)
      emacs-font-name)
  (cond
   ((featurep 'cocoa)
    (setq emacs-font-name "Monaco"))
   ((string-equal system-type "gnu/linux")
    (setq emacs-font-name "WenQuanYi Micro Hei Mono"))
   ((string-equal system-type "windows-nt")
    (setq emacs-font-name "Consolas")))
  (when (display-grayscale-p)
    (set-frame-font (format "%s-%s" (eval emacs-font-name) (eval emacs-font-size)))
    (set-fontset-font (frame-parameter nil 'font) 'unicode (eval emacs-font-name))))

(when sys/win32p
    ;;Chinese Font
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
                        charset (font-spec :family "Microsoft YaHei UI"
                                        :size 18))))



(provide 'init-fonts)

;;; init-fonts.el ends here
