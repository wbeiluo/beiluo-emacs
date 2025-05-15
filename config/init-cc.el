;; init-cc.el --- c/c++-mode configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2022~2025 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;; c/c++ mode configurations.
;;

;;; Code:

(require 'c-eldoc)
(require 'modern-cpp-font-lock)

;; 设置缩进为4
(setq-default c-basic-offset 4)
(setq-default indent-tabs-mode nil)

;; 设置编码格式为stroustrup
(add-hook 'c-mode-common-hook #'(lambda () (c-set-style "stroustrup")))

;; qt keywords and stuff ...
;; set up indenting correctly for new qt kewords
(setq c-protection-key (concat "\\<\\(public\\|public slot\\|protected"
                               "\\|protected slot\\|private\\|private slot"
                               "\\)\\>")
      c-C++-access-key (concat "\\<\\(signals\\|public\\|protected\\|private"
                               "\\|public slots\\|protected slots\\|private slots"
                               "\\)\\>[ \t]*:"))

;; modify the colour of slots to match public, private, etc ...
(font-lock-add-keywords 'c++-mode
                        '(("\\<\\(slots\\|signals\\)\\>" . font-lock-type-face)))
;; make new font for rest of qt keywords
(make-face 'qt-keywords-face)
(set-face-foreground 'qt-keywords-face "DeepSkyBlue1")
;; qt keywords
(font-lock-add-keywords 'c++-mode
                        '(("\\<Q_OBJECT\\>" . 'qt-keywords-face)))
(font-lock-add-keywords 'c++-mode
                        '(("\\<SIGNAL\\|SLOT\\>" . 'qt-keywords-face)))
(font-lock-add-keywords 'c++-mode
                        '(("\\<Q[A-Z][A-Za-z]\\>" . 'qt-keywords-face)))

;; c-eldoc hook
(add-hook 'c-mode-hook #'c-turn-on-eldoc-mode)
(add-hook 'c++-mode-hook #'c-turn-on-eldoc-mode)

;; modern-cpp-font-lock hook
(add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)

(provide 'init-cc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-cc.el ends here
