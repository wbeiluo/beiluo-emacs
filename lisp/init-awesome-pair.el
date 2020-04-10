;;; init-awesome-pair.el --- Configuration for pair -*- lexical-binding: t -*-

;; Copyright (C) 2020 王北洛

;; Author: 王北洛 <wbeiluo@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:

;;; Code:

(use-package awesome-pair
  :load-path "extensions/awesome-pair/"
  :hook ((c-mode-common . awesome-pair-mode)
         (c-mode . awesome-pair-mode)
         (c++-mode . awesome-pair-mode)
         (emacs-lisp-mode . awesome-pair-mode)
         (lisp-mode . awesome-pair-mode)
         (ielm-mode . awesome-pair-mode)
         (makefile-gmake-mode . awesome-pair-mode)
         (qml-mode . awesome-pair-mode)
         (qmake-mode . awesome-pair-mode)
         (web-mode . awesome-pair-mode)
         (markdown-mode . awesome-pair-mode)
         )
  :bind (("(" . awesome-pair-open-round)
         ("[" . awesome-pair-open-bracket)
         ("{" . awesome-pair-open-curly)
         (")" . awesome-pair-close-round)
         ("]" . awesome-pair-close-bracket)
         ("}" . awesome-pair-close-curly)
         ("%" . awesome-pair-match-paren)
         ("\"" . awesome-pair-double-quote)
         ;("M-o" . awesome-pair-backward-delete)
         ;("C-k" . awesome-pair-kill)
         ("M-\"" . awesome-pair-wrap-double-quote)
         ("M-[" . awesome-pair-wrap-bracket)
         ("M-{" . awesome-pair-wrap-curly)
         ("M-(" . awesome-pair-wrap-round)
         ("M-)" . awesome-pair-unwrap)
         ("M-p" . awesome-pair-jump-right)
         ("M-n" . awesome-pair-jump-left)))

(provide 'init-awesome-pair)

;;; init-awesome-pair.el ends here
