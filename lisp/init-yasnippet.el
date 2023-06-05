;;; init-yasnippet.el --- Yasnippet configurations -*- lexical-binding: t -*-

;; Copyright (C) 2020~2023 王北洛

;; Author: 王北洛 <wbeiluo@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;; yasnippet configurations
;;

;;; Code:


(use-package yasnippet-snippets
  :ensure t)

(use-package yasnippet
  :ensure t
  :hook (prog-mode . yas-global-mode))

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
