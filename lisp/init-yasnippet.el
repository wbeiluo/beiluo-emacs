;;; init-yasnippet.el --- Yasnippet configurations -*- lexical-binding: t -*-

;; Copyright (C) 2020 王北洛

;; Author: 王北洛 <wbeiluo@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;; yasnippet configurations
;;

;;; Code:

(use-package yasnippet
  :ensure t
  :hook (prog-mode . yas-global-mode)
  :config
  (use-package yasnippet-snippets :ensure t)

  ;; Disable yasnippet mode on some mode.
  (dolist (hook (list
                 'term-mode-hook
                 ))
    (add-hook hook '(lambda () (yas-minor-mode -1)))))


(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
