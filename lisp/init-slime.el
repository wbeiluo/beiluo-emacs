;;; init-slime.el --- Config Slime -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (setq slime-contribs '(slime-fancy))
  )

(provide 'init-slime)

;;; init-slime.el ends here
