;;; init-program.el --- Program config -*- lexical-binding: t -*-

;; Copyright (C) 2021~2023 王北洛

;; Author: 王北洛 <beiluo.wang@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;; Code:

(use-package magit
  :ensure t
  :mode (("\\COMMIT_EDITMSG\\'" . text-mode)
         ("\\MERGE_MSG\\'" . text-mode))
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-c M-g" . magit-file-popup))

  :config
  (when (fboundp 'transient-append-suffix)
    ;; Add switch: --tags
    (transient-append-suffix 'magit-fetch
      "-p" '("-t" "Fetch all tags" ("-t" "--tags")))))

(use-package diff-hl
  :ensure t
  :hook ((dired-mode         . diff-hl-dired-mode-unless-remote)
         (magit-pre-refresh  . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :init
  (global-diff-hl-mode t)
  :config
  ;; When Emacs runs in terminal, show the indicators in margin instead.
  (unless (display-graphic-p)
    (diff-hl-margin-mode)))

;; (use-package magit-delta
;;   :ensure t
;;   :hook (magit-mode . magit-delta-mode)
;;   :config
;;   (setq magit-delta-hide-plus-minus-markers nil)
;;   )

;; Flexible text folding
(use-package origami
  :bind (("C-<tab> <tab>" . origami-toggle-node)
         ("C-<tab> C-<tab>" . origami-toggle-all-nodes)
         ("C-<tab> o" . origami-open-node)
         ("C-<tab> c" . origami-close-node)
         ("C-<tab> n" . origami-next-fold)
         ("C-<tab> p" . origami-previous-fold))
  :hook (prog-mode . origami-mode)
  :config
  (face-spec-reset-face 'origami-fold-header-face))

;; 显示缩进线
(use-package highlight-indent-guides
  ;;:ensure t
  :hook (prog-mode . highlight-indent-guides-mode)
  :init
  (setq highlight-indent-guides-method 'character))

(use-package highlight-parentheses
  :config
  (global-highlight-parentheses-mode 1))

;; Rainbow Mode
(use-package rainbow-mode
  :diminish
  :hook (emacs-lisp-mode . rainbow-mode))

;; Rainbow delimiters
(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(provide 'init-program)

;;; init-program.el ends here
