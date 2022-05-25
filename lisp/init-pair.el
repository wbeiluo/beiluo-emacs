;;; init-pair.el --- Default configurations -*- lexical-binding: t -*-

;; Copyright (C) 2022 王北洛

;; Author: 王北洛 <wbeiluo@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;; Code:

;;; Install

(use-package smartparens
  :ensure t
  :custom (smartparens-global-mode t)
  :config (progn
            (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
	    (sp-local-pair 'lisp-interaction-mode "'" nil :actions nil)))


;;; keybinding management

;; (define-key smartparens-mode-map (kbd "C-M-f") 'sp-forward-sexp)
;; (define-key smartparens-mode-map (kbd "C-M-b") 'sp-backward-sexp)

;; (define-key smartparens-mode-map (kbd "C-M-d") 'sp-down-sexp)
;; (define-key smartparens-mode-map (kbd "C-M-a") 'sp-backward-down-sexp)
;; (define-key smartparens-mode-map (kbd "C-S-d") 'sp-beginning-of-sexp)
;; (define-key smartparens-mode-map (kbd "C-S-a") 'sp-end-of-sexp)

;; (define-key smartparens-mode-map (kbd "C-M-e") 'sp-up-sexp)
;; (define-key smartparens-mode-map (kbd "C-M-u") 'sp-backward-up-sexp)
;; (define-key smartparens-mode-map (kbd "C-M-t") 'sp-transpose-sexp)

;; (define-key smartparens-mode-map (kbd "C-M-n") 'sp-forward-hybrid-sexp)
;; (define-key smartparens-mode-map (kbd "C-M-p") 'sp-backward-hybrid-sexp)

;; (define-key smartparens-mode-map (kbd "C-M-k") 'sp-kill-sexp)
;; (define-key smartparens-mode-map (kbd "C-M-w") 'sp-copy-sexp)

;; (define-key smartparens-mode-map (kbd "M-<delete>") 'sp-unwrap-sexp)
;; (define-key smartparens-mode-map (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)

;; (define-key smartparens-mode-map (kbd "C-<right>") 'sp-forward-slurp-sexp)
;; (define-key smartparens-mode-map (kbd "C-<left>") 'sp-forward-barf-sexp)
;; (define-key smartparens-mode-map (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
;; (define-key smartparens-mode-map (kbd "C-M-<right>") 'sp-backward-barf-sexp)

;; (define-key smartparens-mode-map (kbd "M-D") 'sp-splice-sexp)
;; (define-key smartparens-mode-map (kbd "C-M-<delete>") 'sp-splice-sexp-killing-forward)
;; (define-key smartparens-mode-map (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
;; (define-key smartparens-mode-map (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around)

;; (define-key smartparens-mode-map (kbd "C-]") 'sp-select-next-thing-exchange)
;; (define-key smartparens-mode-map (kbd "C-<left_bracket>") 'sp-select-previous-thing)
;; (define-key smartparens-mode-map (kbd "C-M-]") 'sp-select-next-thing)

;; (define-key smartparens-mode-map (kbd "M-F") 'sp-forward-symbol)
;; (define-key smartparens-mode-map (kbd "M-B") 'sp-backward-symbol)

;; (define-key smartparens-mode-map (kbd "C-\"") 'sp-change-inner)
;; (define-key smartparens-mode-map (kbd "M-i") 'sp-change-enclosing)

;; (bind-key "C-c f" (lambda () (interactive) (sp-beginning-of-sexp 2)) smartparens-mode-map)
;; (bind-key "C-c b" (lambda () (interactive) (sp-beginning-of-sexp -2)) smartparens-mode-map)

;; (bind-key "C-M-s"
;;           (defhydra smartparens-hydra ()
;;             "Smartparens"
;;             ("d" sp-down-sexp "Down")
;;             ("e" sp-up-sexp "Up")
;;             ("u" sp-backward-up-sexp "Up")
;;             ("a" sp-backward-down-sexp "Down")
;;             ("f" sp-forward-sexp "Forward")
;;             ("b" sp-backward-sexp "Backward")
;;             ("k" sp-kill-sexp "Kill" :color blue)
;;             ("q" nil "Quit" :color blue))
;;           smartparens-mode-map)

;; (bind-key "H-t" 'sp-prefix-tag-object smartparens-mode-map)
;; (bind-key "H-p" 'sp-prefix-pair-object smartparens-mode-map)
;; (bind-key "H-y" 'sp-prefix-symbol-object smartparens-mode-map)
;; (bind-key "H-h" 'sp-highlight-current-sexp smartparens-mode-map)
;; (bind-key "H-e" 'sp-prefix-save-excursion smartparens-mode-map)
;; (bind-key "H-s c" 'sp-convolute-sexp smartparens-mode-map)
;; (bind-key "H-s a" 'sp-absorb-sexp smartparens-mode-map)
;; (bind-key "H-s e" 'sp-emit-sexp smartparens-mode-map)
;; (bind-key "H-s p" 'sp-add-to-previous-sexp smartparens-mode-map)
;; (bind-key "H-s n" 'sp-add-to-next-sexp smartparens-mode-map)
;; (bind-key "H-s j" 'sp-join-sexp smartparens-mode-map)
;; (bind-key "H-s s" 'sp-split-sexp smartparens-mode-map)
;; (bind-key "H-s r" 'sp-rewrap-sexp smartparens-mode-map)
;; (defvar hyp-s-x-map)
;; (define-prefix-command 'hyp-s-x-map)
;; (bind-key "H-s x" hyp-s-x-map smartparens-mode-map)
;; (bind-key "H-s x x" 'sp-extract-before-sexp smartparens-mode-map)
;; (bind-key "H-s x a" 'sp-extract-after-sexp smartparens-mode-map)
;; (bind-key "H-s x s" 'sp-swap-enclosing-sexp smartparens-mode-map)

;; (bind-key "C-x C-t" 'sp-transpose-hybrid-sexp smartparens-mode-map)

;; (bind-key ";" 'sp-comment emacs-lisp-mode-map)

;; (bind-key [remap c-electric-backspace] 'sp-backward-delete-char smartparens-strict-mode-map)


;;; pair management

(sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
(bind-key "C-(" 'sp---wrap-with-40 minibuffer-local-map)

(sp-with-modes 'org-mode
  (sp-local-pair "=" "=" :wrap "C-="))

(sp-with-modes 'textile-mode
  (sp-local-pair "*" "*")
  (sp-local-pair "_" "_")
  (sp-local-pair "@" "@"))

(sp-with-modes 'web-mode
  (sp-local-pair "{{#if" "{{/if")
  (sp-local-pair "{{#unless" "{{/unless"))

;;; tex-mode latex-mode
(sp-with-modes '(tex-mode plain-tex-mode latex-mode)
  (sp-local-tag "i" "\"<" "\">"))

;;; lisp modes
(sp-with-modes sp--lisp-modes
  (sp-local-pair "(" nil
                 :wrap "C-("
                 :pre-handlers '(my-add-space-before-sexp-insertion)
                 :post-handlers '(my-add-space-after-sexp-insertion)))

(defun my-add-space-after-sexp-insertion (id action _context)
  (when (eq action 'insert)
    (save-excursion
      (forward-char (sp-get-pair id :cl-l))
      (when (or (eq (char-syntax (following-char)) ?w)
                (looking-at (sp--get-opening-regexp)))
        (insert " ")))))

(defun my-add-space-before-sexp-insertion (id action _context)
  (when (eq action 'insert)
    (save-excursion
      (backward-char (length id))
      (when (or (eq (char-syntax (preceding-char)) ?w)
                (and (looking-back (sp--get-closing-regexp))
                     (not (eq (char-syntax (preceding-char)) ?'))))
        (insert " ")))))

;;; C++
(sp-with-modes '(malabar-mode c++-mode java-mode)
  (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET"))))
(sp-local-pair 'c++-mode "/*" "*/" :post-handlers '((" | " "SPC")
                                                    ("* ||\n[i]" "RET")))

(sp-with-modes '(js2-mode typescript-mode java-mode)
  (sp-local-pair "/**" "*/" :post-handlers '(("| " "SPC")
                                             ("* ||\n[i]" "RET"))))
(provide 'init-pair)

;;; init-pair.el ends here
