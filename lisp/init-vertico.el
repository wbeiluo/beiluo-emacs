;;; init-vertico.el --- Default configurations -*- lexical-binding: t -*-

;; Copyright (C) 2022 王北洛

;; Author: 王北洛 <wbeiluo@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;; Code:

;; Enable vertico
(use-package vertico
  :init

  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 10)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Use `consult-completion-in-region' if Vertico is enabled.
  ;; Otherwise use the default `completion--in-region' function.
  (setq completion-in-region-function
	(lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
		 args)))

  ;; Grow and shrink the Vertico minibuffer
  (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)
  )

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  ;; Tune the global completion style settings to your liking!
  ;; This affects the minibuffer and non-lsp completion at point.
  (setq completion-styles '(orderless partial-completion basic)
        completion-category-defaults nil
        completion-category-overrides nil))

;; (use-package orderless
;;   :demand t
;;   :config
;;   (defvar +orderless-dispatch-alist
;;     '((?% . char-fold-to-regexp)
;;       (?! . orderless-without-literal)
;;       (?`. orderless-initialism)
;;       (?= . orderless-literal)
;;       (?~ . orderless-flex)))

;;   ;; Recognizes the following patterns:
;;   ;; * ~flex flex~
;;   ;; * =literal literal=
;;   ;; * %char-fold char-fold%
;;   ;; * `initialism initialism`
;;   ;; * !without-literal without-literal!
;;   ;; * .ext (file extension)
;;   ;; * regexp$ (regexp matching at end)
;;   (defun +orderless-dispatch (pattern index _total)
;;     (cond
;;      ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
;;      ((string-suffix-p "$" pattern)
;;       `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x200000-\x300000]*$")))
;;      ;; File extensions
;;      ((and
;;        ;; Completing filename or eshell
;;        (or minibuffer-completing-file-name
;;            (derived-mode-p 'eshell-mode))
;;        ;; File extension
;;        (string-match-p "\\`\\.." pattern))
;;       `(orderless-regexp . ,(concat "\\." (substring pattern 1) "[\x200000-\x300000]*$")))
;;      ;; Ignore single !
;;      ((string= "!" pattern) `(orderless-literal . ""))
;;      ;; Prefix and suffix
;;      ((if-let (x (assq (aref pattern 0) +orderless-dispatch-alist))
;;           (cons (cdr x) (substring pattern 1))
;;         (when-let (x (assq (aref pattern (1- (length pattern))) +orderless-dispatch-alist))
;;           (cons (cdr x) (substring pattern 0 -1)))))))

;;   ;; Define orderless style with initialism by default
;;   (orderless-define-completion-style +orderless-with-initialism
;;     (orderless-matching-styles '(orderless-initialism orderless-literal orderless-regexp)))

;;   ;; You may want to combine the `orderless` style with `substring` and/or `basic`.
;;   ;; There are many details to consider, but the following configurations all work well.
;;   ;; Personally I (@minad) use option 3 currently. Also note that you may want to configure
;;   ;; special styles for special completion categories, e.g., partial-completion for files.
;;   ;;
;;   ;; 1. (setq completion-styles '(orderless))
;;   ;; This configuration results in a very coherent completion experience,
;;   ;; since orderless is used always and exclusively. But it may not work
;;   ;; in all scenarios. Prefix expansion with TAB is not possible.
;;   ;;
;;   ;; 2. (setq completion-styles '(substring orderless))
;;   ;; By trying substring before orderless, TAB expansion is possible.
;;   ;; The downside is that you can observe the switch from substring to orderless
;;   ;; during completion, less coherent.
;;   ;;
;;   ;; 3. (setq completion-styles '(orderless basic))
;;   ;; Certain dynamic completion tables (completion-table-dynamic)
;;   ;; do not work properly with orderless. One can add basic as a fallback.
;;   ;; Basic will only be used when orderless fails, which happens only for
;;   ;; these special tables.
;;   ;;
;;   ;; 4. (setq completion-styles '(substring orderless basic))
;;   ;; Combine substring, orderless and basic.
;;   ;;
;;   (setq completion-styles '(orderless basic)
;;         completion-category-defaults nil
;;         ;;; Enable partial-completion for files.
;;         ;;; Either give orderless precedence or partial-completion.
;;         ;;; Note that completion-category-overrides is not really an override,
;;         ;;; but rather prepended to the default completion-styles.
;;         ;; completion-category-overrides '((file (styles orderless partial-completion))) ;; orderless is tried first
;;         completion-category-overrides '((file (styles partial-completion)) ;; partial-completion is tried first
;;                                         ;; enable initialism by default for symbols
;;                                         (command (styles +orderless-with-initialism))
;;                                         (variable (styles +orderless-with-initialism))
;;                                         (symbol (styles +orderless-with-initialism)))
;;         orderless-component-separator #'orderless-escapable-split-on-space ;; allow escaping space with backslash!
;;         orderless-style-dispatchers '(+orderless-dispatch)))

;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; Enable richer annotations using the Marginalia package
(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

;; Add icons to completion candidates
;; Note: 有对齐问题,暂时不使用
;; (use-package all-the-icons-completion
;;   :ensure t
;;   :init
;;   (all-the-icons-completion-mode)
;;   (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup))


;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
	 
         ;; Other custom bindings
	 ("C-s" . consult-line)                    ;; orig. I-search
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
	 ("M-b" . consult-buffer)                  ;; orig. switch-to-buffer
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
	 ("C-c r" . consult-recent-file)           ;; orig. recentf-open-files

         ;; M-g bindings (goto-map)
	 ("M-g" . hydra-goto/body)

         ;; M-s bindings (search-map)
         ("M-s" . hydra-search/body)

         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ;;("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ;;("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ;;("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ;;("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key (kbd "M-.")
   )

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))

  ;; goto map
  (defhydra hydra-goto (:exit t :hint nil)
  "
  _e_: compile error  _f_: flycheck  _g_: goto-line    _o_: outline
  _k_: global mark    _m_: mark      _I_: imenu-multi  _i_: imenu     "

  ("e"   consult-compile-error)
  ("f"   consult-flycheck)
  ("g"   consult-goto-line)
  ("o"   consult-outline)
  ("m"   consult-mark)
  ("k"   consult-global-mark)
  ("i"   consult-imenu)
  ("I"   consult-imenu-multi)
  ("q"   nil "quit" :color "deep sky blue"))

  ;; search map
  (defhydra hydra-search (:exit t :hint nil)
  "
  _d_: find    _g_: grep  _G_: git grep    _r_: ripgrep     _m_: multi-occur
  _D_: locate  _l_: line  _L_: line multi  _k_: keep-lines  _u_: focus lines   "

  ("d"  consult-find)
  ("D"  consult-locate)
  ("g"  consult-grep)
  ("G"  consult-git-grep)
  ("r"  consult-ripgrep)
  ("l"  consult-line)
  ("L"  consult-line-multi)
  ("m"  consult-multi-occur)
  ("k"  consult-keep-lines)
  ("u"  consult-focus-lines)
  ("e"  consult-isearch-history)
  ("q"   nil "quit" :color "deep sky blue"))
  )

(use-package consult-dir
  :bind (([remap list-directory] . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

;; Consult extensions

;; LSP
(use-package consult-lsp
  :ensure t)

;; Yasnippet
(use-package consult-yasnippet
  :ensure t)

;; Flycheck
(use-package consult-flycheck
  :ensure t)

(use-package consult-project-extra
  :ensure t)

(use-package bookmark-view
  :ensure t)

(use-package consult-org-roam
   :ensure t
   :init
   (require 'consult-org-roam)
   ;; Activate the minor-mode
   (consult-org-roam-mode 1)
   :custom
   (consult-org-roam-grep-func #'consult-ripgrep)
   :config
   ;; Eventually suppress previewing for certain functions
   (consult-customize
    consult-org-roam-forward-links
    :preview-key (kbd "M-."))
   :bind
   ("C-c n e" . consult-org-roam-file-find)
   ("C-c n b" . consult-org-roam-backlinks)
   ("C-c n r" . consult-org-roam-search))


;; Minibuffer actions and context menu
;; (use-package embark
;;   :ensure t

;;   :bind
;;   (("C-;" . embark-act)         ;; pick some comfortable binding
;;    ("C-." . embark-dwim)        ;; good alternative: M-.
;;    ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

;;   :init

;;   ;; Optionally replace the key help with a completing-read interface
;;   (setq prefix-help-command #'embark-prefix-help-command)

;;   :config

;;   ;; Hide the mode line of the Embark live/completions buffers
;;   (add-to-list 'display-buffer-alist
;;                '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
;;                  nil
;;                  (window-parameters (mode-line-format . none)))))

(use-package embark
  :ensure t
  :bind
  ("C-;" . embark-act)
  ("C-." . embark-dwim)
  ("C-h b" . embark-bindings)
  ("C-h B" . embark-bindings-at-point)
  ("C-h M" . embark-bindings-in-keymap)
  ("C-h E" . embark-on-last-message)
  ("M-n" . embark-next-symbol)
  ;("M-s n" . embark-next-symbol) ; for when M-n is taken
  ("M-p" . embark-previous-symbol)
  ;("M-s p" . embark-previous-symbol) ; for when M-p is taken
  (:map completion-list-mode-map
        (";" . embark-act))
  (:map embark-collect-mode-map
        ("a") ; I don't like my own default :)
        (";" . embark-act)
        ("F" . consult-focus-lines))
  (:map embark-package-map
        ("t" . try))
  (:map embark-identifier-map
        ("(" . insert-parentheses))
  (:map embark-expression-map
        ("(" . insert-parentheses))
  (:map embark-email-map
        ("+" . add-email-to-ecomplete)
        ("\\" . remove-email-from-ecomplete))
  (:map embark-encode-map
        ("p" . topaz-paste-region))
  (:map embark-url-map
        ("x" . browse-url-default-browser)
        ("p" . pocket-lib-add-urls))
  :custom
  (embark-quit-after-action nil)
  (prefix-help-command #'embark-prefix-help-command)
  (embark-indicators '(embark-minimal-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))
  (embark-cycle-key ";")
  (embark-help-key "?")
  (embark-confirm-act-all nil)
  :config
  (setq embark-candidate-collectors
        (cl-substitute 'embark-sorted-minibuffer-candidates
                       'embark-minibuffer-candidates
                       embark-candidate-collectors))
  (dolist (cmd '(comment-dwim
                 insert-parentheses
                 markdown-insert-code
                 markdown-insert-italic
                 markdown-insert-bold
                 org-emphasize
                 cdlatex-math-modify
                 TeX-font))
    (push #'embark--mark-target (alist-get cmd embark-pre-action-hooks)))
  (push #'embark--xref-push-marker
        (alist-get 'find-file embark-pre-action-hooks))
  (defun embark-on-last-message (arg)
    "Act on the last message displayed in the echo area."
    (interactive "P")
    (with-current-buffer "*Messages*"
      (goto-char (1- (point-max)))
      (embark-act arg))))

(use-package consult-dir
  :ensure t
  :bind
  (:map minibuffer-local-filename-completion-map
        ("M-." . consult-dir)
        ("M-j" . consult-dir-jump-file)))

;; Consult users will also want the embark-consult package.
;; (use-package embark-consult
;;   :ensure t
;;   :after (embark consult)
;;   :demand t ; only necessary if you have the hook below
;;   ;; if you want to have consult previews as you move around an
;;   ;; auto-updating embark collect buffer
;;   :hook
;;   (embark-collect-mode . consult-preview-at-point-mode))
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :config
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))

(use-package wgrep
  :ensure t
  :commands wgrep-change-to-wgrep-mode
  :config (setq wgrep-auto-save-buffer t))


(provide 'init-vertico)

;;; init-vertico.el ends here
