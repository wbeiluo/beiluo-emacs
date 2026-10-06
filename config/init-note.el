;;; init-note.el --- Org Note Configurations -*- lexical-binding: t -*-

;; Copyright (C) 2026~2026 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;; Code:

;; 笔记管理
(use-package denote
  :ensure t
  :hook
  ((text-mode . denote-fontify-links-mode-maybe)
   (dired-mode . denote-dired-mode))
  :bind
  ( :map global-map
    ("C-c n n" . denote)
    ("C-c n d" . denote-dired)
    ("C-c n g" . denote-grep)
    ;; If you intend to use Denote with a variety of file types, it is
    ;; easier to bind the link-related commands to the `global-map', as
    ;; shown here.  Otherwise follow the same pattern for `org-mode-map',
    ;; `markdown-mode-map', and/or `text-mode-map'.
    ("C-c n l" . denote-link)
    ("C-c n L" . denote-add-links)
    ("C-c n b" . denote-backlinks)
    ("C-c n q c" . denote-query-contents-link) ; create link that triggers a grep
    ("C-c n q f" . denote-query-filenames-link) ; create link that triggers a dired
    ;; Note that `denote-rename-file' can work from any context, not just
    ;; Dired bufffers.  That is why we bind it here to the `global-map'.
    ("C-c n r" . denote-rename-file)
    ("C-c n R" . denote-rename-file-using-front-matter)

    ;; Key bindings specifically for Dired.
    :map dired-mode-map
    ("C-c C-d C-i" . denote-dired-link-marked-notes)
    ("C-c C-d C-r" . denote-dired-rename-files)
    ("C-c C-d C-k" . denote-dired-rename-marked-files-with-keywords)
    ("C-c C-d C-R" . denote-dired-rename-marked-files-using-front-matter))

  :config
  ;; Create note using Org capture
  (with-eval-after-load 'org-capture
    (setq denote-org-capture-specifiers "%l\n%i\n%?")
    (add-to-list 'org-capture-templates
		 '("N" "New note (with Denote)" plain
                   (file denote-last-path)
                   #'denote-org-capture
                   :no-save t
                   :immediate-finish nil
                   :kill-buffer t
                   :jump-to-captured t)))

  (setq denote-directory (expand-file-name "~/Org/notes/"))
  (setq denote-save-buffers nil)
  (setq denote-known-keywords '("emacs" "entertainment" "reading" "studying" "project" "misc"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-prompts '(title keywords))
  (setq denote-excluded-directories-regexp nil)
  (setq denote-excluded-keywords-regexp nil)
  (setq denote-rename-confirmations '(rewrite-front-matter modify-file-name))

  ;; Pick dates, where relevant, with Org's advanced interface:
  (setq denote-date-prompt-use-org-read-date t)

  ;; Automatically rename Denote buffers using the `denote-rename-buffer-format'.
  (denote-rename-buffer-mode 1))

;; 笔记搜索
(use-package consult-notes
  :ensure t
  :commands (consult-notes consult-notes-search-in-all-notes)
  :bind (("C-c n f" . consult-notes)                      ;; 快速查找并打开笔记
         ("C-c n s" . consult-notes-search-in-all-notes)) ;; 全局搜索笔记内容
  :custom
  ;; 设置笔记源 (可以是多个目录)
  (consult-notes-file-dir-sources
   `(("org"     ?o ,(concat org-directory "/"))
     ("notes"   ?n ,(concat org-directory "/notes/"))
     ("work"    ?w ,(concat org-directory "/work/"))
     ("article" ?a ,(concat org-directory "/article/"))
     ("study"   ?s ,(concat org-directory "/study/"))
     ("books"   ?b ,(concat org-directory "/books/"))))

  :config
  ;; Embark support
  (with-eval-after-load 'embark
    (defun consult-notes-open-dired (cand)
      "Open notes directory dired with point on file CAND."
      (interactive "fNote: ")
      ;; dired-jump is in dired-x.el but is moved to dired in Emacs 28
      (dired-jump nil cand))

    (defun consult-notes-grep (cand)
      "Run grep in directory of notes file CAND."
      (interactive "fNote: ")
      (consult-grep (file-name-directory cand)))

    (defvar-keymap consult-notes-map
      :doc "Keymap for Embark notes actions."
      :parent embark-file-map
      "d" #'consult-notes-open-dired
      "g" #'consult-notes-grep)

    (add-to-list 'embark-keymap-alist `(,consult-notes-category . consult-notes-map))

    ;; make embark-export use dired for notes
    (setf (alist-get consult-notes-category embark-exporters-alist) #'embark-export-dired))

  ;; Denote suppory
  (require 'consult-notes-denote)
  (when (locate-library "denote")
    (setq consult-notes-denote-display-id nil) ;; 保持界面简洁
    (consult-notes-denote-mode 1)))

(provide 'init-note)

;;; init-note.el ends here
