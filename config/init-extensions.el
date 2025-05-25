;;; init-extensions.el --- Init extension packages configuration. -*- lexical-binding: t -*-

;; Author: 王北洛  <wbeiluo@gmail.com>
;; Version: 0.1
;; Package-Requires: loaddefs
;; Homepage: https://github.com/wbeiluo/beiluo-emacs
;; Keywords: nil

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Add extension package to load path and generate autoload file.

;;; Code:

(eval-and-compile
  (require 'cl-lib)
  (require 'compile)
  (require 'loaddefs))

(defvar extension-path nil
  "List of extension packages laod path.")

(defun generate-extensions-compile-file ()
  "Compile extensions package files."
  (interactive)
  (mapc
   #'(lambda (x)
       (dolist (file (directory-files x))
         (when (and (string-suffix-p ".el" file t)
                    (not (string-prefix-p "." file)))
           (let ((el-file (expand-file-name file x)))
             (byte-compile-file el-file)))))
   extension-path))

(defun delete-extensions-compile-file ()
  "Delete the extensions compile files."
  (interactive)
  (mapc
   #'(lambda (x)
       (dolist (file (directory-files x))
         (when (string-suffix-p ".elc" file t)
           (let ((elc-file (expand-file-name file x)))
             (progn
               (message "delete file: %S" elc-file)
               (delete-file elc-file))))))
   extension-path))

;; Add all extension package path
(add-to-list 'extension-path (expand-file-name "extensions/dash" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/s" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/f" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/ht" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/hydra" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/gcmh" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/nerd-icons" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/nerd-icons/data" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/transwin" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/solarized-emacs" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/modus-themes" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/shrink-path" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/doom-modeline" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/consult" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/vertico" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/vertico/extensions" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/orderless" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/pinyinlib" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/marginalia" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/nerd-icons-completion" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/embark" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/avy" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/mwim" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/goto-chg" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/goto-last-point" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/ace-window" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/pcmpl-args" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/exec-path-from-shell" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/esh-help" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/magit/lisp" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/transient/lisp" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/with-editor/lisp" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/llama" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/diff-hl" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/pfuture" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/cfrs" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/treemacs/src/elisp" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/treemacs/src/extra" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/treemacs-nerd-icons" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/org-modern" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/org-appear" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/denote" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/consult-notes" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/org-super-links" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/markdown-mode" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/corfu" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/corfu/extensions" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/cape" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/nerd-icons-corfu" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/flycheck" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/flycheck/maint" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/lsp-mode" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/lsp-mode/clients" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/lsp-pyright" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/lsp-ui" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/spinner" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/dape" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/vundo" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/super-save" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/emacs-dashboard" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/symbol-overlay" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/highlight-indent-guides" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/rainbow-delimiters" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/colorful-mode" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/drag-stuff" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/multiple-cursors" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/smartparens" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/origami" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/c-eldoc" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/modern-cpp-font-lock" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/tempel" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/tempel-collection" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/posframe" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/sdcv" user-emacs-directory) t)
(add-to-list 'extension-path (expand-file-name "extensions/emms" user-emacs-directory) t)

;; Add extension-path to load-path
(mapc (lambda (x) (add-to-list 'load-path x)) extension-path)

;; Compile extensions package and generate autoload
(let ((ald-file (concat user-emacs-directory "extensions/pkg-autoload.el")))
  (progn
    (unless (file-exists-p ald-file)
      (progn
        (generate-extensions-compile-file)
        (loaddefs-generate extension-path ald-file)))
    (load ald-file 'noerror 'nomessage)))

(provide 'init-extensions)
;;; init-extensions.el ends here
