;;; init-python.el --- Python lsp configuration. -*- lexical-binding: t -*-

;; Author: 王北洛 <wbeiluo@gmail.com>
;; Version: 0.2
;; Package-Requires: lsp-pyright
;; Homepage: https://github.com/wbeiluo/beiluo-emacs
;; Keywords: python lsp pyright

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

;; Config lsp-mode for python.

;;; Code:

(use-package lsp-pyright
  :ensure t
  :custom
  (python-indent-offset 4)
  (lsp-pyright-langserver-command "pyright")  
  (lsp-pyright-typechecking-mode "basic")
  (lsp-pyright-auto-import-completions t)
  (lsp-pyright-diagnostic-mode "workspace")
  
  ;; 启动由 init-lsp.el 的 python-mode 钩子（lsp-deferred）统一负责，
  ;; 这里只需提前加载 python 运行时与 pyright 客户端，供 lsp-mode 选择
  :hook (python-mode . (lambda ()
                         (require 'python)
                         (require 'lsp-pyright))))

(provide 'init-python)
;;; init-python.el ends here
