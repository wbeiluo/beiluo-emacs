;;; init-python.el --- Python lsp configuration. -*- lexical-binding: t -*-

;; Author: 王北洛 <wbeiluo@gmail.com>
;; Version: 0.1
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

;; config lsp-mode for python.

;;; Require
(require 'python)

;;; Code:

;;设置缩进为4
(setq python-indent-offset 4)

(setq lsp-pyright-langserver-command "pyright")

(add-hook 'python-mode-hook (lambda ()
                              (require 'lsp-pyright)
                              (lsp)))

(provide 'init-python)
;;; init-python.el ends here
