;; init-goto.el --- Goto Anywhere Configurations. -*- lexical-binding: t -*-

;; Copyright (C) 2025~2025 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;; 光标移动及跳转操作
;;

;;; Code:

(defconst extensions-mwim-dir
  (expand-file-name "extensions/mwim" user-emacs-directory))
(defconst extensions-goto-chg-dir
  (expand-file-name "extensions/goto-chg" user-emacs-directory))
(defconst extensions-avy-dir
  (expand-file-name "extensions/avy" user-emacs-directory))

(use-package mwim
  :ensure nil
  :load-path extensions-mwim-dir
  :bind (("C-a" . mwim-beginning-of-code-or-line)
         ("C-e" . mwim-end-of-code-or-line)))

(use-package goto-chg
  :ensure nil
  :load-path extensions-goto-chg-dir
  :bind (("C-," . goto-last-change)
	 ("C-，" . goto-last-change)
         ("C-M-," . goto-last-change-reverse)
	 ("C-M-，" . goto-last-change-reverse)))

;; Jump to things in Emacs tree-style
(use-package avy
  :ensure nil
  :load-path extensions-avy-dir
  :bind
  (("M-j" . avy-goto-char-timer) ;; 跳到单词 (字母)
   ("M-g l" . avy-goto-line)     ;; 跳到行
   ("M-g w" . avy-goto-word-1))  ;; 在所有窗口中跳转
  :custom
  ;; 让标签出现在字符上方，不替换字符
  (avy-style 'at-full)
  ;; 设置超时时间（0.5秒内输入的字符都会被当作搜索词）
  (avy-timeout-secs 0.5)
  ;; 优先级：优先使用左手容易按到的键作为标签
  (avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  
  (avy-handler-function 'avy-handler-default)
  (avy-all-windows t)
  :config
  (defun avy-action-copy-and-stay (pt)
    "拷贝目标处的单词，但光标保持在原处。"
    (save-excursion
      (goto-char pt)
      (copy-region-as-kill (point) (progn (forward-word) (point))))
    (select-window (posn-window (event-start last-input-event)))
    t)

  (defun avy-action-kill-whole-line (pt)
    "直接删除目标所在的整行。"
    (save-excursion
      (goto-char pt)
      (kill-whole-line))
    (select-window (posn-window (event-start last-input-event)))
    t)

  (defun avy-action-embark (pt)
    "对目标点调用 Embark 菜单。"
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window (posn-window (event-start last-input-event))))
    t)

  ;; 将按键映射到动作
  (setf (alist-get ?w avy-dispatch-alist) 'avy-action-copy-and-stay)
  (setf (alist-get ?K avy-dispatch-alist) 'avy-action-kill-whole-line)
  (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark))

(provide 'init-goto)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-goto.el ends here
