;; init-c.el --- c-mode configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2020 王北洛

;; Author: 王北洛 <wbeiluo@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;; c/c++ mode configurations.
;;

;;; Code:

(dolist (hook (list
               'c-mode-hook
               'c-mode-common-hook
               ))
  (add-hook
   hook
   '(lambda ()
      (use-package cc-mode)
      (use-package c-eldoc)

      (defun c-mode-style-setup ()
        (interactive)
        "Set up c-mode and related modes."
        ;; eldoc.
        (c-turn-on-eldoc-mode)

        ;; base-style
        (setq c-default-style "k&r")
        (c-set-style "k&r")
        (setq c-basic-offset 4)
        (imenu-add-menubar-index))
      (c-mode-style-setup))))


;; 文件注释
(defun comment/header-c ()
  "Add header and footer to an .c buffer."
  (interactive)
  (let ((fname (if (buffer-file-name)
                   (file-name-nondirectory (buffer-file-name))
                 (error "This buffer is not visiting a file")))
        (fdate (current-time-string)))
    (save-excursion
      (goto-char (point-min))
      (insert
       "/**\n"
       " ******************************************************************************\n"
       " * @file    " fname "\n"
       " * @author  王北洛 <beiluo.wang@139.com>\n"
       " * @date    " fdate "\n"
       " * @brief   简介...\n"
       " *\n  *\n  *\n"
       " ******************************************************************************\n"
       " *                         Copyright (C) 2020 王北洛\n"
       " ******************************************************************************\n"
       " */\n\n"
       "/* Includes ------------------------------------------------------------------*/\n"
       "#include \"" (substring fname 0 -1) "h\"\n\n"
       "/* Private define ------------------------------------------------------------*/\n\n"
       "/* Private typedef -----------------------------------------------------------*/\n\n"
       "/* Private variables ---------------------------------------------------------*/\n\n"
       "/* Private function prototypes -----------------------------------------------*/\n\n"
       "/* Private functions ---------------------------------------------------------*/\n\n"
       "/* Public functions ----------------------------------------------------------*/\n\n")
      (goto-char (point-max))
      (insert "/***********************************END OF FILE********************************/\n\n")
      (insert ""))))


(defun comment/header-h ()
  "Add header and footer to an .h buffer."
  (interactive)
  (let ((fname (if (buffer-file-name)
                   (file-name-nondirectory (buffer-file-name))
                 (error "This buffer is not visiting a file")))
        (fdate (current-time-string)))
    (save-excursion
      (goto-char (point-min))
      (insert
       "/**\n"
       " ******************************************************************************\n"
       " * @file    " fname "\n"
       " * @author  王北洛 <beiluo.wang@139.com>\n"
       " * @date    " fdate "\n"
       " * @brief   简介...\n"
       " *\n  *\n  *\n"
       " ******************************************************************************\n"
       " *                         Copyright (C) 2020 王北洛\n"
       " ******************************************************************************\n"
       " */\n\n"
       "#ifndef __" (upcase (substring fname 0 -2)) "_H\n"
       "#define __" (upcase (substring fname 0 -2)) "_H\n\n"
       "/* Includes ------------------------------------------------------------------*/\n\n"
       "/* Exported macro ------------------------------------------------------------*/\n\n"
       "/* Exported types ------------------------------------------------------------*/\n\n"
       "/* Exported typedef ----------------------------------------------------------*/\n\n"
       "/* External variables --------------------------------------------------------*/\n\n"
       "/* Exported functions --------------------------------------------------------*/\n\n")
      (goto-char (point-max))
      (insert
       "#endif /* __" (upcase (substring fname 0 -2)) "_H */\n\n"
       "/***********************************END OF FILE********************************/\n\n")
      (insert ""))))


(provide 'init-c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-c.el ends here
