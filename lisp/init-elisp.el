;; init-hydra.el --- Elisp configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2020 王北洛

;; Author: 王北洛 <wbeiluo@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;; elisp configurations.
;;

;;; Code:

(defun wbeiluo/headerise-elisp ()
  "Add minimal header and footer to an elisp buffer in order to placate flycheck."
  (interactive)
  (let ((fname (if (buffer-file-name)
                   (file-name-nondirectory (buffer-file-name))
                 (error "This buffer is not visiting a file"))))
    (save-excursion
      (goto-char (point-min))
      (insert ";;; " fname " --- Insert description here -*- lexical-binding: t -*-\n\n"
              ";; Copyright (C) 2021 王北洛\n\n"
              ";; Author: 王北洛 <wbeiluo@139.com>\n"
              ";; URL: https://github.com/wbeiluo/beiluo-emacs\n\n"
              ";;; Commentary:\n"
              ";;; Code:\n\n")
      (goto-char (point-max))
      (insert ";;; " fname " ends here\n"))))

;; C源文件默认注释
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

;; C头文件默认注释
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

;; C++源文件默认
(defun comment/header-cpp ()
  "Add header and footer to an .cpp buffer."
  (interactive)
  (let ((fname (if (buffer-file-name)
                   (file-name-nondirectory (buffer-file-name))
                 (error "This buffer is not visiting a file")))
        (fdate (current-time-string)))
    (save-excursion
      (goto-char (point-min))
      (insert
       "// Copyright (C) " (substring fdate -4 nil) " 王北洛.\n"
       "// Author:  王北洛 <beiluo.wang@139.com>\n"
       "// Brief :  简介...\n\n\n"
       "// Includes\n"
       "#include \"" (substring fname 0 -3) "h\"\n\n\n")
      (goto-char (point-max))
      (insert ""))))

;; C++头文件默认注释
(defun comment/header-hpp ()
  "Add header and footer to an .h buffer."
  (interactive)
  (let ((fname (if (buffer-file-name)
                   (file-name-nondirectory (buffer-file-name))
                 (error "This buffer is not visiting a file")))
        (fdate (current-time-string)))
    (save-excursion
      (goto-char (point-min))
      (insert
       "// Copyright (C) " (substring fdate -4 nil) " 王北洛.\n"
       "// Author: 王北洛 <beiluo.wang@139.com>\n"
       "// Brief:  简介...\n\n"
       "#ifndef __" (upcase (substring fname 0 -2)) "_H\n"
       "#define __" (upcase (substring fname 0 -2)) "_H\n\n")
      (goto-char (point-max))
      (insert
       "#endif // __" (upcase (substring fname 0 -2)) "_H\n\n")
      (insert ""))))


(provide 'init-elisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-elisp.el ends here
