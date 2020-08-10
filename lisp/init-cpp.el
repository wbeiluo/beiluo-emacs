;;; init-cpp.el --- C++ Configuration -*- lexical-binding: t -*-

;; Copyright (C) 2020 王北洛

;; Author: 王北洛 <wbeiluo@139.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;; Code:

(add-hook
 'c++-mode-hook
 '(lambda ()
    (use-package modern-cpp-font-lock)

    (defun c++-mode-style-setup ()
      (interactive)
      "Set up c++-mode and related modes.
Includes support for Qt code (signal, slots and alikes)."

      ;; cpp font lock.
      (modern-c++-font-lock-global-mode t)

      ;; base-style
      (c-set-style "stroustrup")
      (setq c-basic-offset 4)
      (imenu-add-menubar-index)

      ;; qt keywords and stuff ...
      ;; set up indenting correctly for new qt kewords
      (setq c-protection-key (concat "\\<\\(public\\|public slot\\|protected"
                                     "\\|protected slot\\|private\\|private slot"
                                     "\\)\\>")
            c-C++-access-key (concat "\\<\\(signals\\|public\\|protected\\|private"
                                     "\\|public slots\\|protected slots\\|private slots"
                                     "\\)\\>[ \t]*:"))
      (progn
        ;; modify the colour of slots to match public, private, etc ...
        (font-lock-add-keywords 'c++-mode
                                '(("\\<\\(slots\\|signals\\)\\>" . font-lock-type-face)))
        ;; make new font for rest of qt keywords
        (make-face 'qt-keywords-face)
        (set-face-foreground 'qt-keywords-face "DeepSkyBlue1")
        ;; qt keywords
        (font-lock-add-keywords 'c++-mode
                                '(("\\<Q_OBJECT\\>" . 'qt-keywords-face)))
        (font-lock-add-keywords 'c++-mode
                                '(("\\<SIGNAL\\|SLOT\\>" . 'qt-keywords-face)))
        (font-lock-add-keywords 'c++-mode
                                '(("\\<Q[A-Z][A-Za-z]\\>" . 'qt-keywords-face)))
        ))
    (c++-mode-style-setup)))


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
       "/**\n"
       "  ******************************************************************************\n"
       "  * Copyright (C) " (substring fdate -4 nil) " 王北洛.\n"
       "  *\n"
       "  * Author  王北洛 <beiluo.wang@139.com>\n"
       "  * Date    " fdate "\n"
       "  *\n"
       "  * Brief   简介...\n"
       "  *\n  *\n  *\n"
       "  ******************************************************************************\n"
       "  */\n\n"
       "// Includes\n"
       "#include \"" (substring fname 0 -3) "h\"\n\n"
       "// Code... \n\n")
      (goto-char (point-max))
      (insert "/***********************************END OF FILE********************************/\n\n")
      (insert ""))))

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
       "/**\n"
       "  ******************************************************************************\n"
       "  * Copyright (C) " (substring fdate -4 nil) " 王北洛.\n"
       "  *\n"
       "  * Author  王北洛 <beiluo.wang@139.com>\n"
       "  * Date    " fdate "\n"
       "  *\n"
       "  * Brief   简介...\n"
       "  *\n  *\n  *\n"
       "  ******************************************************************************\n"
       "  */\n\n"
       "#ifndef __" (upcase (substring fname 0 -2)) "_H\n"
       "#define __" (upcase (substring fname 0 -2)) "_H\n\n"
       "// Includes\n\n"
       "// Code... \n\n")
      (goto-char (point-max))
      (insert
       "#endif // __" (upcase (substring fname 0 -2)) "_H\n\n"
       "/***********************************END OF FILE********************************/\n\n")
      (insert ""))))

(provide 'init-cpp)

;;; init-cpp.el ends here
