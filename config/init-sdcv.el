;;; init-sdcv.el --- Sdcv Configuration. -*- lexical-binding: t -*-

;; Copyright (C) 2025~2026 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;;  sdcv词典配置
;;

;;; Code:

(defconst extensions-posframe-dir
  (expand-file-name "extensions/posframe" user-emacs-directory))
(defconst extensions-sdcv-dir
  (expand-file-name "extensions/sdcv" user-emacs-directory))

(use-package posframe
  :ensure nil
  :load-path extensions-posframe-dir)

(use-package sdcv
  :ensure nil
  :load-path extensions-sdcv-dir
  ;; 延迟加载：仅在调用这些命令时才加载插件
  :commands (sdcv-search-pointer sdcv-search-input)
  :bind (("M-s f p" . sdcv-search-pointer)  ;; 光标处查词
         ("M-s f i" . sdcv-search-input))   ;; 手动输入查词
  :init
  ;; 词库路径
  (setq sdcv-dictionary-data-dir (expand-file-name "sdcv-dict" user-emacs-directory))
  :config
  (setq sdcv-say-word-p nil)          ;; 查词时是否发音
  (setq sdcv-tooltip-mode 'posframe)  ;; 设置 sdcv 使用 posframe 显示
  (setq sdcv-tooltip-timeout 10)      ;; 浮窗显示时间

  (setq sdcv-posframe-border-width 1)

  ;; 查询失败显示方式
  (setq sdcv-fail-notify-string "未找到词解")

  ;; 完整查询列表
  (setq sdcv-dictionary-complete-list
        '("stardict1.3英汉辞典"
          "XDICT英汉辞典"
          "XDICT汉英辞典"
          "英汉汉英专业词典"
          "牛津英汉双解美化版"
          )))

(provide 'init-sdcv)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-sdcv.el ends here
