;;; init-sdcv.el --- Sdcv Configuration. -*- lexical-binding: t -*-

;; Copyright (C) 2025~2025 王北洛

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

  ;; 简单查询列表
  (setq sdcv-dictionary-simple-list
        '("牛津英汉双解美化版"
          "懒虫简明英汉词典"
          "朗道汉英字典5.0"
          "WordNet"
          "Jargon"))

  ;; 完整查询列表
  (setq sdcv-dictionary-complete-list
        '(
          "懒虫简明英汉词典"
          "英汉汉英专业词典"
          "XDICT英汉辞典"
          "stardict1.3英汉辞典"
          "WordNet"
          "XDICT汉英辞典"
          "Jargon"
          "懒虫简明汉英词典"
          "FOLDOC"
          "新世纪英汉科技大词典"
          "KDic11万英汉词典"
          "朗道汉英字典5.0"
          "CDICT5英汉辞典"
          "新世纪汉英科技大词典"
          "牛津英汉双解美化版"
          "21世纪双语科技词典"
          "quick_eng-zh_CN"
          )))

(provide 'init-sdcv)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-sdcv.el ends here
