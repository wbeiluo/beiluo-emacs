;;; init-sdcv.el --- Sdcv Configuration. -*- lexical-binding: t -*-

;; Copyright (C) 2025~2025 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;;  sdcv词典配置
;;

;;; Code:

(require 'posframe)
(require 'sdcv)

(setq sdcv-say-word-p nil) ; say word after translation

(setq sdcv-dictionary-data-dir (expand-file-name "sdcv-dict" user-emacs-directory)) ; setup directory of stardict dictionary

(setq sdcv-dictionary-simple-list    ; setup dictionary list for simple search
      '("懒虫简明英汉词典"
        "懒虫简明汉英词典"
        "KDic11万英汉词典"))

(setq sdcv-dictionary-complete-list  ; setup dictionary list for complete search
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
        ))

(provide 'init-sdcv)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-sdcv.el ends here
