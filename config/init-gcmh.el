;;; init-gcmh.el --- gc configurations -*- lexical-binding: t -*-

;; Copyright (C) 2025~2026 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;; Code:

;; The Garbage Collector Magic Hack
(use-package gcmh
  :ensure t
  :custom
  ;; Adjust garbage collection threshold for early startup (see use of gcmh below)
  (gc-cons-threshold (* 512 1024 1024))

  ;; Process performance tuning
  (read-process-output-max (* 4 1024 1024))
  (process-adaptive-read-buffering nil)

  ;; General performance tuning
  (gcmh-high-cons-threshold (* 512 1024 1024))
  ;; 低水位：空闲 GC 后 gc-cons-threshold 会降到这里。默认 800KB 太低，
  ;; 空闲期解析 clangd 推送的大响应(JSON)会连环触发 GC，恢复移动光标时一顿一顿；
  ;; 提到 32MB（主流配置 16~100MB 同档），GC 次数大幅减少
  (gcmh-low-cons-threshold (* 32 1024 1024))
  ;; 空闲 GC 间隔用 auto（= gcmh-auto-idle-delay-factor × 上次 GC 耗时），
  ;; 取代固定 15s：固定值会让长时间移动积累大量垃圾后一次大 GC，
  ;; 恰好撞上恢复输入就明显卡一下；auto 让 GC 高频小步，单次停顿更短
  (gcmh-idle-delay 'auto)
  :config
  (gcmh-mode 1))

(provide 'init-gcmh)

;;; init-gcmh.el ends here
