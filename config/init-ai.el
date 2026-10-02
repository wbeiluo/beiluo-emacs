;;; init-ai.el --- AI Configurations -*- lexical-binding: t -*-

;; Copyright (C) 2026~2026 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;; Code:

(defconst extensions-gptel-dir
  (expand-file-name "extensions/gptel" user-emacs-directory))
(defconst extensions-ob-gptel-dir
  (expand-file-name "extensions/ob-gptel" user-emacs-directory))

;;; AI Chat Assistant
(use-package gptel
  :ensure nil
  :load-path extensions-gptel-dir
  :bind
  (("C-c g"   . gptel)           ; 启动或切换到 gptel 对话缓冲区
   ("C-c G"   . gptel-menu)      ; 打开 gptel 功能菜单
   ("C-c C-g" . gptel-send)      ; 发送当前缓冲区/选中内容到gptel
   :map gptel-mode-map
   ("C-c C-r" . gptel-rewrite))  ; 在对话中要求模型重写上一次回答
  :config
  (use-package gptel-transient)
  (use-package gptel-request)
  (use-package gptel-rewrite)
  (use-package gptel-context)
  (use-package gptel-integrations)
  (use-package gptel-openai)
  (use-package gptel-openai-extras)
  (use-package gptel-openai-responses)
  (use-package gptel-org)

  (setq gptel-api-key #'gptel-api-key-from-auth-source)
  (setq gptel-default-mode 'org-mode)

  (setq chatgpt-backend
        (gptel-make-openai "ChatGPT"
          :stream t
          :models '("gpt-5.6-luna" "gpt-5.6-terra")
          :key #'gptel-api-key))

  (setq deepseek-backend
        (gptel-make-deepseek "Deepseek"
          :stream t
          :models '("deepseek-v4-flash" "deepseek-v4-pro")
          :key #'gptel-api-key))

  (setq mimo-backend
        (gptel-make-openai "MIMO"
          :host "api.xiaomimimo.com"
          :endpoint "/v1/chat/completions"
          :stream t
          :models '("mimo-v2.6-flash" "mimo-v2.6-pro")
          :key #'gptel-api-key))

  (setq gptel-model 'mimo-v2.6-flash
        gptel-backend mimo-backend 
        gptel-use-curl t)

  ;; Preset system message
  (setq gptel-directives
        '((default     . "You are a large language model living in Emacs and a helpful assistant. Respond concisely.")
          (reasoning   . "You are a rigorous analyst. For complex problems, reason step by step and conclude.")
          (programming . "You are a large language model and a careful programmer. Provide code and only code as output without any additional text, prompt or note.")
          (translation . "You are a translation engine. Translate Chinese to English and vice versa. Return only the translation.")
          (writing     . "You are a large language model and a writing assistant. Respond concisely.")
          (chat        . "You are a large language model and a conversation partner. Respond concisely.")))

  ;; Preset model: ChatGPT/gpt-5.6-luna
  (gptel-make-preset 'chatgpt-gpt-5.6-luna
    :description "Using ChatGPT/gpt-5.6-luna"
    :system (alist-get 'default gptel-directives)
    :model 'gpt-5.6-luna
    :backend chatgpt-backend
    :include-reasoning 'ignore
    :request-params nil)

  ;; Preset model: ChatGPT/gpt-5.6-terra
  (gptel-make-preset 'chatgpt-gpt-5.6-terra
    :description "Using ChatGPT/gpt-5.6-terra"
    :system (alist-get 'default gptel-directives)
    :model 'gpt-5.6-terra
    :backend chatgpt-backend
    :include-reasoning 'ignore
    :request-params nil)

  ;; Preset model: Deepseek/deepseek-v4-flash
  (gptel-make-preset 'deekseek-v4-flash
    :description "Using DeepSeek/deepseek-v4-flash"
    :system (alist-get 'default gptel-directives)
    :model 'deepseek-v4-flash
    :backend deepseek-backend
    :include-reasoning 'ignore
    :request-params '(:thinking (:type "disabled")))

  ;; Preset model: Deepseek/deepseek-v4-pro
  (gptel-make-preset 'deepseek-v4-pro
    :description "Using DeepSeek/deepseek-v4-pro"
    :system (alist-get 'default gptel-directives)
    :model 'deepseek-v4-pro
    :backend deepseek-backend
    :include-reasoning 'ignore
    :request-params '(:reasoning_effort "max"))

  ;; Preset model: Xiaomi/mimo-v2.6-flash
  (gptel-make-preset 'mimo-v2.6-flash
    :description "Using Xiaomi/mimo-v2.6-flash"
    :system (alist-get 'default gptel-directives)
    :model 'mimo-v2.6-flash
    :backend mimo-backend
    :include-reasoning 'ignore
    :request-params '(:temperature 1.0
                      :top_p 0.95
                      :frequency_penalty 0
                      :presence_penalty 0
                      :thinking (:type "disabled")))

  ;; Preset model: Xiaomi/mimo-v2.6-pro
  (gptel-make-preset 'mimo-v2.6-pro
    :description "Using Xiaomi/mimo-v2.6-pro"
    :system (alist-get 'default gptel-directives)
    :model 'mimo-v2.6-pro
    :backend mimo-backend
    :include-reasoning 'ignore
    :request-params '(:temperature 1.0
                      :top_p 0.95
                      :frequency_penalty 0
                      :presence_penalty 0)))

;; Org-babel backend for running gptel queries 
(use-package ob-gptel
  :ensure nil
  :load-path extensions-ob-gptel-dir
  :config
  (add-to-list 'org-babel-load-languages '(gptel . t))
  (defun ob-gptel-setup-completions ()
    (add-hook 'completion-at-point-functions
              'ob-gptel-capf nil t))
  :hook (org-mode . ob-gptel-setup-completions))

(provide 'init-ai)
;;; init-ai.el ends here
