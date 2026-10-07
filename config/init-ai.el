;;; init-ai.el --- AI Configurations -*- lexical-binding: t -*-

;; Copyright (C) 2026~2026 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;; Code:

;;; A simple LLM client for Emacs
(use-package gptel
  :ensure t
  :bind
  (("C-c g g" . gptel)           ; 启动或切换到 gptel 对话缓冲区
   ("C-c g m" . gptel-menu)      ; 打开 gptel 功能菜单
   ("C-c g s" . gptel-send)      ; 发送当前缓冲区/选中内容到gptel
   ("C-c g a" . gptel-add)       ; 添加上下文Buffer
   ("C-c g f" . gptel-add-file)  ; 添加上下文文件
   ("C-c g r" . gptel-rewrite))  ; 改写
  :hook ((gptel-post-stream . gptel-auto-scroll)                  ; enable auto-scrolling
         (gptel-post-response-functions . gptel-end-of-response)) ; move the cursor to the next prompt
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

  (setq gptel-default-mode 'org-mode)  ; 默认格式为Org
  (setq gptel-org-branching-context t) ; 每个子树独立上下文
  (setq gptel-org-convert-response t)  ; 自动把markdown转org

  ;; 注册模型
  (setq gptel-api-key #'gptel-api-key-from-auth-source)
  ;; ChatGPT
  (defvar chatgpt-backend
    (gptel-make-openai "ChatGPT"
      :stream t
      :models '("gpt-5.6-luna" "gpt-5.6-terra")
      :key #'gptel-api-key))
  ;; Deepseek
  (defvar deepseek-backend
    (gptel-make-deepseek "Deepseek"
      :stream t
      :models '("deepseek-flash" "deepseek-v4-pro")
      :key #'gptel-api-key))
  ;; Xiaomi MIMO
  (defvar mimo-backend
    (gptel-make-openai "MIMO"
      :host "api.xiaomimimo.com"
      :endpoint "/v1/chat/completions"
      :stream t
      :models '("mimo-v2.6-flash" "mimo-v2.6-pro")
      :key #'gptel-api-key
      :request-params '(:temperature 1.0
                        :top_p 0.95
                        :frequency_penalty 0
                        :presence_penalty 0)))

  ;; 设置默认模型
  (setq gptel-model 'mimo-v2.6-flash
        gptel-backend mimo-backend
        gptel-use-curl t)

  ;; Preset system message
  (setq gptel-directives
        '((default    . "You are a large language model living in Emacs and a helpful assistant. Respond concisely.")
          (analyst    . "You are a rigorous analyst. For complex problems, reason step by step and conclude.")
          (reviewer   . "You are a code reviewer. Only identify issues, do not rewrite the code. Assign a severity level to each issue.")
          (coder      . "You are a careful programmer. Provide code and only code as output without any additional text, prompt or note.")
          (translator . "You are a translation engine. Translate Chinese to English and vice versa. Return only the translation.")
          (writer     . "You are a writing assistant. Respond concisely.")
          (chat       . "You are a conversation partner. Respond concisely.")))

  ;; Preset: 编程助手
  (gptel-make-preset 'coding
                     :description "通用编程助手"
                     :system
                     "You are an experienced software engineer.
Write clean, correct, efficient, and maintainable code.
Follow established coding conventions and best practices.
Prioritize correctness, simplicity, and readability.
Consider edge cases, error handling, and resource usage.
Avoid unnecessary abstractions and dependencies.
Preserve existing behavior unless explicitly requested otherwise.
Explain important design decisions and potential trade-offs.
Do not invent APIs, functions, or library features."
                     ;; :tools '("read_buffer" "modify_buffer")
                     :model 'mimo-v2.6-pro
                     :backend mimo-backend
                     :temperature 0.2
                     :include-reasoning 'ignore
                     :request-params '(:temperature 0.2
                                       :top_p 0.90
                                       :frequency_penalty 0
                                       :presence_penalty 0
                                       :thinking (:type "disabled")))

  ;; Preset: 嵌入式编程
  (gptel-make-preset 'coding-embedded
                     :parents '(coding)
                     :description "嵌入式编程"
                     :system
                     "You are a senior embedded software engineer specializing
in C, C++, STM32, TI C2000, and real-time control systems.

Prioritize:
- Deterministic execution and real-time constraints.
- Memory and CPU efficiency.
- Interrupt safety and concurrency.
- Hardware register correctness.
- Integer overflow and numerical precision.
- Peripheral configuration and timing.
- Robust error handling and fault recovery.
- MISRA-C principles where applicable.

Consider MCU architecture, clock configuration,
interrupt priorities, DMA, peripheral timing,
and hardware limitations.

Do not assume hardware behavior without sufficient evidence.
Do not introduce dynamic memory allocation unless necessary.
Preserve existing architecture and coding conventions.
Clearly distinguish verified facts from assumptions."
                     ;; :tools '("read_buffer" "modify_buffer")
                     :model 'mimo-v2.6-pro
                     :backend mimo-backend
                     :temperature 0.2
                     :include-reasoning 'ignore
                     :request-params '(:temperature 0.2
                                       :top_p 0.90
                                       :frequency_penalty 0
                                       :presence_penalty 0
                                       :thinking (:type "disabled")))

  ;; Preset: 代码解释
  (gptel-make-preset 'explain
                     :description "代码解释"
                     :system
                     "Explain the given code or technical concept clearly.
Start with the overall purpose, then explain the
execution flow, key variables, and important details.
For embedded systems, include relevant hardware,
timing, interrupt, and register-level considerations.
Do not modify the original code."
                     ;; :tools '("read_buffer")
                     :model 'mimo-v2.6-pro
                     :backend mimo-backend
                     :temperature 0.4
                     :include-reasoning 'ignore
                     :request-params '(:temperature 0.4
                                       :top_p 0.95
                                       :frequency_penalty 0
                                       :presence_penalty 0
                                       :thinking (:type "disabled")))

  ;; Preset: 故障分析
  (gptel-make-preset 'debug
                     :description "故障分析"
                     :system
                     "You are a senior embedded debugging engineer.
Analyze the provided symptoms, code, and measurements.
Prioritize root-cause analysis over superficial fixes.
Distinguish confirmed facts from hypotheses.
Provide a ranked list of possible causes,
the reasoning behind each cause,
and experiments to verify them.
Do not rewrite code unless explicitly requested."
                     ;; :tools '("read_buffer")
                     :model 'mimo-v2.6-pro
                     :backend mimo-backend
                     :temperature 0.2
                     :include-reasoning 'ignore
                     :request-params '(:temperature 0.2
                                       :top_p 0.90
                                       :frequency_penalty 0
                                       :presence_penalty 0
                                       :thinking (:type "disabled")))

  ;; Preset: 代码评审
  (gptel-make-preset 'review
                     :description "代码评审"
                     :system
                     "You are a senior embedded software code reviewer.
Only identify problems. Do not rewrite or modify code.
For each issue, provide:
1. Severity: Critical / High / Medium / Low
2. Location
3. Problem description
4. Potential impact
5. Suggested direction for fixing.

Focus on correctness, concurrency, memory safety,
integer overflow, timing, hardware interaction,
and maintainability.
If no issues are found, explicitly state that."
                     ;; :tools '("read_buffer")
                     :model 'deepseek-v4-pro
                     :backend deepseek-backend
                     :temperature 0.2
                     :include-reasoning 'ignore
                     :request-params '(:reasoning_effort "max"))

  ;; Preset: 文档编写
  (gptel-make-preset 'writer
                     :description "文档编写"
                     :system
                     "You are a senior power electronics and embedded
software technical writer.
Write precise, structured engineering documents.
Use professional terminology and clear technical logic.
Distinguish requirements, design decisions,
implementation details, and verification methods.
Avoid fabricated specifications or unsupported claims."
                     ;; :tools '("read_buffer" "modify_buffer")
                     :model 'mimo-v2.6-flash
                     :backend mimo-backend
                     :temperature 0.5
                     :include-reasoning 'ignore
                     :request-params '(:temperature 0.5
                                       :top_p 0.95
                                       :frequency_penalty 0
                                       :presence_penalty 0
                                       :thinking (:type "disabled")))

  ;; Preset: 技术研究
  (gptel-make-preset 'research
                     :description "技术研究"
                     :system
                     "Act as a power electronics research engineer.
Analyze the technical problem from multiple perspectives.
Compare candidate approaches, advantages,
limitations, implementation complexity,
and engineering risks.
Clearly distinguish established facts from assumptions.
When external research tools are available,
use authoritative sources and provide references."
                     ;; :tools '("read_buffer" "modify_buffer" "search_web" "read_url")
                     :model 'deepseek-v4-pro
                     :backend deepseek-backend
                     :temperature 0.5
                     :include-reasoning 'ignore
                     :request-params '(:reasoning_effort "max"))

  ;; Preset: 技术翻译
  (gptel-make-preset 'translate
                     :description "技术翻译"
                     :system
                     "Translate the provided text accurately.
Preserve technical terminology, identifiers,
code blocks, formatting, and original meaning.
Do not add explanations or omit information.
Use standard terminology for power electronics."
                     :model 'deepseek-flash
                     :backend deepseek-backend
                     :temperature 1.0
                     :include-reasoning 'ignore
                     :request-params '(:thinking (:type "disabled"))))

;;; Agentic LLM use for gptel
;; 提供 Web 搜索/URL 抓取（含 YouTube 字幕）、本地文件读写改、Emacs 自省与
;; Elisp 求值、Bash 执行等工具，并可委派 executor / researcher / introspector
;; 等子代理。除搜索、抓取和读文件外的操作默认需要确认。
(use-package gptel-agent
  :ensure t
  :demand t                                 ; 启动即注册预设，任何缓冲区可用 @gptel-agent
  :bind (("C-c g A" . gptel-agent)          ; 在当前项目中启动 agent 会话
         ("C-c g c" . gptel-agent-compact)) ; 压缩会话上下文
  :config
  ;; 自定义 agent 定义目录（.md/.org）：目录必须存在，且要追加到列表末尾——
  ;; gptel-agent-dirs 中靠后的目录覆盖靠前的同名定义（内置定义在包目录里）
  (let ((agent-dir (expand-file-name "agents/" user-emacs-directory)))
    (make-directory agent-dir t)
    (add-to-list 'gptel-agent-dirs agent-dir t))
  ;; 扫描 agents/ 与 skills/ 目录，注册gptel-agent预设
  ;; 定义文件 YAML 写错会中断注册，这里兜底避免影响启动，详见 *Warnings*
  (condition-case err
      (gptel-agent-update)
    (error (display-warning 'init-ai
                            (format "gptel-agent-update 失败: %s"
                                    (error-message-string err))))))

;; Org-babel backend for running gptel queries
;; (use-package ob-gptel
;;   :vc (:url "https://github.com/jwiegley/ob-gptel"
;;        :rev :newest
;;        :branch "main")
;;   :after gptel
;;   :config
;;   (add-to-list 'org-babel-load-languages '(gptel . t))
;;   (defun ob-gptel-setup-completions ()
;;     (add-hook 'completion-at-point-functions
;;               'ob-gptel-capf nil t))
;;   :hook (org-mode . ob-gptel-setup-completions))

(provide 'init-ai)
;;; init-ai.el ends here
