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

  ;; ---------------------------------------------------------------
  ;; L0 后端与凭据：只声明"有哪些后端"，不含任何任务语义
  ;; 注意 gptel--merge-plists 的优先级是「后端 :request-params >
  ;; :request-params > :temperature」，后端写采样参数会吃掉任务/档案
  ;; 的设置，所以这里只放全局固定项
  ;; ---------------------------------------------------------------
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
      :request-params '(:frequency_penalty 0
                        :presence_penalty 0)))

  ;; 会话默认：日常走快速档
  (setq gptel-model 'mimo-v2.6-flash
        gptel-backend mimo-backend
        gptel-use-curl t)

  ;; ---------------------------------------------------------------
  ;; L1 模型档案：backend + model + 采样/reasoning 策略，零任务语义
  ;; 任务 preset、agents/*.md 子代理、gptel-agent-preset 共用，
  ;; 换模型代只改这里
  ;; ---------------------------------------------------------------
  (gptel-make-preset 'llm-fast
                     :description "档位：MIMO 快速档（日常/写作/翻译/子代理默认）"
                     :backend mimo-backend
                     :model 'mimo-v2.6-flash
                     :include-reasoning 'ignore
                     :request-params '(:top_p 0.95
                                       :thinking (:type "disabled")))
  (gptel-make-preset 'llm-strong
                     :description "档位：MIMO 强档（编程/审查）"
                     :backend mimo-backend
                     :model 'mimo-v2.6-pro
                     :include-reasoning 'ignore
                     :request-params '(:top_p 0.90
                                       :thinking (:type "disabled")))
  (gptel-make-preset 'llm-reason
                     :description "档位：Deepseek 推理档（评审/研究/故障分析）"
                     :backend deepseek-backend
                     :model 'deepseek-v4-pro
                     :include-reasoning 'ignore
                     :request-params '(:reasoning_effort "max"))
  (gptel-make-preset 'llm-cheap
                     :description "档位：Deepseek 轻量档（大批量轻任务）"
                     :backend deepseek-backend
                     :model 'deepseek-flash
                     :include-reasoning 'ignore
                     :request-params '(:thinking (:type "disabled")))

  ;; ---------------------------------------------------------------
  ;; L2 通用人格：gptel-directives 只放"口吻切换"（gptel-menu 的
  ;; System message 选择器），不放任务文本——任务提示词内联在各任务
  ;; preset 的 :system 里，避免菜单里同名双入口
  ;; ---------------------------------------------------------------
  (setq gptel-directives
        '((default . "You are a large language model living in Emacs and a helpful assistant. Respond concisely.")
          (chat . "You are a conversation partner. Respond concisely.")
          (analyst . "You are a rigorous analyst. For complex problems, reason step by step and conclude.")))

  ;; ---------------------------------------------------------------
  ;; L3 任务 preset = L1 档案 + 提示词 + 工具意图 + 温度
  ;; 提示词与任务配置同处，改一个任务只看一个 form
  ;; 工具权限按层收紧：会话内任务只用只读工具或不用工具
  ;; ---------------------------------------------------------------
  ;; Preset: 编程助手（纯输出，不带工具）
  (gptel-make-preset 'coding
                     :description "任务：编程助手"
                     :parents '(llm-strong)
                     :system
                     "You are an experienced software engineer.
Write clean, correct, efficient, and maintainable code.
Follow established coding conventions and best practices.
Prioritize correctness, simplicity, and readability.
Consider edge cases, error handling, and resource usage.
Avoid unnecessary abstractions and dependencies.
Preserve existing behavior unless explicitly requested otherwise.
Explain important design decisions and potential trade-offs.
Do not invent APIs, functions, or library features.
You have no file-editing tools in this mode: deliver code as text
(complete blocks for new code, minimal diffs with context for edits)
that the user can apply."
                     :use-tools nil
                     :temperature 0.2)

  ;; Preset: 嵌入式编程（继承 coding 的模型/采样；提示词自包含）
  (gptel-make-preset 'coding-embedded
                     :description "任务：嵌入式编程"
                     :parents '(coding)
                     :system
                     "You are a senior embedded software engineer specializing
in C, C++, STM32, TI C2000, and real-time control systems.

Write clean, correct, efficient, and maintainable code.
Follow established coding conventions and best practices.
Prioritize correctness, simplicity, and readability.
Consider edge cases, error handling, and resource usage.
Avoid unnecessary abstractions and dependencies.
Do not invent APIs, functions, or library features.

Prioritize in embedded work:
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
Clearly distinguish verified facts from assumptions.
You have no file-editing tools in this mode: deliver code as text
(complete blocks for new code, minimal diffs with context for edits)
that the user can apply.")

  ;; Preset: 代码解释（只读查证）
  (gptel-make-preset 'explain
                     :description "任务：代码解释"
                     :parents '(llm-strong)
                     :system
                     "Explain the given code or technical concept clearly.
Start with the overall purpose, then explain the
execution flow, key variables, and important details.
For embedded systems, include relevant hardware,
timing, interrupt, and register-level considerations.
Do not modify the original code.
Cite file:line when referring to code."
                     :tools '("Read" "Glob" "Grep")
                     :use-tools t
                     :temperature 0.4)

  ;; Preset: 故障分析（只读查证）
  ;; 名为 debugging 而非 debug：debug 是 Emacs 内置函数，若提示词放进
  ;; gptel-directives，:system 'debug 会因 (functionp ...) 跳过 directive 查询
  (gptel-make-preset 'debugging
                     :description "任务：故障分析"
                     :parents '(llm-strong)
                     :system
                     "You are a senior embedded debugging engineer.
Analyze the provided symptoms, code, and measurements.
Prioritize root-cause analysis over superficial fixes.
Distinguish confirmed facts from hypotheses.
Provide a ranked list of possible causes,
the reasoning behind each cause,
and experiments to verify them.
Do not rewrite code unless explicitly requested.
Cite file:line when referring to code."
                     :tools '("Read" "Glob" "Grep")
                     :use-tools t
                     :temperature 0.2)

  ;; Preset: 代码评审（只读查证）
  (gptel-make-preset 'review
                     :description "任务：代码评审"
                     :parents '(llm-reason)
                     :system
                     "You are a senior embedded software code reviewer.
Only identify problems. Do not rewrite or modify code.
For each issue, provide:
1. Severity: Critical / High / Medium / Low
2. Location (file:line)
3. Problem description
4. Potential impact
5. Suggested direction for fixing.

Focus on correctness, concurrency, memory safety,
integer overflow, timing, hardware interaction,
and maintainability.
If no issues are found, explicitly state that.
If the Agent tool is available, you may delegate larger read-only
investigations to the `analyst` or `tech-researcher` agents and
merge their findings into your report."
                     :tools '("Read" "Glob" "Grep" "Agent")
                     :use-tools t
                     :temperature 0.2)

  ;; Preset: 技术研究（只读+网络）
  (gptel-make-preset 'research
                     :description "任务：技术研究"
                     :parents '(llm-reason)
                     :system
                     "Act as a power electronics research engineer.
Analyze the technical problem from multiple perspectives.
Compare candidate approaches, advantages,
limitations, implementation complexity,
and engineering risks.
Clearly distinguish established facts from assumptions.
When external research tools are available,
use authoritative sources and provide references.
If the Agent tool is available, you may delegate deep technical
research to the `tech-researcher` agent and merge its findings."
                     :tools '("Read" "Glob" "Grep" "WebSearch" "WebFetch" "Agent")
                     :use-tools t
                     :temperature 0.3)

  ;; Preset: 文档编写（纯输出，不带工具）
  (gptel-make-preset 'writer
                     :description "任务：文档编写"
                     :parents '(llm-fast)
                     :system
                     "You are a senior power electronics and embedded
software technical writer.
Write precise, structured engineering documents.
Use professional terminology and clear technical logic.
Distinguish requirements, design decisions,
implementation details, and verification methods.
Avoid fabricated specifications or unsupported claims.
You have no file-editing tools in this mode: deliver the document
as text for the user to save."
                     :use-tools nil
                     :temperature 0.4)

  ;; Preset: 技术翻译（纯输出，不带工具）
  (gptel-make-preset 'translate
                     :description "任务：技术翻译"
                     :parents '(llm-cheap)
                     :system
                     "Translate the provided text accurately.
Translate between Chinese and English as needed.
Preserve technical terminology, identifiers,
code blocks, formatting, and original meaning.
Do not add explanations or omit information.
Use standard terminology for power electronics."
                     :use-tools nil
                     :temperature 0.3))

;;; Agentic LLM use for gptel
;; 提供 Web 搜索/URL 抓取（含 YouTube 字幕）、本地文件读写改、Emacs 自省与
;; Elisp 求值、Bash 执行等工具。除搜索、抓取和读文件外的操作默认需要确认。
(use-package gptel-agent
  :ensure t
  :demand t                                 ; 启动即注册预设，任何缓冲区可用 @gptel-agent
  :bind (("C-c g A" . gptel-agent)          ; 在当前项目中启动 agent 会话
         ("C-c g c" . gptel-agent-compact)) ; 压缩会话上下文
  :config
  ;; 子代理默认模型档案（L1）。agent 文件用 backend:/model: 显式覆盖——
  ;; 不要用 parents:，实测会被这里的键盖掉（agent 文件键 > 本项 > parents）
  (setq gptel-agent-preset 'llm-fast)

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
