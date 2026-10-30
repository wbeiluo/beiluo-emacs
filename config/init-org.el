;;; init-org.el --- Org Mode Configurations -*- lexical-binding: t -*-

;; Copyright (C) 2020~2026 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;; Org 配置，按职责分为六个部分：
;;
;;   1. 按键 Keybindings —— 全局 / Agenda / Org 按键绑定
;;   2. 外观 Appearance  —— 字体、标题层级、符号美化（org-modern）、强调显示（org-appear）
;;   3. 核心 Core        —— 目录文件、显示折叠、列表图片、TODO 关键字、标签、日志、refile、LaTeX 预览
;;   4. 日程 Agenda      —— 时间线、视图定制（org-super-agenda）、appt 提醒
;;   5. 捕获 Capture     —— 模板、目标定位、收尾钩子
;;   6. 扩展 Extensions  —— org-id、org-super-links
;;
;; 显示风格为克制的现代感：颜色全部交给主题（modus / solarized），
;; 本文件只描述字体、字号、字重与符号；主题切换后由 `my/org-set-faces' 重新挂载样式。
;;
;;; Code:

;;; 1. 按键 Keybindings --------------------------------------------------------

;; Org 及其子模块启动即加载：按键绑定、appt 定时同步、capture 模板
;; 都要在启动后立刻可用，因此这里不做延迟加载。
(require 'org)
(require 'org-agenda)
(require 'org-capture)
(require 'org-id)
(require 'org-element)
(require 'appt)
(require 'notifications)

;; Org-agenda: `i' 直接捕获日记模板（jd），`J' 搜索日程条目
(global-set-key (kbd "\e\e a") #'org-agenda)
(define-key org-agenda-mode-map (kbd "i") #'(lambda () (interactive) (org-capture nil "jd")))
(define-key org-agenda-mode-map (kbd "J") #'consult-org-agenda)

;; Org-capture
(global-set-key (kbd "\e\e c") #'org-capture)

;; Org 时间戳: `C-c .' 插入激活时间戳，`C-c _' 插入非激活时间戳。
;; 默认的 `C-c !' 与 flycheck-keymap-prefix 同键，被 flycheck-mode-map
;; 抢占（minor mode 优先于 major mode），故将非激活时间戳改绑到 `C-c _'。
;; 新版 Org 命令名为 `org-timestamp-inactive'（旧名 `org-time-stamp-inactive' 仍是别名）。
;; 动态块刷新: 本机 org 的 C-c C-x C-u(org-dblock-update) 对缩进区块
;; 报 "Not in a dynamic block", 改绑带回落的包装(出错时刷新全文动态块)
(define-key org-mode-map (kbd "C-c C-x C-u") #'my/org-dblock-update)
(define-key org-mode-map (kbd "C-c _") #'org-timestamp-inactive)

;; 项目管理: `C-c P s' 切换研制阶段(门链结算/更新状态/记日志).
;; `C-c P b' 需求基线圈定(批量标记 :BASELINE: 并登记基线记录表).
;; 风险管理: `C-c P r' 评估确认(IDENT→MITIGATE), `C-c P x' 转议题,
;; `C-c P R' 刷新并查看风险台账; 存量文件升级用 M-x my/org-project-risk-upgrade.
;; 门禁: `C-c P g' 关门/重开/看判据/跳评审表, 门链在 `C-c P s' 转段时结算.
;; C-c 大写字母为用户保留前缀, org-mode-map 未占用 C-c P.
(global-set-key (kbd "C-c P s") #'my/org-project-stage-switch)
(define-key org-mode-map (kbd "C-c P s") #'my/org-project-stage-switch)
(global-set-key (kbd "C-c P b") #'my/org-project-baseline-create)
(define-key org-mode-map (kbd "C-c P b") #'my/org-project-baseline-create)
(global-set-key (kbd "C-c P r") #'my/org-project-risk-assess)
(define-key org-mode-map (kbd "C-c P r") #'my/org-project-risk-assess)
(global-set-key (kbd "C-c P x") #'my/org-project-risk-to-issue)
(define-key org-mode-map (kbd "C-c P x") #'my/org-project-risk-to-issue)
(global-set-key (kbd "C-c P R") #'my/org-project-risk-dashboard)
(define-key org-mode-map (kbd "C-c P R") #'my/org-project-risk-dashboard)
(global-set-key (kbd "C-c P g") #'my/org-project-gate-dispatch)
(define-key org-mode-map (kbd "C-c P g") #'my/org-project-gate-dispatch)

;;; 2. 外观 Appearance ---------------------------------------------------------

;; Org 表格、时间戳、标签、抽屉等使用的中英文等宽字体，
;; 与 init-fonts.el 的平台字体设置保持一致。
(defconst my/org-font-family
  (if (eq system-type 'windows-nt) "LXGW WenKai Mono Medium" "LXGW WenKai Mono")
  "Org专用等宽字体族.")

(defun my/org-set-faces ()
  "设置Org相关face的字体与标题层级样式.

本配置会在明暗主题间自动切换，而主题切换会重置 `set-face-attribute'
的设置，故这里统一写入 `face-override-spec'：优先级高于主题，且只覆盖
字体、字号、字重，颜色仍由主题决定，明暗主题下均可正常显示。"
  ;; 等宽字体：保证表格、时间戳、属性等列对齐。
  ;; :inherit fixed-pitch 不可省：face 合并时相对字号会连乘，比如表格里
  ;; 时间戳的 (org-date org-table) 会变成 0.9×0.9=0.81 倍正文、标题里的
  ;; 时间戳变成 0.9×1.2。显式继承一个绝对字号的 face 把相对值锚定下来，
  ;; 就不会再叠加——solarized 未给这些 face 设继承时会踩中，modus 靠
  ;; :inherit modus-themes-fixed-pitch 天然规避了，故两个主题表现不一致。
  (dolist (face '(org-table org-date org-tag org-meta-line org-document-info
                            org-document-info-keyword org-drawer
                            org-special-keyword org-property-value))
    (face-spec-set face
                   `((t :family ,my/org-font-family
                        :height 0.9
                        :inherit fixed-pitch))
                   'face-override-spec))
  ;; 标题层级：逐级递减字号与字重，配色交给主题
  (dolist (spec '((org-level-1 1.20 bold)
                  (org-level-2 1.15 semi-bold)
                  (org-level-3 1.06 semi-bold)
                  (org-level-4 1.0  normal)
                  (org-level-5 1.0  normal)
                  (org-level-6 1.0  normal)
                  (org-level-7 1.0  normal)
                  (org-level-8 1.0  normal)))
    (face-spec-set (car spec)
                   `((t :height ,(nth 1 spec) :weight ,(nth 2 spec)))
                   'face-override-spec)))
(my/org-set-faces)

;; 风险汇总里的来源缺失标记用 error face 标红。用 face 符号而非写死颜色,
;; 明暗主题切换后仍然有效 (与上面 face-override-spec 的处理同理)。
(font-lock-add-keywords 'org-mode '(("⚠ 未链接" 0 'error t)))

;; 时间戳美化(org-modern-timestamp)会让时间戳套上 org-modern-label 的紧凑字号，
;; 实际渲染宽度比原字符窄，而 org 表格按正文字号量列宽，于是表格错位。
;; 这里在表格内跳过美化：表格对齐不受影响，表格之外照常显示时间戳色块。
(defun my/org-modern-timestamp-skip-in-table (old-fun)
  "表格内不美化时间戳，其他位置照常美化。"
  (unless (save-match-data
            (save-excursion
              (goto-char (match-beginning 1))
              (org-at-table-p)))
    (funcall old-fun)))

;; Org-modern: 标题星号、关键字、复选框、表格、TODO 标签等统一美化。
(use-package org-modern
  :ensure t
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda))
  :custom
  ;; 标题星号
  (org-modern-star 'replace)
  (org-modern-replace-stars "☰☱☲☳☴☵☶☷")
  ;; (org-modern-replace-stars "◉◎○◈◇✳")
  ;; 折叠后隐藏前导星号
  (org-modern-hide-stars 'leading)
  ;; 时间戳美化：色块字号在 :config 中调为 0.9 倍正文，仍比正文字号窄，
  ;; 而 org 表格按正文字号量列宽（org-string-width 会抹掉 face），
  ;; 开启后表格会错位；故表格内跳过美化，其他位置照常
  ;; （见 my/org-modern-timestamp-skip-in-table）
  (org-modern-timestamp t)
  ;; 表格竖线改为细线，视觉更轻
  (org-modern-table t)
  (org-modern-table-vertical 1)
  ;; 关键字 `#+key:' 换成图标（保留关键字值），未列出的关键字只隐藏 `#+' 前缀
  (org-modern-keyword
   '(("title"       . "")
     ("subtitle"    . "󰨖")
     ("author"      . "")
     ("date"        . "")
     ("email"       . "󰇰")
     ("language"    . "")
     ("category"    . "")
     ("description" . "🅘")
     ("options"     . "")
     ("startup"     . "")
     ("setupfile"   . "")
     ("filetags"    . "󰓻")
     ("identifier"  . "󰻾")
     ("downloaded"  . "")
     ("results"     . "")
     ("name"        . "🄝")
     ("caption"     . "🄒")
     ("attr_latex"  . "🄛")
     ("attr_html"   . "🄗")
     ("attr_org"    . "🄞")
     (t             . t)))
  ;; 复选框
  (org-modern-checkbox
   '((?X  . "󰄵")
     (?-  . "󰡖")
     (?\s . "󰄱")))
  ;; 优先级 [#A]/[#B]/[#C] -> 🅐/🅑/🅒，配色见 org-priority-faces
  (org-modern-priority
   '((?A . "🅐")
     (?B . "🅑")
     (?C . "🅒")))
  ;; TODO 关键字：保留反白色块样式，颜色改用主题语义 face
  ;; （error/warning/success/link/shadow），明暗主题下对比度都稳定，
  ;; 不再写死 coral/firebrick 这类固定色值。
  ;; 字号沿用 org-modern 默认（org-modern-label 的 0.8 倍正文大小）；
  ;; 需要固定字号时在此加 :height 即可。
  (org-modern-todo-faces
   '(;; --- 日程管理 Keyword ---
     ("TODO"       . (:inherit error   :weight regular :inverse-video t))
     ("NEXT"       . (:inherit success :weight regular :inverse-video t))
     ("WAIT"       . (:inherit warning :weight regular :inverse-video t))
     ("DONE"       . (:inherit shadow  :weight regular :inverse-video t))
     ("CANCELLED"  . (:inherit shadow  :weight regular :inverse-video t))
     ;; --- 项目问题 Keyword ---
     ("REPORT"     . (:inherit warning :weight regular :inverse-video t))
     ("BUG"        . (:inherit error   :weight regular :inverse-video t))
     ("KNOWNCAUSE" . (:inherit warning :weight regular :inverse-video t))
     ("FIXED"      . (:inherit success :weight regular :inverse-video t))
     ("CLOSED"     . (:inherit shadow  :weight regular :inverse-video t))
     ("WONTFIX"    . (:inherit shadow  :weight regular :inverse-video t))
     ;; --- 代码控制 Keyword ---
     ("DESIGN"     . (:inherit link    :weight regular :inverse-video t))
     ("DEVELOP"    . (:inherit success :weight regular :inverse-video t))
     ("CONFIRMED"  . (:inherit success :weight regular :inverse-video t))
     ("DEPRECATED" . (:inherit shadow  :weight regular :inverse-video t))
     ("ARCHIVED"   . (:inherit shadow  :weight regular :inverse-video t))))
  :config
  ;; 时间戳色块字号调为 0.9 倍正文（org-modern 默认继承
  ;; org-modern-label 的 0.8 倍，偏小）。浮点 :height 是相对底层
  ;; face 高度的缩放因子，四个时间戳 face 都继承 org-modern-label
  ;; （0.8×正文），会与这里设置的值连乘，故写 1.125×0.8=0.9，
  ;; 直接写 0.9 会变成 0.72 倍正文。
  ;; 用 face-override-spec 而非 set-face-attribute：主题切换会重置
  ;; 后者，override spec 在 face-spec-recalc 中最后应用，始终生效。
  (dolist (face '(org-modern-date-active org-modern-date-inactive
                  org-modern-time-active org-modern-time-inactive))
    (face-spec-set face '((t :height 1.125)) 'face-override-spec))
  ;; 表格内跳过时间戳美化，保证 org 表格列宽对齐
  (advice-add 'org-modern--timestamp :around #'my/org-modern-timestamp-skip-in-table))

;; Org-appear: 光标移动到隐藏的强调标记/链接/实体上时自动显示，
;; 与 org-hide-emphasis-markers、org-pretty-entities 搭配使用。
(use-package org-appear
  :ensure t
  :custom
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t)
  (org-appear-autoentities t)
  (org-appear-autokeywords t)
  (org-appear-inside-latex t)
  (org-appear-delay 0.5)
  :hook (org-mode . org-appear-mode))

;;; 3. 核心 Core ---------------------------------------------------------------

(use-package org
  :ensure nil
  :mode ("\\.org\\'" . org-mode)
  :hook (org-mode . visual-line-mode)
  :custom
  ;; ---- 显示与折叠 ----
  ;; 标题行折叠符号
  (org-ellipsis "⋯")
  ;; 隐藏宏标记
  (org-hide-macro-markers t)
  ;; 隐藏强调标签
  (org-hide-emphasis-markers t)
  ;; 隐藏符号（\alpha 等）
  (org-pretty-entities t)
  ;; 高亮 latex 语法
  (org-highlight-latex-and-related '(native script entities))
  ;; 隐藏标题星号
  (org-hide-leading-stars t)
  ;; 根据标题行自动缩进文本（真实缩进，对齐到标题星号之后）
  ;; 注意：与 org-indent-mode 的虚拟缩进互斥——二者叠加会错位，
  ;; 故此处不启用 org-startup-indented，缩进完全交给 org-adapt-indentation。
  (org-adapt-indentation t)
  ;; 自动显示图片
  (org-startup-with-inline-images t)
  ;; 默认以 Overview 的模式展示标题行
  (org-startup-folded 'overview)
  ;; 编辑时检查是否在折叠的不可见区域
  (org-fold-catch-invisible-edits 'smart)
  ;; 时间戳格式: <2022-12-24 星期六> 或 <2022-12-24 星期六 06:53>
  (org-time-stamp-formats '("<%Y-%m-%d %A>" . "<%Y-%m-%d %A %H:%M>"))
  ;; 不同级别标题间的间隔
  (org-cycle-separator-lines 2)

  ;; ---- 目录与文件 ----
  ;; Org 数据目录
  (org-directory "~/Org")
  ;; 笔记的默认存储位置
  (org-default-notes-file (expand-file-name "inbox.org" org-directory))
  ;; 启用的子模块
  (org-modules '(ol-bibtex ol-gnus ol-info ol-eww org-habit org-protocol))
  ;; 打开 Org links 的程序
  (org-file-apps '(("\\.png\\'"     . default)
                   (auto-mode       . emacs)
                   (directory       . emacs)
                   ("\\.mm\\'"      . default)
                   ("\\.x?html?\\'" . default)
                   ("\\.pdf\\'"     . emacs)
                   ("\\.md\\'"      . emacs)
                   ("\\.gif\\'"     . default)
                   ("\\.xlsx\\'"    . default)
                   ("\\.svg\\'"     . default)
                   ("\\.pptx\\'"    . default)
                   ("\\.docx\\'"    . default)))

  ;; ---- 列表、图片与编辑习惯 ----
  ;; 标题行之间总是有空格；列表之间根据情况自动加空格
  (org-blank-before-new-entry '((heading . t)
                                (plain-list-item . auto)))
  ;; 允许字母列表
  (org-list-allow-alphabetical t)
  ;; 列表的下一级设置
  (org-list-demote-modify-bullet '(("-"  . "+")
                                   ("+"  . "1.")
                                   ("1." . "a.")))
  ;; 在当前位置插入新标题行还是在当前标题行后插入，这里设置为当前位置
  (org-insert-heading-respect-content nil)
  ;; 设置图片的最大宽度，如果有 imagemagick 支持将会改变图片实际宽度
  ;; 四种设置方法: (1080), 1080, t, nil
  (org-image-actual-width nil)
  ;; imenu 的最大深度，默认为 2
  (org-imenu-depth 4)
  ;; 回车不触发链接
  (org-return-follows-link nil)
  ;; 上标^下标_ 需要用大括号包裹
  (org-use-sub-superscripts '{})
  ;; 复制粘贴标题行的时候删除 id
  (org-clone-delete-id t)
  ;; 粘贴时调整标题行的级别
  (org-yank-adjusted-subtrees t)
  ;; 在活动区域内的所有标题栏执行某些命令
  (org-loop-over-headlines-in-active-region t)

  ;; ---- TODO 关键字 ----
  ;; 可以设置不同的组
  (org-todo-keywords
   '(;; --- 日程管理任务状态 ---
     ;; TODO      : 普通待办
     ;; NEXT      : 可立即执行的任务
     ;; WAIT      : 等待中（他人/条件/环境）
     ;; DONE      : 已完成
     ;; CANCELLED : 取消
     (sequence "TODO(t)" "NEXT(n)" "WAIT(w!)" "|" "DONE(d!)" "CANCELLED(q@/!)")

     ;; --- 项目问题跟踪状态 ---
     ;; REPORT     : 新报告/待确认
     ;; BUG        : 已确认缺陷
     ;; KNOWNCAUSE : 已知原因，待修复
     ;; FIXED      : 已修复，待关闭
     ;; CLOSED     : 已关闭
     ;; WONTFIX    : 不修复
     (sequence "REPORT(r)" "BUG(b!)" "KNOWNCAUSE(k!)" "FIXED(f!)" "|" "CLOSED(c@/!)" "WONTFIX(x@/!)")

     ;; --- 代码控制状态 ---
     ;; DESIGN    : 设计中
     ;; DEVELOP   : 开发中（新增/修改用 :TYPE: 属性或标签区分）
     ;; CONFIRMED : 已确认(完成测试及验证)
     ;; DEPRECATED: 已废弃，不再维护
     ;; ARCHIVED  : 已归档
     (sequence "DESIGN(s)" "DEVELOP(p!)" "CONFIRMED(e!)" "|" "DEPRECATED(y@/!)" "ARCHIVED(a@/!)")

     ;; --- 风险跟踪状态 ---
     ;; 风险 = 未发生的不确定事件; 已发生的转议题, 不双轨跟踪.
     ;; IDENT    : 已识别, 待评估确认
     ;; MITIGATE : 已确认, 措施执行/监控中
     ;; RESOLVED : 已了结, 由 :CONCLUSION: 区分 闭环/挂账/转议题/误报
     (sequence "IDENT(i)" "MITIGATE(g!)" "|" "RESOLVED(v@/!)")))
  ;; 标题行状态变化时标签同步发生的变化
  (org-todo-state-tags-triggers
   '(("CANCELLED" ("cancelled" . t) ("wait"))
     ("WAIT" ("wait" . t))
     (done ("wait"))
     ("TODO" ("wait") ("cancelled"))
     ("DONE" ("wait") ("cancelled"))

     ("REPORT" ("report" . t) ("fixed") ("closed"))
     ("BUG" ("bug" . t) ("fixed") ("closed"))
     ("KNOWNCAUSE" ("knowncause" . t) ("fixed") ("closed"))
     ("FIXED" ("fixed" . t) ("report") ("bug") ("knowncause"))
     ("CLOSED" ("closed" . t) ("report") ("bug") ("knowncause") ("fixed"))

     ("DESIGN" ("design" . t) ("develop") ("confirmed"))
     ("DEVELOP" ("develop" . t) ("design") ("confirmed"))
     ("CONFIRMED" ("confirmed" . t) ("design") ("develop"))
     ("DEPRECATED" ("deprecated" . t) ("design") ("develop") ("confirmed"))
     ("ARCHIVED" ("archived" . t) ("design") ("develop") ("confirmed"))

     ("IDENT" ("risk" . t) ("mitigate") ("resolved"))
     ("MITIGATE" ("risk" . t) ("resolved"))
     ("RESOLVED" ("resolved" . t) ("risk"))))
  ;; 使用专家模式选择标题栏状态
  (org-use-fast-todo-selection 'expert)
  ;; 父子标题栏状态有依赖
  (org-enforce-todo-dependencies t)
  ;; 标题栏和任务复选框有依赖
  (org-enforce-todo-checkbox-dependencies t)

  ;; ---- 标签 ----
  ;; 始终存在的标签
  (org-tag-persistent-alist '(("read"     . ?r)
                              ("study"    . ?s)
                              ("work"     . ?w)
                              ("project"  . ?p)
                              ("emacs"    . ?e)
                              ("life"     . ?l)
                              ("misc"     . ?m)))
  ;; 预定义好的标签
  (org-tag-alist '((:startgroup)
                   ("play"     . ?y)
                   ("tour"     . ?t)
                   (:endgroup)))
  ;; 标签显示的位置，第 100 列前右对齐
  (org-tags-column -100)
  ;; 自动对齐标签
  (org-auto-align-tags t)
  ;; 标签继承
  (org-use-tag-inheritance t)
  ;; 标签快速选择
  (org-use-fast-tag-selection t)
  ;; 标签选择不需要回车确认
  (org-fast-tag-selection-single-key t)
  ;; 定义了有序属性的标题行也加上 ORDERED 标签
  (org-track-ordered-property-with-tag t)

  ;; ---- 优先级 ----
  ;; 配色直接作用于替换后的 🅐/🅑/🅒
  (org-priority-faces
   '((?A :inherit error   :weight regular :inverse-video nil)
     (?B :inherit warning :weight regular :inverse-video nil)
     (?C :inherit success :weight regular :inverse-video nil)))

  ;; ---- 属性与列视图 ----
  ;; 标题行全局属性设置
  ;; 风险相关属性的候选值集中在此, 供 org-set-property 与 capture 补全;
  ;; 旧 RISK_ALL 的英文三档 (Low/Medium/High) 与模板 :LEVEL: 的中文不一致, 已废弃.
  (org-global-properties '(("EFFORT_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 7:00 8:00")
                           ("APPT_WARNTIME_ALL" . "0 5 10 15 20 25 30 45 60")
                           ;; 等级属性用下划线而非连字符: agenda 搜索语法只认
                           ;; [A-Za-z0-9_], 且 LEVEL 是 org 搜索保留字, 故 RISK_LEVEL.
                           ("RISK_LEVEL_ALL" . "高 中 低")
                           ("PROBABILITY_ALL" . "高 中 低")
                           ("IMPACT_ALL" . "高 中 低")
                           ("RESPONSE_ALL" . "规避 减轻 转移 接受")
                           ("ORIGIN_ALL" . "需求 设计 接口 器件 试验 评审 进度 资源 人员 其他")
                           ("CONCLUSION_ALL" . "闭环 挂账 转议题 误报")
                           ("STYLE_ALL" . "habit")))
  ;; Org columns 的默认格式
  (org-columns-default-format "%25ITEM %TODO %SCHEDULED %DEADLINE %3PRIORITY %TAGS %CLOCKSUM %EFFORT{:}")

  ;; ---- 状态记录 ----
  ;; 当状态从 DONE 改成其他状态时，移除 CLOSED: [timestamp]
  (org-closed-keep-when-no-todo t)
  ;; DONE 时加上时间戳
  (org-log-done 'time)
  ;; 重复执行时加上时间戳
  (org-log-repeat 'time)
  ;; Deadline 修改时加上一条记录
  (org-log-redeadline 'note)
  ;; Schedule 修改时加上一条记录
  (org-log-reschedule 'note)
  ;; 以抽屉的方式记录
  (org-log-into-drawer t)
  ;; 紧接着标题行或者计划/截止时间戳后加上记录抽屉
  (org-log-state-notes-insert-after-drawers nil)

  ;; ---- refile ----
  ;; refile 使用缓存
  (org-refile-use-cache t)
  ;; refile 的目的地，这里设置的是 agenda 文件的所有标题
  (org-refile-targets '((org-agenda-files . (:maxlevel . 8))))
  ;; 将文件名加入到路径
  (org-refile-use-outline-path 'file)
  ;; 是否按步骤 refile
  (org-outline-path-complete-in-steps nil)
  ;; 允许创建新的标题行，但需要确认
  (org-refile-allow-creating-parent-nodes 'confirm)

  ;; ---- 归档 ----
  (org-archive-location "%s_archive::datetree/")

  ;; ---- LaTeX 预览 ----
  ;; 打开文件即预览公式（agenda 预建缓冲会跳过，见 my/org-agenda-prepare-buffers-with-preview）
  (org-startup-with-latex-preview t)
  ;; 预览走 xelatex -> dvisvgm：内置的 latex+dvipng 流程不含中文支持，
  ;; 依赖 texlive + texlive-langchinese（见 README.org）
  (org-preview-latex-default-process 'xelatex)
  (org-preview-latex-process-alist
   '((xelatex
      :programs ("xelatex" "dvisvgm")
      :description "XeLaTeX -> SVG"
      :message "Install xelatex and dvisvgm."
      :image-input-type "xdv"
      :image-output-type "svg"
      :image-size-adjust (1.7 . 1.5)
      :latex-compiler
      ("xelatex -no-pdf -interaction=nonstopmode -halt-on-error -output-directory=%o %f")
      :image-converter
      ("dvisvgm %f --no-fonts --exact-bbox --scale=%S --output=%O"))))
  ;; 中文支持：org 默认的 LaTeX 头只有 amsmath/fontspec，中文字符会整片缺失
  ;; （xelatex 逐字符报 Missing character，渲染成空白）。ctex 被注入头部
  ;; [PACKAGES] 位置，公式预览与 LaTeX 导出同时生效。
  (org-latex-packages-alist '(("" "ctex" t)))
  ;; 预览图片缓存到 ~/Org/assets/（与 images/ 同级；agenda 扫描本就过滤 assets/），
  ;; 图片名是内容哈希，多个 org 文件共用同一目录不会冲突
  (org-preview-latex-image-directory
   (expand-file-name "assets/org-latex-preview/" org-directory))
  :config
  ;; 预览样式：放大倍数、颜色跟随当前主题、SVG 背景透明。
  ;; 只覆盖需要改的键，保留 org 默认的 :matchers 等。
  ;; 尺寸一律用 :scale 调（原 1.8，现 0.8）：它参与预览缓存的哈希，
  ;; 改动会自动重渲染；而 :image-size-adjust 不在哈希里，改它会命中旧缓存不生效。
  (setf (plist-get org-format-latex-options :scale) 0.8)
  (setf (plist-get org-format-latex-options :foreground) 'default)
  (setf (plist-get org-format-latex-options :background) "Transparent"))

;;; 4. 日程 Agenda -------------------------------------------------------------

(use-package org-agenda
  :ensure nil
  :after org
  :custom
  ;; ---- 显示 ----
  ;; 显示时间线
  (org-agenda-use-time-grid t)
  ;; 面包屑分隔符
  (org-agenda-breadcrumbs-separator " ❱ ")
  ;; 时间线的当前时间指示串
  (org-agenda-current-time-string "now ----------------------------")
  ;; 时间线范围和颗粒度设置
  (org-agenda-time-grid '((daily today)
                          (0600 0800 1000 1200
                                1400 1600 1800
                                2000 2200 2400)
                          "......" "--------------------------------"))
  ;; 日程视图的前缀设置
  (org-agenda-prefix-format '((agenda . " %i %-25:c %5t %s")
                              (todo   . " %i %-25:c ")
                              (tags   . " %i %-25:c ")
                              (search . " %i %-25:c ")))
  ;; 对于计划中的任务在视图里的显示
  (org-agenda-scheduled-leaders '("       计划 " "%3d天前开始 "))
  ;; 对于截止日期的任务在视图里的显示
  (org-agenda-deadline-leaders '("       截止 " "%3d天后截止 " "%3d天前过期 "))
  ;; 不同日程类别间的间隔
  (org-agenda-compact-blocks nil)
  ;; 日程视图的块分隔符
  (org-agenda-block-separator ?─)
  ;; 标签显示的位置，第 100 列往前右对齐
  (org-agenda-tags-column -100)

  ;; ---- 数据 ----
  ;; 需要被日程监控的 org 文件
  (org-agenda-files
   (append
    (list (expand-file-name "diary.org" org-directory)
          (expand-file-name "plan.org" org-directory)
          (expand-file-name "inbox.org" org-directory)
          (expand-file-name "work.org" org-directory))
    (seq-filter
     (lambda (f)
       ;; 过滤 archive/assets/trash 目录
       (not (string-match-p "/\\(archive\\|assets\\|trash\\)/" f)))
     (directory-files-recursively (expand-file-name "projects" org-directory) "\\.org$"))))
  ;; org 的日记文件
  (org-agenda-diary-file (expand-file-name "diary.org" org-directory))
  ;; 日记插入精确时间戳
  (org-agenda-insert-diary-extract-time t)

  ;; ---- 行为 ----
  ;; 日视图还是周视图，通过 v-d, v-w, v-m, v-y 切换视图，默认日视图
  (org-agenda-span 'day)
  ;; q 退出时删除 agenda 缓冲区
  (org-agenda-sticky t)
  ;; 是否包含直接日期
  (org-agenda-include-deadlines t)
  ;; 显示每一天，不管有没有条目
  (org-agenda-show-all-dates t)
  ;; 时间不足位时前面加 0
  (org-agenda-time-leading-zero t)
  ;; 日程同时启动 log mode
  (org-agenda-start-with-log-mode t)
  ;; 日程同时启动任务时间记录报告模式
  (org-agenda-start-with-clockreport-mode t)
  ;; 截止的任务完成后不显示
  (org-agenda-skip-deadline-if-done t)
  ;; 当计划的任务完成后不显示
  (org-agenda-skip-scheduled-if-done t)
  ;; 计划过期上限
  (org-scheduled-past-days 365)
  ;; 计划截止上限
  (org-deadline-past-days 365)
  ;; 计划中的任务不提醒截止时间
  (org-agenda-skip-deadline-prewarning-if-scheduled 1)
  (org-agenda-skip-scheduled-if-deadline-is-shown t)
  (org-agenda-skip-timestamp-if-deadline-is-shown t)
  ;; 工时记录报告格式
  (org-agenda-clockreport-parameter-plist
   '(:link t :maxlevel 8 :compact nil :narrow 80 :timestamp t :fileskip0 t))
  (org-agenda-columns-add-appointments-to-effort-sum t)
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-window-setup 'current-window)
  ;; 从星期一开始作为一周第一天
  (org-agenda-start-on-weekday 1)
  ;; 不使用 am/pm
  (org-agenda-timegrid-use-ampm nil)
  ;; 搜索时不看时间
  (org-agenda-search-headline-for-time nil)
  ;; 提前 3 天截止日期到期告警
  (org-deadline-warning-days 3)
  ;; 在日程视图的标签继承
  (org-agenda-use-tag-inheritance t))

;; 图片首开不显示的根因与修复：
;; `org-agenda-prepare-buffers'（agenda/refile/时钟等预扫描文件时调用）会绑定
;; `org-inhibit-startup'，其值取自 `org-agenda-inhibit-startup'；为 t 时整个 org
;; 启动块被跳过，其中就包括 `org-startup-with-inline-images' 触发的图片预览
;; （Org 9.8 走 `org-link-preview'，overlay + 空闲定时器队列）。
;; 之后 find-file 复用这个预建缓冲时不会重跑启动 ⇒ 第一次打开看不到图片；
;; 关掉缓冲重开（新建缓冲）才跑完整启动 ⇒ 有图。
;; 这里恢复启动以生成图片预览与折叠（与正常打开文件一致），但单独关掉 LaTeX
;; 预览：它要调 xelatex 逐片渲染，随 agenda 批量扫描执行会明显卡顿，
;; 公式仍只在真正打开文件时预览（或手动 C-c C-x C-l）。
(defun my/org-agenda-prepare-buffers-with-preview (old-fun files)
  "用 OLD-FUN 处理 FILES：生成图片预览与折叠，跳过昂贵的 LaTeX 预览。"
  (let ((org-inhibit-startup nil)
        (org-startup-with-latex-preview nil))
    (funcall old-fun files)))
(advice-add 'org-agenda-prepare-buffers :around
            #'my/org-agenda-prepare-buffers-with-preview)

;; 自定义视图
(use-package org-super-agenda
  :ensure t
  :after org
  :init
  ;; 防止 agenda buffer 中出现折叠错乱
  (setq org-super-agenda-header-map nil)
  :config
  (org-super-agenda-mode 1)
  :custom
  (org-agenda-custom-commands
   '(("w" "Workflow Agenda"
      ((agenda ""
               ((org-agenda-span 'day)
                (org-super-agenda-groups
                 '((;; 今日完成任务
                    :name "◎ 今日完成" :log t)

                   ;; 日程任务
                   (:name "◎ 日程任务 ➜ 可执行🄝" :todo "NEXT")
                   (:name "◎ 日程任务 ➜ 待规划🄣" :todo "TODO")
                   (:name "◎ 日程任务 ➜ 等待中🄦" :todo "WAIT")

                   ;; 问题处理
                   (:name "◎ 问题处理 ➜ 新报告🄡" :todo "REPORT")
                   (:name "◎ 问题处理 ➜ 缺陷问题🄑" :todo "BUG")
                   (:name "◎ 问题处理 ➜ 已知问题🄚" :todo "KNOWNCAUSE")
                   (:name "◎ 问题处理 ➜ 待关闭🄕" :todo "FIXED")

                   ;; 代码控制
                   (:name "◎ 代码控制 ➜ 设计🄢" :todo "DESIGN")
                   (:name "◎ 代码控制 ➜ 开发🄓" :todo "DEVELOP")
                   (:name "◎ 代码控制 ➜ 已确认🄒" :todo "CONFIRMED")

                   ;; 其他
                   (:discard (:anything t))))))))

     ("W" "Workflow All Todo"
      ((alltodo ""
                ((org-agenda-span 'day)
                 (org-super-agenda-groups
                  '(;; 门禁: 门也是 TODO, 必须排在通用 TODO 组之前
                    (:name "◎ 门禁 ➜ 待关闭" :tag "gate")

                    ;; 日程任务
                    (:name "◎ 日程任务 ➜ 可执行🄝" :todo "NEXT")
                    (:name "◎ 日程任务 ➜ 待规划🄣" :todo "TODO")
                    (:name "◎ 日程任务 ➜ 等待中🄦" :todo "WAIT")

                    ;; 问题处理
                    (:name "◎ 问题处理 ➜ 新报告🄡" :todo "REPORT")
                    (:name "◎ 问题处理 ➜ 缺陷问题🄑" :todo "BUG")
                    (:name "◎ 问题处理 ➜ 已知问题🄚" :todo "KNOWNCAUSE")
                    (:name "◎ 问题处理 ➜ 待关闭🄕" :todo "FIXED")

                    ;; 代码控制
                    (:name "◎ 代码控制 ➜ 设计🄢" :todo "DESIGN")
                    (:name "◎ 代码控制 ➜ 开发🄓" :todo "DEVELOP")
                    (:name "◎ 代码控制 ➜ 已确认🄒" :todo "CONFIRMED")

                    ;; 风险跟踪
                    (:name "◎ 风险 ➜ 待评估" :todo "IDENT")
                    (:name "◎ 风险 ➜ 措施中" :todo "MITIGATE")

                    ;; 其他
                    (:discard (:anything t))))))))

     ("R" "风险台账" tags "RISK_LEVEL=\"高\""
      ((org-agenda-overriding-header "高等级风险台账")
       (org-super-agenda-groups
        '((:name "◎ 待评估" :todo "IDENT")
          (:name "◎ 措施中" :todo "MITIGATE")
          (:name "◎ 挂账" :property ("CONCLUSION" "挂账"))
          (:name "◎ 已了结" :anything t))))))))

;; ---- appt 通知提醒 ----
(defun appt-display-with-notification (min-to-app new-time appt-msg)
  (notifications-notify :title (format "Appointment in %s minutes" min-to-app)
                        :body appt-msg
                        :urgency 'critical)
  (appt-disp-window min-to-app new-time appt-msg))

;; 每 15 分钟更新一次 appt
(run-at-time t 900 #'org-agenda-to-appt)
;; 不显示日期
(setq appt-display-diary nil)
;; 提醒间隔时间，每 15 分钟提醒一次
(setq appt-display-interval 15)
;; modeline 显示提醒
(setq appt-display-mode-line t)
;; 设置提醒响铃
(setq appt-audible t)
;; 提前 30 分钟提醒
(setq appt-message-warning-time 30)
;; linux 下同时在 emacs 内部和系统通知中提醒; 其他环境下仅在 emacs 中提醒
(when (eq system-type 'gnu/linux)
  (setq appt-disp-window-function #'appt-display-with-notification))
;; 激活提醒
(appt-activate 1)
;; 自动同步 org-agenda 文件到 appt
(add-hook 'org-agenda-finalize-hook #'org-agenda-to-appt)

;;; 5. 捕获 Capture ------------------------------------------------------------

;; 通用骨架: 统一的创建时间属性抽屉, 避免各模板重复书写
(defconst my/org-capture--created
  "  :PROPERTIES:\n  :CREATED: %U\n  :END:\n")

(defun my/org-capture-source-link (&optional label)
  "返回捕获来源链接块: \"\\n  LABEL 链接\\n\"; 无链接时返回 nil.
仅书签(rb)/随笔(cn)模板使用, 其他模板不带链接(需要时手动插入).
LABEL 为链接前缀, 默认 \"来源: \"; 传空串则直接插入链接本身."
  (let ((annotation (org-capture-get :annotation)))
    (when (and (stringp annotation)
               (not (member annotation '("" "[[]]"))))
      (concat "\n  " (or label "来源: ") annotation "\n"))))

(defun my/org-capture--top-headings (file)
  "返回 FILE 中的顶级标题列表."
  (with-current-buffer (org-capture-target-buffer file)
    (org-with-wide-buffer
     (goto-char (point-min))
     (let (headings)
       (while (re-search-forward "^\\* " nil t)
         (push (org-get-heading t t t t) headings))
       (nreverse headings)))))

(defun my/org-capture-plan-target ()
  "选择 plan.org 的计划类别并移动到对应顶级标题, 不存在则创建."
  (set-buffer (org-capture-target-buffer "plan.org"))
  (widen)
  (let ((heading (completing-read
                  "计划类别: "
                  (delete-dups (append '("Plan" "Long-term Plan")
                                       (my/org-capture--top-headings "plan.org")))
                  nil nil nil nil "Plan")))
    (goto-char (point-min))
    (if (re-search-forward (concat "^\\* " (regexp-quote heading) "[ \t]*$") nil t)
        (forward-line 0)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert "* " heading "\n")
      (forward-line -1))))

(defun my/org-capture-setup ()
  "捕获缓冲区内提供所有 agenda 标签候选."
  (setq-local org-complete-tags-always-offer-all-agenda-tags t)
  ;; 每次捕获开始清空上次的门记录, 避免中止的 pe 留下陈旧状态
  (setq my/org-capture--review-gate nil))

;; ---- 项目管理 Project ------------------------------------------------------
;; 软件配置项项目文件布局: projects/<工程代号>/<配置项代号>.org, 工程代号即
;; projects/ 子目录名(pp 创建时可选现有或新建), 骨架由 pp 模板生成。
;; 研制阶段(方案设计/样机研制/工程研制/状态鉴定)任一时刻唯一, 记录在「研制过程」标题的
;; :CURRENT-STATE: 属性; 阶段任务、测试/试验记录、软件状态都创建在当前阶段
;; 子树下。门禁在「里程碑」章: 转段门用 :FROM:/:TO: 承载阶段转换记录并由
;; 阶段切换自动结算; 冻结门与需求基线版本同号, 由圈定基线时创建。
(defconst my/org-project--stages '("方案设计" "样机研制" "工程研制" "状态鉴定")
  "研制阶段固定划分.")

(defun my/org-project--state ()
  "返回当前条目的 TODO 状态 (去文本属性), 无状态返回 nil.
org-get-todo-state 带 face 属性, 直接与字符串比较会失败."
  (let ((state (org-get-todo-state)))
    (and state (substring-no-properties state))))

(defconst my/org-project--closed-todos
  '("DONE" "CANCELLED" "ARCHIVED" "DEPRECATED" "CONFIRMED" "CLOSED" "WONTFIX")
  "视为已收尾的 TODO 终态 (收尾检查与基线圈定共用).")

(defun my/org-project--codes ()
  "返回 projects/ 下的工程代号 (子目录名) 列表."
  (let ((dir (expand-file-name "projects" org-directory))
        codes)
    (when (file-directory-p dir)
      (dolist (f (directory-files dir t "\\`[^.]"))
        (when (file-directory-p f)
          (push (file-name-nondirectory f) codes))))
    (sort codes #'string<)))

(defun my/org-project--files ()
  "返回软件配置项项目文件列表, 每项为 (显示名 . 绝对路径).
显示名取 projects/ 下的相对路径, 模板文件除外."
  (let ((dir (expand-file-name "projects" org-directory))
        (skip '("project_templete" "software_project_template"))
        files)
    (when (file-directory-p dir)
      (dolist (f (directory-files-recursively dir "\\.org\\'"))
        (let ((name (file-name-base f)))
          (unless (member name skip)
            (push (cons (file-name-sans-extension (file-relative-name f dir))
                        (expand-file-name f))
                  files)))))
    (nreverse files)))

(defun my/org-capture-project-file ()
  "选择软件配置项项目文件 (每次都选)."
  (let* ((files (my/org-project--files))
         (choice (completing-read "项目(配置项): "
                                  (mapcar #'car files) nil t)))
    (or (cdr (assoc choice files))
        (user-error "项目文件不存在: %s" choice))))

(defvar my/org-capture--last-buffer nil
  "最近一次捕获的目标项目缓冲区.
模板 %(...) 在临时缓冲区展开, 依赖项目上下文的函数经此回退定位项目文件.")

(defun my/org-capture--project-buffer ()
  "返回本次捕获的目标项目缓冲区.
优先取当前缓冲区(直接在项目文件中调用时), 否则用目标函数记录的缓冲区.
注意 buffer-base-buffer 对非 indirect 缓冲区返回 nil, 需回落到缓冲区本身."
  (let ((cur (current-buffer)))
    (cond
     ((and (buffer-file-name cur) (derived-mode-p 'org-mode))
      (or (buffer-base-buffer cur) cur))
     ((buffer-live-p my/org-capture--last-buffer)
      (let ((lb my/org-capture--last-buffer))
        (or (buffer-base-buffer lb) lb))))))

(defun my/org-capture--code ()
  "返回本次捕获目标文件的项目代号 (文件名去后缀)."
  (let ((file (buffer-file-name (my/org-capture--project-buffer))))
    (if file (file-name-base file)
      (user-error "无法确定项目代号: 未找到目标项目文件"))))

(defun my/org-capture--next-id (prefix)
  "生成项目内递增 ID: 代号_PREFIX-NNN.
在目标项目文件中扫描既有编号取最大值加一 (模板展开在临时缓冲区, 需回退到项目文件)."
  (let* ((buf (my/org-capture--project-buffer))
         (code (my/org-capture--code))
         (re (concat (regexp-quote (concat code "_" prefix "-")) "\\([0-9]+\\)"))
         (max 0))
    (with-current-buffer buf
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (while (re-search-forward re nil t)
            (setq max (max max (string-to-number (match-string 1))))))))
    (format "%s_%s-%03d" code prefix (1+ max))))

(defun my/org-project--plan-subtree ()
  "生成「方案设计」阶段子树文本 (方案阶段专属结构)."
  (concat
   "*** 需求分析\n\n"
   "    #+BEGIN: req-by-phase :phase 方案设计\n"
   "    #+END:\n\n"
   "    - 入口条件：\n"
   "    - 阶段计划：\n"
   "    - 技术要求梳理（功能/性能/接口/其他需求用 pn 录入「需求」章）：\n"
   "    - 需求变更记录（pc 捕获，落在「需求」章「需求变更记录」表）：\n"
   "    - 需求基线冻结（B1，C-c P b 圈定，评审记入「里程碑」章 review-B1）：\n\n"
   "*** 总体方案设计\n\n"
   "    - 技术路线：\n"
   "    - 系统架构/软件配置项划分：\n"
   "    - 技术选型（含成熟度初判）：\n"
   "    - 接口方案：\n\n"
   "*** 通用质量特性与约束\n\n"
   "    - 可靠性/安全性/测试性/环境适应性初步分析：\n"
   "    - 设计约束与假设：\n"
   "    - 标准规范符合性（研制依据/标准剪裁）：\n\n"
   "*** 技术成熟度分析与验证\n\n"
   "    - 成熟度分析（不成熟技术清单）：\n\n"
   "    | 序号 | 技术项 | 成熟度 | 风险 | 验证计划 | 验证结论 |\n"
   "    |------+--------+--------+------+----------+----------|\n\n"))

(defun my/org-project--generic-subtree (stage)
  "生成通用阶段子树文本: STAGE 阶段名 (策划与需求/设计与实现/测试与验证/软件状态)."
  (concat
   "*** 策划与需求\n\n"
   "    #+BEGIN: req-by-phase :phase " stage "\n"
   "    #+END:\n\n"
   "    - 入口条件：\n"
   "    - 阶段计划：\n"
   "    - 本阶段需求变更（pc 捕获，落在「需求」章「需求变更记录」表）：\n\n"
   "*** 设计与实现\n\n"
   "    - 设计（架构/详细设计）：\n"
   "    - 代码走查/静态检查：\n"
   "    - 单元测试记录（登记于「测试与试验记录」章）：\n\n"
   (when (string= stage "样机研制")
     (concat
      "*** 样机调试\n\n"
      "    - 调试环境（硬件/仪器/工具链）：\n"
      "    - 板级支持与接口适配：\n"
      "    - 联调记录（登记于「测试与试验记录」章）：\n"
      "    - 异常记录（闭环进「议题」章，记议题编号）：\n\n"))
   (when (string= stage "工程研制")
     (concat
      "*** 环境与可靠性验证\n\n"
      "    - 环境条件要求（高低温/振动/冲击/EMC）：\n"
      "    - 可靠性/安全性验证安排：\n"
      "    - 环境试验记录（登记于「测试与试验记录」章）：\n"
      "    - 问题闭环（闭环进「议题」章，记议题编号）：\n\n"))
   "*** 测试与验证\n\n"
   (unless (member stage '("样机研制" "工程研制")) "    - 对接环境：\n")
   "    - 回归测试说明：\n"
   "    - 测试/试验记录（登记于「测试与试验记录」章）：\n"
   (unless (member stage '("样机研制" "工程研制"))
     "    - 异常记录（闭环进「议题」章，记议题编号）：\n")
   "\n"
   "*** 软件状态\n\n"
   "    版本条目用 ps 捕获, 状态流 DESIGN → DEVELOP → CONFIRMED → ARCHIVED.\n"))

(defun my/org-project--qual-subtree ()
  "生成「状态鉴定」阶段子树文本 (鉴定阶段专属结构)."
  (concat
   "*** 鉴定策划\n\n"
   "    - 鉴定依据（研制任务书/技术要求）：\n"
   "    - 鉴定大纲与计划：\n"
   "    - 状态冻结安排：\n\n"
   "*** 状态管理\n\n"
   "    - 版本定版（软件状态条目在此登记）：\n"
   "    - 配置项基线：\n"
   "    - 技术状态更改控制：\n\n"
   "*** 鉴定测试与试验\n\n"
   "    - 鉴定测试记录（登记于「测试与试验记录」章）：\n"
   "    - 鉴定试验记录（登记于「测试与试验记录」章）：\n"
   "    - 异常闭环（闭环进「议题」章，记议题编号）：\n\n"
   "*** 鉴定材料\n\n"
   "    # 鉴定需提供的材料清单(齐套性核对), 状态列填 已提供/待提供;\n"
   "    # 文档定版与归档在鉴定前完成, 本表只核对齐套性与版本.\n\n"
   "    | 序号 | 材料名称 | 版本 | 状态 | 备注 |\n"
   "    |------+----------+------+------+------|\n"
   "    | 1 | 软件研制任务书 |  |  |  |\n"
   "    | 2 | 软件开发计划 |  |  |  |\n"
   "    | 3 | 软件需求规格说明 |  |  |  |\n"
   "    | 4 | 软件设计说明 |  |  |  |\n"
   "    | 5 | 软件配置项测试报告 |  |  |  |\n"
   "    | 6 | 软件研制总结报告 |  |  |  |\n"
   "    | 7 | 软件源代码或目标码 |  |  |  |\n"))

(defun my/org-project--stage-subtree (code idx stage)
  "生成阶段子树文本: CODE 项目代号, IDX 阶段序号, STAGE 阶段名.
方案设计/状态鉴定阶段用专属子结构, 样机研制/工程研制用通用四节+特征小节."
  (concat
   "** " stage "\n"
   "   :PROPERTIES:\n"
   "   :CUSTOM_ID: " code "_STAGE-" (number-to-string idx) "\n"
   "   :END:\n\n"
   (cond
    ((string= stage "方案设计") (my/org-project--plan-subtree))
    ((string= stage "状态鉴定") (my/org-project--qual-subtree))
    (t (my/org-project--generic-subtree stage)))))

(defun my/org-project--stage-sections (stage)
  "返回阶段 STAGE 的小节名列表 (pt 捕获落点候选)."
  (cond
   ((string= stage "方案设计")
    '("需求分析" "总体方案设计" "通用质量特性与约束" "技术成熟度分析与验证"))
   ((string= stage "样机研制")
    '("策划与需求" "设计与实现" "样机调试" "测试与验证" "软件状态"))
   ((string= stage "工程研制")
    '("策划与需求" "设计与实现" "环境与可靠性验证" "测试与验证" "软件状态"))
   ((string= stage "状态鉴定")
    '("鉴定策划" "状态管理" "鉴定测试与试验" "鉴定材料"))
   (t
    '("策划与需求" "设计与实现" "测试与验证" "软件状态"))))

(defun my/org-project--stages-text (code)
  "生成全部阶段子树文本 (按 my/org-project--stages 顺序)."
  (let ((i 0) parts)
    (dolist (stage my/org-project--stages)
      (setq i (1+ i))
      (push (my/org-project--stage-subtree code i stage) parts))
    (mapconcat #'identity (nreverse parts) "\n")))

;; 风险章正文 (不含标题行): 骨架生成与存量升级共用同一份文本, 避免模板分叉。
;; 章节结构 = 台账视图(矩阵/本阶段/汇总) + 评审记录 + 阶段出口准则;
;; 条目只在「风险」章建档, 阶段子树与评审处只放动态块引用。
(defconst my/org-project--risk-body
  (concat
   "# 风险 = 未发生的不确定事件; 已发生的转议题, 不双轨跟踪.\n"
   "# 流程: pr 识别(IDENT) → C-c P r 评估确认(MITIGATE) → 措施执行/监控\n"
   "#       → 置 RESOLVED, 并在 :CONCLUSION: 写明 闭环|挂账|转议题|误报.\n"
   "# 已发生用 C-c P x 转议题(双向链接); 缺 :ORIGIN-LINK: 的条目在汇总中标红.\n"
   "# 措施用 checkbox 子项: org-enforce-todo-checkbox-dependencies 会阻止\n"
   "# 措施未勾完就置终态(挂账时勾选并注明不执行).\n\n"
   "** 风险矩阵\n\n"
   "    #+BEGIN: risk-matrix\n"
   "    #+END:\n\n"
   "** 本阶段风险\n\n"
   "    #+BEGIN: risk-by-phase :phase current\n"
   "    #+END:\n\n"
   "** 风险台账\n\n"
   "    #+BEGIN: risk-summary\n"
   "    #+END:\n\n"
   "** 风险评审记录\n\n"
   "    #+NAME: risk-review\n"
   "    | 日期 | 风险ID | 原等级 | 新等级 | 结论 | 评审人 |\n"
   "    |------+--------+--------+--------+------+--------|\n\n"
   "** 阶段出口准则\n\n"
   "    | 阶段     | 风险出口要求 |\n"
   "    |----------+--------------|\n"
   "    | 方案设计 | IDENT 清零; 高风险须 MITIGATE 且带验证计划 |\n"
   "    | 样机研制 | 高风险 RESOLVED(闭环/挂账); 中风险措施在执行 |\n"
   "    | 工程研制 | 高/中风险 RESOLVED; 新风险须有 OWNER 与 TRIGGER |\n"
   "    | 状态鉴定 | 全部终态; 挂账风险转入交付遗留清单 |\n")
  "风险章正文 (骨架与存量升级共用).")

(defun my/org-project--risk-chapter ()
  "返回完整风险章文本 (标题行 + 正文)."
  (concat "* 风险\n\n" my/org-project--risk-body))

(defun my/org-project--skeleton (code name product)
  "生成项目文件骨架文本: CODE 配置项代号, NAME 项目名称, PRODUCT 产品代号."
  (concat
   "#+title: " name "\n"
   "#+author: 王北洛\n"
   "#+category: " code "\n"
   "#+description: " name " 研制项目管理\n"
   "#+startup: overview\n"
   "#+filetags: :project:\n\n"
   "# 使用说明:\n"
   "# - 本文件由 capture 模板 pp 生成, 日常信息通过 C-c c 的 p 组模板录入;\n"
   "# - 当前研制阶段唯一, 记录在「研制过程」的 :CURRENT-STATE: 属性;\n"
   "#   转段用 C-c P s: 结算门链(冻结门 + 转段门) → 更新属性 → 记日志;\n"
   "# - 阶段结构按阶段特征定制: 方案设计(需求/总体/质量特性/成熟度),\n"
   "#   样机研制(+样机调试), 工程研制(+环境与可靠性验证),\n"
   "#   状态鉴定(鉴定策划/状态管理/鉴定测试/鉴定材料);\n"
   "# - 状态流: 日常 TODO→NEXT→WAIT→DONE, 议题 REPORT→…→CLOSED,\n"
   "#   软件状态 DESIGN→DEVELOP→CONFIRMED→ARCHIVED;\n"
   "# - 需求条目在「需求」章统一建档(:PHASE: 可多值), 各阶段的动态块\n"
   "#   自动引用, 刷新用 C-c C-x C-u;\n"
   "# - 门禁在「研制过程」>「里程碑」章: 转段门 M1..M4 由 C-c P s 结算,\n"
   "#   冻结门 B<k> 与基线版本同号(B1=RB1), 同一阶段可有多道;\n"
   "# - 需求基线在「需求」章「需求基线」节: C-c P b 全量重圈并建冻结门,\n"
   "#   评审用 pe 记入 review-B<k>, 关门用 C-c P g; 变更用 pc 捕获;\n"
   "# - 评审记录统一记在「里程碑」各门的 review-<ID> 表内(pe 捕获);\n"
   "# - 软件状态条目(ps)记在各阶段「软件状态」节, 状态鉴定阶段记在「状态管理」;\n"
   "# - 风险: pr 识别(IDENT) → C-c P r 评估确认(MITIGATE) → RESOLVED 了结,\n"
   "#   已发生用 C-c P x 转议题; 汇总/矩阵用 C-c P R 刷新查看.\n\n"
   "* 概述\n\n"
   "** " name "\n"
   "   :PROPERTIES:\n"
   "   :CUSTOM_ID: " product "\n"
   "   :END:\n\n"
   "   | 条目       | 内容 | 备注           |\n"
   "   |------------+------+----------------|\n"
   "   | 产品名称   | " name " |                |\n"
   "   | 产品代号   | " product " |                |\n"
   "   | 硬件平台   |      | MCU/板卡型号   |\n"
   "   | 运行环境   |      | 裸机/OS 及版本 |\n"
   "   | 语言工具链 |      | 编译器、调试器 |\n\n"
   "# 本标题为项目引用锚点(CUSTOM_ID=产品代号), 其他项目引用写法:\n"
   "# [[file:projects/工程代号/配置项代号.org#" product "][" name "]]\n\n"
   "* 研制过程\n"
   "   :PROPERTIES:\n"
   "   :CURRENT-STATE: 方案设计\n"
   "   :END:\n\n"
   "# 当前阶段唯一, 由 :CURRENT-STATE: 记录; 门禁在下方「里程碑」章:\n"
   "# 转段门 :FROM:/:TO: 承载阶段转换, 冻结门按基线版本登记, 见 gate-summary.\n\n"
   (my/org-project--gates-text code)
   "** 需求\n\n"
   "# 需求条目在此统一建档（唯一编辑处）, 用 pn 捕获; :PHASE: 取值 = 阶段名\n"
   "# (方案设计/样机研制/工程研制/状态鉴定, 可多值) 或 通用; 各阶段的动态块\n"
   "# 自动引用, 刷新用 C-c C-x C-u. 追踪矩阵在评审前逐条勾稽.\n\n"
   "*** 功能需求\n\n"
   "*** 性能需求\n\n"
   "*** 接口需求\n\n"
   "*** 其他需求\n\n"
   "*** 需求基线\n\n"
   "    # 基线流程(全量重圈): pn 录入需求 → C-c P b 追加 :BASELINE: 新版本\n"
   "    # → 登记基线记录表 → 建/绑冻结门 B<k> → pe 记评审 → C-c P g 关门.\n"
   "    # 同一阶段可建多个基线, 版本号递增; 变更用 pc 捕获(移出基线).\n\n"
   "    #+NAME: baseline-records\n"
   "    | 基线版本 | 阶段 | 日期 | 需求数 | 评审结论 | 备注 |\n"
   "    |----------+------+------+--------+----------+------|\n\n"
   "    #+BEGIN: req-baseline :version RB1\n"
   "    #+END:\n\n"
   "*** 需求变更记录\n\n"
   "    # 用 pc 捕获登记(选需求填变更内容); 被变更需求的 :BASELINE: 清空,\n"
   "    # 重新圈定后进入新基线版本.\n\n"
   "    #+NAME: req-changes\n"
   "    | 日期 | 需求ID | 变更类型 | 变更说明 | 原因 | 影响范围 | 处置 | 关联议题 |\n"
   "    |------+--------+----------+----------+------+------+------+----------|\n\n"
   "*** 追踪矩阵\n\n"
   "    #+BEGIN: req-matrix\n"
   "    #+END:\n\n"
   (my/org-project--stages-text code)
   "\n* 测试与试验记录\n\n"
   "# 各阶段测试/试验记录统一登记于此, 用 pl 捕获(阶段列自动填当前阶段);\n"
   "# 评审前按阶段过滤核对覆盖率.\n\n"
   "** 单元测试记录\n\n"
   "    #+NAME: unit-test\n"
   "    | 阶段 | 用例编号 | 测试项 | 指标要求 | 实测结果 | 结论 | 链接 |\n"
   "    |------+----------+--------+----------+----------+------+------|\n\n"
   "** 功能测试记录\n\n"
   "    #+NAME: func-test\n"
   "    | 阶段 | 用例编号 | 测试项 | 指标要求 | 实测结果 | 结论 | 链接 |\n"
   "    |------+----------+--------+----------+----------+------+------|\n\n"
   "** 性能测试记录\n\n"
   "    #+NAME: perf-test\n"
   "    | 阶段 | 用例编号 | 测试项 | 指标要求 | 实测结果 | 结论 | 链接 |\n"
   "    |------+----------+--------+----------+----------+------+------|\n\n"
   "** 接口测试记录\n\n"
   "    #+NAME: iface-test\n"
   "    | 阶段 | 用例编号 | 测试项 | 指标要求 | 实测结果 | 结论 | 链接 |\n"
   "    |------+----------+--------+----------+----------+------+------|\n\n"
   "** 系统测试记录\n\n"
   "    #+NAME: sys-test\n"
   "    | 阶段 | 日期 | 系统测试项 | 过程数据/现象 | 结论 | 链接 |\n"
   "    |------+------+------------+---------------+------+------|\n\n"
   "** 联调记录\n\n"
   "    #+NAME: debug-record\n"
   "    | 阶段 | 日期 | 联调项 | 过程数据/现象 | 结论 | 链接 |\n"
   "    |------+------+--------+---------------+------+------|\n\n"
   "** 环境试验记录\n\n"
   "    #+NAME: env-record\n"
   "    | 阶段 | 日期 | 试验项目 | 试验条件 | 结果/结论 | 链接 |\n"
   "    |------+------+----------+----------+-----------+------|\n\n"
   "** 鉴定测试记录\n\n"
   "    #+NAME: qual-test\n"
   "    | 阶段 | 用例编号 | 测试项 | 指标要求 | 实测结果 | 结论 | 链接 |\n"
   "    |------+----------+--------+----------+----------+------+------|\n\n"
   "** 试验记录\n\n"
   "    #+NAME: trial-record\n"
   "    | 阶段 | 日期 | 试验项目 | 过程数据/现象 | 结论 | 链接 |\n"
   "    |------+------+----------+---------------+------+------|\n\n"
   "\n* 议题\n\n"
   "# 问题/议题统一台账, 状态流 REPORT → BUG → KNOWNCAUSE → FIXED → CLOSED\n"
   "# (不修 WONTFIX); 调试/试验/交付后问题都记在此, :PHASE: 区分来源.\n\n"
   (my/org-project--risk-chapter)
   "\n* 日志\n\n"
   "# 日志与会议记录都记在此章, 用 pd 捕获(:TYPE: 日志|会议).\n"))

(defun my/org-project--align-tables ()
  "对齐当前缓冲区所有表格."
  (org-with-wide-buffer
   (goto-char (point-min))
   (while (re-search-forward "^[ \t]*|" nil t)
     (goto-char (line-beginning-position))
     (ignore-errors (org-table-align))
     (forward-line 1))))

(defun my/org-capture--goto-olp (file olp)
  "定位到 FILE 的标题路径 OLP, 缺失层级自动创建.
返回时 point 位于末级标题行首."
  (set-buffer (org-capture-target-buffer file))
  (setq my/org-capture--last-buffer (current-buffer))
  (widen)
  (let ((level 1)
        (beg (point-min))
        (end (point-max))
        pos)
    (dolist (heading olp)
      (let ((re (concat "^" (regexp-quote (make-string level ?*)) "[ \t]+"
                        (regexp-quote heading) "[ \t]*$")))
        (setq pos (save-restriction
                    (narrow-to-region beg end)
                    (goto-char beg)
                    (and (re-search-forward re nil t)
                         (line-beginning-position)))))
      (unless pos
        (goto-char end)
        (unless (bolp) (insert "\n"))
        (setq pos (line-beginning-position))
        (insert (make-string level ?*) " " heading "\n"))
      (goto-char pos)
      (setq beg (1+ pos))
      (setq end (save-excursion (org-end-of-subtree t t) (point)))
      (setq level (1+ level)))
    (goto-char pos)))

(defun my/org-capture--goto-named-table (file olp name)
  "定位到 FILE 标题路径 OLP 子树内名为 NAME 的表格, point 置于 #+NAME 行."
  (my/org-capture--goto-olp file olp)
  (let* ((beg (point))
         (end (save-excursion (org-end-of-subtree t t) (point)))
         (re (concat "^[ \t]*#\\+NAME:[ \t]+"
                     (regexp-quote name) "[ \t]*$")))
    (goto-char beg)
    (unless (re-search-forward re end t)
      (user-error "未找到表格 %s (章节: %s)" name (mapconcat #'identity olp " > ")))
    (goto-char (line-beginning-position))))

(defun my/org-capture--current-stage ()
  "返回本次捕获目标项目所处的研制阶段."
  (let ((buf (my/org-capture--project-buffer)))
    (if (not buf)
        (car my/org-project--stages)
      (with-current-buffer buf
        (org-with-wide-buffer
         (goto-char (point-min))
         (if (re-search-forward "^\\* 研制过程[ \t]*$" nil t)
             (or (org-entry-get (point) "CURRENT-STATE")
                 (car my/org-project--stages))
           (car my/org-project--stages)))))))

(defun my/org-capture--stage-prompt ()
  "提示需求所属阶段(可多值), 默认当前阶段."
  (let ((def (my/org-capture--current-stage)))
    (completing-read (format "阶段(可多值, 默认 %s): " def)
                     (append my/org-project--stages '("通用"))
                     nil nil nil nil def)))

(defvar my/org-capture--sw-title-value nil
  "捕获软件状态(ps)时输入的标题.")

(defvar my/org-capture--sw-code-value nil
  "捕获软件状态(ps)时输入的软件代号.")

(defvar my/org-capture--version-value nil
  "捕获软件状态(ps)时输入的版本号.")

(defvar my/org-capture--sw-desc-value nil
  "捕获软件状态(ps)时输入的版本简述.")

(defvar my/org-capture--trigger-value nil
  "捕获软件状态(ps)时选择的触发来源.")

(defun my/org-project--config-name ()
  "返回概述信息表中的产品名称 (软件状态标题默认值)."
  (let ((buf (my/org-capture--project-buffer)))
    (when buf
      (with-current-buffer buf
        (org-with-wide-buffer
         (goto-char (point-min))
         (when (re-search-forward "^\\* 概述[ \t]*$" nil t)
           (let ((end (save-excursion (org-end-of-subtree t t) (point))))
             (when (re-search-forward
                    "^[ \t]*|[ \t]*产品名称[ \t]*|[ \t]*\\([^|\n]+?\\)[ \t]*|" end t)
               (let ((v (string-trim (match-string 1))))
                 (unless (string= v "") v))))))))))

(defun my/org-capture--sw-prompt ()
  "提示 软件代号(默认配置项代号) → 版本号 → 标题 → 版本简述.
标题默认 配置项名称+软件/版本号; 返回标题文本作为条目标题,
其余值暂存供 CUSTOM_ID / 正文复用, 只问一次."
  (setq my/org-capture--sw-code-value
        (read-string "软件代号: " (my/org-capture--code)))
  (setq my/org-capture--version-value (read-string "版本号: "))
  (setq my/org-capture--sw-title-value
        (read-string "标题: "
                     (concat (or (my/org-project--config-name)
                                 (my/org-capture--code))
                             "软件/" my/org-capture--version-value)))
  (setq my/org-capture--sw-desc-value (read-string "版本简述: "))
  my/org-capture--sw-title-value)

(defun my/org-capture--sw-code ()
  "返回本次捕获输入的软件代号."
  (or my/org-capture--sw-code-value ""))

(defun my/org-capture--version ()
  "返回本次捕获输入的版本号 (供模板展开)."
  (or my/org-capture--version-value ""))

(defun my/org-capture--sw-desc ()
  "返回本次捕获输入的版本简述 (供模板展开)."
  (or my/org-capture--sw-desc-value ""))

(defun my/org-capture--sw-custom-id ()
  "生成软件状态 CUSTOM_ID: R+软件代号/版本号."
  (format "R%s/%s" (my/org-capture--sw-code) (my/org-capture--version)))

(defun my/org-capture--trigger ()
  "提示选择软件状态的触发来源."
  (setq my/org-capture--trigger-value
        (completing-read "触发来源: "
                         '("阶段任务" "需求变更" "设计改进" "问题归零" "硬件适配" "其他")
                         nil nil nil nil "阶段任务")))

(defun my/org-project--version-entries ()
  "返回项目内软件状态条目列表: ((CUSTOM_ID 标题) ...).
以 CUSTOM_ID 前缀 R_ 识别, 兼容旧条目的 :VERSION: 属性."
  (let ((buf (my/org-capture--project-buffer))
        entries)
    (when buf
      (with-current-buffer buf
        (org-with-wide-buffer
         (goto-char (point-min))
         (while (re-search-forward "^\\*+ " nil t)
           (let ((id (or (org-entry-get (point) "CUSTOM_ID") ""))
                 (legacy (org-entry-get (point) "VERSION")))
             (when (or (and (string-prefix-p "R" id)
                            (string-match-p "/" id))
                       legacy)
               (push (list id (org-get-heading t t t t)) entries)))))))
    (nreverse entries)))

(defun my/org-capture--affected-version ()
  "提示选择影响版本(软件状态标题), 返回指向该标题的链接; 也可直接输入新值."
  (let* ((entries (my/org-project--version-entries))
         (label (lambda (e)
                  (let* ((id (nth 0 e))
                         (ver (and (string-match "/\\(.+\\)\\'" id)
                                   (match-string 1 id))))
                    (if ver (format "%s（%s）" (nth 1 e) ver) (nth 1 e)))))
         (choice (completing-read "影响版本(选择标题或输入): "
                                  (mapcar label entries) nil nil)))
    (if (string= choice "")
        ""
      (let (found)
        (dolist (e entries)
          (when (string= choice (funcall label e))
            (setq found e)))
        (cond
         ((null found) choice)
         ((string= (nth 0 found) "") choice)
         ;; 链接显示文本只用标题, 不附加版本号(候选列表仍带版本以便区分)
         (t (format "[[#%s][%s]]" (nth 0 found) (nth 1 found))))))))

(defvar my/org-capture--req-type nil
  "捕获需求条目(pn)时选择的需求类型.")

(defun my/org-capture--req-type ()
  "返回捕获需求条目时选择的类型 (供模板展开)."
  (or my/org-capture--req-type "功能"))

(defconst my/org-capture--req-id-letters
  '(("功能" . "F") ("性能" . "P") ("接口" . "I") ("其他" . "O"))
  "需求类型到 ID 中缀的映射: ID 形如 代号_REQ-F-001, 按类型分段编号.")

(defun my/org-capture--req-id ()
  "按捕获时选择的需求类型生成需求 ID (供模板展开): REQ-<类型字母>-NNN."
  (let* ((type (my/org-capture--req-type))
         (letter (or (cdr (assoc type my/org-capture--req-id-letters)) "O")))
    (my/org-capture--next-id (concat "REQ-" letter))))

(defconst my/org-capture--record-tables
  '(("单元测试记录" . "unit-test")
    ("功能测试记录" . "func-test")
    ("性能测试记录" . "perf-test")
    ("接口测试记录" . "iface-test")
    ("系统测试记录" . "sys-test")
    ("联调记录" . "debug-record")
    ("环境试验记录" . "env-record")
    ("鉴定测试记录" . "qual-test")
    ("试验记录" . "trial-record"))
  "测试/试验记录表: 显示名 . #+NAME 标识.")

(defvar my/org-capture--record-kind nil
  "捕获记录表行(pl)时选择的记录表标识.")

(defun my/org-capture--record-row ()
  "按所选记录表生成一行表格 (首列为当前阶段, 末列为相关链接, 供模板展开)."
  (let ((stage (my/org-capture--current-stage))
        (date (format-time-string "[%Y-%m-%d %a]")))
    (pcase my/org-capture--record-kind
      ((or "unit-test" "func-test" "perf-test" "iface-test" "qual-test")
       (format "| %s | %s | %s | %s | %s | %s | %s |"
               stage (read-string "用例编号: ") (read-string "测试项: ")
               (read-string "指标要求: ") (read-string "实测结果: ")
               (read-string "结论: ")
               (string-trim (read-string "相关链接(可空): "))))
      ("sys-test"
       (format "| %s | %s | %s | %s | %s | %s |"
               stage date (read-string "系统测试项: ")
               (read-string "过程数据/现象: ") (read-string "结论: ")
               (string-trim (read-string "相关链接(可空): "))))
      ("debug-record"
       (format "| %s | %s | %s | %s | %s | %s |"
               stage date (read-string "联调项: ")
               (read-string "过程数据/现象: ") (read-string "结论: ")
               (string-trim (read-string "相关链接(可空): "))))
      ("env-record"
       (format "| %s | %s | %s | %s | %s | %s |"
               stage date (read-string "试验项目: ") (read-string "试验条件: ")
               (read-string "结果/结论: ")
               (string-trim (read-string "相关链接(可空): "))))
      (_
       (format "| %s | %s | %s | %s | %s | %s |"
               stage date (read-string "试验项目: ")
               (read-string "过程数据/现象: ") (read-string "结论: ")
               (string-trim (read-string "相关链接(可空): ")))))))

(defvar my/org-capture--log-type nil
  "捕获日志/会议(pd)时选择的类型.")

(defun my/org-capture--log-type ()
  "提示并返回日志条目类型 (供模板展开)."
  (setq my/org-capture--log-type
        (completing-read "类型: " '("日志" "会议") nil t nil nil "日志")))

(defun my/org-capture--log-fields ()
  "按类型返回日志/会议正文字段 (供模板展开)."
  (if (string= my/org-capture--log-type "会议")
      "  - 参会人：\n  - 议题：\n  - 决议：\n  - 待办："
    "  - 今日进展：\n  - 遇到问题：\n  - 明日计划："))

;; ---- 捕获目标 Capture targets ----
(defun my/org-capture-project-new-target ()
  "新建项目(pp): 选工程代号(现有或新建) → 建配置项文件骨架 → 定位「日志」章.
工程代号为 projects/ 下的子目录名, 输入新代号时自动新建文件夹;
产品代号作为「概述」中项目名称标题的 CUSTOM_ID, 供其他项目引用."
  (let* ((eng (completing-read "工程代号(现有或新建): "
                               (my/org-project--codes) nil nil))
         (dir (expand-file-name eng (expand-file-name "projects" org-directory))))
    (when (string= eng "") (user-error "工程代号不能为空"))
    (unless (file-directory-p dir)
      (make-directory dir t)
      (message "已新建工程目录: %s" dir))
    (let* ((code (read-string "配置项代号(文件名): " nil nil eng))
           (product (read-string "产品代号: "))
           (name (read-string "项目名称: " code))
           (file (expand-file-name (concat code ".org") dir)))
      (when (string= product "") (user-error "产品代号不能为空"))
      (set-buffer (org-capture-target-buffer file))
      (widen)
      ;; 文件已存在或缓冲区已有内容(上次未保存的骨架)时不重建
      (when (and (= (buffer-size) 0) (not (file-exists-p file)))
        (insert (my/org-project--skeleton code name product))
        (my/org-project--align-tables)
        (ignore-errors (org-update-all-dblocks)))
      (my/org-capture--goto-olp file '("日志")))))

(defun my/org-capture-task-target ()
  "研制任务(pt): 当前阶段指定小节下."
  (let* ((file (my/org-capture-project-file))
         (stage (my/org-capture--current-stage))
         (sections (my/org-project--stage-sections stage))
         (section (completing-read
                   (format "阶段 %s 的小节: " stage)
                   sections nil t nil nil (car sections))))
    (my/org-capture--goto-olp file (list "研制过程" stage section))))

(defun my/org-capture-record-target ()
  "测试/试验记录(pl): 统一章「测试与试验记录」的指定表, 阶段列自动填当前阶段."
  (let* ((file (my/org-capture-project-file))
         (label (completing-read "记录表: "
                                 (mapcar #'car my/org-capture--record-tables) nil t)))
    (setq my/org-capture--record-kind (cdr (assoc label my/org-capture--record-tables)))
    (my/org-capture--goto-named-table
     file '("测试与试验记录") my/org-capture--record-kind)))

(defvar my/org-capture--review-gate nil
  "本次 pe 捕获选中的门 ID, 供落表后提示关门.")

(defun my/org-capture-review-target ()
  "评审记录(pe): 从本文件的「里程碑」章选门, 落到 review-<ID> 表."
  (let ((file (my/org-capture-project-file)))
    (set-buffer (org-capture-target-buffer file))
    (setq my/org-capture--last-buffer (current-buffer))
    (widen)
    (let* ((gate (my/org-project--gate-select))
           (id (plist-get gate :id)))
      (setq my/org-capture--review-gate id)
      (my/org-project--gate-review-table-goto id))))

(defun my/org-capture-gate-review-finalize ()
  "pe 落表后: 若是冻结门且判据已满足, 询问是否关门."
  (when (and my/org-capture--review-gate
             (buffer-live-p my/org-capture--last-buffer))
    (with-current-buffer my/org-capture--last-buffer
      (let* ((id my/org-capture--review-gate)
             (gate (my/org-project--gate-get id)))
        (when (and gate
                   (equal (plist-get gate :type) "冻结")
                   (my/org-project--gate-open-p gate)
                   (let ((stats (my/org-project--gate-criteria id)))
                     (and (null (nth 2 stats))
                          (> (nth 0 stats) 0))))
          (when (y-or-n-p (format "%s 判据已满足, 现在关闭冻结门? " id))
            (my/org-project--gate-close id t))))))
  (setq my/org-capture--review-gate nil))

(defun my/org-capture-req-target ()
  "需求条目(pn): 「需求」章对应类型小节下."
  (let* ((file (my/org-capture-project-file))
         (type (completing-read "需求类型: " '("功能" "性能" "接口" "其他") nil t)))
    (setq my/org-capture--req-type type)
    (my/org-capture--goto-olp file (list "研制过程" "需求" (concat type "需求")))))

(defvar my/org-capture--req-change-id nil
  "需求变更(pc)选中的需求 CUSTOM_ID, 供落表后联动移出基线.")

(defun my/org-capture--req-prompt-id ()
  "提示选择需求条目, 返回其 ID 链接文本 (供模板展开).
只列带 :PHASE: 的真实需求条目, 门禁条目(在「里程碑」章)不参与变更."
  (let* ((entries (seq-filter (lambda (e) (plist-get e :phase))
                              (with-current-buffer (my/org-capture--project-buffer)
                                (my/org-project--req-entries))))
         (choices (mapcar (lambda (e)
                            (format "%s %s" (plist-get e :id) (plist-get e :title)))
                          entries))
         (choice (completing-read "变更需求: " choices nil t)))
    (setq my/org-capture--req-change-id (car (split-string choice " ")))
    (format "[[#%s][%s]]" my/org-capture--req-change-id
            (my/org-project--req-id-short my/org-capture--req-change-id
                                          (my/org-capture--code)))))

(defun my/org-capture-req-change-target ()
  "需求变更(pc): 「需求」章「需求变更记录」的变更表."
  (my/org-capture--goto-named-table (my/org-capture-project-file)
                                    '("研制过程" "需求" "需求变更记录")
                                    "req-changes"))

(defun my/org-capture-req-change-finalize ()
  "需求变更(pc)落表后联动: 将对应需求移出基线并置回 DESIGN.
中止捕获(org-note-abort)或未选择需求时不联动."
  (let ((id my/org-capture--req-change-id)
        (buf my/org-capture--last-buffer))
    (setq my/org-capture--req-change-id nil)
    (when (and id (not (and (boundp 'org-note-abort) org-note-abort))
               (buffer-live-p buf))
      (with-current-buffer buf
        (org-with-wide-buffer
         (goto-char (point-min))
         (when (re-search-forward "^\\*\\* 需求[ \t]*$" nil t)
           (let ((end (save-excursion (org-end-of-subtree t t) (point))))
             (while (re-search-forward "^\\*+ " end t)
               (when (string= (org-entry-get (point) "CUSTOM_ID") id)
                 (org-entry-put (point) "BASELINE" "")
                 (let ((st (org-get-todo-state)))
                   ;; org-todo 切到同态会取消关键字, 仅在偏离 DESIGN 时归位
                   (when (and st (not (string= st "DESIGN"))) (org-todo "DESIGN")))
                 ;; 钩子顺序上动态块刷新可能先于本联动, 故再刷一次
                 (ignore-errors (org-update-all-dblocks))
                 (message "需求 %s 已移出基线, 状态置回 DESIGN" id))))))))))

(defun my/org-capture-issue-target ()
  "议题(pi): 「议题」章下, 议题条目为章的直接子标题."
  (my/org-capture--goto-olp (my/org-capture-project-file) '("议题")))

(defun my/org-capture-state-target ()
  "软件状态版本(ps): 当前阶段的软件状态小节下.
状态鉴定阶段的状态条目登记在「状态管理」节."
  (let* ((file (my/org-capture-project-file))
         (stage (my/org-capture--current-stage))
         (section (if (string= stage "状态鉴定") "状态管理" "软件状态")))
    (my/org-capture--goto-olp file (list "研制过程" stage section))))

(defun my/org-capture-risk-target ()
  "风险(pr): 「风险」章下; 旧结构文件先就地升级该章(幂等)."
  (let ((file (my/org-capture-project-file)))
    (set-buffer (org-capture-target-buffer file))
    (widen)
    (unless (my/org-project--risk-chapter-p)
      (my/org-project-risk-upgrade))
    (my/org-capture--goto-olp file '("风险"))))

(defun my/org-capture-log-target ()
  "日志/会议(pd): 「日志」章下."
  (my/org-capture--goto-olp (my/org-capture-project-file) '("日志")))

;; ---- 阶段切换 Stage switching ----
(defun my/org-project--pending-items (stage)
  "返回阶段 STAGE 未完成任务、未闭环议题与未入基线需求的描述列表."
  (let (items)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward (concat "^\\*\\* " (regexp-quote stage) "[ \t]*$")
                               nil t)
        (let ((end (save-excursion (org-end-of-subtree t t) (point))))
          (while (re-search-forward "^\\*+ " end t)
            (let ((todo (my/org-project--state))
                  (title (org-get-heading t t t t)))
              (when (and todo (not (member todo my/org-project--closed-todos)))
                (push (format "[%s] %s" todo title) items)))))))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^\\* 议题[ \t]*$" nil t)
        (let ((end (save-excursion (org-end-of-subtree t t) (point))))
          (while (re-search-forward "^\\*\\* " end t)
            (let ((todo (my/org-project--state))
                  (title (org-get-heading t t t t)))
              (when (member todo '("REPORT" "BUG" "KNOWNCAUSE" "FIXED"))
                (push (format "[议题 %s] %s" todo title) items)))))))
    ;; 风险章: 本阶段未闭环风险(高等级标注), 未状态化的旧条目按待评估提示
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward
             "^\\* 风险\\(?:[ \t]+\\[[0-9/%]+\\]\\)?[ \t]*$" nil t)
        (let ((end (save-excursion (org-end-of-subtree t t) (point))))
          (while (re-search-forward "^\\*\\*+ " end t)
            (let ((todo (my/org-project--state))
                  (id (org-entry-get nil "CUSTOM_ID"))
                  (level (org-entry-get nil "RISK_LEVEL"))
                  (phase (org-entry-get nil "PHASE")))
              (when (and id
                         (or (null todo) (member todo '("IDENT" "MITIGATE")))
                         (or (null phase) (member stage (split-string phase))))
                (push (format "[风险 %s·%s] %s"
                              (or level "未分级")
                              (cond ((equal todo "IDENT") "待评估")
                                    ((equal todo "MITIGATE") "措施中")
                                    (t "未状态化"))
                              (org-get-heading t t t t))
                      items)))))))
    (let ((unbased (seq-filter
                    (lambda (e)
                      (and (not (org-string-nw-p (plist-get e :baseline)))
                           (member stage (split-string (or (plist-get e :phase) "")))
                           (not (member (plist-get e :todo) my/org-project--closed-todos))))
                    (my/org-project--req-entries))))
      (dolist (e unbased)
        (push (format "[需求未入基线] %s %s" (plist-get e :id) (plist-get e :title))
              items)))
    (nreverse items)))

(defun my/org-project--phase-req-unbased (stage)
  "返回阶段 STAGE 中未入基线的有效需求条目."
  (seq-filter (lambda (e)
                (and (not (org-string-nw-p (plist-get e :baseline)))
                     (member stage (split-string (or (plist-get e :phase) "")))))
              (my/org-project--req-entries)))

(defun my/org-project--gate-chain-summary (from to)
  "返回转段门链概况 (供切换日志)."
  (let* ((gate (my/org-project--gate-transition-for from to))
         (freezes (my/org-project--gate-for-phase from "冻结"))
         (done (seq-count (lambda (g) (equal (plist-get g :todo) "DONE"))
                          freezes))
         (void (seq-count (lambda (g) (equal (plist-get g :todo) "CANCELLED"))
                          freezes)))
    (format "门禁 %s · 冻结门 %d/%d 已关%s"
            (if gate
                (format "%s %s" (plist-get gate :suffix)
                        (if (my/org-project--gate-open-p gate) "未关闭" "已关闭"))
              (format "%s→%s 无转段门" from to))
            done (length freezes)
            (if (> void 0) (format " (%d 作废)" void) ""))))

(defun my/org-project--insert-switch-log (from to reason result &optional forced)
  "在「日志」章插入阶段切换记录; FORCED 非 nil 表示强制转段."
  (goto-char (point-min))
  (re-search-forward "^\* 日志[ \t]*$")
  (org-end-of-subtree t t)
  (unless (bolp) (insert "\n"))
  ;; %A 全称星期(星期四), 与 org-time-stamp-formats 一致; %a 会缩写成「周四」
  (insert (format "** %s 阶段切换：%s → %s\n"
                  (format-time-string "[%Y-%m-%d %A]") from to))
  (insert "   :PROPERTIES:\n   :TYPE: 阶段切换\n   :CREATED: "
          (format-time-string "[%Y-%m-%d %A %H:%M]") "\n   :END:\n")
  (insert "   - 切换原因：" reason (if forced "（强制转段，存在未收尾项）" "") "\n")
  (insert (format "   - %s 测试结论：%s\n" from result))
  (insert (format "   - %s\n" (my/org-project--gate-chain-summary from to)))
  (insert (format "   - %s\n" (my/org-project--baseline-summary)))
  (insert (format "   - %s\n" (my/org-project--risk-summary-line)))
  (insert (format "   - 后续任务、测试/试验记录、软件状态均在「%s」下创建\n" to)))

(defun my/org-project--gate-switch-gaps (cur stage)
  "返回从阶段 CUR 转到 STAGE 的门禁缺口列表."
  (let ((gate (my/org-project--gate-transition-for cur stage))
        (freezes (my/org-project--gate-open-freeze cur)))
    (append
     (when (null gate)
       (list (format "未定义 %s → %s 的转段门" cur stage)))
     (mapcar (lambda (g)
               (format "冻结门 %s（%s）未关闭"
                       (plist-get g :suffix)
                       (or (plist-get g :version) "未绑版本")))
             freezes)
     (when gate
       (mapcar (lambda (gap) (concat (plist-get gate :suffix) " " gap))
               (my/org-project--gate-gaps gate))))))

(defun my/org-project-stage-switch ()
  "切换项目当前研制阶段: 门链结算 → 更新 :CURRENT-STATE: → 记日志.
门链 = 本阶段冻结门(须全部关闭) + FROM 为当前阶段的转段门(判据/前置门).
有缺口时列出并二次确认; 强制转段写 :OVERRIDE: 并记入日志."
  (interactive)
  (let ((file (my/org-capture-project-file)))
    (set-buffer (org-capture-target-buffer file))
    (widen)
    (let* ((cur (my/org-capture--current-stage))
           (stage (completing-read (format "切换到阶段(当前 %s): " cur)
                                   my/org-project--stages nil t))
           (pending (my/org-project--pending-items cur))
           (gate-gaps (and (not (string= stage cur))
                           (my/org-project--gate-switch-gaps cur stage)))
           (high-risk (seq-some (lambda (p) (string-prefix-p "[风险 高" p))
                                pending))
           (gaps (append gate-gaps pending))
           forced reason result gate)
      (when (string= stage cur) (user-error "已在阶段「%s」" cur))
      (when (or gaps high-risk)
        (with-current-buffer (get-buffer-create "*项目转段检查*")
          (erase-buffer)
          (insert (format "阶段「%s」→「%s」\n\n" cur stage))
          (when gate-gaps
            (insert "门禁缺口:\n")
            (dolist (g gate-gaps) (insert "  - " g "\n"))
            (insert "\n"))
          (when pending
            (insert "未收尾条目:\n")
            (dolist (p pending) (insert "  - " p "\n")))
          (display-buffer (current-buffer)))
        (unless (y-or-n-p
                 (format "仍有 %d 项缺口%s, 确认强制转段? "
                         (length gaps)
                         (if high-risk ", 含未闭环高风险" "")))
          (user-error "已取消阶段切换"))
        (setq forced t))
      (setq reason (read-string "切换原因: "))
      (setq result (read-string (format "「%s」测试结论: " cur)))
      (setq gate (my/org-project--gate-transition-for cur stage))
      (when gate
        (my/org-project--gate-goto (plist-get gate :id))
        (org-entry-put nil "REASON" reason)
        (org-entry-put nil "TEST-RESULT" result)
        (when forced
          (org-entry-put nil "OVERRIDE" (format-time-string "[%Y-%m-%d %A %H:%M]")))
        (my/org-project--gate-close (plist-get gate :id) forced))
      (goto-char (point-min))
      (re-search-forward "^\* 研制过程[ \t]*$")
      (org-entry-put nil "CURRENT-STATE" stage)
      (my/org-project--insert-switch-log cur stage reason result forced)
      (save-buffer)
      (message "已切换到阶段「%s」（原「%s」）%s" stage cur
               (if forced " [强制]" "")))))

;; ---- 需求动态块 Requirement dynamic block ----
(defun my/org-project--req-entries ()
  "扫描「需求」章, 返回需求条目的 plist 列表.
每项含 :pos :id :title :type :phase :baseline :design :module :test-case :todo.
只收同时带 CUSTOM_ID 与 :PHASE: 的条目 (pn 捕获的需求; 小节标题无
CUSTOM_ID, 门禁条目在「里程碑」章, 均自动排除)."
  (let (entries)
    (org-with-wide-buffer
     (save-excursion
       (goto-char (point-min))
       (when (re-search-forward "^\\*\\* 需求[ \t]*$" nil t)
         (let ((end (save-excursion (org-end-of-subtree t t) (point))))
           (while (re-search-forward "^\\*+ " end t)
             (let ((id (org-entry-get (point) "CUSTOM_ID"))
                   (phase (org-entry-get (point) "PHASE")))
               (when (and id phase)
                 (push (list :pos (point)
                             :id id
                             :title (org-get-heading t t t t)
                             :type (org-entry-get (point) "TYPE")
                             :phase phase
                             :baseline (org-entry-get (point) "BASELINE")
                             :design (org-entry-get (point) "DESIGN")
                             :module (org-entry-get (point) "MODULE")
                             :test-case (org-entry-get (point) "TEST-CASE")
                             :todo (my/org-project--state))
                       entries))))))))
    (nreverse entries)))

(defun my/org-project--req-id-short (id code)
  "需求 ID 显示文本: 去掉项目代号前缀 CODE_."
  (my/org-project--id-short id code))

(defun org-dblock-write:req-by-phase (params)
  "按 :PHASE: 属性过滤「需求」章条目, 生成需求引用表.
PARAMS 的 :phase 指定阶段名, 与条目 :PHASE: 多值(空格分隔)成员匹配;
ID 列为指向需求条目的 CUSTOM_ID 链接."
  (let* ((phase (plist-get params :phase))
         ;; org 用 read 解析块参数, 中文阶段名会成为符号, 需归一成字符串
         (phase (cond ((stringp phase) phase)
                      ((symbolp phase) (symbol-name phase))
                      (t (format "%s" phase))))
         (code (file-name-base (buffer-file-name)))
         (rows (seq-filter (lambda (e)
                             (member phase (split-string (or (plist-get e :phase) ""))))
                           (my/org-project--req-entries))))
    (insert "| 需求ID | 需求条目 | 类型 | 状态 |\n")
    (insert "|--------+----------+------+------|\n")
    (dolist (e rows)
      (insert (format "| [[#%s][%s]] | %s | %s | %s |\n"
                      (plist-get e :id)
                      (my/org-project--req-id-short (plist-get e :id) code)
                      (plist-get e :title)
                      (or (plist-get e :type) "")
                      (or (plist-get e :todo) ""))))))

(defun org-dblock-write:req-baseline (params)
  "按 :BASELINE: 属性过滤「需求」章条目, 生成基线版本快照表.
PARAMS 的 :version 指定基线版本(RBn), 与条目 :BASELINE: 多值(空格分隔)成员匹配;
版本快照可随时重生成, 即基线内容的审计依据."
  (let* ((version (plist-get params :version))
         ;; org 用 read 解析块参数, RBn 会成为符号, 需归一成字符串
         (version (cond ((stringp version) version)
                        ((symbolp version) (symbol-name version))
                        (t (format "%s" version))))
         (code (file-name-base (buffer-file-name)))
         (rows (seq-filter
                (lambda (e)
                  (member version (split-string (or (plist-get e :baseline) ""))))
                (my/org-project--req-entries))))
    (insert "| 需求ID | 需求条目 | 类型 | 状态 |\n")
    (insert "|--------+----------+------+------|\n")
    (dolist (e rows)
      (insert (format "| [[#%s][%s]] | %s | %s | %s |\n"
                      (plist-get e :id)
                      (my/org-project--req-id-short (plist-get e :id) code)
                      (plist-get e :title)
                      (or (plist-get e :type) "")
                      (or (plist-get e :todo) ""))))))

(defun org-dblock-write:req-matrix (_params)
  "生成需求追踪矩阵: 汇总「需求」章条目的追踪属性与覆盖率.
列: 需求ID(链接) / :PHASE: / :TYPE: / 标题 / :BASELINE: / :DESIGN: /
:MODULE: / :TEST-CASE: / TODO 状态; 表后输出覆盖统计(含未入基线条数),
评审前据此核对勾稽缺口."
  (let* ((code (file-name-base (buffer-file-name)))
         (rows (seq-filter (lambda (e) (plist-get e :phase))
                           (my/org-project--req-entries))))
    (insert "| 需求ID | 阶段 | 类型 | 需求条目 | 基线 | 设计章节 | 实现模块 | 测试用例 | 状态 |\n")
    (insert "|--------+------+------+----------+------+----------+----------+----------+------|\n")
    (let ((n 0) (d 0) (m 0) (tc 0) (nb 0))
      (dolist (e rows)
        (setq n (1+ n))
        (when (org-string-nw-p (plist-get e :design)) (setq d (1+ d)))
        (when (org-string-nw-p (plist-get e :module)) (setq m (1+ m)))
        (when (org-string-nw-p (plist-get e :test-case)) (setq tc (1+ tc)))
        (unless (org-string-nw-p (plist-get e :baseline)) (setq nb (1+ nb)))
        (insert (format "| [[#%s][%s]] | %s | %s | %s | %s | %s | %s | %s | %s |\n"
                        (plist-get e :id)
                        (my/org-project--req-id-short (plist-get e :id) code)
                        (or (plist-get e :phase) "")
                        (or (plist-get e :type) "")
                        (plist-get e :title)
                        (or (plist-get e :baseline) "")
                        (or (plist-get e :design) "")
                        (or (plist-get e :module) "")
                        (or (plist-get e :test-case) "")
                        (or (plist-get e :todo) ""))))
      (insert (format "# 覆盖统计: 需求 %d 条 · 设计 %d · 模块 %d · 测试 %d · 未入基线 %d\n"
                      n d m tc nb)))))

;; ---- 风险 Risk ----
;; 风险 = 未发生的不确定事件; 已发生的走议题(REPORT), 由 `C-c P x' 双向链接
;; 后风险置 RESOLVED, 不双轨跟踪。状态只有三态:
;;   IDENT(待评估) → MITIGATE(措施执行/监控) → RESOLVED(已了结)
;; 了结方式由 :CONCLUSION: 区分 闭环/挂账/转议题/误报。「风险」章是唯一建档
;; 处, 阶段与评审只看 risk-by-phase / risk-matrix / risk-summary 动态块;
;; 存量文件用 my/org-project-risk-upgrade 就地升级, pr 捕获也会懒升级。
(defconst my/org-project--risk-chapter-re
  "^\\* 风险\\(?:[ \t]+\\[[0-9/%]+\\]\\)?[ \t]*$"
  "「风险」章标题匹配 (兼容旧模板的 [0/0] 统计 cookie).")

(defconst my/org-project--risk-levels
  '(("高" ("高" . "高") ("中" . "高") ("低" . "中"))
    ("中" ("高" . "高") ("中" . "中") ("低" . "低"))
    ("低" ("高" . "中") ("中" . "低") ("低" . "低")))
  "风险等级矩阵: (可能性 (后果 . 等级) ...).")

(defvar my/org-capture--risk-prob nil
  "捕获风险(pr)时选择的可能性, 供模板计算 :RISK_LEVEL:.")

(defvar my/org-capture--risk-impact nil
  "捕获风险(pr)时选择的影响, 供模板计算 :RISK_LEVEL:.")

(defun my/org-project--risk-level (prob impact)
  "按可能性 PROB 与影响 IMPACT 返回风险等级 (高/中/低)."
  (or (cdr (assoc impact (cdr (assoc prob my/org-project--risk-levels))))
      "中"))

(defun my/org-capture--risk-prompt (kind label default)
  "提示风险评估项 KIND (probability/impact), 记录取值供模板展开."
  (let ((value (completing-read (format "%s: " label)
                                '("高" "中" "低") nil t nil nil default)))
    (if (eq kind 'probability)
        (setq my/org-capture--risk-prob value)
      (setq my/org-capture--risk-impact value))
    value))

(defun my/org-capture--risk-level ()
  "返回捕获时按概率×影响算出的等级 (供模板展开)."
  (my/org-project--risk-level (or my/org-capture--risk-prob "中")
                              (or my/org-capture--risk-impact "中")))

(defun my/org-project--id-short (id code)
  "显示用条目 ID: 去掉项目代号前缀 CODE_."
  (if (and id (string-prefix-p (concat code "_") id))
      (substring id (1+ (length code)))
    id))

(defun my/org-project--file-stage ()
  "返回当前文件「研制过程」章的 :CURRENT-STATE:, 无则取第一阶段."
  (org-with-wide-buffer
   (save-excursion
     (goto-char (point-min))
     (if (re-search-forward "^\\* 研制过程[ \t]*$" nil t)
         (or (org-entry-get nil "CURRENT-STATE") (car my/org-project--stages))
       (car my/org-project--stages)))))

(defun my/org-project--risk-entries ()
  "扫描「风险」章, 返回风险条目的 plist 列表.
每项含 :pos :id :title :phase :origin :origin-link :probability :impact
:level :response :trigger :owner :review :conclusion :accept-reason
:evidence :issue :todo; 只收带 CUSTOM_ID 的条目."
  (let (entries)
    (org-with-wide-buffer
     (save-excursion
       (goto-char (point-min))
       (when (re-search-forward my/org-project--risk-chapter-re nil t)
         (let ((end (save-excursion (org-end-of-subtree t t) (point))))
           (while (re-search-forward "^\\*\\*+ " end t)
             (let ((id (org-entry-get nil "CUSTOM_ID")))
               (when id
                 (push (list :pos (point)
                             :id id
                             :title (org-get-heading t t t t)
                             :phase (org-entry-get nil "PHASE")
                             :origin (org-entry-get nil "ORIGIN")
                             :origin-link (org-entry-get nil "ORIGIN-LINK")
                             :probability (org-entry-get nil "PROBABILITY")
                             :impact (org-entry-get nil "IMPACT")
                             :level (org-entry-get nil "RISK_LEVEL")
                             :response (org-entry-get nil "RESPONSE")
                             :trigger (org-entry-get nil "TRIGGER")
                             :owner (org-entry-get nil "OWNER")
                             :review (org-entry-get nil "REVIEW")
                             :conclusion (org-entry-get nil "CONCLUSION")
                             :accept-reason (org-entry-get nil "ACCEPT-REASON")
                             :evidence (org-entry-get nil "EVIDENCE")
                             :issue (org-entry-get nil "ISSUE")
                             :todo (my/org-project--state))
                       entries))))))))
    (nreverse entries)))

(defun my/org-project--risk-open-p (entry)
  "风险条目 ENTRY 是否未了结 (无状态或 IDENT/MITIGATE)."
  (let ((todo (plist-get entry :todo)))
    (or (null todo) (member todo '("IDENT" "MITIGATE")))))

(defun my/org-project--risk-in-stage-p (entry stage)
  "风险条目 ENTRY 是否属于阶段 STAGE; 缺 :PHASE: 的旧条目视为属于任何阶段."
  (let ((phase (plist-get entry :phase)))
    (or (not (org-string-nw-p phase))
        (member stage (split-string phase)))))

(defun my/org-project--risk-summary-line ()
  "返回风险台账概况字符串 (供阶段切换日志与仪表盘)."
  (let ((total 0) (ident 0) (mit 0) (res 0) (acc 0) (high 0) (nolink 0))
    (dolist (e (my/org-project--risk-entries))
      (setq total (1+ total))
      (pcase (plist-get e :todo)
        ("IDENT" (setq ident (1+ ident)))
        ("MITIGATE" (setq mit (1+ mit)))
        ("RESOLVED"
         (setq res (1+ res))
         (when (equal (plist-get e :conclusion) "挂账") (setq acc (1+ acc))))
        (_ (setq ident (1+ ident))))     ; 旧条目未状态化, 按待评估计
      (when (and (my/org-project--risk-open-p e)
                 (equal (plist-get e :level) "高"))
        (setq high (1+ high)))
      (unless (org-string-nw-p (plist-get e :origin-link))
        (setq nolink (1+ nolink))))
    (format "风险 %d 条 · 待评估 %d · 措施中 %d · 已了结 %d(挂账 %d) · 高等级未闭环 %d · 来源未链接 %d"
            total ident mit res acc high nolink)))

(defun my/org-project--risk-select ()
  "选择风险条目 ID: 光标在风险条目上时直接取, 否则从本章列表选择."
  (let* ((entries (my/org-project--risk-entries))
         (here (and (derived-mode-p 'org-mode) (org-entry-get nil "CUSTOM_ID")))
         (ids (mapcar (lambda (e) (plist-get e :id)) entries)))
    (unless entries (user-error "本项目还没有风险条目, 先用 pr 捕获"))
    (if (member here ids)
        here
      (car (split-string
            (completing-read
             "风险条目: "
             (mapcar (lambda (e) (format "%s %s" (plist-get e :id)
                                         (plist-get e :title)))
                     entries)
             nil t)
            " ")))))

(defun my/org-project--risk-goto (id)
  "按 ID 定位风险条目, 成功则 point 停在该标题行."
  (goto-char (point-min))
  (when (re-search-forward
         (concat "^[ \t]*:CUSTOM_ID:[ \t]+" (regexp-quote id) "[ \t]*$") nil t)
    (org-back-to-heading t)))

(defun my/org-project-risk-assess ()
  "风险评估确认: 计算等级并补齐确认要素, 状态 IDENT → MITIGATE.
概率/影响决定 :RISK_LEVEL:, 再补 :TRIGGER:/:RESPONSE:/:OWNER:/:REVIEW:,
写入 :ASSESSED: 时间戳; 缺 :ORIGIN-LINK: 时只提示, 不阻断."
  (interactive)
  (let ((id (my/org-project--risk-select)))
    (unless (my/org-project--risk-goto id)
      (user-error "未找到风险条目 %s" id))
    (unless (org-string-nw-p (org-entry-get nil "ORIGIN-LINK"))
      (message "提示: 风险 %s 缺少 :ORIGIN-LINK:, 汇总里会标红" id))
    (let* ((prob (completing-read "可能性: " '("高" "中" "低") nil t nil nil
                                  (or (org-entry-get nil "PROBABILITY") "中")))
           (impact (completing-read "影响: " '("高" "中" "低") nil t nil nil
                                    (or (org-entry-get nil "IMPACT") "中")))
           (level (my/org-project--risk-level prob impact))
           (resp (completing-read "应对策略: " '("减轻" "规避" "转移" "接受")
                                  nil t nil nil
                                  (or (org-entry-get nil "RESPONSE") "减轻")))
           (trigger (read-string "预警判据(可观测): " nil nil
                                 (org-entry-get nil "TRIGGER")))
           (owner (read-string "责任人: " nil nil
                               (or (org-entry-get nil "OWNER") user-full-name)))
           (review (read-string "下次复核(可空): " nil nil
                                (org-entry-get nil "REVIEW"))))
      (org-entry-put nil "PROBABILITY" prob)
      (org-entry-put nil "IMPACT" impact)
      (org-entry-put nil "RISK_LEVEL" level)
      (org-entry-put nil "RESPONSE" resp)
      (org-entry-put nil "TRIGGER" trigger)
      (org-entry-put nil "OWNER" owner)
      (org-entry-put nil "REVIEW" review)
      (org-entry-put nil "ASSESSED" (format-time-string "[%Y-%m-%d %A %H:%M]"))
      (when (member (org-get-todo-state) '(nil "IDENT"))
        (org-todo "MITIGATE"))
      (save-buffer)
      (message "风险 %s 已确认: %s×%s → %s (%s)" id prob impact level
               (org-get-todo-state)))))

(defun my/org-project--issue-titles ()
  "返回「议题」章中带 CUSTOM_ID 的条目 (标题 . ID) 列表."
  (let (items)
    (org-with-wide-buffer
     (save-excursion
       (goto-char (point-min))
       (when (re-search-forward "^\\* 议题\\(?:[ \t]+\\[[0-9/%]+\\]\\)?[ \t]*$" nil t)
         (let ((end (save-excursion (org-end-of-subtree t t) (point))))
           (while (re-search-forward "^\\*\\*+ " end t)
             (let ((iid (org-entry-get nil "CUSTOM_ID")))
               (when iid
                 (push (cons (org-get-heading t t t t) iid) items))))))))
    (nreverse items)))

(defun my/org-project--issue-new (title phase)
  "在「议题」章新建 REPORT 条目, 返回其 CUSTOM_ID.
插入过程不移动调用方的 point (风险条目写完属性还要用)."
  (let ((id (my/org-capture--next-id "ISSUE")))
    (save-excursion
      (goto-char (point-min))
      (unless (re-search-forward "^\\* 议题\\(?:[ \t]+\\[[0-9/%]+\\]\\)?[ \t]*$" nil t)
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert "* 议题\n"))
      (org-end-of-subtree t t)
      (unless (bolp) (insert "\n"))
      (insert (format (concat "** REPORT %s\n"
                              "   :PROPERTIES:\n"
                              "   :CUSTOM_ID: %s\n"
                              "%s"
                              "   :SEVERITY: 一般\n"
                              "   :CREATED: %s\n"
                              "   :END:\n\n"
                              "   *1. 问题描述*\n\n"
                              "   *2. 分析定位*\n\n"
                              "   *3. 措施验证*\n\n"
                              "   *4. 结论*\n")
                      title id
                      (if (org-string-nw-p phase)
                          (format "   :PHASE: %s\n" phase)
                        "")
                      (format-time-string "[%Y-%m-%d %A %H:%M]"))))
    id))

(defun my/org-project-risk-to-issue ()
  "风险已发生: 转议题并双向链接, 风险置 RESOLVED(:CONCLUSION: 转议题).
可新建议题或关联既有议题; 转议题属于风险退出, 不受措施 checkbox 依赖阻塞."
  (interactive)
  (let* ((id (my/org-project--risk-select))
         (entries (my/org-project--risk-entries))
         (entry (seq-find (lambda (e) (equal (plist-get e :id) id)) entries)))
    (unless (my/org-project--risk-goto id)
      (user-error "未找到风险条目 %s" id))
    (let* ((existing (my/org-project--issue-titles))
           (choice (completing-read "关联议题: " (cons "新建议题" existing)
                                    nil t))
           (issue-id (if (equal choice "新建议题")
                         (my/org-project--issue-new (plist-get entry :title)
                                                    (plist-get entry :phase))
                       (cdr (assoc choice existing)))))
      (org-entry-put nil "ISSUE"
                     (format "[[#%s][%s]]" issue-id
                             (my/org-project--id-short
                              issue-id (file-name-base (buffer-file-name)))))
      (org-entry-put nil "CONCLUSION" "转议题")
      (let ((org-enforce-todo-checkbox-dependencies nil)
            (org-enforce-todo-dependencies nil))
        (when (org-get-todo-state)
          (org-todo "RESOLVED")))
      (save-buffer)
      (message "风险 %s 已转议题 %s" id issue-id))))

;; ---- 风险动态块 Risk dynamic blocks ----
(defun org-dblock-write:risk-by-phase (params)
  "按 :PHASE: 过滤「风险」章条目, 生成阶段风险表.
PARAMS 的 :phase 为阶段名, 或 current 表示取文件 :CURRENT-STATE:."
  (let* ((phase (plist-get params :phase))
         (phase (cond ((stringp phase) phase)
                      ((symbolp phase) (symbol-name phase))
                      (t (format "%s" phase))))
         (phase (if (string= phase "current")
                    (my/org-project--file-stage)
                  phase))
         (code (file-name-base (buffer-file-name)))
         (rows (seq-filter (lambda (e) (my/org-project--risk-in-stage-p e phase))
                           (my/org-project--risk-entries))))
    (insert "| 风险ID | 等级 | 状态 | 策略 | 责任人 | 预警判据 |\n")
    (insert "|--------+------+------+------+--------+----------|\n")
    (dolist (e rows)
      (insert (format "| [[#%s][%s]] | %s | %s | %s | %s | %s |\n"
                      (plist-get e :id)
                      (my/org-project--id-short (plist-get e :id) code)
                      (or (plist-get e :level) "")
                      (or (plist-get e :todo) "未状态化")
                      (or (plist-get e :response) "")
                      (or (plist-get e :owner) "")
                      (or (plist-get e :trigger) ""))))))

(defun org-dblock-write:risk-matrix (_params)
  "生成 3×3 风险矩阵 (可能性 × 影响), 格内为未闭环风险 ID."
  (let* ((code (file-name-base (buffer-file-name)))
         (entries (seq-filter #'my/org-project--risk-open-p
                              (my/org-project--risk-entries))))
    (insert "| 可能性\\影响 | 低 | 中 | 高 |\n")
    (insert "|-------------+----+----+----|\n")
    (dolist (prob '("高" "中" "低"))
      (insert (format "| %s |" prob))
      (dolist (impact '("低" "中" "高"))
        (let* ((cell (seq-filter
                      (lambda (e)
                        (and (equal (or (plist-get e :probability) "中") prob)
                             (equal (or (plist-get e :impact) "中") impact)))
                      entries))
               (text (if cell
                         (mapconcat (lambda (e)
                                      (my/org-project--id-short
                                       (plist-get e :id) code))
                                    cell " ")
                       "—")))
          (insert (format " %s |" text))))
      (insert "\n"))
    (insert (format "# 仅统计未闭环风险 (IDENT/MITIGATE), 共 %d 条\n"
                    (length entries)))))

(defun org-dblock-write:risk-summary (_params)
  "生成风险台账汇总: 统计行 + 未闭环清单 + 挂账清单.
缺 :ORIGIN-LINK: 的条目显示「⚠ 未链接」, 由 font-lock 标红."
  (let* ((code (file-name-base (buffer-file-name)))
         (entries (my/org-project--risk-entries))
         (open (seq-filter #'my/org-project--risk-open-p entries))
         (accepted (seq-filter (lambda (e) (equal (plist-get e :conclusion) "挂账"))
                               entries)))
    (insert (my/org-project--risk-summary-line) "\n\n")
    (insert "| 风险ID | 等级 | 状态 | 阶段 | 来源 | 责任人 | 复核 | 预警判据 |\n")
    (insert "|--------+------+------+------+------+--------+------+----------|\n")
    (dolist (e open)
      (insert (format "| [[#%s][%s]] | %s | %s | %s | %s | %s | %s | %s |\n"
                      (plist-get e :id)
                      (my/org-project--id-short (plist-get e :id) code)
                      (or (plist-get e :level) "")
                      (or (plist-get e :todo) "未状态化")
                      (or (plist-get e :phase) "")
                      (if (org-string-nw-p (plist-get e :origin-link))
                          (plist-get e :origin-link)
                        "⚠ 未链接")
                      (or (plist-get e :owner) "")
                      (or (plist-get e :review) "")
                      (or (plist-get e :trigger) ""))))
    (when accepted
      (insert "\n挂账风险:\n\n")
      (insert "| 风险ID | 等级 | 挂账理由 | 复核 |\n")
      (insert "|--------+------+----------+------|\n")
      (dolist (e accepted)
        (insert (format "| [[#%s][%s]] | %s | %s | %s |\n"
                        (plist-get e :id)
                        (my/org-project--id-short (plist-get e :id) code)
                        (or (plist-get e :level) "")
                        (or (plist-get e :accept-reason) "")
                        (or (plist-get e :review) "")))))))

(defun my/org-project-risk-dashboard ()
  "刷新风险动态块, 并在 *项目风险* 缓冲区显示分布与未闭环清单."
  (interactive)
  (unless (and (derived-mode-p 'org-mode) (buffer-file-name))
    (user-error "请在项目 org 文件中执行"))
  (org-with-wide-buffer (org-update-all-dblocks))
  (let ((entries (my/org-project--risk-entries))
        (buf (get-buffer-create "*项目风险*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "项目: %s\n\n" (buffer-file-name)))
        (insert (my/org-project--risk-summary-line) "\n\n")
        (let ((open (seq-filter #'my/org-project--risk-open-p entries)))
          (if (null open)
              (insert "未闭环风险: (无)\n")
            (insert "未闭环风险:\n")
            (dolist (e open)
              (insert (format "  - [%s%s] %s%s  %s\n"
                              (or (plist-get e :todo) "未状态化")
                              (if (org-string-nw-p (plist-get e :level))
                                  (concat " " (plist-get e :level)) "")
                              (plist-get e :title)
                              (if (org-string-nw-p (plist-get e :origin-link))
                                  "" "  ⚠ 未链接")
                              (or (plist-get e :owner) ""))))))
        (let ((org-inhibit-startup t))
          (org-mode))
        (goto-char (point-min))
        (setq buffer-read-only t)))
    (display-buffer buf)))

;; ---- 风险章升级 Risk chapter upgrade ----
(defun my/org-project--risk-chapter-p ()
  "当前缓冲区「风险」章是否已含新结构 (risk-matrix 动态块)."
  (save-excursion
    (goto-char (point-min))
    (and (re-search-forward my/org-project--risk-chapter-re nil t)
         (let ((end (save-excursion (org-end-of-subtree t t) (point))))
           (save-excursion
             (re-search-forward "^[ \t]*#\\+BEGIN:[ \t]+risk-matrix" end t))))))

(defun my/org-project--risk-keep-subtrees ()
  "摘出「风险」章下真实风险条目的原文, 供升级时保留.
模板残留 (CUSTOM_ID 以 XXX_ 开头或标题为 XXXX) 丢弃."
  (let (texts)
    (org-with-wide-buffer
     (goto-char (point-min))
     (when (re-search-forward my/org-project--risk-chapter-re nil t)
       (let* ((beg (progn (forward-line 1) (point)))
              (end (save-excursion (org-end-of-subtree t t) (point))))
         (goto-char beg)
         (while (re-search-forward "^\\*\\*+ " end t)
           (let* ((hbeg (line-beginning-position))
                  (hend (save-excursion (org-end-of-subtree t t) (point)))
                  (id (org-entry-get nil "CUSTOM_ID"))
                  (title (org-get-heading t t t t)))
             (unless (or (string-prefix-p "XXX_" (or id ""))
                         (member title '("XXXX" "")))
               (push (replace-regexp-in-string
                      "^\\([ \t]*\\):LEVEL:"
                      "\\1:RISK_LEVEL:"
                      (buffer-substring-no-properties hbeg hend))
                     texts)))
           (goto-char (save-excursion (org-end-of-subtree t t) (point)))))))
    (nreverse texts)))

(defun my/org-project-risk-upgrade ()
  "把当前项目文件的「风险」章升级为新结构 (幂等).
只重写「风险」章: 保留既有风险条目(含正文), 丢弃模板残留, 重建说明、
矩阵/阶段/台账动态块、评审记录表与阶段出口准则表; 其他章节不动."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "请在项目 org 文件中执行"))
  (org-with-wide-buffer
   (goto-char (point-min))
   (let ((exists (re-search-forward my/org-project--risk-chapter-re nil t)))
     (if (and exists (my/org-project--risk-chapter-p))
         (message "「风险」章已是新结构, 无需升级")
       (let ((kept (and exists (my/org-project--risk-keep-subtrees))))
         (goto-char (point-min))
         (if (re-search-forward my/org-project--risk-chapter-re nil t)
             (let ((beg (line-beginning-position))
                   (end (save-excursion (org-end-of-subtree t t) (point))))
               (delete-region beg end)
               (goto-char beg)
               (insert (my/org-project--risk-chapter) "\n"))
           ;; 没有风险章: 插到「日志」章前, 否则文件末尾
           (goto-char (point-max))
           (if (re-search-backward "^\\* 日志[ \t]*$" nil t)
               (progn (forward-line 0)
                      (insert (my/org-project--risk-chapter) "\n"))
             (unless (bolp) (insert "\n"))
             (insert "\n" (my/org-project--risk-chapter) "\n")))
         (when kept
           (dolist (text kept) (insert text)))
         (org-update-all-dblocks)
         (message "「风险」章已升级为新结构 (保留 %d 条既有条目)"
                  (length kept)))))))

(defun my/org-project-risk-upgrade-all ()
  "批量把 projects/ 下项目文件的「风险」章升级为新结构.
升级前用 tar 备份 projects/ (~/Org 不在版本控制内), 已升级的文件自动跳过."
  (interactive)
  (let* ((files (my/org-project--files))
         (pending (seq-filter
                   (lambda (f)
                     (let ((org-inhibit-startup t))
                       (with-current-buffer (find-file-noselect (cdr f))
                         (not (my/org-project--risk-chapter-p)))))
                   files)))
    (unless pending (user-error "所有项目文件的风险章都已是新结构"))
    (when (y-or-n-p (format "将升级 %d 个文件的风险章, 并先备份 projects/? "
                            (length pending)))
      (let ((tar (expand-file-name
                  (format "projects-risk-backup-%s.tar.gz"
                          (format-time-string "%Y%m%d-%H%M%S"))
                  org-directory)))
        (call-process "tar" nil nil nil "-czf" tar "-C" org-directory "projects")
        (message "已备份: %s" tar))
      (let ((n 0))
        (dolist (f pending)
          (let ((org-inhibit-startup t))
            (with-current-buffer (find-file-noselect (cdr f))
              (my/org-project-risk-upgrade)
              (save-buffer)
              (setq n (1+ n)))))
        (message "已升级 %d 个文件的风险章" n)))))

;; ---- 门禁 Gate ----
;; 门 = 「里程碑」章下的一个条目: 类型(转段/冻结) + 判据 checkbox + 评审表
;; review-<后缀> + 证据链接。转段门 M1..M4 与阶段转换一一对应, 由 C-c P s
;; 结算; 冻结门 B<k> 与基线版本同号(B2 = RB2), 同一阶段可有多道, 圈定基线时
;; 由 C-c P b 创建, 评审后关门。判据未勾完时 checkbox 依赖会阻止关门,
;; 关闭入口统一走"列缺口 + 二次确认"(C-c P g / pe 落表后的提示)。
(defconst my/org-project--gate-chapter-re "^\\*+ 里程碑[ \t]*$"
  "「里程碑」章标题匹配 (兼容存量文件的顶级章与骨架的二级章).")

(defun my/org-project--gate-chapter-level ()
  "返回「里程碑」章的标题层级, 不存在返回 nil."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward my/org-project--gate-chapter-re nil t)
      (org-current-level))))

(defconst my/org-project--gate-transitions
  '(("M1" "方案评审通过" "方案设计" "样机研制")
    ("M2" "样机研制完成" "样机研制" "工程研制")
    ("M3" "工程研制完成" "工程研制" "状态鉴定")
    ("M4" "状态鉴定通过" "状态鉴定" nil))
  "转段门定义: (编号 名称 来源阶段 目标阶段); 目标为 nil 表示项目终点门.")

(defconst my/org-project--gate-criteria
  '(("M1" "需求基线已冻结（本阶段冻结门全部关闭）"
         "技术成熟度与验证闭环"
         "遗留问题已转议题或风险")
    ("M2" "样机调试与测试闭环"
         "遗留问题已挂账"
         "软件状态已更新")
    ("M3" "环境与可靠性验证闭环"
         "测试记录覆盖全部需求"
         "软件状态已冻结")
    ("M4" "鉴定测试与试验闭环"
         "软件状态已固化归档"
         "遗留风险已挂账")
    ("freeze" "需求条目全部入基线"
              "需求评审通过（结论：通过/有条件通过）"
              "变更请求已处置或挂账"))
  "门的判据清单: 门编号或 freeze.")

(defun my/org-project--gate-criteria-text (key indent)
  "生成判据 checkbox 文本, KEY 为门编号或 freeze, INDENT 为缩进."
  (mapconcat (lambda (item) (concat indent "- [ ] " item "\n"))
             (cdr (assoc key my/org-project--gate-criteria))
             ""))

(defun my/org-project--gate-review-table-text (suffix indent)
  "生成名为 review-SUFFIX 的空评审表文本, INDENT 为缩进."
  (concat indent "#+NAME: review-" suffix "\n"
          indent "| 日期 | 评审类型 | 评审对象 | 人员 | 结论 | 遗留问题 | 待办 |\n"
          indent "|------+----------+----------+------+------+----------+------|\n"))

(defun my/org-project--gate-text-transition (code id name from to &optional level)
  "生成转段门文本: CODE 项目代号, ID 编号, NAME 名称, FROM/TO 阶段.
LEVEL 为「里程碑」章层级, 门标题取其下一级 (默认 2)."
  (concat
   (make-string (1+ (or level 2)) ?*) " TODO " id " " name " :gate:transition:\n"
   "    :PROPERTIES:\n"
   "    :CUSTOM_ID: " code "_" id "\n"
   "    :GATE-TYPE: 转段\n"
   "    :FROM:     " from "\n"
   (if to (concat "    :TO:       " to "\n") "")
   "    :REASON:\n"
   "    :TEST-RESULT:\n"
   "    :OWNER:\n"
   "    :END:\n\n"
   (my/org-project--gate-criteria-text id "    ")
   "\n"
   (my/org-project--gate-review-table-text id "    ")
   "\n"))

(defun my/org-project--gate-text-freeze (code k version phase &optional level)
  "生成冻结门文本: CODE 项目代号, K 基线版本号, VERSION 版本, PHASE 阶段.
LEVEL 为「里程碑」章层级, 门标题取其下一级 (默认 2)."
  (let ((suffix (format "B%d" k))
        (stars (make-string (1+ (or level 2)) ?*)))
    (concat
     stars " TODO " suffix " 需求基线冻结 :gate:freeze:\n"
     "    :PROPERTIES:\n"
     "    :CUSTOM_ID: " code "_" suffix "\n"
     "    :GATE-TYPE: 冻结\n"
     "    :ARTIFACT:  需求基线\n"
     "    :VERSION:   " (or version "") "\n"
     "    :PHASE:     " (or phase "") "\n"
     "    :OWNER:\n"
     "    :EVIDENCE:  [[#baseline-records][基线记录表]] [[#req-matrix][追踪矩阵]]\n"
     "    :END:\n\n"
     (my/org-project--gate-criteria-text "freeze" "    ")
     "\n"
     (my/org-project--gate-review-table-text suffix "    ")
     "\n")))

(defconst my/org-project--gate-chapter-preamble
  (concat
   "# 门禁台账: 转段门 M1..M4 改变 :CURRENT-STATE:, 冻结门 B<k> 冻结需求基线\n"
   "# (与基线版本同号, 同一阶段可有多道)。每道门 = 判据 checkbox + 评审表\n"
   "# review-<ID> + 证据链接; 判据未勾完时 checkbox 依赖会阻止关门。\n"
   "# 关门 `C-c P g' (列缺口 + 二次确认), 转段 `C-c P s' (门链结算)。\n\n")
  "「里程碑」章的说明文字.")

(defun my/org-project--gates-text (code &optional level)
  "生成「里程碑」章完整子树文本: 汇总块 + B1 占位 + 四道转段门.
LEVEL 为章的层级 (默认 2, 即「研制过程」下的二级章)."
  (let ((level (or level 2)))
    (concat
     (make-string level ?*) " 里程碑\n\n"
     my/org-project--gate-chapter-preamble
     "   #+BEGIN: gate-summary\n"
     "   #+END:\n\n"
     (my/org-project--gate-text-freeze code 1 nil nil level)
     "\n"
     (mapconcat (lambda (spec)
                  (apply #'my/org-project--gate-text-transition
                         code (append spec (list level))))
                my/org-project--gate-transitions ""))))

(defun my/org-project--gate-suffix (id)
  "门的表名后缀: ID 最后一个下划线之后的部分 (1501_X_B2 -> B2)."
  (if (string-match "\\([^_]+\\'\\)" id)
      (match-string 1 id)
    id))

(defun my/org-project--gates ()
  "扫描「里程碑」章, 返回门条目 plist 列表.
每项含 :pos :id :suffix :title :type :from :to :artifact :version
:phase :requires :owner :evidence :todo."
  (let (gates)
    (org-with-wide-buffer
     (save-excursion
       (goto-char (point-min))
       (when (re-search-forward my/org-project--gate-chapter-re nil t)
         (let* ((level (1+ (org-current-level)))
                (end (save-excursion (org-end-of-subtree t t) (point)))
                (re (concat "^" (regexp-quote (make-string level ?*)) " ")))
           (while (re-search-forward re end t)
             (let ((id (org-entry-get nil "CUSTOM_ID"))
                   (kind (org-entry-get nil "GATE-TYPE")))
               (when (and id kind)
                 (push (list :pos (point)
                             :id id
                             :suffix (my/org-project--gate-suffix id)
                             :title (org-get-heading t t t t)
                             :type kind
                             :from (org-entry-get nil "FROM")
                             :to (org-entry-get nil "TO")
                             :artifact (org-entry-get nil "ARTIFACT")
                             :version (org-entry-get nil "VERSION")
                             :phase (org-entry-get nil "PHASE")
                             :requires (org-entry-get nil "REQUIRES")
                             :owner (org-entry-get nil "OWNER")
                             :evidence (org-entry-get nil "EVIDENCE")
                             :todo (my/org-project--state))
                       gates))))))))
    (nreverse gates)))

(defun my/org-project--gate-get (id)
  "按 ID 返回门 plist, 不存在返回 nil."
  (seq-find (lambda (g) (equal (plist-get g :id) id))
            (my/org-project--gates)))

(defun my/org-project--gate-open-p (gate)
  "门 GATE 是否未关闭 (TODO 或未状态化)."
  (not (member (plist-get gate :todo) '("DONE" "CANCELLED"))))

(defun my/org-project--gate-goto (id)
  "按 ID 定位门条目, 成功返回 non-nil 且 point 在标题行."
  (goto-char (point-min))
  (when (re-search-forward
         (concat "^[ \t]*:CUSTOM_ID:[ \t]+" (regexp-quote id) "[ \t]*$") nil t)
    (org-back-to-heading t)))

(defun my/org-project--gate-criteria (id)
  "返回门 ID 的判据状态: (已勾 总数 未勾文本列表)."
  (unless (my/org-project--gate-goto id)
    (user-error "未找到门 %s" id))
  (let ((end (save-excursion (org-end-of-subtree t t) (point)))
        (done 0) (total 0) gaps)
    (save-excursion
      (while (re-search-forward
              "^[ \t]*[-+][ \t]+\\[\\([ X-]\\)\\][ \t]+\\(.*\\)$" end t)
        (setq total (1+ total))
        (if (equal (match-string 1) " ")
            (push (string-trim (match-string 2)) gaps)
          (setq done (1+ done)))))
    (list done total (nreverse gaps))))

(defun my/org-project--gate-requires-unmet (gate)
  "返回门 GATE 的 :REQUIRES: 中未满足的门 ID 列表."
  (let (unmet)
    (dolist (id (split-string (or (plist-get gate :requires) "") "[ ,]+" t))
      (let ((g (my/org-project--gate-get id)))
        (when (or (null g) (my/org-project--gate-open-p g))
          (push id unmet))))
    (nreverse unmet)))

(defun my/org-project--gate-gaps (gate)
  "返回门 GATE 的缺口描述列表 (判据未勾 + 前置门未过)."
  (let* ((id (plist-get gate :id))
         (stats (my/org-project--gate-criteria id))
         (done (nth 0 stats))
         (total (nth 1 stats))
         (unchecked (nth 2 stats))
         (unmet (my/org-project--gate-requires-unmet gate)))
    (append
     (when (< done total)
       (list (format "判据 %d/%d" done total)))
     (mapcar (lambda (item) (concat "未勾判据: " item)) unchecked)
     (mapcar (lambda (req) (format "前置门未关闭: %s" req)) unmet))))

(defun my/org-project--gate-review-count (id)
  "返回门 ID 评审表的数据行数."
  (unless (my/org-project--gate-goto id)
    (user-error "未找到门 %s" id))
  (let ((end (save-excursion (org-end-of-subtree t t) (point)))
        (name (concat "review-" (my/org-project--gate-suffix id)))
        (n 0))
    (save-excursion
      (when (re-search-forward
             (concat "^[ \t]*#\\+NAME:[ \t]+" (regexp-quote name) "[ \t]*$")
             end t)
        (forward-line 1)
        (when (org-at-table-p)
          (let ((tend (save-excursion (org-table-end))))
            (forward-line 2)            ; 跳过表头与分隔行
            (while (< (point) tend)
              (when (looking-at "^[ \t]*|")
                (setq n (1+ n)))
              (forward-line 1))))))
    n))

(defun my/org-project--gate-review-table-ensure (id)
  "确保门 ID 下有 review-<后缀> 表, 缺失则在门子树末尾补一张."
  (unless (my/org-project--gate-goto id)
    (user-error "未找到门 %s" id))
  (let* ((beg (point))
         (end (save-excursion (org-end-of-subtree t t) (point)))
         (suffix (my/org-project--gate-suffix id))
         (re (concat "^[ \t]*#\\+NAME:[ \t]+review-"
                     (regexp-quote suffix) "[ \t]*$")))
    (goto-char beg)
    (unless (re-search-forward re end t)
      (goto-char end)
      (unless (bolp) (insert "\n"))
      (insert "\n" (my/org-project--gate-review-table-text suffix "    "))
      (my/org-project--align-tables))))

(defun my/org-project--gate-review-table-goto (id)
  "定位到门 ID 的评审表 #+NAME 行, 缺表则补建."
  (my/org-project--gate-review-table-ensure id)
  (my/org-project--gate-goto id)
  (let ((end (save-excursion (org-end-of-subtree t t) (point)))
        (name (concat "review-" (my/org-project--gate-suffix id))))
    (goto-char (point-min))
    (unless (re-search-forward
             (concat "^[ \t]*#\\+NAME:[ \t]+" (regexp-quote name) "[ \t]*$")
             end t)
      (user-error "未找到评审表 %s" name))
    (goto-char (line-beginning-position))))

(defun my/org-project--gate-close (id &optional force)
  "关闭门 ID: 列缺口并二次确认后置 DONE.
FORCE 非 nil 时跳过确认, 并绕过 checkbox 依赖强制关门."
  (let ((gate (my/org-project--gate-get id)))
    (unless gate (user-error "未找到门 %s" id))
    (let ((gaps (my/org-project--gate-gaps gate)))
      (unless (or force (null gaps))
        (unless (y-or-n-p
                 (format "%s 仍有缺口（%s）, 确认关门? "
                         id (mapconcat #'identity gaps "; ")))
          (user-error "已取消关门")))
      (my/org-project--gate-goto id)
      ;; CLOSED 由 org-log-done 自动写入, org-entry-put 不能直接设该属性
      (let ((org-enforce-todo-checkbox-dependencies nil)
            (org-enforce-todo-dependencies nil))
        (org-todo "DONE"))
      (message "门 %s 已关闭%s" id (if gaps "（存在缺口，已确认）" "")))))

(defun my/org-project--gate-cancel (id)
  "作废门 ID (置 CANCELLED), 用于被新版本基线取代的冻结门."
  (unless (my/org-project--gate-goto id)
    (user-error "未找到门 %s" id))
  (let ((org-enforce-todo-checkbox-dependencies nil)
        (org-enforce-todo-dependencies nil))
    (org-todo "CANCELLED"))
  (message "门 %s 已作废" id))

(defun my/org-project--gate-reopen (id)
  "重开门 ID (置 TODO 并清空 CLOSED)."
  (unless (my/org-project--gate-goto id)
    (user-error "未找到门 %s" id))
  (let ((org-closed-keep-when-no-todo nil))
    (org-todo "TODO"))
  (message "门 %s 已重开" id))

(defun my/org-project--gate-for-phase (phase &optional type)
  "返回属于 PHASE 阶段的门列表; 可按 TYPE(转段/冻结) 过滤.
未绑定阶段的冻结门视为属于任何阶段 (首个基线尚未圈定时)."
  (seq-filter
   (lambda (g)
     (and (or (null type) (equal (plist-get g :type) type))
          (or (not (org-string-nw-p (plist-get g :phase)))
              (equal (plist-get g :phase) phase))))
   (my/org-project--gates)))

(defun my/org-project--gate-open-freeze (phase)
  "返回 PHASE 阶段未关闭的冻结门列表."
  (seq-filter #'my/org-project--gate-open-p
              (my/org-project--gate-for-phase phase "冻结")))

(defun my/org-project--gate-transition-for (from to)
  "返回来源阶段 FROM、目标阶段 TO 的转段门; TO 为 nil 时不限目标."
  (seq-find (lambda (g)
              (and (equal (plist-get g :type) "转段")
                   (equal (plist-get g :from) from)
                   (or (null to) (equal (plist-get g :to) to))))
            (my/org-project--gates)))

(defun my/org-project--gate-label (gate)
  "门的补全显示文本."
  (format "%s %s [%s·%s]"
          (plist-get gate :suffix)
          (or (plist-get gate :title) "")
          (plist-get gate :type)
          (or (plist-get gate :todo) "无状态")))

(defun my/org-project--gate-sort-key (gate)
  "门的排序键: (类型序 . 编号); 冻结门在前, 同类型按编号数字排序."
  (let* ((suffix (plist-get gate :suffix))
         (num (if (and suffix (string-match "\\([0-9]+\\)\\'" suffix))
                  (string-to-number (match-string 1 suffix))
                0)))
    (cons (if (equal (plist-get gate :type) "冻结") 0 1) num)))

(defun my/org-project--gate-select (&optional open-only)
  "选择门, 返回门 plist; OPEN-ONLY 时只在未关闭的门中选.
默认值取当前阶段最新一道未关闭的冻结门, 否则第一道门."
  (let* ((gates (my/org-project--gates))
         (cands (if open-only
                    (seq-filter #'my/org-project--gate-open-p gates)
                  gates))
         (phase (my/org-project--file-stage))
         (default (or (car (last (seq-filter
                                  (lambda (g)
                                    (and (my/org-project--gate-open-p g)
                                         (equal (plist-get g :type) "冻结")
                                         (equal (plist-get g :phase) phase)))
                                  cands)))
                      (car cands))))
    (unless cands (user-error "本项目没有可用的门, 请检查「里程碑」章"))
    (let ((choice (completing-read "门: "
                                   (mapcar #'my/org-project--gate-label cands)
                                   nil t nil nil
                                   (my/org-project--gate-label default))))
      (seq-find (lambda (g) (equal (my/org-project--gate-label g) choice))
                cands))))

(defun my/org-project--gate-insert-freeze (code k phase &optional version)
  "在「里程碑」章末尾插入冻结门 B<K>, 返回门 ID."
  (goto-char (point-min))
  (unless (re-search-forward my/org-project--gate-chapter-re nil t)
    (user-error "未找到「里程碑」章"))
  (let ((level (org-current-level)))
    (org-end-of-subtree t t)
    (unless (bolp) (insert "\n"))
    (insert "\n" (my/org-project--gate-text-freeze code k version phase level)))
  (my/org-project--align-tables)
  (format "%s_B%d" code k))

(defun my/org-project--gate-ensure-freeze (code k phase version)
  "确保版本 VERSION 有对应冻结门并绑定 VERSION/PHASE, 返回门 ID.
优先复用 :VERSION: 已等于 VERSION 的门 (文件改名/代号不一致时仍能对上),
其次复用 B<K>, 都没有则新建."
  (let* ((by-version (seq-find
                      (lambda (g)
                        (and (equal (plist-get g :type) "冻结")
                             (equal (plist-get g :version) version)))
                      (my/org-project--gates)))
         (id (or (and by-version (plist-get by-version :id))
                 (format "%s_B%d" code k))))
    (if (my/org-project--gate-get id)
        (progn
          (my/org-project--gate-goto id)
          (org-entry-put nil "VERSION" version)
          (unless (org-string-nw-p (org-entry-get nil "PHASE"))
            (org-entry-put nil "PHASE" phase))
          id)
      (my/org-project--gate-insert-freeze code k phase version))))

(defun my/org-project-gate-dispatch ()
  "门禁操作: 关门 / 重开 / 查看判据 / 跳到评审表.
关门与转段都遵循「列缺口 + 二次确认」; M4 终点门只能从这里关闭."
  (interactive)
  (unless (and (derived-mode-p 'org-mode) (buffer-file-name))
    (user-error "请在项目 org 文件中执行"))
  (let* ((gate (my/org-project--gate-select))
         (id (plist-get gate :id))
         (action (completing-read
                  (format "门 %s: " (my/org-project--gate-label gate))
                  '("关门" "重开" "查看判据" "跳到评审表") nil t)))
    (pcase action
      ("关门" (my/org-project--gate-close id))
      ("重开" (my/org-project--gate-reopen id))
      ("查看判据"
       (let* ((stats (my/org-project--gate-criteria id))
              (done (nth 0 stats))
              (total (nth 1 stats))
              (gaps (nth 2 stats)))
         (message "%s 判据 %d/%d%s" id done total
                  (if gaps (concat "; 未勾: " (mapconcat #'identity gaps " / "))
                    ""))))
      ("跳到评审表" (my/org-project--gate-review-table-goto id)))))

(defun org-dblock-write:gate-summary (_params)
  "生成门禁台账: 按阶段分组列出全部门, 判据为人为判断, 事实列由数据计算."
  (let* ((gates (my/org-project--gates))
         (reqs (my/org-project--req-entries))
         (unbased (seq-count (lambda (e)
                               (not (org-string-nw-p (plist-get e :baseline))))
                             reqs))
         (phases (append my/org-project--stages '(""))))
    (insert "| 门 | 类型 | 状态 | 阶段 | 版本 | 判据 | 事实 | 评审 |\n")
    (insert "|----+------+------+------+------+------+------+------|\n")
    (dolist (phase phases)
      (let ((rows (sort (seq-filter
                         (lambda (g)
                           ;; 转段门按来源阶段归组, 冻结门按 :PHASE:
                           (equal (or (plist-get g :phase)
                                      (plist-get g :from) "")
                                  (or phase "")))
                         gates)
                        (lambda (a b)
                          (let ((ka (my/org-project--gate-sort-key a))
                                (kb (my/org-project--gate-sort-key b)))
                            (if (= (car ka) (car kb))
                                (< (cdr ka) (cdr kb))
                              (< (car ka) (car kb))))))))
        (when rows
          (insert (format "# %s\n" (if (string= phase "") "未绑定阶段" phase)))
          (dolist (g rows)
            (let* ((id (plist-get g :id))
                   ;; 判据/评审表读取会移动 point, 必须包在 save-excursion 里
                   (stats (save-excursion (my/org-project--gate-criteria id)))
                   (done (nth 0 stats))
                   (total (nth 1 stats))
                   (facts
                    (if (equal (plist-get g :type) "冻结")
                        (format "未入基线 %d 条" unbased)
                      (let* ((from (plist-get g :from))
                             (fs (my/org-project--gate-for-phase from "冻结"))
                             (done (seq-count
                                    (lambda (x)
                                      (equal (plist-get x :todo) "DONE"))
                                    fs))
                             (void (seq-count
                                    (lambda (x)
                                      (equal (plist-get x :todo) "CANCELLED"))
                                    fs)))
                        (if (> void 0)
                            (format "冻结门 %d/%d 已关 (%d 作废)"
                                    done (length fs) void)
                          (format "冻结门 %d/%d 已关" done (length fs)))))))
              (insert (format "| [[#%s][%s]] | %s | %s | %s | %s | %d/%d | %s | %d |\n"
                              id (plist-get g :suffix)
                              (plist-get g :type)
                              (or (plist-get g :todo) "无状态")
                              (or (plist-get g :phase) "")
                              (or (plist-get g :version) "")
                              done total facts
                              (save-excursion
                                (my/org-project--gate-review-count id)))))))))
    (insert (format "# 门 %d 道 · 未关闭 %d 道 · 需求未入基线 %d 条\n"
                    (length gates)
                    (seq-count #'my/org-project--gate-open-p gates)
                    unbased))))

;; ---- 门禁升级 Gate chapter upgrade ----
(defun my/org-project--gate-chapter-p ()
  "当前缓冲区「里程碑」章是否已含 gate-summary 动态块."
  (save-excursion
    (goto-char (point-min))
    (and (re-search-forward my/org-project--gate-chapter-re nil t)
         (let ((end (save-excursion (org-end-of-subtree t t) (point))))
           (save-excursion
             (re-search-forward "^[ \t]*#\\+BEGIN:[ \t]+gate-summary" end t))))))

(defun my/org-project--named-table-rows (name)
  "返回本缓冲区名为 NAME 的表格的数据行文本(不含表头与分隔行), 无则 nil."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward
           (concat "^[ \t]*#\\+NAME:[ \t]+" (regexp-quote name) "[ \t]*$") nil t)
      (forward-line 1)
      (when (org-at-table-p)
        (let ((end (save-excursion (org-table-end))))
          (forward-line 2)
          (when (< (point) end)
            (buffer-substring-no-properties (point) end)))))))

(defun my/org-project--table-append-rows (name rows)
  "把 ROWS 文本追加到名为 NAME 的表格末尾; 表不存在时忽略."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward
           (concat "^[ \t]*#\\+NAME:[ \t]+" (regexp-quote name) "[ \t]*$") nil t)
      (forward-line 1)
      (when (org-at-table-p)
        (goto-char (org-table-end))
        (insert rows)
        (org-table-align)))))

(defun my/org-project--rb-number (ver)
  "从 RB<n> 提取 n; 非法版本返回 0."
  (if (and ver (string-match "\\`RB\\([0-9]+\\)\\'" ver))
      (string-to-number (match-string 1 ver))
    0))

(defun my/org-project--baseline-versions ()
  "返回基线记录表中的 (版本 . 阶段) 列表; 无记录表时从需求 :BASELINE: 推导."
  (or (save-excursion
        (goto-char (point-min))
        (when (re-search-forward
               "^[ \t]*#\\+NAME:[ \t]+baseline-records[ \t]*$" nil t)
          (forward-line 1)
          (when (org-at-table-p)
            (let ((end (save-excursion (org-table-end))) rows)
              (forward-line 2)
              (while (< (point) end)
                (when (looking-at "^[ \t]*|\\([^|]*\\)|\\([^|]*\\)|")
                  ;; 先取出匹配串: string-trim 内部会重置 match data
                  (let ((ver (match-string 1))
                        (phase (match-string 2)))
                    (setq ver (string-trim (or ver ""))
                          phase (string-trim (or phase "")))
                    (when (string-match "\\`RB[0-9]+\\'" ver)
                      (push (cons ver (and (org-string-nw-p phase) phase)) rows))))
                (forward-line 1))
              (nreverse rows)))))
      (let (vers)
        (dolist (e (my/org-project--req-entries))
          (dolist (v (split-string (or (plist-get e :baseline) "")))
            (when (string-match "\\`RB[0-9]+\\'" v)
              (push (cons v (plist-get e :phase)) vers))))
        (seq-uniq vers (lambda (a b) (equal (car a) (car b)))))))

(defun my/org-project--legacy-m2-info ()
  "返回旧「M2 需求基线建立」条目的 (存在 . (TODO . CLOSED)) 信息."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^\\*+ TODO M2 需求基线建立\\(?:[ \t].*\\)?$" nil t)
      (let ((todo (my/org-project--state))
            (closed (org-entry-get nil "CLOSED")))
        (cons t (cons todo closed))))))

(defun my/org-project--legacy-m2-remove ()
  "删除需求章的旧 M2 条目与其 review-M2 表."
  (goto-char (point-min))
  (when (re-search-forward
         "^\\*+ TODO M2 需求基线建立\\(?:[ \t].*\\)?$" nil t)
    (let ((beg (line-beginning-position))
          (end (save-excursion (org-end-of-subtree t t) (point))))
      (delete-region beg end)))
  (goto-char (point-min))
  (when (re-search-forward
         "^[ \t]*#\\+NAME:[ \t]+review-M2[ \t]*$" nil t)
    (let ((beg (line-beginning-position)))
      (forward-line 1)
      (when (org-at-table-p)
        (let ((end (save-excursion (org-table-end))))
          (delete-region beg (min (1+ end) (point-max))))))))

(defun my/org-project-gate-upgrade ()
  "把当前项目文件的「里程碑」章升级为门禁台账 (幂等).
迁移旧评审行 (M1→M1, M2→B<最新版本>, M3→M2, M4→M3, M5→M4);
按 baseline-records 逐版本补建冻结门; 删除需求章的旧 M2 与其评审表."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "请在项目 org 文件中执行"))
  (org-with-wide-buffer
   (if (my/org-project--gate-chapter-p)
       (message "「里程碑」章已是门禁台账, 无需升级")
     (let* ((code (file-name-base (buffer-file-name)))
            (versions (my/org-project--baseline-versions))
            (maxver (max 1
                         (my/org-project--baseline-max
                          (my/org-project--req-entries))
                         (if versions
                             (apply #'max
                                    (mapcar (lambda (v)
                                              (my/org-project--rb-number (car v)))
                                            versions))
                           0)))
            (legacy (my/org-project--legacy-m2-info))
            (rows (mapcar (lambda (name)
                            (cons name (my/org-project--named-table-rows
                                        (concat "review-" name))))
                          '("M1" "M2" "M3" "M4" "M5"))))
       ;; 先删旧 M2 与其评审表, 再重建章, 避免误删新 M2 的 review-M2
       (my/org-project--legacy-m2-remove)
       (goto-char (point-min))
       (if (not (re-search-forward my/org-project--gate-chapter-re nil t))
           (user-error "未找到「里程碑」章, 请先在「研制过程」下补建")
         (let* ((level (org-current-level))
                (beg (line-beginning-position))
                (end (save-excursion (org-end-of-subtree t t) (point))))
           (delete-region beg end)
           (goto-char beg)
           (insert (my/org-project--gates-text code level) "\n")))
       ;; 迁入旧评审行
       (dolist (pair '(("M1" . "M1") ("M3" . "M2") ("M4" . "M3") ("M5" . "M4")))
         (let ((old (cdr (assoc (car pair) rows))))
           (when old
             (my/org-project--table-append-rows
              (concat "review-" (cdr pair)) old))))
       ;; 按基线记录补建冻结门
       (dolist (v versions)
         (let ((k (and (string-match "\\`RB\\([0-9]+\\)\\'" (car v))
                       (string-to-number (match-string 1 (car v))))))
           (when k
             (my/org-project--gate-ensure-freeze code k (cdr v) (car v)))))
       ;; 旧 M2 的评审行迁到最新版本的冻结门 (门已建好)
       (let ((oldm2 (cdr (assoc "M2" rows))))
         (when oldm2
           (my/org-project--table-append-rows
            (format "review-B%d" maxver) oldm2)))
       ;; 若基线从 RB2 起, B1 占位门无对应版本, 作废避免挡住转段
       (let ((b1 (my/org-project--gate-get (format "%s_B1" code))))
         (when (and versions
                    b1
                    (not (org-string-nw-p (plist-get b1 :version))))
           (my/org-project--gate-cancel (plist-get b1 :id))))
       ;; 旧 M2 已关闭则关闭对应冻结门
       (when (and (car legacy)
                  (member (car (cdr legacy)) '("DONE")))
         (let ((id (format "%s_B%d" code maxver)))
           (when (my/org-project--gate-get id)
             (my/org-project--gate-goto id)
             (let ((org-enforce-todo-checkbox-dependencies nil)
                   (org-enforce-todo-dependencies nil))
               (org-todo "DONE")))))
       (org-update-all-dblocks)
       (message "「里程碑」章已升级为门禁台账 (冻结门 %d 道)"
                (length (seq-filter
                         (lambda (g) (equal (plist-get g :type) "冻结"))
                         (my/org-project--gates))))))))

(defun my/org-project-gate-upgrade-all ()
  "批量把 projects/ 下项目文件的「里程碑」章升级为门禁台账.
升级前用 tar 备份 projects/ (~/Org 不在版本控制内), 已升级的自动跳过."
  (interactive)
  (let* ((files (my/org-project--files))
         (pending (seq-filter
                   (lambda (f)
                     (let ((org-inhibit-startup t))
                       (with-current-buffer (find-file-noselect (cdr f))
                         (not (my/org-project--gate-chapter-p)))))
                   files)))
    (unless pending (user-error "所有项目文件的里程碑章都已是门禁台账"))
    (when (y-or-n-p (format "将升级 %d 个文件的门禁章, 并先备份 projects/? "
                            (length pending)))
      (let ((tar (expand-file-name
                  (format "projects-gate-backup-%s.tar.gz"
                          (format-time-string "%Y%m%d-%H%M%S"))
                  org-directory)))
        (call-process "tar" nil nil nil "-czf" tar "-C" org-directory "projects")
        (message "已备份: %s" tar))
      (let ((n 0))
        (dolist (f pending)
          (let ((org-inhibit-startup t))
            (with-current-buffer (find-file-noselect (cdr f))
              (my/org-project-gate-upgrade)
              (save-buffer)
              (setq n (1+ n)))))
        (message "已升级 %d 个文件的门禁章" n)))))

;; ---- 需求基线 Requirement baseline ----
(defun my/org-project--baseline-max (entries)
  "扫描需求条目 ENTRIES 的 :BASELINE: 属性, 返回既有最大版本号 (无则 0)."
  (let ((max 0))
    (dolist (e entries)
      (dolist (v (split-string (or (plist-get e :baseline) "")))
        (when (string-match "\\`RB\\([0-9]+\\)\\'" v)
          (setq max (max max (string-to-number (match-string 1 v)))))))
    max))

(defun my/org-project--baseline-record-add (file ver phase count)
  "在项目 FILE 的基线记录表追加一行: 版本 VER, 阶段 PHASE, 需求数 COUNT.
表不存在时仅提示跳过."
  (set-buffer (org-capture-target-buffer file))
  (widen)
  (goto-char (point-min))
  (if (not (re-search-forward "^[ \t]*#\\+NAME:[ \t]+baseline-records[ \t]*$" nil t))
      (message "未找到基线记录表 baseline-records, 跳过登记")
    (forward-line 1)
    (when (org-at-table-p)
      (goto-char (org-table-end))
      (insert (format "| %s | %s | %s | %d |  |  |\n"
                      ver (or phase "") (format-time-string "%Y-%m-%d") count))
      (org-table-align))))

(defun my/org-project--req-active-p (entry)
  "需求 ENTRY 是否参与基线 (已归档/废弃/取消的不参与)."
  (not (member (plist-get entry :todo) '("ARCHIVED" "DEPRECATED" "CANCELLED"))))

(defun my/org-project--req-goto (id)
  "按 CUSTOM_ID 定位需求条目, 成功返回 non-nil 且 point 在标题行."
  (goto-char (point-min))
  (when (re-search-forward
         (concat "^[ \t]*:CUSTOM_ID:[ \t]+" (regexp-quote id) "[ \t]*$") nil t)
    (org-back-to-heading t)))

(defun my/org-project-baseline-create ()
  "需求基线全量重圈: 选全部有效需求, 追加新版本并建/绑冻结门.
同一阶段可建多个基线, 版本递增(RB1, RB2...), 冻结门与版本同号。
未选中的需求保留历史 :BASELINE:, 不进入新版本; 存在未关闭的同阶段
冻结门时先询问作废(CANCELLED), 再建新门。"
  (interactive)
  (let ((file (my/org-capture-project-file)))
    (set-buffer (org-capture-target-buffer file))
    (widen)
    (let* ((code (file-name-base (buffer-file-name)))
           (phase (my/org-project--file-stage))
           (entries (my/org-project--req-entries))
           (cands (seq-filter #'my/org-project--req-active-p entries))
           (defver (format "RB%d" (1+ (my/org-project--baseline-max entries))))
           (ver (read-string (format "基线版本 (默认 %s): " defver) nil nil defver))
           (kn (and (string-match "\\`RB\\([0-9]+\\)\\'" ver)
                    (string-to-number (match-string 1 ver))))
           (choices (mapcar (lambda (e)
                              (format "%s %s" (plist-get e :id)
                                      (plist-get e :title)))
                            cands))
           (sel (completing-read-multiple
                 "圈定需求(回车=全选, 逗号分隔可排除): " choices))
           (ids (if (or (null sel) (equal sel '("")))
                    (mapcar (lambda (e) (plist-get e :id)) cands)
                  (mapcar (lambda (s) (car (split-string s " "))) sel)))
           (n 0))
      (unless cands (user-error "没有可参与基线的需求条目"))
      (unless kn (user-error "版本号需形如 RB<n>: %s" ver))
      (unless ids (user-error "未选择任何需求"))
      ;; 1) 追加 :BASELINE: (多值, 已存在则跳过)
      (dolist (id ids)
        (when (my/org-project--req-goto id)
          (let* ((cur (org-entry-get nil "BASELINE"))
                 (vals (split-string (or cur ""))))
            (unless (member ver vals)
              (org-entry-put nil "BASELINE"
                             (string-trim (concat (or cur "") " " ver)))
              (setq n (1+ n))))))
      (my/org-project--baseline-record-add file ver phase n)
      ;; 2) 作废同阶段未关闭的旧冻结门, 再建/绑新门
      (dolist (g (my/org-project--gate-open-freeze phase))
        (when (and (equal (plist-get g :artifact) "需求基线")
                   (not (equal (plist-get g :version) ver))
                   (y-or-n-p (format "冻结门 %s（%s）尚未关闭, 作废? "
                                     (plist-get g :suffix)
                                     (or (plist-get g :version) "未绑版本"))))
          (my/org-project--gate-cancel (plist-get g :id))))
      (my/org-project--gate-ensure-freeze code kn phase ver)
      (my/org-project--baseline-dblock-ensure file ver)
      (with-current-buffer (org-capture-target-buffer file)
        (org-update-all-dblocks))
      ;; 3) 判据满足则提示关门, 否则提示下一步
      (let* ((gate-id (format "%s_B%d" code kn))
             (unbased (seq-count
                       (lambda (e)
                         (not (org-string-nw-p (plist-get e :baseline))))
                       (seq-filter #'my/org-project--req-active-p entries)))
             (reviews (my/org-project--gate-review-count gate-id)))
        (if (and (= unbased 0) (> reviews 0))
            (when (y-or-n-p (format "%s 判据已满足, 现在关闭冻结门? " gate-id))
              (my/org-project--gate-close gate-id t))
          (message "已圈定 %s: %d 条需求, 门 %s (未入基线 %d 条, 评审 %d 条; 记完用 C-c P g 关门)"
                   ver n gate-id unbased reviews))))))

(defun my/org-project--baseline-dblock-ensure (file ver)
  "项目 FILE 若无 :version VER 的 req-baseline 快照块, 在基线记录表后插入一个."
  (set-buffer (org-capture-target-buffer file))
  (widen)
  (goto-char (point-min))
  (unless (re-search-forward
           (concat "^[ \t]*#\\+BEGIN:[ \t]+req-baseline :version "
                   (regexp-quote ver) "[ \t]*$") nil t)
    (goto-char (point-min))
    (when (re-search-forward "^[ \t]*#\\+NAME:[ \t]+baseline-records[ \t]*$" nil t)
      (goto-char (org-table-end))
      (forward-line 1)
      (insert "\n    #+BEGIN: req-baseline :version " ver "\n    #+END:\n"))))

(defun my/org-project--baseline-summary ()
  "返回需求基线概况字符串 (供阶段切换日志): 最新版本与入基线条数、未入基线条数.
无任何已圈定需求时返回「基线未建立」."
  (let* ((entries (seq-filter (lambda (e) (plist-get e :phase))
                              (my/org-project--req-entries)))
         (max (my/org-project--baseline-max entries))
         (in 0) (out 0))
    (dolist (e entries)
      (if (org-string-nw-p (plist-get e :baseline))
          (setq in (1+ in))
        (setq out (1+ out))))
    (if (= max 0)
        (format "需求基线未建立 (%d 条未入基线)" out)
      (format "需求基线 RB%d · %d 条已入基线 · %d 条未入基线" max in out))))

(defun my/org-dblock-update ()
  "更新光标处动态块, 失败时回落更新全文所有动态块.
本机 org(native 编译) 的 `org-dblock-update' 对缩进的动态块总是报
\"Not in a dynamic block\", 故加此包装: 优先原命令, 出错则
`org-update-all-dblocks' (项目文件内的 req-by-phase 均为幂等刷新)."
  (interactive)
  (condition-case _
      (org-dblock-update)
    (error (org-update-all-dblocks))))

(defun my/org-capture-refresh-dblocks ()
  "捕获完成后刷新目标缓冲区的 req-by-phase 动态块."
  (when (buffer-live-p my/org-capture--last-buffer)
    (with-current-buffer my/org-capture--last-buffer
      (ignore-errors (org-update-all-dblocks)))))

(use-package org-capture
  :ensure nil
  :after org
  :hook ((org-capture-mode . my/org-capture-setup)
         (org-capture-mode . delete-other-windows)
         ;; 捕获完成后同步 appt 提醒, 避免新日程最长 15 分钟内不触发提醒
         (org-capture-after-finalize . org-agenda-to-appt)
         ;; 需求变更(pc)落表后将对应需求移出基线并置回 DESIGN
         (org-capture-after-finalize . my/org-capture-req-change-finalize)
         ;; 刷新项目文件中的需求动态块(req-by-phase)
         (org-capture-after-finalize . my/org-capture-refresh-dblocks)
         ;; pe 记完冻结门评审后, 判据满足则提示关门
         (org-capture-after-finalize . my/org-capture-gate-review-finalize))
  :custom
  ;; 从日程视图捕获时使用光标所在日期(决定 datetree 归档日期与 %U/%t 的展开)
  (org-capture-use-agenda-date t)
  ;; 模板按键分组: c=收集箱 j=日程计划 p=项目管理 r=参考 x=计时备注
  (org-capture-templates
   `(;; ---- 收集箱 Inbox ----
     ("c" "收集箱 Inbox")
     ("ct" "任务" entry (file+olp "inbox.org" "Tasks")
      ,(concat "* TODO %?\n" my/org-capture--created "\n%i")
      :empty-lines 1 :prepend t)
     ("cp" "项目任务" entry (file+olp "inbox.org" "Projects")
      ,(concat "* TODO %?\n"
               "  :PROPERTIES:\n  :PROJECT: %^{Project name}\n"
               "  :TYPE: %^{类型|需求|任务|风险|议题|日志|其他}\n"
               "  :CREATED: %U\n  :END:\n\n%i")
      :empty-lines 1 :prepend t)
     ("ci" "问题 Issue" entry (file+olp "inbox.org" "Issues")
      ,(concat "* REPORT %?\n"
               "  :PROPERTIES:\n"
               ;; 第一项为默认值(回车选中), 同时保留在候选列表中; 也可直接手动输入
               "  :SOURCE: %^{问题来源|其他|客户反馈|现场问题|测试发现|代码评审|自查发现|其他}\n"
               "  :CREATED: %U\n  :END:\n"
               "\n  一、问题描述\n\n  二、分析定位\n\n  三、措施验证\n\n  四、结论\n")
      :empty-lines 1 :prepend t)
     ;; 随笔: 每次带来源链接
     ("cn" "随笔 Notes" entry (file+olp "inbox.org" "Notes")
      "* %? %^g\n%(my/org-capture-source-link)\n%i"
      :empty-lines 1 :prepend t)

     ;; ---- 日程 / 计划 Journal ----
     ("j" "日程 / 计划 Journal")
     ("jd" "日记" entry (file+olp+datetree "diary.org")
      ,(concat "* TODO %?\n  SCHEDULED: %^t\n" my/org-capture--created)
      :empty-lines 1 :jump-to-captured t)
     ("jp" "计划" entry (function my/org-capture-plan-target)
      ,(concat "* TODO %?\n  SCHEDULED: %^t  DEADLINE: %^t\n"
               my/org-capture--created)
      :empty-lines 1)

     ;; ---- 项目管理 Project ----
     ;; 落点由目标函数解析: 选软件配置项文件后按「研制过程」的
     ;; :CURRENT-STATE: 定位到当前阶段; 新建项目用 pp 生成骨架
     ("p" "项目管理 Project")
     ("pp" "新建项目" entry (function my/org-capture-project-new-target)
      ,(concat "* %u 项目启动\n"
               "  :PROPERTIES:\n  :TYPE: 项目\n  :CREATED: %U\n  :END:\n\n"
               "  - 说明：\n")
      :empty-lines 1 :jump-to-captured t)
     ("pt" "研制任务" entry (function my/org-capture-task-target)
      ,(concat "* TODO %?\n  SCHEDULED: %^t  DEADLINE: %^t\n"
               "  :PROPERTIES:\n"
               "  :CUSTOM_ID: %(my/org-capture--next-id \"TASK\")\n"
               "  :EFFORT: %^{工作量|1d}\n  :END:\n")
      :empty-lines 1)
     ("pl" "测试/试验记录" table-line (function my/org-capture-record-target)
      "%(my/org-capture--record-row)"
      :empty-lines 1)
     ("pe" "评审记录" table-line (function my/org-capture-review-target)
      "| %u | %^{评审类型|内部评审|专家评审|用户评审} | %^{评审对象} | %^{人员} | %^{结论|通过|有条件通过|不通过} | %^{遗留问题(议题链接,可空)} | %^{待办} |"
      :empty-lines 1)
     ("pn" "需求条目" entry (function my/org-capture-req-target)
      ,(concat "* DESIGN %?\n  :PROPERTIES:\n"
               "  :CUSTOM_ID: %(my/org-capture--req-id)\n"
               "  :TYPE: %(my/org-capture--req-type)\n"
               "  :SOURCE: %^{来源|任务书/合同|用户要求|系统需求分解|标准规范|设计输入|继承复用|其他}\n"
               "  :PHASE: %(my/org-capture--stage-prompt)\n"
               "  :BASELINE:\n"
               "  :DESIGN: %^{设计章节(可空)}\n"
               "  :MODULE: %^{实现模块(可空)}\n"
               "  :TEST-CASE: %^{测试用例(可空)}\n"
               "  :CREATED: %U\n  :END:\n")
      :empty-lines 1)
     ("pc" "需求变更" table-line (function my/org-capture-req-change-target)
      "| %u | %(my/org-capture--req-prompt-id) | %^{变更类型|新增|修改|删除|挂起} | %^{变更说明} | %^{原因} | %^{影响范围(设计/模块/测试,可空)} | %^{处置|重新圈定|挂账|终止} | %^{关联议题(可空)} |"
      :empty-lines 1)
     ("pi" "议题" entry (function my/org-capture-issue-target)
      ,(concat "* REPORT %?\n  :PROPERTIES:\n"
               "  :CUSTOM_ID: %(my/org-capture--next-id \"ISSUE\")\n"
               "  :PHASE: %^{问题来源|方案设计|样机调试|联调|试验|环境试验|状态鉴定|交付后|评审|其他}\n"
               "  :SEVERITY: %^{严重程度|轻微|一般|严重|致命}\n"
               "  :AFFECTED: %(my/org-capture--affected-version)\n"
               "  :CREATED: %U\n  :END:\n\n"
               "  *1. 问题描述*\n\n  *2. 分析定位*\n\n  *3. 措施验证*\n\n  *4. 结论*\n")
      :empty-lines 1)
     ("ps" "软件状态版本" entry (function my/org-capture-state-target)
      ,(concat "* DESIGN %(my/org-capture--sw-prompt)\n  :PROPERTIES:\n"
               "  :CUSTOM_ID: %(my/org-capture--sw-custom-id)\n"
               "  :PHASE: %(my/org-capture--current-stage)\n"
               "  :TRIGGER: %(my/org-capture--trigger)\n"
               "  :CREATED: %U\n  :END:\n\n"
               "  - 版本简述：%(my/org-capture--sw-desc)\n\n"
               "  | 修改项 | 原因 | 影响范围 | 关联议题 |\n"
               "  |--------+------+----------+----------|\n\n"
               "  | 测试项 | 指标要求 | 实测结果 | 结论 |\n"
               "  |--------+----------+----------+------|\n")
      :empty-lines 1)
     ("pr" "风险" entry (function my/org-capture-risk-target)
      ,(concat "* IDENT %?\n  :PROPERTIES:\n"
               "  :CUSTOM_ID: %(my/org-capture--next-id \"RISK\")\n"
               "  :PHASE: %(my/org-capture--stage-prompt)\n"
               "  :ORIGIN: %^{风险来源|设计|需求|接口|器件|试验|评审|进度|资源|人员|其他}\n"
               "  :ORIGIN-LINK: %^{来源链接(建议填, 可空)}\n"
               "  :PROBABILITY: %(my/org-capture--risk-prompt 'probability \"可能性\" \"中\")\n"
               "  :IMPACT: %(my/org-capture--risk-prompt 'impact \"影响\" \"中\")\n"
               "  :RISK_LEVEL: %(my/org-capture--risk-level)\n"
               "  :RESPONSE: %^{应对策略|减轻|规避|转移|接受}\n"
               "  :TRIGGER: %^{预警判据(可观测, 可空)}\n"
               "  :OWNER: %^{责任人|王北洛}\n"
               "  :REVIEW: %^{下次复核(可空)}\n"
               "  :CREATED: %U\n  :END:\n\n"
               "  - 风险描述：\n"
               "  - 触发后果：\n"
               "  - 措施：\n"
               "    - [ ] \n"
               "  - 应急/降级预案：\n"
               "  - 残余风险：\n")
      :empty-lines 1)
     ("pd" "日志/会议" entry (function my/org-capture-log-target)
      ,(concat "* %?\n  :PROPERTIES:\n"
               "  :TYPE: %(my/org-capture--log-type)\n"
               "  :CREATED: %U\n  :END:\n"
               "%(my/org-capture--log-fields)\n")
      :empty-lines 1)

     ;; ---- 参考 Reference ----
     ("r" "参考 Reference")
     ;; 书签: 每次带来源链接
     ("rb" "书签" entry (file+olp "inbox.org" "Bookmarks")
      "* %:description %?\n  %U\n%(my/org-capture-source-link \"\")"
      :empty-lines 1 :prepend t)
     ;; Denote 笔记: 内容格式见 init-note.el 的 denote-org-capture-specifiers
     ("rn" "Denote 笔记" plain
      (file (lambda () (require 'denote) denote-last-path))
      #'denote-org-capture
      :no-save t :kill-buffer t :jump-to-captured t)

     ;; ---- 其他 ----
     ("x" "计时任务备注" item (clock) "- %? %U"))))

;;; 6. 扩展 Extensions ---------------------------------------------------------

;; 自动生成 ID 链接标题
(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
;; org-id 存储位置
(setq org-id-locations-file (concat org-directory "/.org-id-locations"))

;; 双向链接
(use-package org-super-links
  :vc (:url "https://github.com/toshism/org-super-links"
       :rev :last-release)
  :bind (("C-c s l" . org-super-links-link)
         ("C-c s s" . org-super-links-store-link)
         ("C-c s i" . org-super-links-insert-link)
         ("C-c s d" . org-super-links-quick-insert-drawer-link)
         ("C-c s I" . org-super-links-quick-insert-inline-link)
         ("C-c s C-d" . org-super-links-delete-link))
  :config
  ;; 不依赖 Helm
  (setq org-super-links-search-function 'org-super-links-get-location)
  (setq org-super-links-related-into-drawer t)
  ;; 自动为链接的目标添加 ID（如果目标没有 ID）
  (setq org-super-links-link-prefix 'org-super-links-link-prefix-timestamp))

;; org 自带链接命令也统一挂到 C-c s 前缀下，与 org-super-links 放在一起。
;; 打开链接保持默认的 `C-c C-o'（org-open-at-point），这里不重复绑定。
;; l/s/i/d/I/C-d 由上面的 org-super-links 占用，此处只补剩下的常用命令。
(global-set-key (kbd "C-c s L") #'org-insert-link)              ; 插入/编辑链接（默认 C-c C-l）
(global-set-key (kbd "C-c s S") #'org-store-link)               ; 存储链接（默认 C-c l，与 lsp 前缀冲突）
(global-set-key (kbd "C-c s u") #'org-insert-last-stored-link)  ; 插入最近存储的链接（默认 C-c M-l）
(global-set-key (kbd "C-c s t") #'org-toggle-link-display)      ; 链接字面 <-> 描述显示切换
(global-set-key (kbd "C-c s n") #'org-next-link)                ; 跳到下一个链接
(global-set-key (kbd "C-c s p") #'org-previous-link)            ; 跳到上一个链接
(global-set-key (kbd "C-c s b") #'org-mark-ring-goto)           ; 跳转后返回原处（默认 C-c &）
(global-set-key (kbd "C-c s g") #'org-id-get-create)            ; 给当前标题生成 ID

(provide 'init-org)

;;; init-org.el ends here
