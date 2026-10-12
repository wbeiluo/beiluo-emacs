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
(define-key org-mode-map (kbd "C-c _") #'org-timestamp-inactive)

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
  ;; 时间戳美化：色块字号（org-modern-label 的 0.8 倍）比正文字号窄，
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
     (sequence "DESIGN(s)" "DEVELOP(p!)" "CONFIRMED(e!)" "|" "DEPRECATED(y@/!)" "ARCHIVED(a@/!)")))
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
     ("ARCHIVED" ("archived" . t) ("design") ("develop") ("confirmed"))))
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
  (org-global-properties '(("EFFORT_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 7:00 8:00")
                           ("APPT_WARNTIME_ALL" . "0 5 10 15 20 25 30 45 60")
                           ("RISK_ALL" . "Low Medium High")
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
                  '(;; 日程任务
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
                    (:discard (:anything t)))))))))))

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
  (setq-local org-complete-tags-always-offer-all-agenda-tags t))

(use-package org-capture
  :ensure nil
  :after org
  :hook ((org-capture-mode . my/org-capture-setup)
         (org-capture-mode . delete-other-windows)
         ;; 捕获完成后同步 appt 提醒, 避免新日程最长 15 分钟内不触发提醒
         (org-capture-after-finalize . org-agenda-to-appt))
  :custom
  ;; 从日程视图捕获时使用光标所在日期(决定 datetree 归档日期与 %U/%t 的展开)
  (org-capture-use-agenda-date t)
  ;; 模板按键分组: c=收集箱 j=日程计划 r=参考 x=计时备注
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
