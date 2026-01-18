;;; init-org.el --- Org Mode Configurations -*- lexical-binding: t -*-

;; Copyright (C) 2020~2025 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;; Code:

(defconst extensions-ts-dir
  (expand-file-name "extensions/ts" user-emacs-directory))
(defconst extensions-org-super-agenda-dir
  (expand-file-name "extensions/org-super-agenda" user-emacs-directory))
(defconst extensions-org-modern-dir
  (expand-file-name "extensions/org-modern" user-emacs-directory))
(defconst extensions-org-appear-dir
  (expand-file-name "extensions/org-appear" user-emacs-directory))
(defconst extensions-denote-dir
  (expand-file-name "extensions/denote" user-emacs-directory))
(defconst extensions-consult-notes-dir
  (expand-file-name "extensions/consult-notes" user-emacs-directory))
(defconst extensions-org-super-links-dir
  (expand-file-name "extensions/org-super-links" user-emacs-directory))

(require 'org)
(require 'org-agenda)
(require 'appt)
(require 'notifications)
(require 'org-capture)

;;; Org mode设置 ----------------------------------------------------------------

(custom-set-faces
 ;; 设置Org mode标题以及每级标题行的大小
 '(org-document-title ((t (:height 1.2 :weight bold))))
 '(org-level-1 ((t (:height 1.15 :weight bold))))
 '(org-level-2 ((t (:height 1.10 :weight bold))))
 '(org-level-3 ((t (:height 1.05 :weight bold))))
 '(org-level-4 ((t (:height 1.0 :weight bold))))
 '(org-level-5 ((t (:height 1.0 :weight bold))))
 '(org-level-6 ((t (:height 1.0 :weight bold))))
 '(org-level-7 ((t (:height 1.0 :weight bold))))
 '(org-level-8 ((t (:height 1.0 :weight bold))))
 '(org-level-9 ((t (:height 1.0 :weight bold))))
 '(org-table ((t (:font "LXGW WenKai Mono:pixelsize=26"))))
 '(org-date ((t (:font "LXGW WenKai Mono:pixelsize=26")))))

;; 在org mode里美化字符串标志
(defun my/org-prettify-symbols ()
  "Set up prettify symbols icon."
  (setq prettify-symbols-alist
        (mapcan (lambda (x) (list x (cons (upcase (car x)) (cdr x))))
                '(("[#A]"            . "🅐")
                  ("[#B]"            . "🅑")
                  ("[#C]"            . "🅒")
                  ("[ ]"             . "󰄱")
                  ("[X]"             . "󰄵")
                  ("[-]"             . "󰡖")
                  ("#+begin_src"     . "")
                  ("#+end_src"       . "")
                  ("#+begin_example" . "")
                  ("#+end_example"   . "")
                  ("#+results:"      . "")
                  ("#+attr_latex:"   . "🄛")
                  ("#+attr_html:"    . "🄗")
                  ("#+attr_org:"     . "🄞")
                  ("#+name:"         . "🄝")
                  ("#+caption:"      . "🄒")
                  ("#+date:"         . "")
                  ("#+author:"       . "")
                  ("#+setupfile:"    . "")
                  ("#+email:"        . "󰇰")
                  ("#+startup:"      . "")
                  ("#+options:"      . "")
                  ("#+title:"        . "")
                  ("#+subtitle:"     . "󰨖")
                  ("#+description:"  . "🅘")
                  ("#+filetags:"     . "󰓻")
                  ("#+identifier:"   . "󰻾")
                  ("#+downloaded:"   . "")
                  ("#+language:"     . "")
                  ("#+begin_quote"   . "")
                  ("#+end_quote"     . "")
                  ("#+begin_results" . "⋯")
                  ("#+end_results"   . "⋯"))))
  (setq prettify-symbols-unprettify-at-point t)
  (prettify-symbols-mode 1))
(add-hook 'org-mode-hook #'my/org-prettify-symbols)

;; 设置优先级样式
(setq org-priority-faces
      '((?A :inherit org-priority :weight regular :foreground "IndianRed" :inverse-video nil)
        (?B :inherit org-priority :weight regular :foreground "DarkOrange" :inverse-video nil)
        (?C :inherit org-priority :weight regular :foreground "ForestGreen" :inverse-video nil)))

;; 提升latex预览的图片清晰度
(plist-put org-format-latex-options :scale 1.8)

;; 设置标题行之间总是有空格；列表之间根据情况自动加空格
(setq org-blank-before-new-entry '((heading . t)
                                   (plain-list-item . auto)))

;; 设置打开Org links的程序
(setq org-file-apps '(("\\.png\\'"     . default)
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

;; 设置Org mode的目录
(setq org-directory "~/Org")
;; 设置笔记的默认存储位置
(setq org-default-notes-file (expand-file-name "inbox.org" org-directory))
;; 启用一些子模块
(setq org-modules '(ol-bibtex ol-gnus ol-info ol-eww org-habit org-protocol))
;; 设置标题行折叠符号
(setq org-ellipsis "⋯")
;; 在活动区域内的所有标题栏执行某些命令
(setq org-loop-over-headlines-in-active-region t)
;; 隐藏宏标记
(setq org-hide-macro-markers t)
;; 隐藏强调标签
(setq org-hide-emphasis-markers t)
;; 隐藏符号
(setq org-pretty-entities t)
;; 高亮latex语法
(setq org-highlight-latex-and-related '(native script entities))
;; 开启缩进模式
(setq org-indent-mode t)
;; 隐藏标题星号
(setq org-hide-leading-stars t)
;; 根据标题栏自动缩进文本
(setq org-adapt-indentation t)
;; 自动显示图片
(setq org-startup-with-inline-images t)
;; 默认以Overview的模式展示标题行
(setq org-startup-folded 'overview)
;; 允许字母列表
(setq org-list-allow-alphabetical t)
;; 列表的下一级设置
(setq org-list-demote-modify-bullet '(("-"  . "+")
                                      ("+"  . "1.")
                                      ("1." . "a.")))
;; 编辑时检查是否在折叠的不可见区域
(setq org-fold-catch-invisible-edits 'smart)
;; 在当前位置插入新标题行还是在当前标题行后插入，这里设置为当前位置
(setq org-insert-heading-respect-content nil)
;; 设置图片的最大宽度，如果有imagemagick支持将会改变图片实际宽度
;; 四种设置方法：(1080), 1080, t, nil
(setq org-image-actual-width nil)
;; imenu的最大深度，默认为2
(setq org-imenu-depth 4)
;; 回车要不要触发链接，这里设置不触发
(setq org-return-follows-link nil)
;; 上标^下标_是否需要特殊字符包裹，这里设置需要用大括号包裹
(setq org-use-sub-superscripts '{})
;; 复制粘贴标题行的时候删除id
(setq org-clone-delete-id t)
;; 粘贴时调整标题行的级别
(setq org-yank-adjusted-subtrees t)

;; TOOD的关键词设置，可以设置不同的组
(setq org-todo-keywords
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
        ;; DEVELOP   : 新功能开发中
        ;; MODIFY    : 修改已有代码
        ;; CONFIRMED : 已确认(完成测试及验证)
        ;; DEPRECATED: 已废弃，不再维护
        ;; ARCHIVED  : 已归档
        (sequence "DESIGN(s)" "DEVELOP(p!)" "MODIFY(m!)" "CONFIRMED(e!)" "|" "DEPRECATED(y@/!)" "ARCHIVED(a@/!)")

        ;; --- 长期规划状态 ---
        ;; VISION    : 方向与目标
        ;; FOCUS     : 聚焦选择
        ;; BUILD     : 能力建设
        ;; PRACTICE  : 持续实践
        ;; REVIEW    : 评估
        ;; ACHIEVED  : 实现目标
        (sequence "VISION(v)" "FOCUS(o!)" "BUILD(u!)" "PRACTICE(t!)" "REVIEW(i!)" "|" "ACHIEVED(g)")
        ))


;; 当标题行状态变化时标签同步发生的变化
(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("cancelled" . t) ("wait"))
              ("WAIT" ("wait" . t))
              (done ("wait"))
              ("TODO" ("wait") ("cancelled"))
              ("DONE" ("wait") ("cancelled"))

              ("REPORT" ("report" . t) ("fixed") ("closed"))
              ("BUG" ("bug" . t) ("fixed") ("closed"))
              ("KNOWNCAUSE" ("knowncause" . t) ("fixed") ("closed"))
              ("FIXED" ("fixed" . t) ("report") ("bug") ("knowncause"))
              ("CLOSED" ("closed" . t) ("report") ("bug") ("knowncause") ("fixed"))

              ("DESIGN" ("design" . t) ("develop") ("modify") ("confirmed"))
              ("DEVELOP" ("develop" . t) ("design") ("modify") ("confirmed"))
              ("MODIFY" ("modify" . t) ("design") ("develop") ("confirmed"))
              ("CONFIRMED" ("confirmed" . t) ("design") ("develop") ("modify"))
              ("DEPRECATED" ("deprecated" . t) ("design") ("develop") ("modify") ("confirmed"))
              ("ARCHIVED" ("archived" . t) ("design") ("develop") ("modify") ("confirmed"))

              ;; -------- 长期规划阶段 --------
              ("VISION" ("vision" . t) ("focus") ("build") ("practice") ("review") ("achieved"))
              ("FOCUS" ("focus" . t) ("vision") ("build") ("practice") ("review") ("achieved"))
              ("BUILD" ("build" . t) ("vision") ("focus") ("practice") ("review") ("achieved"))
              ("PRACTICE" ("practice" . t) ("vision") ("focus") ("build") ("review") ("achieved"))
              ("REVIEW" ("review" . t) ("vision") ("focus") ("build") ("practice") ("achieved"))
              ("ACHIEVED" ("achieved" . t) ("vision") ("focus") ("build") ("practice") ("review"))
              )))

;; 始终存在的的标签
(setq org-tag-persistent-alist '(("read"     . ?r)
                                 ("study"    . ?s)
                                 ("work"     . ?w)
                                 ("project"  . ?p)
                                 ("emacs"    . ?e)
                                 ("life"     . ?l)
                                 ("misc"     . ?m)))
;; 预定义好的标签
(setq org-tag-alist '((:startgroup)
                      ("play"     . ?y)
                      ("tour"     . ?t)
                      (:endgroup)))

;; 使用专家模式选择标题栏状态
(setq org-use-fast-todo-selection 'expert)
;; 父子标题栏状态有依赖
(setq org-enforce-todo-dependencies t)
;; 标题栏和任务复选框有依赖
(setq org-enforce-todo-checkbox-dependencies t)
;; 标题行全局属性设置
(setq org-global-properties '(("EFFORT_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 7:00 8:00")
                              ("APPT_WARNTIME_ALL" . "0 5 10 15 20 25 30 45 60")
                              ("RISK_ALL" . "Low Medium High")
                              ("STYLE_ALL" . "habit")))
;; Org columns的默认格式
(setq org-columns-default-format "%25ITEM %TODO %SCHEDULED %DEADLINE %3PRIORITY %TAGS %CLOCKSUM %EFFORT{:}")
;; 当状态从DONE改成其他状态时，移除 CLOSED: [timestamp]
(setq org-closed-keep-when-no-todo t)
;; DONE时加上时间戳
(setq org-log-done 'time)
;; 重复执行时加上时间戳
(setq org-log-repeat 'time)
;; Deadline修改时加上一条记录
(setq org-log-redeadline 'note)
;; Schedule修改时加上一条记录
(setq org-log-reschedule 'note)
;; 以抽屉的方式记录
(setq org-log-into-drawer t)
;; 紧接着标题行或者计划/截止时间戳后加上记录抽屉
(setq org-log-state-notes-insert-after-drawers nil)
;; refile使用缓存
(setq org-refile-use-cache t)
;; refile的目的地，这里设置的是agenda文件的所有标题
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 8))))
;; 将文件名加入到路径
(setq org-refile-use-outline-path 'file)
;; 是否按步骤refile
(setq org-outline-path-complete-in-steps nil)
;; 允许创建新的标题行，但需要确认
(setq org-refile-allow-creating-parent-nodes 'confirm)
;; 设置标签的默认位置，第100列右对齐
(setq org-tags-column -100)
;; 自动对齐标签
(setq org-auto-align-tags t)
;; 标签继承
(setq org-use-tag-inheritance t)
;; 在日程视图的标签继承
(setq org-agenda-use-tag-inheritance t)
;; 标签快速选择
(setq org-use-fast-tag-selection t)
;; 标签选择不需要回车确认
(setq org-fast-tag-selection-single-key t)
;; 定义了有序属性的标题行也加上 OREDERD 标签
(setq org-track-ordered-property-with-tag t)
;; 归档设置
(setq org-archive-location "%s_archive::datetree/")
;; 启用org-mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-hook 'org-mode-hook #'visual-line-mode)


;;; Org agenda设置 --------------------------------------------------------------
;; 显示时间线
(setq org-agenda-use-time-grid t)
;; 设置面包屑分隔符
(setq org-agenda-breadcrumbs-separator " ❱ ")
;; 设置时间线的当前时间指示串
(setq org-agenda-current-time-string "now ----------------------------")
;; 时间线范围和颗粒度设置
(setq org-agenda-time-grid (quote ((daily today)
                                   (0600 0800 1000 1200
                                         1400 1600 1800
                                         2000 2200 2400)
                                   "......" "--------------------------------")))
;; 日程视图的前缀设置
(setq org-agenda-prefix-format '((agenda . " %i %-25:c %5t %s")
                                 (todo   . " %i %-25:c ")
                                 (tags   . " %i %-25:c ")
                                 (search . " %i %-25:c ")))
;; 对于计划中的任务在视图里的显示
(setq org-agenda-scheduled-leaders
      '("计划 " "%02d天前开始 "))
;; 对于截止日期的任务在视图里的显示
(setq org-agenda-deadline-leaders
      '("截止 " "%02d天后截止 " "过期%02d天 "))

;; 自定义视图
(use-package ts
  :ensure nil
  :load-path extensions-ts-dir)

(use-package org-super-agenda
  :ensure nil
  :load-path extensions-org-super-agenda-dir
  :after org
  :init
  (setq org-super-agenda-header-map nil) ; 防止 agenda buffer 中出现折叠错乱
  :config
  (org-super-agenda-mode 1)

  (defconst my/org-super-agenda-workflow-groups
    '(
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
      (:name "◎ 代码控制 ➜ 开发🄓/修改🄜" :todo ("DEVELOP" "MODIFY"))
      (:name "◎ 代码控制 ➜ 已确认🄒" :todo "CONFIRMED")

      ;; 长期规划
      (:name "◎ 长期规划 ➜ 目标🄥/聚焦🄞" :todo ("VISION" "FOCUS"))
      (:name "◎ 长期规划 ➜ 能力建设🄤" :todo "BUILD")
      (:name "◎ 长期规划 ➜ 持续实践🄣" :todo "PRACTICE")
      (:name "◎ 长期规划 ➜ 评估🄘" :todo "REVIEW")

      ;; 其他
      (:discard (:anything t))))

  (add-to-list
   'org-agenda-custom-commands
   '("w" "Workflow View"
     ((alltodo ""
               ((org-agenda-overriding-header "◉ 工作流视图")
                (org-super-agenda-groups
                 my/org-super-agenda-workflow-groups)))))))

;; 时间戳格式设置: <2022-12-24 星期六> 或 <2022-12-24 星期六 06:53>
(setq org-time-stamp-formats '("<%Y-%m-%d %A>" . "<%Y-%m-%d %A %H:%M>"))
;; 不同日程类别间的间隔
(setq org-cycle-separator-lines 2)
;; 设置需要被日程监控的org文件
(setq org-agenda-files
      (append
       (list (expand-file-name "diary.org" org-directory)
             (expand-file-name "plan.org" org-directory)
             (expand-file-name "inbox.org" org-directory)
             (expand-file-name "work.org" org-directory))
       (seq-filter
        (lambda (f)
          ;; 过滤archive/assets/trash目录
          (not (string-match-p "/\\(archive\\|assets\\|trash\\)/" f)))
        (directory-files-recursively (expand-file-name "projects" org-directory) "\\.org$"))))

;; 设置org的日记文件
(setq org-agenda-diary-file (expand-file-name "diary.org" org-directory))
;; 日记插入精确时间戳
(setq org-agenda-insert-diary-extract-time t)
;; 设置日程视图更加紧凑
(setq org-agenda-compact-blocks nil)
;; 日程视图的块分隔符
(setq org-agenda-block-separator ?─)
;; 日视图还是周视图，通过 v-d, v-w, v-m, v-y 切换视图，默认周视图
(setq org-agenda-span 'day)
;; q退出时删除agenda缓冲区
(setq org-agenda-sticky t)
;; 是否包含直接日期
(setq org-agenda-include-deadlines t)
;; 禁止日程启动画面
(setq org-agenda-inhibit-startup t)
;; 显示每一天，不管有没有条目
(setq org-agenda-show-all-dates t)
;; 时间不足位时前面加0
(setq org-agenda-time-leading-zero t)
;; 日程同时启动log mode
(setq org-agenda-start-with-log-mode t)
;; 日程同时启动任务时间记录报告模式
(setq org-agenda-start-with-clockreport-mode t)
;; 截止的任务完成后不显示
(setq org-agenda-skip-deadline-if-done t)
;; 当计划的任务完成后不显示
(setq org-agenda-skip-scheduled-if-done t)
;; 计划过期上限
(setq org-scheduled-past-days 365)
;; 计划截止上限
(setq org-deadline-past-days 365)
;; 计划中的任务不提醒截止时间
(setq org-agenda-skip-deadline-prewarning-if-scheduled 1)
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)
(setq org-agenda-skip-timestamp-if-deadline-is-shown t)
;; 设置工时记录报告格式
(setq org-agenda-clockreport-parameter-plist
      '(:link t :maxlevel 8 :compact nil :narrow 80 :timestamp t :fileskip0 t))
(setq org-agenda-columns-add-appointments-to-effort-sum t)
(setq org-agenda-restore-windows-after-quit t)
(setq org-agenda-window-setup 'current-window)
;; 标签显示的位置，第100列往前右对齐
(setq org-agenda-tags-column -100)
;; 从星期一开始作为一周第一天
(setq org-agenda-start-on-weekday 1)
;; 是否使用am/pm
(setq org-agenda-timegrid-use-ampm nil)
;; 搜索是不看时间
(setq org-agenda-search-headline-for-time nil)
;; 提前3天截止日期到期告警
(setq org-deadline-warning-days 3)

;; 通知提醒
(defun appt-display-with-notification (min-to-app new-time appt-msg)
  (notifications-notify :title (format "Appointment in %s minutes" min-to-app)
                        :body appt-msg
                        :urgency 'critical)
  (appt-disp-window min-to-app new-time appt-msg))

;; 每15分钟更新一次appt
(run-at-time t 900 #'org-agenda-to-appt)
;; 不显示日期
(setq appt-display-diary nil)
;; 提醒间隔时间，每15分钟提醒一次
(setq appt-display-interval 15)
;; modeline显示提醒
(setq appt-display-mode-line t)
;; 设置提醒响铃
(setq appt-audible t)
;; 提前30分钟提醒
(setq appt-message-warning-time 30)
;; 通知提醒函数
;; linux下同时在emacs内部和系统通知中提醒; 其他环境下仅在emacs中提醒
(when (eq system-type 'gnu/linux)
  (setq appt-disp-window-function #'appt-display-with-notification))
;; 激活提醒
(appt-activate 1)
;; 自动同步org-agenda文件到appt
(add-hook 'org-agenda-finalize-hook #'org-agenda-to-appt)

;;; Org capture设置 -------------------------------------------------------------
(setq org-capture-use-agenda-date nil)
;; define common template
(setq org-capture-templates `((;; Inbox 收集箱
                               "i" "Inbox Task" entry (file+headline "inbox.org" "Tasks")
                               "* TODO %i%?\n  %U\n  %a"
                               :empty-lines-after 1
                               :prepend t)
                              ;; Diary 日常日程
                              ("d" "Diary" entry (file+olp+datetree "diary.org")
                               "* TODO %?\n  SCHEDULED: %^t\n"
                               :empty-lines 1
                               :jump-to-captured t)
                              ;; Notes 笔记
                              ("n" "Notes" entry (file+headline "inbox.org" "Notes")
                               "* %? %^g\n%i\n"
                               :empty-lines-after 1)
                              ;; Bookmarks 书签记录
                              ("b" "Bookmarks" entry (file+headline "inbox.org" "Bookmarks")
                               "* %:description\n\n%a%?"
                               :empty-lines 1
                               :immediate-finish t)
                              ;; Plan 计划
                              ("p" "Plan" entry
                               (file+headline "plan.org" "Plan")
                               "* VISION %?\n  SCHEDULED: %^t  DEADLINE: %^t\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n\n  %a"
                               :empty-lines 1)
                              ;; Plan 长期计划
                              ("P" "Long-term Plan" entry
                               (file+headline "plan.org" "Long-term Plan")
                               "* VISION %?\n  SCHEDULED: %^t  DEADLINE: %^t\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n\n  %a"
                               :empty-lines 1)
                              ;; Issue 问题报告
                              ("I" "Issue" entry (file+headline "inbox.org" "Issues")
                               "* REPORT %i%?\n\n  一、问题描述\n\n  二、分析定位\n\n  三、措施验证\n\n  四、结论\n"
                               :empty-lines-after 1
                               :prepend t)
                              (;; Project Inbox
                               "t" "Project Inbox" entry (file+headline "inbox.org" "Projects")
                               "* TODO %i%?\n  %U\n  :PROPERTIES:\n  :PROJECT: %^{Project name}\n  :TYPE: %^{类型|需求|任务|风险|议题|日志|其他}\n  :END:\n"
                               :empty-lines-after 1
                               :prepend t)))

;; Add hook
(add-hook 'org-capture-mode-hook #'(lambda ()
                                     (setq-local org-complete-tags-always-offer-all-agenda-tags t)))
(add-hook 'org-capture-mode-hook #'delete-other-windows)

;;; Org extensions -------------------------------------------------------------

;; Org-modern
(use-package org-modern
  :ensure nil
  :load-path extensions-org-modern-dir
  :custom
  ;; 设置star样式 ☰☱☲☳☴☵☶☷
  (org-modern-replace-stars "☰☱☲☳☴☵☶☷")
  ;; (org-modern-replace-stars "◉◎○◈◇*")
  ;; (org-modern-replace-stars "一二三四五六七八九十")
  ;; (org-modern-replace-stars "❶❷❸❹❺❻❼❽❾❿")
  ;; (org-modern-replace-stars "⒈⒉⒊⒋⒌⒍⒎⒏⒐⒑")
  (org-modern-star 'replace)
  ;; 关闭table美化
  (org-modern-table nil)
  ;; 关闭时间戳美化，避免表格不对齐
  (org-modern-timestamp nil)
  ;; 关闭优先级美化，使用prettify-symbols-mode
  (org-modern-priority nil)
  ;; 关闭关键字美化，使用prettify-symbols-mode
  (org-modern-keyword nil)
  ;; 设置TODO样式
  (org-modern-todo-faces
   '(;; --- 日程管理 Keyword ---
     ("TODO"       . (:inherit org-verbatim :weight regular :height 0.9 :foreground "coral" :inverse-video t))
     ("NEXT"       . (:inherit org-verbatim :weight regular :height 0.9 :foreground "ForestGreen" :inverse-video t))
     ("WAIT"       . (:inherit org-verbatim :weight regular :height 0.9 :foreground "DarkOrange" :inverse-video t))
     ("DONE"       . (:inherit org-verbatim :weight regular :height 0.9 :foreground "DimGrey" :inverse-video t))
     ("CANCELLED"  . (:inherit org-verbatim :weight regular :height 0.9 :foreground "LightGray" :inverse-video t))
     ;; --- 项目问题 Keyword ---
     ("REPORT"     . (:inherit org-verbatim :weight regular :height 0.9 :foreground "coral" :inverse-video t))
     ("BUG"        . (:inherit org-verbatim :weight regular :height 0.9 :foreground "firebrick" :inverse-video t))
     ("KNOWNCAUSE" . (:inherit org-verbatim :weight regular :height 0.9 :foreground "DarkOrange" :inverse-video t))
     ("FIXED"      . (:inherit org-verbatim :weight regular :height 0.9 :foreground "LightGray" :inverse-video t))
     ("CLOSED"     . (:inherit org-verbatim :weight regular :height 0.9 :foreground "DimGrey" :inverse-video t))
     ;; --- 代码控制 Keyword ---
     ("DESIGN"     . (:inherit org-verbatim :weight regular :height 0.9 :foreground "SteelBlue" :inverse-video t))
     ("DEVELOP"    . (:inherit org-verbatim :weight regular :height 0.9 :foreground "ForestGreen" :inverse-video t))
     ("MODIFY"     . (:inherit org-verbatim :weight regular :height 0.9 :foreground "ForestGreen" :inverse-video t))
     ("CONFIRMED"  . (:inherit org-verbatim :weight regular :height 0.9 :foreground "DarkSeaGreen" :inverse-video t))
     ("DEPRECATED" . (:inherit org-verbatim :weight regular :height 0.9 :foreground "LightGray" :inverse-video t))
     ("ARCHIVED"   . (:inherit org-verbatim :weight regular :height 0.9 :foreground "DimGrey" :inverse-video t))
     ;; --- 长期规划 Keyword ---
     ("VISION"     . (:inherit org-verbatim :weight regular :height 0.9 :foreground "SteelBlue" :inverse-video t))
     ("FOCUS"      . (:inherit org-verbatim :weight regular :height 0.9 :foreground "ForestGreen" :inverse-video t))
     ("BUILD"      . (:inherit org-verbatim :weight regular :height 0.9 :foreground "ForestGreen" :inverse-video t))
     ("PRACTICE"   . (:inherit org-verbatim :weight regular :height 0.9 :foreground "DarkSeaGreen" :inverse-video t))
     ("REVIEW"     . (:inherit org-verbatim :weight regular :height 0.9 :foreground "LightGray" :inverse-video t))
     ("ACHIEVED"   . (:inherit org-verbatim :weight regular :height 0.9 :foreground "DimGrey" :inverse-video t))
     ))

  :hook ((org-mode . org-modern-mode)
	 (org-agenda-finalize . org-modern-agenda)))

;; 自动显示隐藏符号
(use-package org-appear
  :ensure nil
  :load-path extensions-org-appear-dir
  :custom
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t)
  (org-appear-autoentities t)
  (org-appear-autokeywords t)
  (org-appear-inside-latex t)
  (org-appear-delay 0.5)
  :hook (org-mode . org-appear-mode))

;; 笔记管理
(use-package denote
  :ensure nil
  :load-path extensions-denote-dir
  :hook
  ((text-mode . denote-fontify-links-mode-maybe)
   (dired-mode . denote-dired-mode))
  :bind
  ( :map global-map
    ("C-c n n" . denote)
    ("C-c n d" . denote-dired)
    ("C-c n g" . denote-grep)
    ;; If you intend to use Denote with a variety of file types, it is
    ;; easier to bind the link-related commands to the `global-map', as
    ;; shown here.  Otherwise follow the same pattern for `org-mode-map',
    ;; `markdown-mode-map', and/or `text-mode-map'.
    ("C-c n l" . denote-link)
    ("C-c n L" . denote-add-links)
    ("C-c n b" . denote-backlinks)
    ("C-c n q c" . denote-query-contents-link) ; create link that triggers a grep
    ("C-c n q f" . denote-query-filenames-link) ; create link that triggers a dired
    ;; Note that `denote-rename-file' can work from any context, not just
    ;; Dired bufffers.  That is why we bind it here to the `global-map'.
    ("C-c n r" . denote-rename-file)
    ("C-c n R" . denote-rename-file-using-front-matter)

    ;; Key bindings specifically for Dired.
    :map dired-mode-map
    ("C-c C-d C-i" . denote-dired-link-marked-notes)
    ("C-c C-d C-r" . denote-dired-rename-files)
    ("C-c C-d C-k" . denote-dired-rename-marked-files-with-keywords)
    ("C-c C-d C-R" . denote-dired-rename-marked-files-using-front-matter))

  :config
  ;; Create note using Org capture
  (with-eval-after-load 'org-capture
    (setq denote-org-capture-specifiers "%l\n%i\n%?")
    (add-to-list 'org-capture-templates
		 '("N" "New note (with Denote)" plain
                   (file denote-last-path)
                   #'denote-org-capture
                   :no-save t
                   :immediate-finish nil
                   :kill-buffer t
                   :jump-to-captured t)))

  (setq denote-directory (expand-file-name "~/Org/notes/"))
  (setq denote-save-buffers nil)
  (setq denote-known-keywords '("emacs" "entertainment" "reading" "studying" "project" "misc"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-prompts '(title keywords))
  (setq denote-excluded-directories-regexp nil)
  (setq denote-excluded-keywords-regexp nil)
  (setq denote-rename-confirmations '(rewrite-front-matter modify-file-name))

  ;; Pick dates, where relevant, with Org's advanced interface:
  (setq denote-date-prompt-use-org-read-date t)

  ;; Automatically rename Denote buffers using the `denote-rename-buffer-format'.
  (denote-rename-buffer-mode 1))

;; 笔记搜索
(use-package consult-notes
  :ensure nil
  :load-path extensions-consult-notes-dir
  :commands (consult-notes consult-notes-search-in-all-notes)
  :bind (("C-c n f" . consult-notes)                      ;; 快速查找并打开笔记
         ("C-c n s" . consult-notes-search-in-all-notes)) ;; 全局搜索笔记内容
  :custom
  ;; 设置笔记源 (可以是多个目录)
  (consult-notes-file-dir-sources
   `(("org"     ?o ,(concat org-directory "/"))
     ("notes"   ?n ,(concat org-directory "/notes/"))
     ("work"    ?w ,(concat org-directory "/work/"))
     ("article" ?a ,(concat org-directory "/article/"))
     ("study"   ?s ,(concat org-directory "/study/"))
     ("books"   ?b ,(concat org-directory "/books/"))))
  
  :config 
  ;; Embark support
  (with-eval-after-load 'embark
    (defun consult-notes-open-dired (cand)
      "Open notes directory dired with point on file CAND."
      (interactive "fNote: ")
      ;; dired-jump is in dired-x.el but is moved to dired in Emacs 28
      (dired-jump nil cand))

    (defun consult-notes-grep (cand)
      "Run grep in directory of notes file CAND."
      (interactive "fNote: ")
      (consult-grep (file-name-directory cand)))

    (defvar-keymap consult-notes-map
      :doc "Keymap for Embark notes actions."
      :parent embark-file-map
      "d" #'consult-notes-dired
      "g" #'consult-notes-grep)

    (add-to-list 'embark-keymap-alist `(,consult-notes-category . consult-notes-map))

    ;; make embark-export use dired for notes
    (setf (alist-get consult-notes-category embark-exporters-alist) #'embark-export-dired)))

;; Denote suppory
(use-package consult-notes-denote
  :ensure nil
  :load-path extensions-consult-notes-dir
  :config
  (when (locate-library "denote")
    (setq consult-notes-denote-display-id nil) ;; 保持界面简洁
    (consult-notes-denote-mode 1)))

;; 笔记链接
(use-package org-super-links
  :ensure nil
  :load-path extensions-org-super-links-dir
  :bind (("C-c s l" . org-super-links-link)
         ("C-c s i" . org-super-links-insert-link)
         ("C-c s s" . org-super-links-store-link)
         ("C-c s d" . org-super-links-quick-insert-drawer)
         ("C-c s C-i" . org-super-links-quick-insert-inline-link)
         ("C-c s C-d" . org-super-links-delete-link))
  :config
  (setq org-super-links-related-into-drawer t)
  ;; 自动为链接的目标添加 ID（如果目标没有 ID）
  (setq org-super-links-link-prefix 'org-super-links-link-prefix-timestamp))

;; 自动生成ID链接标题
(require 'org-id)
(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
;; 设置org-id存储位置
(setq org-id-locations-file (concat org-directory "/.org-id-locations"))

(provide 'init-org)

;;; init-org.el ends here
