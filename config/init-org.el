;;; init-org.el --- Org Mode Configurations -*- lexical-binding: t -*-

;; Copyright (C) 2020~2026 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;; Code:

(require 'org)
(require 'org-agenda)
(require 'appt)
(require 'notifications)
(require 'org-capture)
(require 'org-element)

;;; Global Keyword
;; Org-agenda
(global-set-key (kbd "\e\e a") #'org-agenda)
(define-key org-agenda-mode-map (kbd "i") #'(lambda () (interactive) (org-capture nil "jd")))
(define-key org-agenda-mode-map (kbd "J") #'consult-org-agenda)

;; Org-capture
(global-set-key (kbd "\e\e c") #'org-capture)

;; Org 时间戳: `C-c .' 插入激活时间戳，`C-c _' 插入非激活时间戳。
;; 默认的 `C-c !' 与 flycheck-keymap-prefix 同键，被 flycheck-mode-map
;; 抢占（minor mode 优先于 major mode），故将非激活时间戳改绑到 `C-c _'。
;; 新版 Org 中命令名为 `org-timestamp-inactive'，此处用别名保持兼容。
(define-key org-mode-map (kbd "C-c _") #'org-time-stamp-inactive)

;;; Org mode设置 ----------------------------------------------------------------

;; 设置Org字体
(dolist (face '(org-table org-date org-tag org-meta-line org-document-info
                          org-document-info-keyword org-drawer org-special-keyword org-property-value))
  (set-face-attribute face nil
                      :family "LXGW WenKai Mono"
                      :height 0.9))

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
                  ("#+category:"     . "")
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
;; 隐藏标题星号
(setq org-hide-leading-stars t)
;; 根据标题栏自动缩进文本（真实缩进，对齐到标题星号之后）
;; 注意：与 org-indent-mode 的虚拟缩进互斥——二者叠加会错位，
;; 故此处不启用 org-startup-indented，缩进完全交给 org-adapt-indentation。
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
        (sequence "DESIGN(s)" "DEVELOP(p!)" "MODIFY(m!)" "CONFIRMED(e!)" "|" "DEPRECATED(y@/!)" "ARCHIVED(a@/!)")))

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
              ("ARCHIVED" ("archived" . t) ("design") ("develop") ("modify") ("confirmed")))))

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
      '("       计划 " "%3d天前开始 "))
;; 对于截止日期的任务在视图里的显示
(setq org-agenda-deadline-leaders
      '("       截止 " "%3d天后截止 " "%3d天前过期 "))

;; 自定义视图
(use-package org-super-agenda
  :ensure t
  :after org
  :init
  (setq org-super-agenda-header-map nil) ; 防止 agenda buffer 中出现折叠错乱
  :config
  (org-super-agenda-mode 1)

  ;; org super agenda视图
  (setq org-agenda-custom-commands
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
                        (:name "◎ 代码控制 ➜ 开发🄓/修改🄜" :todo ("DEVELOP" "MODIFY"))
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
                        (:name "◎ 代码控制 ➜ 开发🄓/修改🄜" :todo ("DEVELOP" "MODIFY"))
                        (:name "◎ 代码控制 ➜ 已确认🄒" :todo "CONFIRMED")

                        ;; 其他
                        (:discard (:anything t))))))))
          )))

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

;; 从日程视图捕获时使用光标所在日期(决定 datetree 归档日期与 %U/%t 的展开)
(setq org-capture-use-agenda-date t)

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

;; 模板按键分组: c=收集箱 j=日程计划 r=参考 x=计时备注
(setq org-capture-templates
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
        ("j" "日程计划 Journal")
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
        ("x" "计时任务备注" item (clock) "- %? %U")))

;; Add hook
(add-hook 'org-capture-mode-hook #'(lambda ()
                                     (setq-local org-complete-tags-always-offer-all-agenda-tags t)))
(add-hook 'org-capture-mode-hook #'delete-other-windows)
;; 捕获完成后同步 appt 提醒, 避免新日程最长 15 分钟内不触发提醒
(add-hook 'org-capture-after-finalize-hook #'org-agenda-to-appt)

;;; Org extensions -------------------------------------------------------------

;; Org-modern
(use-package org-modern
  :ensure t
  :custom
  ;; 设置star样式
  ;; (org-modern-replace-stars "☰☱☲☳☴☵☶☷")
  (org-modern-replace-stars "◉◎○◈◇✳")
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
     ("ARCHIVED"   . (:inherit org-verbatim :weight regular :height 0.9 :foreground "DimGrey" :inverse-video t))))

  :hook ((org-mode . org-modern-mode)
	 (org-agenda-finalize . org-modern-agenda)))

;; 自动显示隐藏符号
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

;; 自动生成ID链接标题
(require 'org-id)
(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
;; 设置org-id存储位置
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
  ;; 不依赖Helm
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

;; LaTeX 配置
(setq org-startup-with-latex-preview t)
(setq org-preview-latex-default-process 'xelatex)
(setq org-preview-latex-process-alist
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

(setq org-format-latex-options
      (plist-put org-format-latex-options
                 :scale 1.8))
;; 字体颜色默认使用当前颜色
(setq org-format-latex-options
      (plist-put org-format-latex-options
                 :foreground 'default))
;; 设置SVG背景透明
(setq org-format-latex-options
      (plist-put org-format-latex-options
                 :background "Transparent"))

(provide 'init-org)

;;; init-org.el ends here
