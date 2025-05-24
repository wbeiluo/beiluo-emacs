;;; init-org.el --- Org Mode Configurations -*- lexical-binding: t -*-

;; Copyright (C) 2020~2025 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;; Code:

(require 'org)
(require 'org-agenda)
(require 'appt)
(require 'notifications)
(require 'org-capture)
(require 'org-modern)
(require 'org-appear)
(require 'denote)
(require 'consult-notes)
(require 'consult-notes-denote)
(require 'org-super-links)

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
 ;; 设置org-table字体
 '(org-table ((t (:font "LXGW WenKai Mono:pixelsize=26")))))

;; 在org mode里美化字符串标志
(defun my/org-prettify-symbols ()
  (setq prettify-symbols-alist
        (mapcan (lambda (x) (list x (cons (upcase (car x)) (cdr x))))
                '(("[#A]"            . "🅐")
                  ("[#B]"            . "🅑")
                  ("[#C]"            . "🅒")
                  ("[ ]"             . "󰄱")
                  ("[X]"             . "󰄵")
                  ("[-]"             . "󰡖")
                  ;;("#+begin_src"     . "")
                  ;;("#+end_src"       . "")
                  ;;("#+begin_example" . "")
                  ;;("#+end_example"   . "")
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
(setq org-default-notes-file (expand-file-name "capture.org" org-directory))
;; 启用一些子模块
(setq org-modules '(ol-bibtex ol-gnus ol-info ol-eww org-habit org-protocol))
;; 设置标题行折叠符号
(setq org-ellipsis "..")
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
;; 关闭缩进模式
(setq org-indent-mode nil)
;; 当启用缩进模式时自动隐藏前置星号
(setq org-indent-mode-turns-on-hiding-stars t)
;; 关闭缩进
(setq org-startup-indented nil)
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
(setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "HOLD(h!)" "WAIT(w!)" "|" "DONE(d!)" "CANCELLED(c@/!)")
                          (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f@/!)" "CLOSED(c@/!)")))

;; 当标题行状态变化时标签同步发生的变化
;; Moving a task to CANCELLED adds a CANCELLED tag
;; Moving a task to WAIT adds a WAIT tag
;; Moving a task to HOLD adds WAIT and HOLD tags
;; Moving a task to a done state removes WAIT and HOLD tags
;; Moving a task to TODO removes WAIT, CANCELLED, and HOLD tags
;; Moving a task to DONE removes WAIT, CANCELLED, and HOLD tags
(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAIT" ("WAIT" . t))
              ("HOLD" ("WAIT") ("HOLD" . t))
              (done ("WAIT") ("HOLD"))
              ("TODO" ("WAIT") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAIT") ("CANCELLED") ("HOLD")))))

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
;; 不自动对齐标签
(setq org-auto-align-tags t);nil
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

;; =====================
;; 自定义日程视图，分别显示TODO，NEXT-LINE，NEXT中的任务
;; n键显示自定义视图，p键纯文本视图，a键默认视图
;; =====================
;; (defvar my-org-custom-daily-agenda
;;   `((todo "TODO"
;;           ((org-agenda-block-separator nil)
;;            (org-agenda-overriding-header "所有待办任务\n")))
;;     (todo "NEXT"
;;           ((org-agenda-block-separator nil)
;;            (org-agenda-overriding-header "\n进行中的任务\n")))
;;     (todo "WAIT"
;;           ((org-agenda-block-separator nil)
;;            (org-agenda-overriding-header "\n等待中的任务\n")))
;;     (agenda "" ((org-agenda-block-separator nil)
;;                 (org-agenda-overriding-header "\n今日日程\n"))))
;;   "Custom agenda for use in `org-agenda-custom-commands'.")
;; (setq org-agenda-custom-commands
;;       `(("n" "Daily agenda and top priority tasks"
;;          ,my-org-custom-daily-agenda)
;;         ("p" "Plain text daily agenda and top priorities"
;;          ,my-org-custom-daily-agenda
;;          ((org-agenda-with-colors nil)
;;           (org-agenda-prefix-format "%t %s")
;;           (org-agenda-current-time-string ,(car (last org-agenda-time-grid)))
;;           (org-agenda-fontify-priorities nil)
;;           (org-agenda-remove-tags t))
;;          ("agenda.txt"))))

;; 时间戳格式设置: <2022-12-24 星期六> 或 <2022-12-24 星期六 06:53>
(setq org-time-stamp-formats '("<%Y-%m-%d %A>" . "<%Y-%m-%d %A %H:%M>"))
;; 不同日程类别间的间隔
(setq org-cycle-separator-lines 2)
;; 设置需要被日程监控的org文件
(setq org-agenda-files
      (list (expand-file-name "diary.org" org-directory)
            (expand-file-name "tasks.org" org-directory)
            (expand-file-name "work.org" org-directory)))
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
      '(:link t :maxlevel 8 :compact nil :narrow 80 :timestamp t))
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
(setq org-capture-templates `(("t" "Tasks" entry (file+headline "tasks.org" "Reminders")
                               "* TODO %i%?"
                               :empty-lines-after 1
                               :prepend t)
                              ("w" "Work" entry (file+headline "work.org" "Reminders")
                               "* TODO %i%?"
                               :empty-lines-after 1
                               :prepend t)
                              ("n" "Notes" entry (file+headline "capture.org" "Notes")
                               "* %? %^g\n%i\n"
                               :empty-lines-after 1)
                              ;; For EWW
                              ("b" "Bookmarks" entry (file+headline "capture.org" "Bookmarks")
                               "* %:description\n\n%a%?"
                               :empty-lines 1
                               :immediate-finish t)
                              ("d" "Diary")
                              ("dt" "Today's TODO list" entry (file+olp+datetree "diary.org")
                               "* Today's todo list [/]\n%T\n\n** TODO %?"
                               :empty-lines 1
                               :jump-to-captured t)
                              ("do" "Other stuff" entry (file+olp+datetree "diary.org")
                               "* %?\n%T\n\n%i"
                               :empty-lines 1
                               :jump-to-captured t)))
;; Add hook
(add-hook 'org-capture-mode-hook #'(lambda ()
                                     (setq-local org-complete-tags-always-offer-all-agenda-tags t)))
(add-hook 'org-capture-mode-hook #'delete-other-windows)

;;; Org extensions -------------------------------------------------------------

;; Org-modern
;; 设置star样式
(setq org-modern-replace-stars "☯☰☱☲☳☴☵☶☷")
(setq org-modern-star 'replace)
;; 关闭table美化
(setq org-modern-table nil)
;; 关闭时间戳美化，避免表格不对齐
(setq org-modern-timestamp nil)
;; 关闭优先级美化，使用prettify-symbols-mode
(setq org-modern-priority nil)
;; 关闭关键字美化，使用prettify-symbols-mode
(setq org-modern-keyword nil)
;; 修改样式
;; (custom-set-face
;;   ;; 设置label大小
;;   '(org-modern-label ((t (:height 1.0 :width condensed :weight regular :underline nil)))))
;; 设置TODO样式
(setq org-modern-todo-faces
      '(("TODO"       . (:inherit org-verbatim :weight regular :foreground "IndianRed" :inverse-video t))
        ("NEXT"       . (:inherit org-verbatim :weight regular :foreground "ForestGreen" :inverse-video t))
        ("WAIT"       . (:inherit org-verbatim :weight regular :foreground "coral" :inverse-video t))
        ("HOLD"       . (:inherit org-verbatim :weight regular :foreground "DarkOrange" :inverse-video t))
        ("DONE"       . (:inherit org-verbatim :weight regular :foreground "dim gray" :inverse-video t))
        ("CANCELLED"  . (:inherit org-verbatim :weight regular :foreground "LightGray" :inverse-video t))
        ("REPORT"     . (:inherit org-verbatim :weight regular :foreground "coral" :inverse-video t))
        ("BUG"        . (:inherit org-verbatim :weight regular :foreground "firebrick" :inverse-video t))
        ("KNOWNCAUSE" . (:inherit org-verbatim :weight regular :foreground "DarkOrange" :inverse-video t))
        ("FIXED"      . (:inherit org-verbatim :weight regular :foreground "LightGray" :inverse-video t))
        ("CLOSED"     . (:inherit org-verbatim :weight regular :foreground "LightGray" :inverse-video t))))

;; 设置优先级样式
;; (setq org-modern-priority-faces
;;       '((?A :inherit org-priority :weight regular :foreground "tomato" :inverse-video t)
;;         (?B :inherit org-priority :weight regular :foreground "salmon" :inverse-video t)
;;         (?C :inherit org-priority :weight regular :foreground "SandyBrown" :inverse-video t)))

;; Add hook
(add-hook 'org-mode-hook #'org-modern-mode)
(add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

;; 自动显示隐藏符号
(setq org-appear-autolinks t)
(setq org-appear-autosubmarkers t)
(setq org-appear-autoentities t)
(setq org-appear-autokeywords t)
(setq org-appear-inside-latex t)
(setq org-appear-delay 0.5)
;; Add hook
(add-hook 'org-mode-hook #'org-appear-mode)

;; 笔记管理
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
(denote-rename-buffer-mode 1)

;; Add hook
(add-hook 'dired-mode-hook #'denote-dired-mode)

;; 笔记搜索
(setq consult-notes-file-dir-sources
      `(("org"     ?o ,(concat org-directory "/"))
        ("notes"   ?n ,(concat org-directory "/notes/"))
        ("work"    ?w ,(concat org-directory "/work/"))
        ("article" ?a ,(concat org-directory "/article/"))
        ("study"   ?s ,(concat org-directory "/study/"))
        ("books"   ?b ,(concat org-directory "/books/"))))

;; Denote suppory
(when (locate-library "denote")
  (consult-notes-denote-mode))

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
  (setf (alist-get consult-notes-category embark-exporters-alist) #'embark-export-dired))

;; 笔记链接
(setq org-super-links-related-into-drawer t)
(setq	org-super-links-link-prefix 'org-super-links-link-prefix-timestamp)

(provide 'init-org)

;;; init-org.el ends here
