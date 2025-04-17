;;; init-org.el --- Org Mode Configurations -*- lexical-binding: t -*-

;; Copyright (C) 2020~2025 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;; Code:

;;; Org Mode基础设置
(use-package org
  :ensure nil
  :mode ("\\.org\\'" . org-mode)
  :hook ((org-mode . visual-line-mode)
         (org-mode . my/org-prettify-symbols))
  :commands (org-find-exact-headline-in-buffer org-set-tags)
  :custom-face
  ;; 设置Org mode标题以及每级标题行的大小
  (org-document-title ((t (:height 1.2 :weight bold))))
  (org-level-1 ((t (:height 1.15 :weight bold))))
  (org-level-2 ((t (:height 1.10 :weight bold))))
  (org-level-3 ((t (:height 1.05 :weight bold))))
  (org-level-4 ((t (:height 1.0 :weight bold))))
  (org-level-5 ((t (:height 1.0 :weight bold))))
  (org-level-6 ((t (:height 1.0 :weight bold))))
  (org-level-7 ((t (:height 1.0 :weight bold))))
  (org-level-8 ((t (:height 1.0 :weight bold))))
  (org-level-9 ((t (:height 1.0 :weight bold))))
  ;; 设置org-table字体
  (org-table ((t (:font "LXGW WenKai Mono:pixelsize=26"))))
  :config
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
        	    ;("#+begin_src"     . "")
        	    ;("#+end_src"       . "")
        	    ;("#+begin_example" . "")
        	    ;("#+end_example"   . "")
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

  ;; 设置优先级样式
  (setq org-priority-faces
        '((?A :inherit org-priority :weight regular :foreground "IndianRed" :background nil :inverse-video nil)
          (?B :inherit org-priority :weight regular :foreground "DarkOrange" :background nil :inverse-video nil)
          (?C :inherit org-priority :weight regular :foreground "ForestGreen" :background nil :inverse-video nil)))

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

  ;; 自动显示隐藏符号
  (use-package org-appear
    :ensure t
    :hook (org-mode . org-appear-mode)
    :config
    (setq org-appear-autolinks t)
    (setq org-appear-autosubmarkers t)
    (setq org-appear-autoentities t)
    (setq org-appear-autokeywords t)
    (setq org-appear-inside-latex t)
    (setq org-appear-delay 0.5))

  :custom
  ;; 设置Org mode的目录
  (org-directory "~/Org")
  ;; 设置笔记的默认存储位置
  (org-default-notes-file (expand-file-name "capture.org" org-directory))
  ;; 启用一些子模块
  (org-modules '(ol-bibtex ol-gnus ol-info ol-eww org-habit org-protocol))
  ;; 设置标题行折叠符号
  (org-ellipsis "⋯")
  ;; 在活动区域内的所有标题栏执行某些命令
  (org-loop-over-headlines-in-active-region t)
  ;; 隐藏宏标记
  (org-hide-macro-markers t)
  ;; 隐藏强调标签
  (org-hide-emphasis-markers t)
  ;; 隐藏符号
  (org-pretty-entities t)
  ;; 高亮latex语法
  (org-highlight-latex-and-related '(native script entities))
  ;; 以UTF-8显示
  (org-pretty-entities t)
  ;; 关闭缩进模式
  (org-indent-mode nil)
  ;; 当启用缩进模式时自动隐藏前置星号
  (org-indent-mode-turns-on-hiding-stars t)
  ;; 关闭缩进
  (org-startup-indented nil)
  ;; 根据标题栏自动缩进文本
  (org-adapt-indentation t)
  ;; 自动显示图片
  (org-startup-with-inline-images t)
  ;; 默认以Overview的模式展示标题行
  (org-startup-folded 'overview)
  ;; 允许字母列表
  (org-list-allow-alphabetical t)
  ;; 列表的下一级设置
  (org-list-demote-modify-bullet '(("-"  . "+")
                                   ("+"  . "1.")
  				   ("1." . "a.")))
  ;; 编辑时检查是否在折叠的不可见区域
  (org-fold-catch-invisible-edits 'smart)
  ;; 在当前位置插入新标题行还是在当前标题行后插入，这里设置为当前位置
  (org-insert-heading-respect-content nil)
  ;; 设置图片的最大宽度，如果有imagemagick支持将会改变图片实际宽度
  ;; 四种设置方法：(1080), 1080, t, nil
  (org-image-actual-width nil)
  ;; imenu的最大深度，默认为2
  (org-imenu-depth 4)
  ;; 回车要不要触发链接，这里设置不触发
  (org-return-follows-link nil)
  ;; 上标^下标_是否需要特殊字符包裹，这里设置需要用大括号包裹
  (org-use-sub-superscripts '{})
  ;; 复制粘贴标题行的时候删除id
  (org-clone-delete-id t)
  ;; 粘贴时调整标题行的级别
  (org-yank-adjusted-subtrees t)

  ;; TOOD的关键词设置，可以设置不同的组
  (org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "HOLD(h!)" "WAIT(w!)" "|" "DONE(d!)" "CANCELLED(c@/!)")
  		       (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f!)")))

  ;; 当标题行状态变化时标签同步发生的变化
  ;; Moving a task to CANCELLED adds a CANCELLED tag
  ;; Moving a task to WAIT adds a WAIT tag
  ;; Moving a task to HOLD adds WAIT and HOLD tags
  ;; Moving a task to a done state removes WAIT and HOLD tags
  ;; Moving a task to TODO removes WAIT, CANCELLED, and HOLD tags
  ;; Moving a task to DONE removes WAIT, CANCELLED, and HOLD tags
  (org-todo-state-tags-triggers
   (quote (("CANCELLED" ("CANCELLED" . t))
  	   ("WAIT" ("WAIT" . t))
  	   ("HOLD" ("WAIT") ("HOLD" . t))
  	   (done ("WAIT") ("HOLD"))
  	   ("TODO" ("WAIT") ("CANCELLED") ("HOLD"))
  	   ("DONE" ("WAIT") ("CANCELLED") ("HOLD")))))

    ;; 始终存在的的标签
  (org-tag-persistent-alist '(("read"     . ?r)
  			      ("study"    . ?s)
  			      ("work"     . ?w)
                              ("project"  . ?p)
  			      ("emacs"    . ?e)
  			      ("life"     . ?l)))
  ;; 预定义好的标签
  (org-tag-alist '((:startgroup)
  		   ("play"     . ?y)
  		   ("tour"     . ?t)
  		   (:endgroup)))

  ;; 使用专家模式选择标题栏状态
  (org-use-fast-todo-selection 'expert)
  ;; 父子标题栏状态有依赖
  (org-enforce-todo-dependencies t)
  ;; 标题栏和任务复选框有依赖
  (org-enforce-todo-checkbox-dependencies t)
  ;; 标题行全局属性设置
  (org-global-properties '(("EFFORT_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 7:00 8:00")
        		   ("APPT_WARNTIME_ALL" . "0 5 10 15 20 25 30 45 60")
        		   ("RISK_ALL" . "Low Medium High")
        		   ("STYLE_ALL" . "habit")))
  ;; Org columns的默认格式
  (org-columns-default-format "%25ITEM %TODO %SCHEDULED %DEADLINE %3PRIORITY %TAGS %CLOCKSUM %EFFORT{:}")
  ;; 当状态从DONE改成其他状态时，移除 CLOSED: [timestamp]
  (org-closed-keep-when-no-todo t)
  ;; DONE时加上时间戳
  (org-log-done 'time)
  ;; 重复执行时加上时间戳
  (org-log-repeat 'time)
  ;; Deadline修改时加上一条记录
  (org-log-redeadline 'note)
  ;; Schedule修改时加上一条记录
  (org-log-reschedule 'note)
  ;; 以抽屉的方式记录
  (org-log-into-drawer t)
  ;; 紧接着标题行或者计划/截止时间戳后加上记录抽屉
  (org-log-state-notes-insert-after-drawers nil)
  ;; refile使用缓存
  (org-refile-use-cache t)
  ;; refile的目的地，这里设置的是agenda文件的所有标题
  (org-refile-targets '((org-agenda-files . (:maxlevel . 6))))
  ;; 将文件名加入到路径
  (org-refile-use-outline-path 'file)
  ;; 是否按步骤refile
  (org-outline-path-complete-in-steps nil)
  ;; 允许创建新的标题行，但需要确认
  (org-refile-allow-creating-parent-nodes 'confirm)
  ;; 设置标签的默认位置，第100列右对齐
  (org-tags-column -100)
  ;; 不自动对齐标签
  (org-auto-align-tags nil)
  ;; 标签继承
  (org-use-tag-inheritance t)
  ;; 在日程视图的标签继承
  (org-agenda-use-tag-inheritance t)
  ;; 标签快速选择
  (org-use-fast-tag-selection t)
  ;; 标签选择不需要回车确认
  (org-fast-tag-selection-single-key t)
  ;; 定义了有序属性的标题行也加上 OREDERD 标签
  (org-track-ordered-property-with-tag t)
  ;; 归档设置
  (org-archive-location "%s_archive::datetree/"))

(use-package org-modern
  :ensure t
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda))
  :custom
  ;; 设置star样式
  (org-modern-replace-stars "☯☰☱☲☳☴☵☶☷")
  (org-modern-star 'replace)
  ;; 关闭table美化
  (org-modern-table nil)
  ;; 关闭时间戳美化，避免表格不对齐
  (org-modern-timestamp nil)
  ;; 关闭优先级美化，使用prettify-symbols-mode
  (org-modern-priority nil)
  ;; 关闭关键字美化，使用prettify-symbols-mode
  (org-modern-keyword nil)
  
  :config
  ;; TODO 样式
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
          ("FIXED"      . (:inherit org-verbatim :weight regular :foreground "LightGray" :inverse-video t))))

  ;; 优先级样式
  ;; (setq org-modern-priority-faces
  ;;       '((?A :inherit org-priority :weight regular :foreground "tomato" :inverse-video t)
  ;;         (?B :inherit org-priority :weight regular :foreground "salmon" :inverse-video t)
  ;;         (?C :inherit org-priority :weight regular :foreground "SandyBrown" :inverse-video t)))
  )

;;; 日程设置
(use-package org-agenda
  :ensure nil
  :hook (org-agenda-finalize . org-agenda-to-appt)
  :bind (("\e\e a" . org-agenda)
         :map org-agenda-mode-map
         ("i" . (lambda () (interactive) (org-capture nil "d")))
         ("J" . consult-org-agenda))
  :config
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
  :custom
  ;; 设置需要被日程监控的org文件
  (org-agenda-files
   (list (expand-file-name "tasks.org" org-directory)
         (expand-file-name "diary.org" org-directory)))
  ;; 设置org的日记文件
  (org-agenda-diary-file (expand-file-name "diary.org" org-directory))
  ;; 日记插入精确时间戳
  (org-agenda-insert-diary-extract-time t)
  ;; 设置日程视图更加紧凑
  (org-agenda-compact-blocks nil)
  ;; 日程视图的块分隔符
  (org-agenda-block-separator ?─)
  ;; 日视图还是周视图，通过 v-d, v-w, v-m, v-y 切换视图，默认周视图
  (org-agenda-span 'day)
  ;; q退出时删除agenda缓冲区
  (org-agenda-sticky t)
  ;; 是否包含直接日期
  (org-agenda-include-deadlines t)
  ;; 禁止日程启动画面
  (org-agenda-inhibit-startup t)
  ;; 显示每一天，不管有没有条目
  (org-agenda-show-all-dates t)
  ;; 时间不足位时前面加0
  (org-agenda-time-leading-zero t)
  ;; 日程同时启动log mode
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
  ;; 设置工时记录报告格式
  (org-agenda-clockreport-parameter-plist
   '(:link t :maxlevel 5 :compact nil :narrow 80 :timestamp t))
  (org-agenda-columns-add-appointments-to-effort-sum t)
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-window-setup 'current-window)
  ;; 标签显示的位置，第80列往前右对齐
  (org-agenda-tags-column -80)
  ;; 从星期一开始作为一周第一天
  (org-agenda-start-on-weekday 1)
  ;; 是否使用am/pm
  (org-agenda-timegrid-use-ampm nil)
  ;; 搜索是不看时间
  (org-agenda-search-headline-for-time nil)
  ;; 提前3天截止日期到期告警
  (org-deadline-warning-days 3))

(use-package org-capture
  :ensure nil
  :bind ("\e\e c" . (lambda () (interactive) (org-capture)))
  :hook ((org-capture-mode . (lambda ()
                               (setq-local org-complete-tags-always-offer-all-agenda-tags t)))
         (org-capture-mode . delete-other-windows))
  :custom
  (org-capture-use-agenda-date nil)
  ;; define common template
  (org-capture-templates `(("t" "Tasks" entry (file+headline "tasks.org" "Reminders")
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
                            :jump-to-captured t))))

;;; 笔记管理
(use-package denote
  :ensure t
  :hook (dired-mode . denote-dired-mode)
  :bind (("C-c n n" . denote)
         ("C-c n r" . denote-rename-file)
         ("C-c n l" . denote-link)
         ("C-c n b" . denote-backlinks)
         ("C-c n d" . denote-dired))
  :init
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

  :config
  (setq denote-directory (expand-file-name "~/Org/notes/"))
  (setq denote-save-buffers nil)
  (setq denote-known-keywords '("emacs" "entertainment" "reading" "studying" "project"))
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

(use-package consult-notes
  :ensure t
  :commands (consult-notes
             consult-notes-search-in-all-notes)
  :bind (("C-c n f" . consult-notes)
         ("C-c n c" . consult-notes-search-in-all-notes))
  :config
  (setq consult-notes-file-dir-sources
        `(("org"     ?o ,(concat org-directory "/"))
          ("notes"   ?n ,(concat org-directory "/notes/"))
          ("work"    ?w ,(concat org-directory "/work/"))
          ("article" ?a ,(concat org-directory "/article/"))
          ("study"   ?s ,(concat org-directory "/study/"))
          ("books"   ?b ,(concat org-directory "/books/"))))

  ;; embark support
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

    (embark-define-keymap consult-notes-map
                          "Keymap for Embark notes actions."
                          :parent embark-file-map
                          ("d" consult-notes-dired)
                          ("g" consult-notes-grep))

    (add-to-list 'embark-keymap-alist `(,consult-notes-category . consult-notes-map))

    ;; make embark-export use dired for notes
    (setf (alist-get consult-notes-category embark-exporters-alist) #'embark-export-dired))
  )

;; (use-package org-super-links
;;   :quelpa (org-super-links :fetcher github :repo "toshism/org-super-links")
;;   :bind (("C-c s s"   . org-super-links-link)
;;          ("C-c s l"   . org-super-links-store-link)
;;          ("C-c s C-l" . org-super-links-insert-link)
;;          ("C-c s d"   . org-super-links-quick-insert-drawer-link)
;;          ("C-c s i"   . org-super-links-quick-insert-inline-link)
;;          ("C-c s C-d" . org-super-links-delete-link))
;;   :config
;;   (setq org-super-links-related-into-drawer t)
;;   (setq	org-super-links-link-prefix 'org-super-links-link-prefix-timestamp))

;; (use-package org-super-links
;;   :quelpa (org-super-links :repo "toshism/org-super-links" :fetcher github :commit "0.4")
;;   :bind (("C-c s s" . org-super-links-link)
;;          ("C-c s l" . org-super-links-store-link)
;;          ("C-c s C-l" . org-super-links-insert-link)
;;          ("C-c s d" . org-super-links-quick-insert-drawer-link)
;;          ("C-c s i" . org-super-links-quick-insert-inline-link)
;;          ("C-c s C-d" . org-super-links-delete-link))
;;   :config
;;   (setq org-super-links-related-into-drawer t
;;   	org-super-links-link-prefix 'org-super-links-link-prefix-timestamp))



;; (use-package appt
;;   :ensure nil
;;   :hook ((after-init . (lambda () (appt-activate 1)))
;;          (org-finalize-agenda . org-agenda-to-appt))
;;   :config
;;   ;; 通知提醒
;;   (defun appt-display-with-notification (min-to-app new-time appt-msg)
;;     (notify-send :title (format "Appointment in %s minutes" min-to-app)
;;                  :body appt-msg
;;                  :urgency 'critical)
;;     (appt-disp-window min-to-app new-time appt-msg))

;;   ;; 每15分钟更新一次appt
;;   (run-at-time t 900 #'org-agenda-to-appt)

;;   :custom
;;   ;; 是否显示日记
;;   (appt-display-diary nil)
;;   ;; 提醒间隔时间，每15分钟提醒一次
;;   (appt-display-interval 15)
;;   ;; 模式栏显示提醒
;;   (appt-display-mode-line t)
;;   ;; 设置提醒响铃
;;   (appt-audible t)
;;   ;; 提前30分钟提醒
;;   (appt-message-warning-time 30)
;;   ;; 通知提醒函数
;;   (appt-disp-window-function #'appt-display-with-notification)
;;   )

;; (use-package org-src
;;   :ensure nil
;;   :hook (org-babel-after-execute . org-redisplay-inline-images)
;;   :bind (("s-l" . show-line-number-in-src-block)
;;          :map org-src-mode-map
;;          ("C-c C-c" . org-edit-src-exit))
;;   :init
;;   ;; 设置代码块的默认头参数
;;   (setq org-babel-default-header-args
;;         '(
;;           (:eval    . "never-export")     ; 导出时不执行代码块
;;           (:session . "none")
;;           (:results . "replace")          ; 执行结果替换
;;           (:exports . "both")             ; 导出代码和结果
;;           (:cache   . "no")
;;           (:noweb   . "no")
;;           (:hlines  . "no")
;;           (:wrap    . "results")          ; 结果通过#+begin_results包裹
;;           (:tangle  . "no")               ; 不写入文件
;;           ))
;;   :config
;;   ;; ==================================
;;   ;; 如果出现代码运行结果为乱码，可以参考：
;;   ;; https://github.com/nnicandro/emacs-jupyter/issues/366
;;   ;; ==================================
;;   (defun display-ansi-colors ()
;;     (ansi-color-apply-on-region (point-min) (point-max)))
;;   (add-hook 'org-babel-after-execute-hook #'display-ansi-colors)

;;   ;; ==============================================
;;   ;; 通过overlay在代码块里显示行号，s-l显示，任意键关闭
;;   ;; ==============================================
;;   (defvar number-line-overlays '()
;;     "List of overlays for line numbers.")

;;   (defun show-line-number-in-src-block ()
;;     (interactive)
;;     (save-excursion
;;       (let* ((src-block (org-element-context))
;;              (nlines (- (length
;;                          (s-split
;;                           "\n"
;;                           (org-element-property :value src-block)))
;;                         1)))
;;         (goto-char (org-element-property :begin src-block))
;;         (re-search-forward (regexp-quote (org-element-property :value src-block)))
;;         (goto-char (match-beginning 0))

;;         (cl-loop for i from 1 to nlines
;;                  do
;;                  (beginning-of-line)
;;                  (let (ov)
;;                    (setq ov (make-overlay (point) (point)))
;;                    (overlay-put ov 'before-string (format "%3s | " (number-to-string i)))
;;                    (add-to-list 'number-line-overlays ov))
;;                  (next-line))))

;;     ;; now read a char to clear them
;;     (read-key "Press a key to clear numbers.")
;;     (mapc 'delete-overlay number-line-overlays)
;;     (setq number-line-overlays '()))

;;   ;; =================================================
;;   ;; 执行结果后，如果结果所在的文件夹不存在将自动创建
;;   ;; =================================================
;;   (defun check-directory-exists-before-src-execution (orig-fun
;;                                                       &optional arg
;;                                                       info
;;                                                       params)
;;     (when (and (assq ':file (cadr (cdr (org-babel-get-src-block-info))))
;;                (member (car (org-babel-get-src-block-info)) '("mermaid" "ditaa" "dot" "lilypond" "plantuml" "gnuplot" "d2")))
;;       (let ((foldername (file-name-directory (alist-get :file (nth 2 (org-babel-get-src-block-info))))))
;;         (if (not (file-exists-p foldername))
;;             (mkdir foldername)))))
;;   (advice-add 'org-babel-execute-src-block :before #'check-directory-exists-before-src-execution)

;;   ;; =================================================
;;   ;; 自动给结果的图片加上相关属性
;;   ;; =================================================
;;   (setq original-image-width-before-del "400") ; 设置图片的默认宽度为400
;;   (setq original-caption-before-del "")        ; 设置默认的图示文本为空

;;   (defun insert-attr-decls ()
;;     "insert string before babel execution results"
;;     (insert (concat "\n#+CAPTION:"
;;                     original-caption-before-del
;;                     "\n#+ATTR_ORG: :width "
;;                     original-image-width-before-del
;;                     "\n#+ATTR_LATEX: :width "
;;                     (if (>= (/ (string-to-number original-image-width-before-del) 800.0) 1)
;;                         "1.0"
;;                       (number-to-string (/ (string-to-number original-image-width-before-del) 800.0)))
;;                     "\\linewidth :float nil"
;;                     "\n#+ATTR_HTML: :width "
;;                     original-image-width-before-del
;;                     )))

;;   (defun insert-attr-decls-at (s)
;;     "insert string right after specific string"
;;     (let ((case-fold-search t))
;;       (if (search-forward s nil t)
;;           (progn
;;             ;; (search-backward s nil t)
;;             (insert-attr-decls)))))

;;   (defun insert-attr-decls-at-results (orig-fun
;;                                        &optional arg
;;                                        info
;;                                        param)
;;     "insert extra image attributes after babel execution"
;;     (interactive)
;;     (progn
;;       (when (member (car (org-babel-get-src-block-info)) '("mermaid" "ditaa" "dot" "lilypond" "plantuml" "gnuplot" "d2"))
;;         (setq original-image-width-before-del (number-to-string (if-let* ((babel-width (alist-get :width (nth 2 (org-babel-get-src-block-info))))) babel-width (string-to-number original-image-width-before-del))))
;;         (save-excursion
;;           ;; `#+begin_results' for :wrap results, `#+RESULTS:' for non :wrap results
;;           (insert-attr-decls-at "#+begin_results")))
;;       (org-redisplay-inline-images)))
;;   (advice-add 'org-babel-execute-src-block :after #'insert-attr-decls-at-results)

;;   ;; 再次执行时需要将旧的图片相关参数行删除，并从中头参数中获得宽度参数，参考
;;   ;; https://emacs.stackexchange.com/questions/57710/how-to-set-image-size-in-result-of-src-block-in-org-mode
;;   (defun get-attributes-from-src-block-result (&rest args)
;;     "get information via last babel execution"
;;     (let ((location (org-babel-where-is-src-block-result))
;;           ;; 主要获取的是图示文字和宽度信息，下面这个正则就是为了捕获这两个信息
;;           (attr-regexp "[:blank:]*#\\+\\(ATTR_ORG: :width \\([0-9]\\{3\\}\\)\\|CAPTION:\\(.*\\)\\)"))
;;       (setq original-caption-before-del "") ; 重置为空
;;       (when location
;;         (save-excursion
;;           (goto-char location)
;;           (when (looking-at (concat org-babel-result-regexp ".*$"))
;;             (next-line 2)               ; 因为有个begin_result的抽屉，所以往下2行
;;             ;; 通过正则表达式来捕获需要的信息
;;             (while (looking-at attr-regexp)
;;               (when (match-string 2)
;;                 (setq original-image-width-before-del (match-string 2)))
;;               (when (match-string 3)
;;                 (setq original-caption-before-del (match-string 3)))
;;               (next-line)               ; 因为设置了:wrap，所以这里不需要删除这一行
;;               )
;;             )))))
;;   (advice-add 'org-babel-execute-src-block :before #'get-attributes-from-src-block-result)

;;   :custom
;;   ;; 代码块语法高亮
;;   (org-src-fontify-natively t)
;;   ;; 使用编程语言的TAB绑定设置
;;   (org-src-tab-acts-natively t)
;;   ;; 保留代码块前面的空格
;;   (org-src-preserve-indentation t)
;;   ;; 代码块编辑窗口的打开方式：当前窗口+代码块编辑窗口
;;   (org-src-window-setup 'reorganize-frame)
;;   ;; 执行前是否需要确认
;;   (org-confirm-babel-evaluate nil)
;;   ;; 代码块默认前置多少空格
;;   (org-edit-src-content-indentation 0)
;;   ;; 代码块的语言模式设置，设置之后才能正确语法高亮
;;   (org-src-lang-modes '(("C"            . c)
;;                         ("C++"          . c++)
;;                         ("bash"         . sh)
;;                         ("cpp"          . c++)
;;                         ("elisp"        . emacs-lisp)
;;                         ("python"       . python)
;;                         ("shell"        . sh)
;;                         ("mysql"        . sql)
;;                         ))
;;   ;; 在这个阶段，只需要加载默认支持的语言
;;   (org-babel-load-languages '((python          . t)
;;                               (awk             . t)
;;                               (C               . t)
;;                               (calc            . t)
;;                               (emacs-lisp      . t)
;;                               (eshell          . t)
;;                               (shell           . t)
;;                               (sql             . t)
;;                               (css             . t)
;;                               ))
;;   )

;; (use-package plantuml-mode
;;   :ensure t
;;   :mode ("\\.plantuml\\'" . plantuml-mode)
;;   :init
;;   ;; enable plantuml babel support
;;   (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
;;   (org-babel-do-load-languages 'org-babel-load-languages
;;                                (append org-babel-load-languages
;;                                        '((plantuml . t))))
;;   :config
;;   (setq org-plantuml-exec-mode 'plantuml)
;;   (setq org-plantuml-executable-path "plantuml")
;;   (setq plantuml-executable-path "plantuml")
;;   (setq plantuml-default-exec-mode 'executable)
;;   ;; set default babel header arguments
;;   (setq org-babel-default-header-args:plantuml
;;         '((:exports . "results")
;;           (:results . "file")
;;           ))
;;   )



(provide 'init-org)

;;; init-org.el ends here
