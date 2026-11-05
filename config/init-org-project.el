;;; init-org-project.el --- Org Project Management -*- lexical-binding: t -*-

;; Copyright (C) 2020~2026 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;; Org 项目管理，由 init-org.el 提供 org 基础配置后加载：
;;
;;   1. 项目文件与编号 —— projects/<工程代号>/<配置项>.org 布局、CUSTOM_ID 生成
;;   2. 研制阶段       —— :CURRENT-STATE: 唯一阶段、阶段子树模板与阶段切换
;;   3. 里程碑           —— M1..M4 转段检查点、结算与 milestone-summary
;;   4. 需求与基线     —— 需求条目建档、全量重圈、基线记录表与快照动态块
;;   5. 风险与议题     —— 三态风险台账、评估/转议题/汇总、议题落点
;;   6. 捕获与视图     —— p 组模板、评审/测试记录落点、agenda 风险与里程碑台账
;;
;; 项目数据在 ~/Org/projects/ 下，本文件只放代码；升级旧项目用
;; `my/org-project-risk-upgrade' / `my/org-project-milestone-upgrade'。
;;
;;; Code:

(require 'org)
(require 'org-agenda)
(require 'org-capture)
(require 'org-id)
(require 'cl-lib)

;; ---- 项目管理 Project ------------------------------------------------------
;; 软件配置项项目文件布局: projects/<工程代号>/<配置项代号>.org, 工程代号即
;; projects/ 子目录名(pp 创建时可选现有或新建), 骨架由 pp 模板生成。
;; 研制阶段(方案设计/样机研制/工程研制/状态鉴定)任一时刻唯一, 记录在「研制过程」标题的
;; :CURRENT-STATE: 属性; 阶段任务、测试/试验记录、软件状态都创建在当前阶段
;; 子树下。里程碑即检查点: 「里程碑」章的 M1..M4 用 :FROM:/:TO: 承载阶段
;; 转换记录并由阶段切换自动结算, 没有独立的门概念。需求基线数据(记录表
;; 状态列 + 快照)集中在「需求」章「需求基线」节; 冻结判据、评审表与冻结
;; 操作挂在各阶段的需求类小节(宿主小节映射), 阶段引用块 baseline-by-phase、
;; 转段检查、milestone-summary 与切换日志都读记录表本阶段最新行的状态,
;; 单一数据源。
(defconst my/org-project--stages '("方案设计" "样机研制" "工程研制" "状态鉴定")
  "研制阶段固定划分.")

;; 需求基线记录表(baseline-records)的行状态词汇: 圈定=待评审, 冻结确认=
;; 已冻结, 同阶段重新圈定且未冻结的旧行=被取代。存量空状态按已冻结处理。
(defconst my/org-project--baseline-status-new "待评审"
  "圈定基线后记录表行的初始状态.")

(defconst my/org-project--baseline-status-frozen "已冻结"
  "冻结确认后的记录表行状态.")

(defconst my/org-project--baseline-status-superseded "被取代"
  "同阶段重新圈定时, 未冻结旧行的标记状态.")

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

(defun my/org-project--plan-subtree (idx)
  "生成「方案设计」阶段子树文本 (方案阶段专属结构). IDX 为阶段序号."
  (concat
   "*** 需求分析\n\n"
   "    #+BEGIN: req-by-phase :phase 方案设计\n"
   "    #+END:\n\n"
   "    - 入口条件：\n"
   "    - 阶段计划：\n"
   "    - 技术要求梳理（功能/性能/接口/其他需求用 pn 录入「需求」章）：\n"
   "    - 需求变更记录（pc 捕获，落在「需求」章「需求变更记录」表）：\n"
   (my/org-project--baseline-control-text "方案设计" idx "    ") "\n"
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

(defun my/org-project--generic-subtree (stage idx)
  "生成通用阶段子树文本: STAGE 阶段名, IDX 阶段序号 (控制块评审表名后缀)."
  (concat
   "*** 策划与需求\n\n"
   "    #+BEGIN: req-by-phase :phase " stage "\n"
   "    #+END:\n\n"
   "    - 入口条件：\n"
   "    - 阶段计划：\n"
   "    - 本阶段需求变更（pc 捕获，落在「需求」章「需求变更记录」表）：\n"
   (my/org-project--baseline-control-text stage idx "    ") "\n"
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

(defun my/org-project--qual-subtree (idx)
  "生成「状态鉴定」阶段子树文本 (鉴定阶段专属结构). IDX 为阶段序号."
  (concat
   "*** 鉴定策划\n\n"
   "    - 鉴定依据（研制任务书/技术要求）：\n"
   "    - 鉴定大纲与计划：\n"
   "    - 状态冻结安排：\n\n"
   "*** 状态管理\n\n"
   "    - 版本定版（软件状态条目在此登记）：\n"
   "    - 配置项基线：\n"
   "    - 技术状态更改控制：\n"
   (my/org-project--baseline-control-text "状态鉴定" idx "    ") "\n"
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
    ((string= stage "方案设计") (my/org-project--plan-subtree idx))
    ((string= stage "状态鉴定") (my/org-project--qual-subtree idx))
    (t (my/org-project--generic-subtree stage idx)))))

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
   "#   转段用 C-c P s: 结算里程碑(基线冻结状态 + 转段里程碑) → 更新属性 → 记日志;\n"
   "# - 阶段结构按阶段特征定制: 方案设计(需求/总体/质量特性/成熟度),\n"
   "#   样机研制(+样机调试), 工程研制(+环境与可靠性验证),\n"
   "#   状态鉴定(鉴定策划/状态管理/鉴定测试/鉴定材料);\n"
   "# - 状态流: 日常 TODO→NEXT→WAIT→DONE, 议题 REPORT→…→CLOSED,\n"
   "#   软件状态 DESIGN→DEVELOP→CONFIRMED→ARCHIVED;\n"
   "# - 需求条目在「需求」章统一建档(:PHASE: 可多值), 各阶段的动态块\n"
   "#   自动引用, 刷新用 C-c C-x C-u;\n"
   "# - 里程碑在「研制过程」>「里程碑」章: 转段里程碑 M1..M4 由 C-c P s 结算;\n"
   "# - 需求基线数据在「需求」章「需求基线」节(记录表状态: 待评审→已冻结),\n"
   "#   C-c P b 全量重圈打到当前阶段; 冻结判据/评审/冻结在各阶段需求类小节,\n"
   "#   冻结与解冻用 C-c P g, 评审用 pe 记入阶段评审表, 变更用 pc 捕获;\n"
   "# - 评审记录统一记在「里程碑」各条目的 review-<ID> 表内(pe 捕获);\n"
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
   "# 当前阶段唯一, 由 :CURRENT-STATE: 记录; 里程碑(即转段检查点)在下方\n"
   "# 「里程碑」章: :FROM:/:TO: 承载阶段转换; 基线冻结状态见「需求」章记录表与 milestone-summary.\n\n"
   (my/org-project--milestones-text code)
   "** 需求\n\n"
   "# 需求条目在此统一建档（唯一编辑处）, 用 pn 捕获; :PHASE: 取值 = 阶段名\n"
   "# (方案设计/样机研制/工程研制/状态鉴定, 可多值) 或 通用; 各阶段的动态块\n"
   "# 自动引用, 刷新用 C-c C-x C-u. 追踪矩阵在评审前逐条勾稽.\n\n"
   "*** 功能需求\n\n"
   "*** 性能需求\n\n"
   "*** 接口需求\n\n"
   "*** 其他需求\n\n"
   "*** 需求基线\n\n"
   "    # 唯一数据源(阶段引用/转段检查/台账都读本表): C-c P b 全量重圈 → 追加\n"
   "    # :BASELINE: 与本表新行(阶段=当前阶段, 状态=待评审); 冻结在各阶段需求类\n"
   "    # 小节执行(C-c P g, 判据与评审齐备后置「已冻结」); 同一阶段可多个基线,\n"
   "    # 版本全项目递增; 变更用 pc 捕获(移出基线), 重新圈定后进新版本行.\n\n"
   "    #+NAME: baseline-records\n"
   "    | 基线版本 | 阶段 | 日期 | 需求数 | 状态 | 评审结论 | 备注 |\n"
   "    |----------+------+------+--------+------+----------+------|\n\n"
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

(defun my/org-capture--baseline-prompt ()
  "提示选择软件状态依据的需求基线版本 (ps 模板展开), 默认当前阶段最新基线.
当前阶段还没有基线时返回空串, 属性可事后手工补填."
  (let* ((buf (my/org-capture--project-buffer))
         (stage (and buf (with-current-buffer buf (my/org-capture--current-stage))))
         (rows (and buf stage
                    (with-current-buffer buf
                      (seq-filter (lambda (r) (equal (plist-get r :phase) stage))
                                  (my/org-project--baseline-records-rows))))))
    (if (null rows)
        ""
      (completing-read "需求基线(回车=本阶段最新, 可空): "
                       (mapcar (lambda (r) (plist-get r :ver)) rows)
                       nil nil nil nil (plist-get (car (last rows)) :ver)))))

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

(defvar my/org-capture--review-stage nil
  "本次 pe 捕获选中的基线评审阶段 (评审对象为「本阶段基线」时非 nil),
供模板展开选版本与落表后提示冻结.")

(defun my/org-capture--review-object ()
  "评审对象 (pe 模板展开): 基线评审从记录表选本阶段版本, 里程碑评审自由输入."
  (if my/org-capture--review-stage
      (let* ((stage my/org-capture--review-stage)
             (buf (my/org-capture--project-buffer))
             (vers (and buf
                        (with-current-buffer buf
                          (mapcar (lambda (r) (plist-get r :ver))
                                  (seq-filter (lambda (r)
                                               (equal (plist-get r :phase) stage))
                                              (my/org-project--baseline-records-rows)))))))
        (if (null vers)
            (user-error "阶段「%s」还没有基线, 请先用 C-c P b 圈定" stage)
          (completing-read "基线版本: " vers nil t nil nil (car (last vers)))))
    (read-string "评审对象: ")))

(defun my/org-capture-review-target ()
  "评审记录(pe): 选本阶段基线或里程碑.
基线评审落当前阶段宿主小节的 baseline-review-<n> 表(缺控制块先补建),
里程碑评审落 review-<ID> 表."
  (let ((file (my/org-capture-project-file)))
    (set-buffer (org-capture-target-buffer file))
    (setq my/org-capture--last-buffer (current-buffer))
    (widen)
    (let* ((stage (my/org-project--file-stage))
           (milestones (my/org-project--milestone-entries))
           (choice (completing-read
                    "评审对象: "
                    (cons "本阶段基线" (mapcar #'my/org-project--milestone-label milestones))
                    nil t nil nil "本阶段基线")))
      (if (equal choice "本阶段基线")
          (progn
            (setq my/org-capture--review-stage stage)
            (my/org-project--stage-review-goto stage))
        (setq my/org-capture--review-stage nil)
        (let ((milestone (seq-find (lambda (g) (equal (my/org-project--milestone-label g) choice))
                              milestones)))
          (unless milestone (user-error "未找到里程碑 %s" choice))
          (my/org-project--milestone-review-table-goto (plist-get milestone :id)))))))

(defun my/org-capture-review-finalize ()
  "pe 落表后: 基线评审且最新基线判据已满足时, 询问是否冻结."
  (let ((stage my/org-capture--review-stage))
    (setq my/org-capture--review-stage nil)
    (when (and stage
               (not (and (boundp 'org-note-abort) org-note-abort))
               (buffer-live-p my/org-capture--last-buffer))
      (with-current-buffer my/org-capture--last-buffer
        (widen)
        (let* ((row (my/org-project--stage-latest-baseline stage))
               (stats (and row (ignore-errors
                                (my/org-project--stage-criteria stage)))))
          (when (and row
                     (not (my/org-project--baseline-frozen-p (plist-get row :status)))
                     stats
                     (null (nth 2 stats))
                     (> (my/org-project--stage-review-count
                         stage (plist-get row :ver))
                        0))
            (when (y-or-n-p (format "%s 判据已满足, 现在冻结基线? "
                                    (plist-get row :ver)))
              (my/org-project-baseline-freeze stage t))))))))

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
只列带 :PHASE: 的真实需求条目, 里程碑条目(在「里程碑」章)不参与变更."
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

(defun my/org-project--stage-req-unbased (stage)
  "返回阶段 STAGE 中未入基线的有效需求条目数 (归档/废弃/取消不计)."
  (seq-count (lambda (e) (not (org-string-nw-p (plist-get e :baseline))))
             (seq-filter (lambda (e)
                           (and (my/org-project--req-active-p e)
                                (member stage (split-string
                                               (or (plist-get e :phase) "")))))
                         (my/org-project--req-entries))))

(defun my/org-project--milestone-chain-summary (from to)
  "返回里程碑结算概况与本阶段基线冻结状态 (供切换日志)."
  (let* ((milestone (my/org-project--milestone-for from to))
         (row (my/org-project--stage-latest-baseline from))
         (bl (cond
              ((null row) "基线未建立")
              ((my/org-project--baseline-frozen-p (plist-get row :status))
               (format "基线 %s 已冻结" (plist-get row :ver)))
              (t (format "基线 %s 未冻结" (plist-get row :ver))))))
    (format "里程碑 %s · %s"
            (if milestone
                (format "%s %s" (plist-get milestone :suffix)
                        (if (my/org-project--milestone-open-p milestone) "未关闭" "已关闭"))
              (format "%s→%s 无对应里程碑" from to))
            bl)))

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
  (insert (format "   - %s\n" (my/org-project--milestone-chain-summary from to)))
  (insert (format "   - %s\n" (my/org-project--baseline-summary)))
  (insert (format "   - %s\n" (my/org-project--risk-summary-line)))
  (insert (format "   - 后续任务、测试/试验记录、软件状态均在「%s」下创建\n" to)))

(defun my/org-project--switch-gaps (cur stage)
  "返回从阶段 CUR 转到 STAGE 的里程碑缺口列表 (转段里程碑 + 基线冻结状态)."
  (let ((milestone (my/org-project--milestone-for cur stage))
        (bl-gap (my/org-project--stage-baseline-gap cur)))
    (append
     (when (null milestone)
       (list (format "未定义 %s → %s 的转段里程碑" cur stage)))
     (when bl-gap (list bl-gap))
     (when milestone
       (mapcar (lambda (gap) (concat (plist-get milestone :suffix) " " gap))
               (my/org-project--milestone-gaps milestone))))))

(defun my/org-project-stage-switch ()
  "切换项目当前研制阶段: 里程碑结算 → 更新 :CURRENT-STATE: → 记日志.
检查点 = 本阶段需求基线(须已冻结) + FROM 为当前阶段的里程碑(判据/前置里程碑).
有缺口时列出并二次确认; 强制转段写 :OVERRIDE: 并记入日志."
  (interactive)
  (let ((file (my/org-capture-project-file)))
    (set-buffer (org-capture-target-buffer file))
    (widen)
    (let* ((cur (my/org-capture--current-stage))
           (stage (completing-read (format "切换到阶段(当前 %s): " cur)
                                   my/org-project--stages nil t))
           (pending (my/org-project--pending-items cur))
           (milestone-gaps (and (not (string= stage cur))
                           (my/org-project--switch-gaps cur stage)))
           (high-risk (seq-some (lambda (p) (string-prefix-p "[风险 高" p))
                                pending))
           (gaps (append milestone-gaps pending))
           forced reason result milestone)
      (when (string= stage cur) (user-error "已在阶段「%s」" cur))
      (when (or gaps high-risk)
        (with-current-buffer (get-buffer-create "*项目转段检查*")
          (erase-buffer)
          (insert (format "阶段「%s」→「%s」\n\n" cur stage))
          (when milestone-gaps
            (insert "里程碑缺口:\n")
            (dolist (g milestone-gaps) (insert "  - " g "\n"))
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
      (setq milestone (my/org-project--milestone-for cur stage))
      (when milestone
        (my/org-project--milestone-goto (plist-get milestone :id))
        (org-entry-put nil "REASON" reason)
        (org-entry-put nil "TEST-RESULT" result)
        (when forced
          (org-entry-put nil "OVERRIDE" (format-time-string "[%Y-%m-%d %A %H:%M]")))
        (my/org-project--milestone-close (plist-get milestone :id) forced))
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
CUSTOM_ID, 里程碑条目在「里程碑」章, 均自动排除)."
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

(defun org-dblock-write:baseline-by-phase (params)
  "按记录表阶段列过滤, 生成本阶段基线状态引用表 (阶段子树控制块内用).
PARAMS 的 :phase 指定阶段名; 数据源自「需求」章 baseline-records,
状态列是转段检查与台账共用的同一份状态."
  (let* ((phase (plist-get params :phase))
         ;; org 用 read 解析块参数, 中文阶段名会成为符号, 需归一成字符串
         (phase (cond ((stringp phase) phase)
                      ((symbolp phase) (symbol-name phase))
                      (t (format "%s" phase))))
         (rows (seq-filter (lambda (r) (equal (plist-get r :phase) phase))
                           (my/org-project--baseline-records-rows))))
    (if (null rows)
        (insert "# 本阶段暂无基线（用 C-c P b 圈定）\n")
      (insert "| 基线版本 | 日期 | 需求数 | 状态 | 评审结论 | 备注 |\n")
      (insert "|----------+------+------+------+----------+------|\n")
      (dolist (r rows)
        (insert (format "| %s | %s | %s | %s | %s | %s |\n"
                        (plist-get r :ver)
                        (plist-get r :date)
                        (plist-get r :count)
                        (if (org-string-nw-p (plist-get r :status))
                            (plist-get r :status)
                          my/org-project--baseline-status-frozen)
                        (plist-get r :review)
                        (plist-get r :note)))))))

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

;; ---- 里程碑 Milestone ----
;; 里程碑即检查点: 「里程碑」章下的一个条目 = 判据 checkbox + 评审表
;; review-<后缀> + 证据链接。M1..M4 与阶段转换一一对应, 由 C-c P s 结算。
;; 需求基线不设里程碑标题: 记录表在「需求」章(状态列 待评审→已冻结), 冻结
;; 判据/评审表/冻结操作挂在各阶段需求类小节, 转段检查、milestone-summary 与
;; 切换日志经 `my/org-project--stage-baseline-gap' / baseline-by-phase 读同一份
;; 状态。关闭与冻结统一走"列缺口 + 二次确认"(C-c P g / pe 落表后的提示)。
(defconst my/org-project--milestone-chapter-re "^\\*+ 里程碑[ \t]*$"
  "「里程碑」章标题匹配 (兼容存量文件的顶级章与骨架的二级章).")

(defun my/org-project--milestone-chapter-level ()
  "返回「里程碑」章的标题层级, 不存在返回 nil."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward my/org-project--milestone-chapter-re nil t)
      (org-current-level))))

(defconst my/org-project--milestone-defs
  '(("M1" "方案评审通过" "方案设计" "样机研制")
    ("M2" "样机研制完成" "样机研制" "工程研制")
    ("M3" "工程研制完成" "工程研制" "状态鉴定")
    ("M4" "状态鉴定通过" "状态鉴定" nil))
  "里程碑定义: (编号 名称 来源阶段 目标阶段); 目标为 nil 表示项目终点里程碑.")

(defconst my/org-project--milestone-criteria
  '(("M1" "本阶段需求基线已冻结（最新基线状态为已冻结）"
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
         "遗留风险已挂账"))
  "里程碑的判据清单: 里程碑编号. (基线冻结判据见 my/org-project--baseline-criteria)")

(defun my/org-project--milestone-criteria-text (key indent)
  "生成判据 checkbox 文本, KEY 为里程碑编号, INDENT 为缩进."
  (mapconcat (lambda (item) (concat indent "- [ ] " item "\n"))
             (cdr (assoc key my/org-project--milestone-criteria))
             ""))

(defun my/org-project--milestone-review-table-text (suffix indent)
  "生成名为 review-SUFFIX 的空评审表文本, INDENT 为缩进."
  (concat indent "#+NAME: review-" suffix "\n"
          indent "| 日期 | 评审类型 | 评审对象 | 人员 | 结论 | 遗留问题 | 待办 |\n"
          indent "|------+----------+----------+------+------+----------+------|\n"))

(defun my/org-project--milestone-text (code id name from to &optional level)
  "生成里程碑文本: CODE 项目代号, ID 编号, NAME 名称, FROM/TO 阶段.
LEVEL 为「里程碑」章层级, 里程碑标题取其下一级 (默认 2)."
  (concat
   (make-string (1+ (or level 2)) ?*) " TODO " id " " name " :milestone:\n"
   "    :PROPERTIES:\n"
   "    :CUSTOM_ID: " code "_" id "\n"
   "    :FROM:     " from "\n"
   (if to (concat "    :TO:       " to "\n") "")
   "    :REASON:\n"
   "    :TEST-RESULT:\n"
   "    :OWNER:\n"
   "    :END:\n\n"
   (my/org-project--milestone-criteria-text id "    ")
   "\n"
   (my/org-project--milestone-review-table-text id "    ")
   "\n"))

(defconst my/org-project--milestone-chapter-preamble
  (concat
   "# 里程碑台账: M1..M4 承载转段并改变 :CURRENT-STATE:。每个里程碑 = 判据\n"
   "# checkbox + 评审表 review-<ID> + 证据链接; 判据未勾完时 checkbox 依赖会阻止关闭。\n"
   "# 关闭 `C-c P g' (列缺口 + 二次确认), 转段 `C-c P s' (里程碑结算)。\n"
   "# 需求基线不设里程碑: 记录表在「需求」章, 冻结判据/评审/冻结在各阶段需求类小节。\n\n")
  "「里程碑」章的说明文字.")

(defun my/org-project--milestones-text (code &optional level)
  "生成「里程碑」章完整子树文本: 汇总块 + 四个转段里程碑.
LEVEL 为章的层级 (默认 2, 即「研制过程」下的二级章)."
  (let ((level (or level 2)))
    (concat
     (make-string level ?*) " 里程碑\n\n"
     my/org-project--milestone-chapter-preamble
     "   #+BEGIN: milestone-summary\n"
     "   #+END:\n\n"
     (mapconcat (lambda (spec)
                  (apply #'my/org-project--milestone-text
                         code (append spec (list level))))
                my/org-project--milestone-defs ""))))

(defun my/org-project--milestone-suffix (id)
  "里程碑的表名后缀: ID 最后一个下划线之后的部分 (1501_X_B2 -> B2)."
  (if (string-match "\\([^_]+\\'\\)" id)
      (match-string 1 id)
    id))

(defun my/org-project--milestone-entries ()
  "扫描「里程碑」章, 返回里程碑条目 plist 列表 (带 CUSTOM_ID 的条目即里程碑).
每项含 :pos :id :suffix :title :from :to :requires :owner :todo."
  (let (entries)
    (org-with-wide-buffer
     (save-excursion
       (goto-char (point-min))
       (when (re-search-forward my/org-project--milestone-chapter-re nil t)
         (let* ((level (1+ (org-current-level)))
                (end (save-excursion (org-end-of-subtree t t) (point)))
                (re (concat "^" (regexp-quote (make-string level ?*)) " ")))
           (while (re-search-forward re end t)
             (let ((id (org-entry-get nil "CUSTOM_ID")))
               (when id
                 (push (list :pos (point)
                             :id id
                             :suffix (my/org-project--milestone-suffix id)
                             :title (org-get-heading t t t t)
                             :from (org-entry-get nil "FROM")
                             :to (org-entry-get nil "TO")
                             :requires (org-entry-get nil "REQUIRES")
                             :owner (org-entry-get nil "OWNER")
                             :todo (my/org-project--state))
                       entries))))))))
    (nreverse entries)))

(defun my/org-project--milestone-get (id)
  "按 ID 返回里程碑 plist, 不存在返回 nil."
  (seq-find (lambda (g) (equal (plist-get g :id) id))
            (my/org-project--milestone-entries)))

(defun my/org-project--milestone-open-p (milestone)
  "里程碑是否未完成 (TODO 或未状态化)."
  (not (member (plist-get milestone :todo) '("DONE" "CANCELLED"))))

(defun my/org-project--milestone-goto (id)
  "按 ID 定位里程碑条目, 成功返回 non-nil 且 point 在标题行."
  (goto-char (point-min))
  (when (re-search-forward
         (concat "^[ \t]*:CUSTOM_ID:[ \t]+" (regexp-quote id) "[ \t]*$") nil t)
    (org-back-to-heading t)))

(defun my/org-project--milestone-criteria-stats (id)
  "返回里程碑 ID 的判据状态: (已勾 总数 未勾文本列表)."
  (unless (my/org-project--milestone-goto id)
    (user-error "未找到里程碑 %s" id))
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

(defun my/org-project--milestone-requires-unmet (milestone)
  "返回里程碑 :REQUIRES: 中未完成的前置里程碑 ID 列表."
  (let (unmet)
    (dolist (id (split-string (or (plist-get milestone :requires) "") "[ ,]+" t))
      (let ((g (my/org-project--milestone-get id)))
        (when (or (null g) (my/org-project--milestone-open-p g))
          (push id unmet))))
    (nreverse unmet)))

(defun my/org-project--milestone-gaps (milestone)
  "返回里程碑的缺口描述列表 (判据未勾 + 前置里程碑未完成)."
  (let* ((id (plist-get milestone :id))
         (stats (my/org-project--milestone-criteria-stats id))
         (done (nth 0 stats))
         (total (nth 1 stats))
         (unchecked (nth 2 stats))
         (unmet (my/org-project--milestone-requires-unmet milestone)))
    (append
     (when (< done total)
       (list (format "判据 %d/%d" done total)))
     (mapcar (lambda (item) (concat "未勾判据: " item)) unchecked)
     (mapcar (lambda (req) (format "前置里程碑未关闭: %s" req)) unmet))))

(defun my/org-project--milestone-review-count (id)
  "返回里程碑评审表的数据行数."
  (unless (my/org-project--milestone-goto id)
    (user-error "未找到里程碑 %s" id))
  (let ((end (save-excursion (org-end-of-subtree t t) (point)))
        (name (concat "review-" (my/org-project--milestone-suffix id)))
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

(defun my/org-project--milestone-review-table-ensure (id)
  "确保里程碑下有 review-<后缀> 表, 缺失则在里程碑子树末尾补一张."
  (unless (my/org-project--milestone-goto id)
    (user-error "未找到里程碑 %s" id))
  (let* ((beg (point))
         (end (save-excursion (org-end-of-subtree t t) (point)))
         (suffix (my/org-project--milestone-suffix id))
         (re (concat "^[ \t]*#\\+NAME:[ \t]+review-"
                     (regexp-quote suffix) "[ \t]*$")))
    (goto-char beg)
    (unless (re-search-forward re end t)
      (goto-char end)
      (unless (bolp) (insert "\n"))
      (insert "\n" (my/org-project--milestone-review-table-text suffix "    "))
      (my/org-project--align-tables))))

(defun my/org-project--milestone-review-table-goto (id)
  "定位到里程碑的评审表 #+NAME 行, 缺表则补建."
  (my/org-project--milestone-review-table-ensure id)
  (my/org-project--milestone-goto id)
  (let ((end (save-excursion (org-end-of-subtree t t) (point)))
        (name (concat "review-" (my/org-project--milestone-suffix id))))
    (goto-char (point-min))
    (unless (re-search-forward
             (concat "^[ \t]*#\\+NAME:[ \t]+" (regexp-quote name) "[ \t]*$")
             end t)
      (user-error "未找到评审表 %s" name))
    (goto-char (line-beginning-position))))

(defun my/org-project--milestone-close (id &optional force)
  "关闭里程碑 ID: 列缺口并二次确认后置 DONE.
FORCE 非 nil 时跳过确认, 并绕过 checkbox 依赖强制关闭."
  (let ((milestone (my/org-project--milestone-get id)))
    (unless milestone (user-error "未找到里程碑 %s" id))
    (let ((gaps (my/org-project--milestone-gaps milestone)))
      (unless (or force (null gaps))
        (unless (y-or-n-p
                 (format "%s 仍有缺口（%s）, 确认关闭? "
                         id (mapconcat #'identity gaps "; ")))
          (user-error "已取消关闭")))
      (my/org-project--milestone-goto id)
      ;; CLOSED 由 org-log-done 自动写入, org-entry-put 不能直接设该属性
      (let ((org-enforce-todo-checkbox-dependencies nil)
            (org-enforce-todo-dependencies nil))
        (org-todo "DONE"))
      (message "里程碑 %s 已关闭%s" id (if gaps "（存在缺口，已确认）" "")))))

(defun my/org-project--milestone-reopen (id)
  "重开里程碑 ID (置 TODO 并清空 CLOSED)."
  (unless (my/org-project--milestone-goto id)
    (user-error "未找到里程碑 %s" id))
  (let ((org-closed-keep-when-no-todo nil))
    (org-todo "TODO"))
  (message "里程碑 %s 已重开" id))

(defun my/org-project--milestone-for (from to)
  "返回来源阶段 FROM、目标阶段 TO 的里程碑; TO 为 nil 时不限目标."
  (seq-find (lambda (g)
              (and (equal (plist-get g :from) from)
                   (or (null to) (equal (plist-get g :to) to))))
            (my/org-project--milestone-entries)))

(defun my/org-project--milestone-label (milestone)
  "里程碑的补全显示文本."
  (format "%s %s [%s]"
          (plist-get milestone :suffix)
          (or (plist-get milestone :title) "")
          (or (plist-get milestone :todo) "无状态")))

(defun my/org-project--milestone-sort-key (milestone)
  "里程碑的排序键: 按编号数字排序 (M1 < M2 ...)."
  (let ((suffix (plist-get milestone :suffix)))
    (if (and suffix (string-match "\\([0-9]+\\)\\'" suffix))
        (string-to-number (match-string 1 suffix))
      0)))

(defun my/org-project-milestone-dispatch ()
  "里程碑/基线操作: 先选对象(本阶段基线 / 里程碑), 再选动作.
基线动作: 冻结 / 解冻 / 查看判据 / 跳到评审表 (数据在「需求」章记录表);
里程碑动作: 关闭 / 重开 / 查看判据 / 跳到评审表。都遵循「列缺口 + 二次确认」;
M4 终点里程碑只能从这里关闭."
  (interactive)
  (unless (and (derived-mode-p 'org-mode) (buffer-file-name))
    (user-error "请在项目 org 文件中执行"))
  (widen)
  (let* ((stage (my/org-project--file-stage))
         (milestones (my/org-project--milestone-entries))
         (target (completing-read
                  "对象: "
                  (cons "本阶段基线" (mapcar #'my/org-project--milestone-label milestones))
                  nil t nil nil "本阶段基线")))
    (if (equal target "本阶段基线")
        (let ((action (completing-read (format "基线(%s): " stage)
                                       '("冻结" "解冻" "查看判据" "跳到评审表")
                                       nil t)))
          (pcase action
            ("冻结" (my/org-project-baseline-freeze stage))
            ("解冻" (my/org-project-baseline-unfreeze stage))
            ("查看判据"
             (my/org-project--stage-baseline-ensure stage)
             (let* ((stats (my/org-project--stage-criteria stage))
                    (gaps (nth 2 stats)))
               (message "%s 基线判据 %d/%d%s" stage (nth 0 stats) (nth 1 stats)
                        (if gaps (concat "; 未勾: "
                                         (mapconcat #'identity gaps " / "))
                          ""))))
            ("跳到评审表" (my/org-project--stage-review-goto stage))))
      (let* ((milestone (seq-find (lambda (g) (equal (my/org-project--milestone-label g) target))
                             milestones))
             (id (and milestone (plist-get milestone :id)))
             (action (completing-read
                      (format "里程碑 %s: " target)
                      '("关闭" "重开" "查看判据" "跳到评审表") nil t)))
        (unless milestone (user-error "未找到里程碑 %s" target))
        (pcase action
          ("关闭" (my/org-project--milestone-close id))
          ("重开" (my/org-project--milestone-reopen id))
          ("查看判据"
           (let* ((stats (my/org-project--milestone-criteria-stats id))
                  (done (nth 0 stats))
                  (total (nth 1 stats))
                  (gaps (nth 2 stats)))
             (message "%s 判据 %d/%d%s" id done total
                      (if gaps (concat "; 未勾: " (mapconcat #'identity gaps " / "))
                        ""))))
          ("跳到评审表" (my/org-project--milestone-review-table-goto id)))))))

(defun org-dblock-write:milestone-summary (_params)
  "生成里程碑台账: 按来源阶段分组列出全部里程碑, 判据为人为判断, 事实列由数据计算."
  (let* ((milestones (my/org-project--milestone-entries))
         (reqs (my/org-project--req-entries))
         (unbased (seq-count (lambda (e)
                               (not (org-string-nw-p (plist-get e :baseline))))
                             reqs))
         (phases (append my/org-project--stages '(""))))
    (insert "| 里程碑 | 状态 | 阶段 | 判据 | 事实 | 评审 |\n")
    (insert "|--------+------+------+------+------+------|\n")
    (dolist (phase phases)
      (let ((rows (sort (seq-filter
                         (lambda (g)
                           ;; 里程碑按来源阶段归组
                           (equal (or (plist-get g :from) "") (or phase "")))
                         milestones)
                        (lambda (a b)
                          (< (my/org-project--milestone-sort-key a)
                             (my/org-project--milestone-sort-key b))))))
        (when rows
          (insert (format "# %s\n" (if (string= phase "") "未绑定阶段" phase)))
          (dolist (g rows)
            (let* ((id (plist-get g :id))
                   ;; 判据/评审表读取会移动 point, 必须包在 save-excursion 里
                   (stats (save-excursion (my/org-project--milestone-criteria-stats id)))
                   (done (nth 0 stats))
                   (total (nth 1 stats))
                   (facts
                    (let ((from (plist-get g :from)))
                      (if from
                          ;; 里程碑事实 = 来源阶段基线冻结状态 + 未入基线条数
                          (let ((row (my/org-project--stage-latest-baseline from)))
                            (format "%s · 未入基线 %d 条"
                                    (cond
                                     ((null row) "本阶段未打基线")
                                     ((my/org-project--baseline-frozen-p
                                       (plist-get row :status))
                                      (format "基线 %s 已冻结" (plist-get row :ver)))
                                     (t (format "基线 %s 未冻结" (plist-get row :ver))))
                                    unbased))
                        (format "未入基线 %d 条" unbased)))))
              (insert (format "| [[#%s][%s]] | %s | %s | %d/%d | %s | %d |\n"
                              id (plist-get g :suffix)
                              (or (plist-get g :todo) "无状态")
                              (or (plist-get g :from) "")
                              done total facts
                              (save-excursion
                                (my/org-project--milestone-review-count id)))))))))
    (insert (format "# 里程碑 %d 个 · 未完成 %d 个 · 需求未入基线 %d 条\n"
                    (length milestones)
                    (seq-count #'my/org-project--milestone-open-p milestones)
                    unbased))))

;; ---- 里程碑升级 Milestone chapter upgrade ----
(defun my/org-project--milestone-chapter-p ()
  "当前缓冲区「里程碑」章是否已含 milestone-summary 动态块."
  (save-excursion
    (goto-char (point-min))
    (and (re-search-forward my/org-project--milestone-chapter-re nil t)
         (let ((end (save-excursion (org-end-of-subtree t t) (point))))
           (save-excursion
             (re-search-forward "^[ \t]*#\\+BEGIN:[ \t]+milestone-summary" end t))))))

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

(defun my/org-project-milestone-upgrade ()
  "把当前项目文件的「里程碑」章升级为里程碑台账 (幂等).
迁移旧评审行 (M1→M1, M3→M2, M4→M3, M5→M4); 删除需求章的旧 M2 与其评审表.
需求基线不设里程碑: 基线记录与冻结状态在「需求」章 baseline-records, 旧 M2
「需求基线建立」的评审行无对应条目, 随旧 M2 条目一并丢弃."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "请在项目 org 文件中执行"))
  (org-with-wide-buffer
   (if (my/org-project--milestone-chapter-p)
       (message "「里程碑」章已是里程碑台账, 无需升级")
     (let* ((code (file-name-base (buffer-file-name)))
            (rows (mapcar (lambda (name)
                            (cons name (my/org-project--named-table-rows
                                        (concat "review-" name))))
                          '("M1" "M3" "M4" "M5"))))
       ;; 先删旧 M2 与其评审表, 再重建章, 避免误删新 M2 的 review-M2
       (my/org-project--legacy-m2-remove)
       (goto-char (point-min))
       (if (not (re-search-forward my/org-project--milestone-chapter-re nil t))
           (user-error "未找到「里程碑」章, 请先在「研制过程」下补建")
         (let* ((level (org-current-level))
                (beg (line-beginning-position))
                (end (save-excursion (org-end-of-subtree t t) (point))))
           (delete-region beg end)
           (goto-char beg)
           (insert (my/org-project--milestones-text code level) "\n")))
       ;; 迁入旧评审行
       (dolist (pair '(("M1" . "M1") ("M3" . "M2") ("M4" . "M3") ("M5" . "M4")))
         (let ((old (cdr (assoc (car pair) rows))))
           (when old
             (my/org-project--table-append-rows
              (concat "review-" (cdr pair)) old))))
       (org-update-all-dblocks)
       (message "「里程碑」章已升级为里程碑台账 (里程碑 %d 个)"
                (length (my/org-project--milestone-entries)))))))

(defun my/org-project-milestone-upgrade-all ()
  "批量把 projects/ 下项目文件的「里程碑」章升级为里程碑台账.
升级前用 tar 备份 projects/ (~/Org 不在版本控制内), 已升级的自动跳过."
  (interactive)
  (let* ((files (my/org-project--files))
         (pending (seq-filter
                   (lambda (f)
                     (let ((org-inhibit-startup t))
                       (with-current-buffer (find-file-noselect (cdr f))
                         (not (my/org-project--milestone-chapter-p)))))
                   files)))
    (unless pending (user-error "所有项目文件的里程碑章都已是里程碑台账"))
    (when (y-or-n-p (format "将升级 %d 个文件的里程碑章, 并先备份 projects/? "
                            (length pending)))
      (let ((tar (expand-file-name
                  (format "projects-milestone-backup-%s.tar.gz"
                          (format-time-string "%Y%m%d-%H%M%S"))
                  org-directory)))
        (call-process "tar" nil nil nil "-czf" tar "-C" org-directory "projects")
        (message "已备份: %s" tar))
      (let ((n 0))
        (dolist (f pending)
          (let ((org-inhibit-startup t))
            (with-current-buffer (find-file-noselect (cdr f))
              (my/org-project-milestone-upgrade)
              (save-buffer)
              (setq n (1+ n)))))
        (message "已升级 %d 个文件的里程碑章" n)))))

;; ---- 需求基线 Requirement baseline ----
;; 数据唯一存放处: 「需求」章 baseline-records 记录表(状态 待评审→已冻结)
;; + 按版本的 req-baseline 快照。阶段子树只挂控制块(判据 checkbox + 引用块 +
;; 评审表, 宿主小节见 `my/org-project--stage-baseline-hosts'); 转段检查、
;; milestone-summary、切换日志都读记录表本阶段最新行状态。全量重圈 C-c P b 只写
;; :BASELINE: 与记录表行, 不生成任何标题。
(defconst my/org-project--baseline-criteria
  '("本阶段需求全部入基线"
    "基线评审通过（结论：通过/有条件通过）"
    "变更请求已处置或挂账")
  "需求基线冻结判据清单 (与阶段控制块 checkbox 一致).")

(defconst my/org-project--stage-baseline-hosts
  '(("方案设计" . "需求分析")
    ("样机研制" . "策划与需求")
    ("工程研制" . "策划与需求")
    ("状态鉴定" . "状态管理"))
  "阶段到基线控制块宿主小节的映射 (阶段子树不设独立基线节).")

(defun my/org-project--stage-baseline-host (stage)
  "返回阶段 STAGE 基线控制块所在宿主小节名, 未知阶段默认「策划与需求」."
  (or (cdr (assoc stage my/org-project--stage-baseline-hosts)) "策划与需求"))

(defun my/org-project--stage-index (stage)
  "返回阶段 STAGE 在 my/org-project--stages 中的序号 (1 起), 未知返回 0."
  (let ((i (seq-position my/org-project--stages stage)))
    (if i (1+ i) 0)))

(defun my/org-project--stage-review-table-name (stage)
  "返回阶段 STAGE 评审表的 #+NAME 标识."
  (format "baseline-review-%d" (my/org-project--stage-index stage)))

(defun my/org-project--baseline-control-text (stage idx indent)
  "生成阶段 STAGE 的基线控制块文本: 判据 checkbox + 引用块 + 评审表.
IDX 为阶段序号(评审表名 baseline-review-IDX 后缀), INDENT 为缩进;
骨架生成与缺块补建共用."
  (concat indent "# 需求基线冻结判据（勾齐且有评审后用 C-c P g 冻结）:\n"
          (mapconcat (lambda (item) (concat indent "- [ ] " item "\n"))
                     my/org-project--baseline-criteria "")
          indent "# 本阶段基线状态（引用「需求」章 baseline-records, 刷新 C-c C-x C-u）:\n"
          indent "#+BEGIN: baseline-by-phase :phase " stage "\n"
          indent "#+END:\n\n"
          indent "#+NAME: baseline-review-" (number-to-string idx) "\n"
          indent "| 日期 | 评审类型 | 评审对象 | 人员 | 结论 | 遗留问题 | 待办 |\n"
          indent "|------+----------+----------+------+------+----------+------|\n"))

(defun my/org-project--baseline-max (entries)
  "扫描需求条目 ENTRIES 的 :BASELINE: 与记录表行, 返回最大版本号 (无则 0)."
  (let ((max 0))
    (dolist (e entries)
      (dolist (v (split-string (or (plist-get e :baseline) "")))
        (when (string-match "\\`RB\\([0-9]+\\)\\'" v)
          (setq max (max max (string-to-number (match-string 1 v)))))))
    (dolist (row (my/org-project--baseline-records-rows))
      (setq max (max max (my/org-project--rb-number (plist-get row :ver)))))
    max))

(defun my/org-project--baseline-records-rows ()
  "解析 baseline-records 表, 返回行 plist 列表 (读取用, point 不变).
每项含 :ver :phase :date :count :status :review :note; 无表或无行返回 nil."
  (save-restriction
   (save-excursion
     (widen)
     (goto-char (point-min))
     (when (re-search-forward "^[ \t]*#\\+NAME:[ \t]+baseline-records[ \t]*$" nil t)
       (forward-line 1)
       (when (org-at-table-p)
         (let ((end (org-table-end)) rows)
           (forward-line 2)
           (while (< (point) end)
             (when (looking-at
                    (concat "^[ \t]*|\\([^|]*\\)|\\([^|]*\\)|\\([^|]*\\)|"
                            "\\([^|]*\\)|\\([^|]*\\)|\\([^|]*\\)|\\([^|]*\\)|"))
               ;; 先取出全部匹配串: string-trim 内部会重置 match data
               (let ((ver (match-string 1)) (ph (match-string 2))
                     (date (match-string 3)) (count (match-string 4))
                     (status (match-string 5)) (review (match-string 6))
                     (note (match-string 7)))
                 (setq ver (string-trim ver) ph (string-trim ph)
                       date (string-trim date) count (string-trim count)
                       status (string-trim status) review (string-trim review)
                       note (string-trim note))
                 (when (string-match "\\`RB[0-9]+\\'" ver)
                   (push (list :ver ver :phase ph :date date :count count
                               :status status :review review :note note)
                         rows))))
             (forward-line 1))
           (nreverse rows)))))))

(defun my/org-project--stage-latest-baseline (stage)
  "返回记录表中属于阶段 STAGE 的最新基线行 plist (版本号最大), 无则 nil."
  (let (latest)
    (dolist (row (my/org-project--baseline-records-rows))
      (when (equal (plist-get row :phase) stage)
        (when (or (null latest)
                  (> (my/org-project--rb-number (plist-get row :ver))
                     (my/org-project--rb-number (plist-get latest :ver))))
          (setq latest row))))
    latest))

(defun my/org-project--baseline-frozen-p (status)
  "行状态 STATUS 是否视为已冻结 (存量空状态按已冻结处理, 不挡转段)."
  (or (not (org-string-nw-p status))
      (string= status my/org-project--baseline-status-frozen)))

(defun my/org-project--baseline-cell-set (ver col value)
  "把基线记录表中版本 VER 行的第 COL 列置为 VALUE (列号 1 起)."
  (save-restriction
   (save-excursion
     (widen)
     (goto-char (point-min))
     (unless (re-search-forward "^[ \t]*#\\+NAME:[ \t]+baseline-records[ \t]*$" nil t)
       (user-error "未找到基线记录表 baseline-records"))
     (forward-line 1)
     (unless (org-at-table-p) (user-error "基线记录表缺失或格式异常"))
     (let ((end (org-table-end)) found)
       (while (and (not found) (< (point) end))
         (if (not (looking-at "^[ \t]*|"))
             (forward-line 1)
           (let* ((cells (split-string
                          (buffer-substring (line-beginning-position)
                                            (line-end-position)) "|"))
                  (first (string-trim (or (nth 1 cells) ""))))
             (if (equal first ver)
                 (progn
                   (org-table-goto-column col)
                   (let ((b (point)))
                     (skip-chars-forward "^|\n")
                     (delete-region b (point))
                     (insert value))
                   (setq found t))
               (forward-line 1)))))
       (unless found (user-error "基线记录表中无版本 %s" ver))
       (org-table-align)))))

(defun my/org-project--baseline-status-set (ver status)
  "把基线记录表版本 VER 的行状态列(第 5 列)置为 STATUS."
  (my/org-project--baseline-cell-set ver 5 status))

(defun my/org-project--stage-baseline-gap (stage)
  "返回阶段 STAGE 的基线冻结缺口描述; 已冻结或尚未圈定时返回 nil.
尚未圈定的阶段由「需求未入基线」检查兜底, 此处不报缺口."
  (let ((row (my/org-project--stage-latest-baseline stage)))
    (when (and row (not (my/org-project--baseline-frozen-p (plist-get row :status))))
      (format "基线 %s 未冻结（状态 %s）"
              (plist-get row :ver)
              (if (org-string-nw-p (plist-get row :status))
                  (plist-get row :status)
                my/org-project--baseline-status-new)))))

(defun my/org-project--stage-baseline-goto (stage)
  "定位到阶段 STAGE 宿主小节标题, 成功返回 non-nil 且 point 在标题行.
调用方需已进入目标项目缓冲区并 widen."
  (goto-char (point-min))
  (when (re-search-forward (concat "^\\*\\* " (regexp-quote stage) "[ \t]*$") nil t)
    (org-back-to-heading t)
    (let* ((beg (point))
           (end (save-excursion (org-end-of-subtree t t) (point)))
           (re (concat "^\\*\\*\\* "
                       (regexp-quote (my/org-project--stage-baseline-host stage))
                       "[ \t]*$")))
      (goto-char beg)
      (when (re-search-forward re end t)
        (org-back-to-heading t)))))

(defun my/org-project--stage-baseline-ensure (stage)
  "确保阶段 STAGE 宿主小节含基线控制块, 缺失则整块补建; 返回 point 在小节标题.
以「# 需求基线冻结判据」注释行为存在标记 (控制块整体创建)."
  (unless (my/org-project--stage-baseline-goto stage)
    (user-error "未找到阶段「%s」的小节「%s」"
                stage (my/org-project--stage-baseline-host stage)))
  (let ((beg (point))
        (end (save-excursion (org-end-of-subtree t t) (point))))
    (goto-char beg)
    (unless (re-search-forward "^[ \t]*# 需求基线冻结判据" end t)
      (goto-char end)
      (unless (bolp) (insert "\n"))
      (insert "\n"
              (my/org-project--baseline-control-text
               stage (my/org-project--stage-index stage) "    "))
      (my/org-project--align-tables)
      (message "已在「%s > %s」补建基线控制块"
               stage (my/org-project--stage-baseline-host stage)))
    (goto-char beg)))

(defun my/org-project--stage-criteria (stage)
  "返回阶段 STAGE 基线判据状态: (已勾 总数 未勾文本列表).
判据区 = 宿主小节内「# 需求基线冻结判据」注释行到评审表 #+NAME 行之间."
  (unless (my/org-project--stage-baseline-goto stage)
    (user-error "阶段「%s」缺宿主小节「%s」, 无法读取基线判据"
                stage (my/org-project--stage-baseline-host stage)))
  (let ((sec-end (save-excursion (org-end-of-subtree t t) (point))))
    (unless (re-search-forward "^[ \t]*# 需求基线冻结判据" sec-end t)
      (user-error "阶段「%s」缺基线判据区 (用 C-c P g 冻结时会自动补建)" stage))
    (let* ((cbeg (line-beginning-position))
           (cend (save-excursion
                   (if (re-search-forward
                        "^[ \t]*#\\+NAME:[ \t]+baseline-review-" sec-end t)
                       (line-beginning-position)
                     sec-end)))
           (done 0) (total 0) gaps)
      (goto-char cbeg)
      (while (re-search-forward
              "^[ \t]*[-+][ \t]+\\[\\([ X-]\\)\\][ \t]+\\(.*\\)$" cend t)
        (setq total (1+ total))
        (if (equal (match-string 1) " ")
            (push (string-trim (match-string 2)) gaps)
          (setq done (1+ done))))
      (list done total (nreverse gaps)))))

(defun my/org-project--stage-review-rows (stage)
  "返回阶段 STAGE 评审表的数据行字段列表 (每行 7 字段, 已 trim), 无则 nil."
  (let ((raw (my/org-project--named-table-rows
              (my/org-project--stage-review-table-name stage))))
    (when raw
      (delq nil
            (mapcar (lambda (line)
                      (when (string-match-p "^[ \t]*|" line)
                        (mapcar #'string-trim (split-string line "|"))))
                    (split-string raw "\n"))))))

(defun my/org-project--stage-review-count (stage ver)
  "阶段 STAGE 评审表中评审对象包含版本 VER 的数据行数."
  (let ((n 0))
    (dolist (cells (my/org-project--stage-review-rows stage))
      (when (and (nth 3 cells) ver
                 (string-match-p (regexp-quote ver) (nth 3 cells)))
        (setq n (1+ n))))
    n))

(defun my/org-project--stage-review-conclusion (stage ver)
  "阶段 STAGE 评审表中评审对象包含版本 VER 的最后一行结论, 无则 nil."
  (let (concl)
    (dolist (cells (my/org-project--stage-review-rows stage))
      (when (and (nth 3 cells) ver
                 (string-match-p (regexp-quote ver) (nth 3 cells))
                 (org-string-nw-p (nth 5 cells)))
        (setq concl (nth 5 cells))))
    concl))

(defun my/org-project--stage-review-goto (stage)
  "定位到阶段 STAGE 宿主小节评审表 #+NAME 行 (缺控制块先补建)."
  (my/org-project--stage-baseline-ensure stage)
  (let* ((beg (point))
         (end (save-excursion (org-end-of-subtree t t) (point)))
         (re (concat "^[ \t]*#\\+NAME:[ \t]+"
                     (regexp-quote (my/org-project--stage-review-table-name stage))
                     "[ \t]*$")))
    (goto-char beg)
    (unless (re-search-forward re end t)
      (user-error "阶段「%s」缺评审表 %s"
                  stage (my/org-project--stage-review-table-name stage)))
    (goto-char (line-beginning-position))))

(defun my/org-project-baseline-freeze (stage &optional force)
  "冻结阶段 STAGE 的最新基线: 列缺口并二次确认后置「已冻结」.
FORCE 非 nil 跳过确认 (pe/圈定收尾联动). 评审结论为空时回填评审表该版本结论."
  (widen)
  (my/org-project--stage-baseline-ensure stage)
  (let ((row (my/org-project--stage-latest-baseline stage)))
    (unless row (user-error "阶段「%s」还没有基线, 请先用 C-c P b 圈定" stage))
    (when (my/org-project--baseline-frozen-p (plist-get row :status))
      (user-error "%s 已处于「%s」状态"
                  (plist-get row :ver)
                  (or (plist-get row :status) my/org-project--baseline-status-frozen)))
    (let* ((ver (plist-get row :ver))
           (stats (my/org-project--stage-criteria stage))
           (gaps (copy-sequence (nth 2 stats)))
           (reviews (my/org-project--stage-review-count stage ver))
           (unbased (my/org-project--stage-req-unbased stage))
           (had-gaps nil))
      (when (> unbased 0)
        (push (format "本阶段 %d 条需求未入基线" unbased) gaps))
      (when (= reviews 0)
        (push "该版本无评审记录（用 pe 落表）" gaps))
      (setq had-gaps (not (null gaps)))
      (unless force
        (if (null gaps)
            (unless (y-or-n-p (format "%s 判据与评审齐备, 冻结? " ver))
              (user-error "已取消冻结"))
          (unless (y-or-n-p
                   (format "%s 存在缺口: %s; 仍要冻结? "
                           ver (mapconcat #'identity (nreverse gaps) "；")))
            (user-error "已取消冻结"))))
      (my/org-project--baseline-status-set ver my/org-project--baseline-status-frozen)
      (unless (org-string-nw-p (plist-get row :review))
        (let ((concl (my/org-project--stage-review-conclusion stage ver)))
          (when concl (my/org-project--baseline-cell-set ver 6 concl))))
      (message "基线 %s 已冻结（阶段 %s）%s" ver stage
               (if had-gaps "（缺口已确认）" "")))))

(defun my/org-project-baseline-unfreeze (stage &optional force)
  "解冻阶段 STAGE 的最新基线: 置回「待评审」. FORCE 非 nil 跳过确认."
  (widen)
  (let ((row (my/org-project--stage-latest-baseline stage)))
    (unless row (user-error "阶段「%s」还没有基线" stage))
    (unless (my/org-project--baseline-frozen-p (plist-get row :status))
      (user-error "%s 当前不是已冻结状态" (plist-get row :ver)))
    (unless (or force
                (y-or-n-p (format "解冻 %s (状态置回待评审)? "
                                  (plist-get row :ver))))
      (user-error "已取消解冻"))
    (my/org-project--baseline-status-set (plist-get row :ver)
                                          my/org-project--baseline-status-new)
    (message "基线 %s 已解冻（阶段 %s, 状态待评审）"
             (plist-get row :ver) stage)))

(defun my/org-project--baseline-record-add (file ver phase count)
  "在项目 FILE 的基线记录表追加一行: 版本 VER, 阶段 PHASE, 需求数 COUNT.
状态列填「待评审」; 表不存在时仅提示跳过."
  (set-buffer (org-capture-target-buffer file))
  (widen)
  (goto-char (point-min))
  (if (not (re-search-forward "^[ \t]*#\\+NAME:[ \t]+baseline-records[ \t]*$" nil t))
      (message "未找到基线记录表 baseline-records, 跳过登记")
    (forward-line 1)
    (when (org-at-table-p)
      (goto-char (org-table-end))
      (insert (format "| %s | %s | %s | %d | %s |  |  |\n"
                      ver (or phase "") (format-time-string "%Y-%m-%d") count
                      my/org-project--baseline-status-new))
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
  "需求基线全量重圈: 选全部有效需求, 追加记录表新行并按需询问冻结.
版本号全局递增(RB1, RB2...), 行落在 baseline-records (阶段=当前阶段,
状态=待评审); 同阶段未冻结旧行标「被取代」; 不生成任何标题.
未选中的需求保留历史 :BASELINE:, 不进入新版本."
  (interactive)
  (let ((file (my/org-capture-project-file)))
    (set-buffer (org-capture-target-buffer file))
    (widen)
    (let* ((phase (my/org-project--file-stage))
           (entries (my/org-project--req-entries))
           (cands (seq-filter #'my/org-project--req-active-p entries))
           (defver (format "RB%d" (1+ (my/org-project--baseline-max entries))))
           (ver (read-string (format "基线版本 (默认 %s): " defver) nil nil defver))
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
      (unless (string-match "\\`RB[0-9]+\\'" ver)
        (user-error "版本号需形如 RB<n>: %s" ver))
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
      ;; 2) 同阶段未冻结旧行标「被取代」
      (dolist (row (my/org-project--baseline-records-rows))
        (when (and (equal (plist-get row :phase) phase)
                   (not (my/org-project--baseline-frozen-p
                         (plist-get row :status))))
          (my/org-project--baseline-status-set
           (plist-get row :ver) my/org-project--baseline-status-superseded)))
      ;; 3) 登记新行(待评审), 确保阶段控制块与快照块就位并刷新动态块
      (my/org-project--baseline-record-add file ver phase n)
      (my/org-project--stage-baseline-ensure phase)
      (my/org-project--baseline-dblock-ensure file ver)
      (org-update-all-dblocks)
      ;; 4) 判据与评审齐备时询问直接冻结, 否则提示下一步
      (let* ((stats (my/org-project--stage-criteria phase))
             (reviews (my/org-project--stage-review-count phase ver))
             (unbased (my/org-project--stage-req-unbased phase)))
        (if (and (null (nth 2 stats)) (> reviews 0) (= unbased 0))
            (when (y-or-n-p (format "%s 判据已齐且有评审, 现在冻结? " ver))
              (my/org-project-baseline-freeze phase t))
          (message "已圈定 %s: %d 条需求(阶段 %s, 状态待评审); 判据 %d/%d, 评审 %d 条, 本阶段未入基线 %d 条 —— 用 C-c P g 冻结"
                   ver n phase (nth 0 stats) (nth 1 stats) reviews unbased))))))

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

(defun my/org-capture-refresh-dblocks ()
  "捕获完成后刷新目标缓冲区的 req-by-phase 动态块."
  (when (buffer-live-p my/org-capture--last-buffer)
    (with-current-buffer my/org-capture--last-buffer
      (ignore-errors (org-update-all-dblocks)))))


;; ---- 按键、视图与模板接入 ----------------------------------------------------

;; 项目管理: `C-c P s' 切换研制阶段(里程碑结算/更新状态/记日志).
;; `C-c P b' 需求基线圈定(批量标记 :BASELINE: 并登记 baseline-records, 状态=待评审).
;; 风险管理: `C-c P r' 评估确认(IDENT→MITIGATE), `C-c P x' 转议题,
;; `C-c P R' 刷新并查看风险台账; 存量文件升级用 M-x my/org-project-risk-upgrade.
;; 里程碑/基线: `C-c P g' 先选对象(本阶段基线/里程碑)再选动作(冻结/解冻/关闭/重开/
;; 看判据/跳评审表), 结算在 `C-c P s' 转段时进行.
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
(global-set-key (kbd "C-c P g") #'my/org-project-milestone-dispatch)
(define-key org-mode-map (kbd "C-c P g") #'my/org-project-milestone-dispatch)

;; 风险汇总里的来源缺失标记用 error face 标红。用 face 符号而非写死颜色,
;; 明暗主题切换后仍然有效 (与上面 face-override-spec 的处理同理)。
(font-lock-add-keywords 'org-mode '(("⚠ 未链接" 0 'error t)))

;; 风险/里程碑台账视图：高等级风险（含待评估/措施中/挂账分组）
(add-to-list 'org-agenda-custom-commands
             '("R" "风险台账" tags "RISK_LEVEL=\"高\""
      ((org-agenda-overriding-header "高等级风险台账")
       (org-super-agenda-groups
        '((:name "◎ 待评估" :todo "IDENT")
          (:name "◎ 措施中" :todo "MITIGATE")
          (:name "◎ 挂账" :property ("CONCLUSION" "挂账"))
          (:name "◎ 已了结" :anything t)))))
             t)

;; 捕获收尾：需求变更移出基线、刷新项目动态块、评审后提示冻结基线
;; 注意写全 -hook 后缀: Emacs 31 的 add-hook 不再自动补 -hook
(add-hook 'org-capture-after-finalize-hook #'my/org-capture-req-change-finalize t)
(add-hook 'org-capture-after-finalize-hook #'my/org-capture-refresh-dblocks t)
(add-hook 'org-capture-after-finalize-hook #'my/org-capture-review-finalize t)

;; 项目管理捕获模板（p 组）
(defconst my/org-project--capture-templates
  `(
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
      "| %u | %^{评审类型|内部评审|专家评审|用户评审} | %(my/org-capture--review-object) | %^{人员} | %^{结论|通过|有条件通过|不通过} | %^{遗留问题(议题链接,可空)} | %^{待办} |"
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
               "  :BASELINE: %(my/org-capture--baseline-prompt)\n"
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
    )
  "项目管理捕获模板（p 组），加载时插入 org-capture-templates。")

;; 插到「参考」组之前，保持 c → j → p → r → x 的菜单顺序
(let ((before (seq-take-while (lambda (tpl) (not (equal (car tpl) "r")))
                              org-capture-templates))
      (after (seq-drop-while (lambda (tpl) (not (equal (car tpl) "r")))
                             org-capture-templates)))
  (setq org-capture-templates
        (append before my/org-project--capture-templates after)))

(provide 'init-org-project)

;;; init-org-project.el ends here
