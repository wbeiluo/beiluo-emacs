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
;;   4. 需求与基线     —— 需求条目建档、全量重圈、基线记录表与快照动态块、继承导入
;;   5. 风险与议题     —— 三态风险台账、评估/转议题/汇总、议题落点
;;   6. 捕获与视图     —— p 组模板、评审/测试记录落点与按类型编号、需求-测试勾稽、agenda 风险与里程碑台账
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
注意 `buffer-base-buffer 对非 indirect 缓冲区返回 nil, 需回落到缓冲区本身."
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
  "生成项目内递增 ID: PREFIX 为类型前缀, 编号形如 代号_PREFIX-NNN.
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
  "生成「方案设计」阶段子树文本 (方案阶段专属结构), IDX 为阶段序号."
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
   "    版本条目用 ps 捕获, 状态流 DESIGN → DEVELOP → CONFIRMED → ARCHIVED.\n"
   "    修改项/测试项结合研制任务与测试/试验记录自动登记(ps 捕获时选, C-c P v 补/同步).\n"))

(defun my/org-project--qual-subtree (idx)
  "生成「状态鉴定」阶段子树文本 (鉴定阶段专属结构), IDX 为阶段序号."
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
  "生成全部阶段子树文本: CODE 为项目代号, 阶段顺序同 my/org-project--stages."
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
   "# - 软件状态条目(ps)记在各阶段「软件状态」节, 状态鉴定阶段记在「状态管理」,\n"
   "#   修改项/测试项由 ps 捕获或 C-c P v 从研制任务/测试记录自动登记;\n"
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
   "# [[file:projects/工程代号/配置项代号.org::#" product "][" name "]]\n\n"
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
   "# 自动引用, 刷新用 C-c C-x C-u. 追踪矩阵在评审前逐条勾稽: C-c P c 从\n"
    "# 测试记录「验证需求」列回填 :TEST-CASE: 并列缺口(测试证据随 pl 登记).\n"
    "# 继承旧项目需求用 C-c P i 从源项目已冻结基线导入(:SOURCE: 继承复用,\n"
    "# :INHERITED: 溯源链接直达源条目).\n\n"
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
   "# 各阶段测试/试验记录统一登记于此, 用 pl 捕获(阶段列自动填当前阶段,\n"
   "# 编号列按类型自动编号: 代号_UT-001, 供其他地方链接和引用);\n"
   "# 存量行补编号 C-c P n, 插入引用链接 C-c P l, 按编号跳转 C-c P t.\n"
    "# 「验证需求」列随 pl 捕获多选需求(需求↔测试关联的数据源), 勾稽回填\n"
    "# 需求 :TEST-CASE: 并列缺口用 C-c P c.\n"
   "# 评审前按阶段过滤核对覆盖率.\n\n"
   "** 单元测试记录\n\n"
   "    #+NAME: unit-test\n"
   "    | 阶段 | 用例编号 | 测试项 | 指标要求 | 实测结果 | 结论 | 验证需求 |\n"
   "    |------+----------+--------+----------+----------+------+------|\n\n"
   "** 功能测试记录\n\n"
   "    #+NAME: func-test\n"
   "    | 阶段 | 用例编号 | 测试项 | 指标要求 | 实测结果 | 结论 | 验证需求 |\n"
   "    |------+----------+--------+----------+----------+------+------|\n\n"
   "** 性能测试记录\n\n"
   "    #+NAME: perf-test\n"
   "    | 阶段 | 用例编号 | 测试项 | 指标要求 | 实测结果 | 结论 | 验证需求 |\n"
   "    |------+----------+--------+----------+----------+------+------|\n\n"
   "** 接口测试记录\n\n"
   "    #+NAME: iface-test\n"
   "    | 阶段 | 用例编号 | 测试项 | 指标要求 | 实测结果 | 结论 | 验证需求 |\n"
   "    |------+----------+--------+----------+----------+------+------|\n\n"
   "** 系统测试记录\n\n"
   "    #+NAME: sys-test\n"
   "    | 阶段 | 用例编号 | 日期 | 系统测试项 | 过程数据/现象 | 结论 | 验证需求 |\n"
   "    |------+----------+------+------------+---------------+------+------|\n\n"
   "** 联调记录\n\n"
   "    #+NAME: debug-record\n"
   "    | 阶段 | 记录编号 | 日期 | 联调项 | 过程数据/现象 | 结论 | 验证需求 |\n"
   "    |------+----------+------+--------+---------------+------+------|\n\n"
   "** 环境试验记录\n\n"
   "    #+NAME: env-record\n"
   "    | 阶段 | 记录编号 | 日期 | 试验项目 | 试验条件 | 结果/结论 | 验证需求 |\n"
   "    |------+----------+------+----------+----------+-----------+------|\n\n"
   "** 鉴定测试记录\n\n"
   "    #+NAME: qual-test\n"
   "    | 阶段 | 用例编号 | 测试项 | 指标要求 | 实测结果 | 结论 | 验证需求 |\n"
   "    |------+----------+--------+----------+----------+------+------|\n\n"
   "** 试验记录\n\n"
   "    #+NAME: trial-record\n"
   "    | 阶段 | 记录编号 | 日期 | 试验项目 | 过程数据/现象 | 结论 | 验证需求 |\n"
   "    |------+----------+------+----------+---------------+------+------|\n\n"
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

(defun my/org-project--overview-field (label)
  "返回概述信息表中 LABEL 行的内容 (找不到或为空返回 nil)."
  (let ((buf (my/org-capture--project-buffer)))
    (when buf
      (with-current-buffer buf
        (org-with-wide-buffer
         (goto-char (point-min))
         (when (re-search-forward "^\\* 概述[ \t]*$" nil t)
           (let ((end (save-excursion (org-end-of-subtree t t) (point))))
             (when (re-search-forward
                    (concat "^[ \t]*|[ \t]*" (regexp-quote label)
                            "[ \t]*|[ \t]*\\([^|\n]+?\\)[ \t]*|")
                    end t)
               (let ((v (string-trim (match-string 1))))
                 (unless (string= v "") v))))))))))

(defun my/org-project--config-name ()
  "返回概述信息表中的产品名称 (软件状态标题默认值)."
  (my/org-project--overview-field "产品名称"))

(defun my/org-project--product-code ()
  "返回概述信息表中的产品代号 (软件状态软件代号默认值)."
  (my/org-project--overview-field "产品代号"))

(defun my/org-capture--sw-prompt ()
  "提示 软件代号(默认产品代号) → 版本号 → 标题 → 版本简述.
标题默认 产品名称+软件/版本号; 返回标题文本作为条目标题,
其余值暂存供 CUSTOM_ID / 正文复用, 只问一次."
  (setq my/org-capture--sw-code-value
        (read-string "软件代号: "
                     (or (my/org-project--product-code)
                         (my/org-capture--code))))
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

(defvar my/org-capture--sw-id-value nil
  "捕获软件状态(ps)生成的 CUSTOM_ID, 供收尾对齐「修改项」「测试项」两表.")

(defun my/org-capture--sw-custom-id ()
  "生成软件状态 CUSTOM_ID: 软件代号/版本号."
  (setq my/org-capture--sw-id-value
        (format "%s/%s" (my/org-capture--sw-code) (my/org-capture--version))))

(defun my/org-capture--sw-change-rows ()
  "生成软件状态「修改项」表数据行 (供 ps 模板展开), 未选则为空.
非空时末尾多带一个换行: ps 模板里「测试项」表紧跟其后, org 把连续
以 | 开头的行当同一张表, 必须空行隔开才会分成两张表."
  (let* ((code (my/org-capture--code))
         (tasks (with-current-buffer (my/org-capture--project-buffer)
                  (my/org-project--sw-pick-tasks)))
         (rows (mapconcat (lambda (task)
                            (my/org-project--row-text
                             (my/org-project--task-change-row task code)))
                          tasks "\n")))
    (if (string-empty-p rows) rows (concat rows "\n"))))

(defun my/org-capture--sw-test-rows ()
  "生成软件状态「测试项」表数据行 (供 ps 模板展开), 未选则为空."
  (let* ((file (buffer-file-name (my/org-capture--project-buffer)))
         (records (with-current-buffer (my/org-capture--project-buffer)
                    (my/org-project--sw-pick-records))))
    (mapconcat (lambda (rec)
                 (my/org-project--row-text
                  (my/org-project--record-test-row rec file)))
               records "\n")))

(defun my/org-capture-sw-finalize ()
  "捕获 ps 落点后: 对齐新条目的「修改项」「测试项」两表, 中止捕获不处理."
  (let ((id my/org-capture--sw-id-value)
        (buf my/org-capture--last-buffer))
    (setq my/org-capture--sw-id-value nil)
    (when (and id
               (not (and (boundp 'org-note-abort) org-note-abort))
               (buffer-live-p buf))
      (with-current-buffer buf
        (org-with-wide-buffer
         (save-excursion
          (goto-char (point-min))
          (when (re-search-forward
                 (concat "^[ \t]*:CUSTOM_ID:[ \t]+"
                         (regexp-quote id) "[ \t]*$")
                 nil t)
            (org-back-to-heading t)
            (let ((beg (line-beginning-position))
                  (end (save-excursion (org-end-of-subtree t t) (point))))
              (dolist (tbl (my/org-project--sw-table-regions beg end))
                (goto-char (nth 1 tbl))
                (org-table-align))))))))))

(defun my/org-capture--trigger ()
  "提示选择软件状态的触发来源."
  (setq my/org-capture--trigger-value
        (completing-read "触发来源: "
                         '("阶段任务" "新研" "需求变更" "设计改进"
                           "问题归零" "硬件适配" "其他")
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
以 CUSTOM_ID 含 / 识别 (旧式带 R 前缀与新式不带的都算), 兼容旧条目的
:VERSION: 属性."
  (let ((buf (my/org-capture--project-buffer))
        entries)
    (when buf
      (with-current-buffer buf
        (org-with-wide-buffer
         (goto-char (point-min))
         (while (re-search-forward "^\\*+ " nil t)
           (let ((id (or (org-entry-get (point) "CUSTOM_ID") ""))
                 (legacy (org-entry-get (point) "VERSION")))
             (when (or (string-match-p "/" id) legacy)
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
  '(("单元测试记录" "unit-test" "UT" "用例编号" ("测试项" "指标要求" "实测结果" "结论"))
    ("功能测试记录" "func-test" "FT" "用例编号" ("测试项" "指标要求" "实测结果" "结论"))
    ("性能测试记录" "perf-test" "PT" "用例编号" ("测试项" "指标要求" "实测结果" "结论"))
    ("接口测试记录" "iface-test" "IT" "用例编号" ("测试项" "指标要求" "实测结果" "结论"))
    ("系统测试记录" "sys-test" "ST" "用例编号" ("系统测试项" nil "过程数据/现象" "结论"))
    ("联调记录" "debug-record" "DBG" "记录编号" ("联调项" nil "过程数据/现象" "结论"))
    ("环境试验记录" "env-record" "ENV" "记录编号" ("试验项目" "试验条件" "结果/结论" nil))
    ("鉴定测试记录" "qual-test" "QT" "用例编号" ("测试项" "指标要求" "实测结果" "结论"))
    ("试验记录" "trial-record" "TR" "记录编号" ("试验项目" nil "过程数据/现象" "结论")))
  "测试/试验记录表: (显示名 #+NAME 标识 ID 前缀 编号列列名 测试项列映射).
记录 ID 形如 代号_FT-001, 按类型分段编号, 供其他地方链接和引用;
测试类表编号列叫「用例编号」, 记录类表叫「记录编号」, 都在第 2 列;
列映射是记录列名到软件状态「测试项」表 (测试项/指标要求/实测结果/结论) 的
对应 (nil 表示留空), 供 ps 捕获与登记命令 (C-c P v) 自动登记.")

(defun my/org-capture--record-spec (kind)
  "返回记录表 KIND 的条目 (显示名 KIND ID前缀 编号列列名 测试项列映射), 未知返回 nil."
  (seq-find (lambda (spec) (equal (nth 1 spec) kind)) my/org-capture--record-tables))

(defvar my/org-capture--record-kind nil
  "捕获记录表行(pl)时选择的记录表标识.")

(defun my/org-capture--record-id ()
  "生成记录 ID (供模板展开): 代号_<类型前缀>-NNN, 按类型分段递增."
  (my/org-capture--next-id
   (nth 2 (my/org-capture--record-spec my/org-capture--record-kind))))

(defun my/org-capture--record-req-links ()
  "提示多选本记录验证的需求条目(可空), 返回「验证需求」列文本 (供模板展开).
选中的需求写成 [[#ID][短ID]] 链接, 自由输入(如旧手工编号)按原样保留.
需求↔测试关联以此列为数据源, 勾稽命令 (C-c P c) 反向回填需求 :TEST-CASE:."
  (let* ((buf (my/org-capture--project-buffer))
         (entries (and buf (with-current-buffer buf
                             (seq-filter (lambda (e) (plist-get e :phase))
                                         (my/org-project--req-entries)))))
         (choices (mapcar (lambda (e)
                            (format "%s %s" (plist-get e :id) (plist-get e :title)))
                          entries))
         (sel (completing-read-multiple "验证需求(可空, 逗号分隔多选): " choices)))
    (mapconcat
     (lambda (s)
       (let* ((id (car (split-string (string-trim s) " ")))
              (e (seq-find (lambda (x) (equal (plist-get x :id) id)) entries)))
         (if e (format "[[#%s][%s]]" id
                       (my/org-project--req-id-short id (my/org-capture--code)))
           s)))
     (delete "" sel) " ")))

(defun my/org-capture--record-row ()
  "按所选记录表生成一行表格 (首列为当前阶段, 第 2 列为自动编号, 末列为验证需求, 供模板展开)."
  (let ((stage (my/org-capture--current-stage))
        (date (format-time-string "[%Y-%m-%d %a]"))
        (id (my/org-capture--record-id)))
    (pcase my/org-capture--record-kind
      ((or "unit-test" "func-test" "perf-test" "iface-test" "qual-test")
       (format "| %s | %s | %s | %s | %s | %s | %s |"
               stage id (read-string "测试项: ")
               (read-string "指标要求: ") (read-string "实测结果: ")
               (read-string "结论: ")
               (my/org-capture--record-req-links)))
      ("sys-test"
       (format "| %s | %s | %s | %s | %s | %s | %s |"
               stage id date (read-string "系统测试项: ")
               (read-string "过程数据/现象: ") (read-string "结论: ")
               (my/org-capture--record-req-links)))
      ("debug-record"
       (format "| %s | %s | %s | %s | %s | %s | %s |"
               stage id date (read-string "联调项: ")
               (read-string "过程数据/现象: ") (read-string "结论: ")
               (my/org-capture--record-req-links)))
      ("env-record"
       (format "| %s | %s | %s | %s | %s | %s | %s |"
               stage id date (read-string "试验项目: ") (read-string "试验条件: ")
               (read-string "结果/结论: ")
               (my/org-capture--record-req-links)))
      (_
       (format "| %s | %s | %s | %s | %s | %s | %s |"
               stage id date (read-string "试验项目: ")
               (read-string "过程数据/现象: ") (read-string "结论: ")
               (my/org-capture--record-req-links))))))

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
  "测试/试验记录(pl): 统一章「测试与试验记录」的指定表, 阶段列自动填当前阶段.
编号列(用例编号/记录编号)缺失的存量表自动补列, 记录 ID 由模板自动生成."
  (let* ((file (my/org-capture-project-file))
         (label (completing-read "记录表: "
                                 (mapcar #'car my/org-capture--record-tables) nil t)))
    (setq my/org-capture--record-kind (nth 1 (assoc label my/org-capture--record-tables)))
    (my/org-capture--goto-named-table
     file '("测试与试验记录") my/org-capture--record-kind)
    (save-excursion
      (forward-line 1)
      (my/org-project--record-ensure-id-column
       (line-beginning-position)
       (nth 3 (my/org-capture--record-spec my/org-capture--record-kind))))))

(defvar my/org-capture--review-stage nil
  "本次 pe 捕获选中的基线评审阶段 (评审对象为「本阶段基线」时非 nil, 否则为 nil).
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
  "捕获 pe 落表后: 基线评审且最新基线判据已满足时, 询问是否冻结."
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

(defconst my/org-project--test-reverify-mark "⚠ 待重新验证"
  "需求变更后前置到 :TEST-CASE: 的待重新验证标记 (勾稽报告提示, 复验通过后手工删除).")

(defun my/org-capture-req-change-finalize ()
  "需求变更(pc)落表后联动: 将对应需求移出基线并置回 DESIGN.
已有测试证据的在 :TEST-CASE: 前置 ⚠ 待重新验证 标记(勾稽提示,
复验通过后手工删除), 中止捕获(org-note-abort)或未选择需求时不联动."
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
                 (let* ((tc (org-entry-get (point) "TEST-CASE"))
                        (marked (and (org-string-nw-p tc)
                                     (not (string-match-p
                                           (regexp-quote my/org-project--test-reverify-mark)
                                           tc))))
                        (st (org-get-todo-state)))
                   ;; 变更后原测试证据可能失效, 标记待重新验证(勾稽提示, 复验后手工删)
                   (when marked
                     (org-entry-put (point) "TEST-CASE"
                                    (concat my/org-project--test-reverify-mark " " tc)))
                   ;; org-todo 切到同态会取消关键字, 仅在偏离 DESIGN 时归位
                   (when (and st (not (string= st "DESIGN"))) (org-todo "DESIGN"))
                   ;; 钩子顺序上动态块刷新可能先于本联动, 故再刷一次
                   (ignore-errors (org-update-all-dblocks))
                   (message "需求 %s 已移出基线, 状态置回 DESIGN%s" id
                            (if marked " (测试证据标记待重新验证)" ""))))))))))))

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

;; ---- 测试/试验记录 Test records ----
;; 记录 ID 按类型分段编号(代号_FT-001): pl 捕获自动生成, 存量行用 C-c P n 补号
;; (只填空编号, 手工编号保留). 「验证需求」列是需求↔测试关联的唯一数据源
;; (pl 捕获时多选需求), C-c P c 勾稽反向回填需求 :TEST-CASE: 并列缺口;
;; 任意处 C-c P l 插入搜索链接, C-c P t 按编号跳转到记录行.

(defun my/org-project--row-cells (line)
  "拆分表格行 LINE 为单元格文本列表 (去掉首尾竖线与空白)."
  (let ((s (string-trim line)))
    (mapcar #'string-trim (split-string (substring s 1 -1) "|"))))

(defun my/org-project--record-table-regions ()
  "扫描「测试与试验记录」章, 返回各记录表的 (KIND BEG END) 列表.
BEG 为表首行行首, END 为表尾行下一行行首; 该章或表不存在时相应缺省."
  (let (regions)
    (org-with-wide-buffer
     (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^\\* 测试与试验记录[ \t]*$" nil t)
        (let ((cbeg (line-beginning-position))
              (end (save-excursion (org-end-of-subtree t t) (point))))
          (dolist (spec my/org-capture--record-tables)
            (goto-char cbeg)
            (when (re-search-forward
                   (concat "^[ \t]*#\\+NAME:[ \t]+"
                           (regexp-quote (nth 1 spec)) "[ \t]*$")
                   end t)
              (forward-line 1)
              (when (looking-at "[ \t]*|")
                (let ((beg (line-beginning-position)))
                  (while (looking-at "[ \t]*|") (forward-line 1))
                  (push (list (nth 1 spec) beg (line-beginning-position))
                        regions)))))))))
    (nreverse regions)))

(defun my/org-project--record-table-map (beg end)
  "解析 BEG..END 的记录表, 返回 (HEADER . ROWS).
HEADER 为表头单元格列表, ROWS 为 (ROW POS CELLS) 列表:
ROW 为 org-table 行号(不含分隔行, 表头为 1), POS 为行首位置."
  (let ((n 0) header rows)
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (unless (looking-at org-table-hline-regexp)
          (setq n (1+ n))
          (let ((cells (my/org-project--row-cells
                        (buffer-substring (line-beginning-position)
                                          (line-end-position)))))
            (if (= n 1)
                (setq header cells)
              (push (list n (line-beginning-position) cells) rows))))
        (forward-line 1)))
    (cons header (nreverse rows))))

(defun my/org-project--record-ensure-id-column (beg column)
  "确保 BEG 起的记录表含编号列 COLUMN, 无则插到第 2 列, 返回其列号 (1 起)."
  (save-excursion
    (goto-char beg)
    (let ((idx (seq-position (my/org-project--row-cells
                              (buffer-substring (line-beginning-position)
                                                (line-end-position)))
                             column)))
      (if idx
          (1+ idx)
        (goto-char beg)
        (org-table-goto-column 2)
        (org-table-insert-column)
        (org-table-put 1 2 column)
        (org-table-align)
        2))))

(defun my/org-project--record-fill-ids (beg end kind)
  "给 BEG..END 表内编号为空的记录行补记录 ID (KIND 类型), 返回补号数量.
表缺编号列时先补列; 已有编号(含手工编号)保留."
  (let* ((spec (my/org-capture--record-spec kind))
         (column (my/org-project--record-ensure-id-column beg (nth 3 spec)))
         (rows (cdr (my/org-project--record-table-map beg end)))
         (n 0))
    (save-excursion
      (goto-char beg)
      (dolist (row rows)
        (when (string-blank-p (or (nth (1- column) (nth 2 row)) ""))
          (org-table-put (car row) column
                         (my/org-capture--next-id (nth 2 spec)))
          (setq n (1+ n)))))
    (when (> n 0)
      (goto-char beg)
      (org-table-align))
    n))

(defun my/org-project--record-entries ()
  "扫描「测试与试验记录」章, 返回已编号记录的 plist 列表.
每项含 :id :kind :title :pos :header :cells; 编号为空的行跳过.
:title 取该类记录的「测试项」列映射首列文本, 找不到则为空."
  (let (entries)
    (dolist (region (my/org-project--record-table-regions))
      (let* ((kind (nth 0 region))
             (map (my/org-project--record-table-map (nth 1 region) (nth 2 region)))
             (header (car map))
             (id-col (or (seq-position header "用例编号")
                         (seq-position header "记录编号")))
             (title-col (let ((name (car (nth 4 (my/org-capture--record-spec kind)))))
                          (and name (seq-position header name)))))
        (when id-col
          (dolist (row (cdr map))
            (let ((cells (nth 2 row))
                  (id (string-trim (or (nth id-col (nth 2 row)) ""))))
              (unless (string-empty-p id)
                (push (list :id id
                            :kind kind
                            :title (string-trim
                                    (or (and title-col (nth title-col cells))
                                        ""))
                            :pos (nth 1 row)
                            :header header
                            :cells cells)
                      entries)))))))
    (nreverse entries)))

(defun my/org-project--record-sw-fields (record)
  "按列映射返回记录 RECORD 对应软件状态「测试项」表的 4 格文本.
4 格依次为测试项/指标要求/实测结果/结论, 无对应列或缺格为空串."
  (let ((map (nth 4 (my/org-capture--record-spec (plist-get record :kind))))
        (header (plist-get record :header))
        (cells (plist-get record :cells)))
    (mapcar (lambda (name)
              (let ((idx (and name (seq-position header name))))
                (if idx (or (nth idx cells) "") "")))
            map)))

(defun my/org-project--record-link (id file &optional from)
  "生成指向 FILE 中记录 ID 的搜索链接文本: [[file:相对路径::ID][短ID]].
相对路径以 FROM 文件 (缺省当前缓冲区文件) 所在目录为基准."
  (format "[[file:%s::%s][%s]]"
          (file-relative-name file (file-name-directory
                                    (or from (buffer-file-name) file)))
          id
          (my/org-project--id-short id (file-name-base file))))

(defun my/org-project--record-candidates ()
  "返回全部项目文件的已编号记录候选: ((LABEL . PLIST) ...).
LABEL 形如 「400_UT-001 电压稳定度 (Test/400)」, PLIST 含 :file."
  (let (cands)
    (dolist (f (my/org-project--files))
      (with-current-buffer (find-file-noselect (cdr f))
        (dolist (e (my/org-project--record-entries))
          (push (cons (format "%s %s (%s)" (plist-get e :id)
                              (plist-get e :title) (car f))
                      (append e (list :file (cdr f))))
                cands))))
    (nreverse cands)))

(defun my/org-project--record-select ()
  "提示选择一条测试/试验记录, 返回其 plist (:id :kind :title :pos :file)."
  (let ((cands (my/org-project--record-candidates)))
    (unless cands
      (user-error "没有已编号的测试/试验记录 (pl 捕获自动编号, 存量用 C-c P n 补号)"))
    (cdr (assoc (completing-read "测试/试验记录: " (mapcar #'car cands) nil t)
                cands))))

(defun my/org-project-record-number ()
  "为「测试与试验记录」章的记录行按类型补编号 (C-c P n), 幂等.
编号列缺失的表补列(插到第 2 列), 编号为空的行按表类型补号;
已有编号(含手工编号)保留, 新记录的编号由 pl 捕获自动生成."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "请在项目 org 文件中执行"))
  (let ((total 0) (tables 0))
    ;; 逆序处理: 处理靠后的表会移动其后内容的位置
    (dolist (region (reverse (my/org-project--record-table-regions)))
      (let ((n (my/org-project--record-fill-ids (nth 1 region) (nth 2 region)
                                               (nth 0 region))))
        (when (> n 0)
          (setq total (+ total n)
                tables (1+ tables)))))
    (message (if (> total 0)
                 (format "已补编号 %d 条 (%d 张表)" total tables)
               "所有记录行都已有编号"))))

(defun my/org-project-record-goto ()
  "按记录编号跳转到「测试与试验记录」章对应记录行 (C-c P t)."
  (interactive)
  (let ((rec (my/org-project--record-select)))
    (pop-to-buffer (find-file-noselect (plist-get rec :file)))
    (widen)
    (goto-char (plist-get rec :pos))
    (org-reveal)
    (message "记录 %s %s" (plist-get rec :id) (plist-get rec :title))))

(defun my/org-project-record-link-insert ()
  "选择测试/试验记录, 在 point 插入指向记录行的搜索链接 (C-c P l)."
  (interactive)
  (let ((rec (my/org-project--record-select)))
    (insert (my/org-project--record-link (plist-get rec :id)
                                        (plist-get rec :file)))))

(defun my/org-project--record-link-search (search)
  "处理器 (org-execute-file-search-functions): 按记录编号 SEARCH 直达记录行.
软件状态「测试项」表的记录链接([[file:..::编号][..]])常与目标同文件且链接
在前(记录章在文件后部), org 文本搜索会先命中链接自身的路径部分而落回原地;
这里按编号精确匹配「测试与试验记录」章的记录行, 命中即跳转并返回非 nil.
无匹配(含非项目文件)返回 nil, 交给 org 常规搜索."
  (when (derived-mode-p 'org-mode)
    (let* ((id (string-trim search))
           (pos (org-with-wide-buffer
                 (save-excursion
                   (plist-get (seq-find (lambda (e) (string= (plist-get e :id) id))
                                        (my/org-project--record-entries))
                              :pos)))))
      (when pos
        (unless (and (<= (point-min) pos) (>= (point-max) pos))
          (widen))
        (goto-char pos)
        t))))

;; ---- 软件状态 Software state ----
;; 版本条目(ps)的「修改项」「测试项」两表结合研制任务(pt)与测试/试验记录(pl)
;; 自动登记. 归属即引用: 任务/记录被某版本的表行(带链接)引用即视为归属该版本,
;; 其他版本不再提供该候选. 修改项行自动带任务链接+标题, 原因/影响范围/关联议题
;; 留空手补; 测试项行按列映射带出记录数据. ps 捕获时选择生成, C-c P v 随时补行
;; 并同步最新数据(修改项表手写列不动), 定版前跑一遍即得最新状态.

(defun my/org-project--task-entries ()
  "扫描研制任务 (pt 捕获的带 CUSTOM_ID 条目), 返回 plist 列表 (:id :title :todo)."
  (let (entries)
    (org-with-wide-buffer
     (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\*+ " nil t)
        (let ((id (org-entry-get (point) "CUSTOM_ID")))
          (when (and id (string-match-p "_TASK-[0-9]+\\'" id))
            (push (list :id id
                        :title (org-get-heading t t t t)
                        :todo (my/org-project--state))
                  entries))))))
    (nreverse entries)))

(defun my/org-project--sw-entry-regions ()
  "扫描软件状态条目, 返回 (ID TITLE BEG END) 列表 (含旧式 :VERSION: 条目).
BEG/END 为条目标题行首与子树末尾."
  (let (entries)
    (org-with-wide-buffer
     (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\*+ " nil t)
        (let ((id (or (org-entry-get (point) "CUSTOM_ID") ""))
              (legacy (org-entry-get (point) "VERSION")))
          (when (or (string-match-p "/" id) legacy)
            (push (list id (org-get-heading t t t t)
                        (line-beginning-position)
                        (save-excursion (org-end-of-subtree t t) (point)))
                  entries))))))
    (nreverse entries)))

(defun my/org-project--sw-table-regions (beg end)
  "在 BEG..END 内按表头识别软件状态的两表, 返回 ((KIND BEG END) ...).
KIND 为 changes(修改项表)或 tests(测试项表), BEG/END 约定同记录表区域."
  (let (regions)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward "^[ \t]*|" end t)
        (goto-char (line-beginning-position))
        (let* ((tbeg (point))
               (header (my/org-project--row-cells
                        (buffer-substring (line-beginning-position)
                                          (line-end-position)))))
          (while (and (< (point) end) (looking-at "[ \t]*|")) (forward-line 1))
          (let ((tend (line-beginning-position)))
            (cond
             ((equal (car header) "修改项")
              (push (list 'changes tbeg tend) regions))
             ((equal (car header) "测试项")
              (push (list 'tests tbeg tend) regions)))))))
    (nreverse regions)))

(defun my/org-project--link-target (cell)
  "返回单元格文本 CELL 中第一个 org 链接的跳转目标 (去掉 # 前缀), 无链接返回 nil.
只识别 [[#id][..]] 与 [[file:..::id][..]] 两种带跳转目标的链接."
  (when (string-match
         "\\[\\[\\(?:#\\|[^][]+::\\)\\([^][]+\\)\\]\\[[^][]*\\]\\]" cell)
    (match-string 1 cell)))

(defun my/org-project--sw-referenced-ids ()
  "返回软件状态条目两表已引用的 ID 列表 (行内链接目标).
归属判定用: 任务/记录被某版本的表引用即视为归属该版本."
  (let (ids)
    (dolist (entry (my/org-project--sw-entry-regions))
      (dolist (tbl (my/org-project--sw-table-regions (nth 2 entry) (nth 3 entry)))
        (dolist (row (cdr (my/org-project--record-table-map (nth 1 tbl) (nth 2 tbl))))
          (dolist (cell (nth 2 row))
            (let ((target (my/org-project--link-target cell)))
              (when target (push target ids)))))))
    (delete-dups ids)))

(defun my/org-project--task-change-row (task code)
  "生成软件状态「修改项」表一行的单元格列表: TASK 为研制任务条目, CODE 为项目代号; 首格为任务链接+标题, 人工列留空."
  (list (format "[[#%s][%s]] %s"
                (plist-get task :id)
                (my/org-project--id-short (plist-get task :id) code)
                (plist-get task :title))
        "" "" ""))

(defun my/org-project--record-test-row (record file)
  "生成软件状态「测试项」表一行的单元格列表: RECORD 为记录条目, FILE 为项目文件; 首格为记录链接+测试项, 其余取记录数据."
  (let ((fields (my/org-project--record-sw-fields record)))
    (cons (format "%s %s"
                  (my/org-project--record-link (plist-get record :id) file file)
                  (nth 0 fields))
          (cdr fields))))

(defun my/org-project--row-text (cells)
  "把单元格列表 CELLS 组装成表格行文本 (2 空格缩进)."
  (concat "  | " (mapconcat #'identity cells " | ") " |"))

(defun my/org-project--sw-pick-tasks ()
  "多选未归属的研制任务 (逗号分隔多选, 空=不选), 返回任务 plist 列表."
  (let* ((taken (my/org-project--sw-referenced-ids))
         (cands (seq-filter (lambda (e) (not (member (plist-get e :id) taken)))
                            (my/org-project--task-entries)))
         (label (lambda (e)
                  (format "%s %s [%s]" (plist-get e :id) (plist-get e :title)
                          (or (plist-get e :todo) ""))))
         (sel (completing-read-multiple "修改项-研制任务(可空, 逗号分隔多选): "
                                        (mapcar label cands))))
    (delq nil (mapcar (lambda (s)
                        (seq-find (lambda (e) (string= s (funcall label e)))
                                  cands))
                      sel))))

(defun my/org-project--sw-pick-records ()
  "多选未归属的测试/试验记录 (逗号分隔多选, 空=不选), 返回记录 plist 列表."
  (let* ((taken (my/org-project--sw-referenced-ids))
         (cands (seq-filter (lambda (e) (not (member (plist-get e :id) taken)))
                            (my/org-project--record-entries)))
         (label (lambda (e)
                  (format "%s %s (%s)" (plist-get e :id) (plist-get e :title)
                          (car (my/org-capture--record-spec (plist-get e :kind))))))
         (sel (completing-read-multiple "测试项-测试/试验记录(可空, 逗号分隔多选): "
                                        (mapcar label cands))))
    (delq nil (mapcar (lambda (s)
                        (seq-find (lambda (e) (string= s (funcall label e)))
                                  cands))
                      sel))))

(defun my/org-project--rewrite-row (pos cells)
  "把 POS 处表格行替换为 CELLS 组成的行 (保留缩进); 调用方负责表格对齐."
  (goto-char pos)
  (let ((indent (if (looking-at "[ \t]*") (match-string 0) "")))
    (delete-region (line-beginning-position) (line-end-position))
    (insert indent "| " (mapconcat #'identity cells " | ") " |")))

(defun my/org-project--sw-sync-rows (beg end id-rows keep)
  "用 ID-ROWS 同步 BEG..END 表内引用了 ID 的行, 返回同步行数.
ID-ROWS 为 ((ID CELLS) ...), 按行首格的链接目标匹配; KEEP 非 nil 时只重写
首格 (修改项表的原因/影响范围/关联议题是手写列, 不被同步覆盖).
从后往前改写, 避免行位置漂移."
  (let ((n 0))
    (dolist (row (reverse (cdr (my/org-project--record-table-map beg end))))
      (let* ((cells (nth 2 row))
             (id (my/org-project--link-target (car cells)))
             (new (cdr (assoc id id-rows))))
        (when new
          (my/org-project--rewrite-row
           (nth 1 row) (if keep (cons (car new) (cdr cells)) new))
          (setq n (1+ n)))))
    n))

(defun my/org-project--sw-append-rows (end rows)
  "在 END 处的表末追加 ROWS (单元格列表), 返回追加行数."
  (if (null rows)
      0
    (save-excursion
      (goto-char end)
      (forward-line -1)
      (end-of-line)
      (dolist (cells rows)
        (insert "\n" (my/org-project--row-text cells)))
      (length rows))))

(defun my/org-project--sw-entry-at-point ()
  "返回 point 所在软件状态条目 (ID TITLE BEG END), 非软件状态条目返回 nil."
  (save-excursion
    (when (ignore-errors (progn (org-back-to-heading t) t))
      (let ((id (or (org-entry-get (point) "CUSTOM_ID") ""))
            (legacy (org-entry-get (point) "VERSION")))
        (when (or (string-match-p "/" id) legacy)
          (list id (org-get-heading t t t t)
                (line-beginning-position)
                (save-excursion (org-end-of-subtree t t) (point))))))))

(defun my/org-project--sw-entry-select ()
  "提示选择软件状态条目, 返回 (ID TITLE BEG END)."
  (let* ((entries (my/org-project--sw-entry-regions))
         (label (lambda (e) (format "%s %s" (nth 0 e) (nth 1 e))))
         (choice (completing-read "软件状态条目: "
                                  (mapcar label entries) nil t)))
    (seq-find (lambda (e) (string= choice (funcall label e))) entries)))

(defun my/org-project-version-register ()
  "为软件状态条目登记修改项/测试项 (C-c P v).
多选研制任务与测试/试验记录生成两表行(行内链接引用); 已归属其他版本的
不再候选, 表内已有行同步最新数据(修改项表的原因/影响范围/关联议题留空
手补, 不被覆盖), ps 捕获时也走同一套选择, 未选的随时在这里补."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "请在项目 org 文件中执行"))
  (let* ((entry (or (my/org-project--sw-entry-at-point)
                    (my/org-project--sw-entry-select)))
         (mbeg (copy-marker (nth 2 entry)))
         (mend (copy-marker (nth 3 entry)))
         (file (buffer-file-name))
         (code (file-name-base file))
         (tasks (my/org-project--sw-pick-tasks))
         (records (my/org-project--sw-pick-records))
         (task-rows (mapcar (lambda (task)
                              (cons (plist-get task :id)
                                    (my/org-project--task-change-row task code)))
                            (my/org-project--task-entries)))
         (rec-rows (mapcar (lambda (rec)
                             (cons (plist-get rec :id)
                                   (my/org-project--record-test-row rec file)))
                           (my/org-project--record-entries)))
         (new-tasks (mapcar (lambda (task)
                              (cdr (assoc (plist-get task :id) task-rows)))
                            tasks))
         (new-records (mapcar (lambda (rec)
                                (cdr (assoc (plist-get rec :id) rec-rows)))
                              records))
         (synced 0) (added 0))
    (dolist (kind '(changes tests))
      (let* ((all-rows (if (eq kind 'changes) task-rows rec-rows))
             (new-rows (if (eq kind 'changes) new-tasks new-records))
             (tbl (seq-find (lambda (r) (eq (nth 0 r) kind))
                            (my/org-project--sw-table-regions mbeg mend))))
        (unless tbl
          (user-error "软件状态条目缺「修改项」或「测试项」表"))
        (setq synced (+ synced (my/org-project--sw-sync-rows
                                (nth 1 tbl) (nth 2 tbl) all-rows
                                (eq kind 'changes))))
        ;; 同步改写会移动表末位置, 重新定位后再追加
        (setq tbl (seq-find (lambda (r) (eq (nth 0 r) kind))
                            (my/org-project--sw-table-regions mbeg mend)))
        (setq added (+ added (my/org-project--sw-append-rows (nth 2 tbl) new-rows)))))
    (dolist (tbl (my/org-project--sw-table-regions mbeg mend))
      (save-excursion
        (goto-char (nth 1 tbl))
        (org-table-align)))
    (message "软件状态登记: 新增 %d 行, 同步 %d 行" added synced)))

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
  "返回 FROM→TO 转段的里程碑结算概况与本阶段基线冻结状态 (供切换日志)."
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
  "在「日志」章插入 FROM→TO 阶段切换记录, REASON 为转段原因, RESULT 为转段结论; FORCED 非 nil 表示强制转段."
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
每项含 :pos :id :title :type :phase :baseline :design :module :test-case
:inherited :todo; 只收同时带 CUSTOM_ID 与 :PHASE: 的条目 (pn 捕获的需求;
小节标题无 CUSTOM_ID, 里程碑条目在「里程碑」章, 均自动排除)."
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
                             :inherited (org-entry-get (point) "INHERITED")
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

;; ---- 需求-测试勾稽 Requirement-test reconciliation ----
;; 需求↔测试关联的唯一数据源是记录行「验证需求」列的需求链接(pl 捕获多选);
;; 需求的 :TEST-CASE: 是派生视图, 由 C-c P c 从记录行反向汇总回填(旧链接与
;; 非链接文本保留, 并集去重). pc 需求变更会给已有证据前置 ⚠ 待重新验证 标记,
;; 复验通过后手工删除; 单向关联(需求侧链接了记录但记录行未回指)由勾稽提示.

(defun my/org-project--hash-link-targets (text)
  "返回 TEXT 中 [[#ID][..]] 形式链接的目标 ID 列表 (保持顺序)."
  (let ((re "\\[\\[#\\([^][]+\\)\\]\\[[^][]*\\]\\]") ids)
    (save-match-data
      (let ((start 0))
        (while (string-match re text start)
          (push (match-string 1 text) ids)
          (setq start (match-end 0)))))
    (nreverse ids)))

(defun my/org-project--req-test-links (text)
  "返回 TEXT 中所有 [[..][..]] 链接的原文列表 (保持顺序, 供并集保留旧引用)."
  (let (links)
    (save-match-data
      (let ((start 0))
        (while (string-match org-link-bracket-re text start)
          (push (match-string 0 text) links)
          (setq start (match-end 0)))))
    (nreverse links)))

(defun my/org-project--req-test-merge (old new-links)
  "合并 :TEST-CASE: 旧值 OLD 与记录行汇总的记录链接 NEW-LINKS, 返回新值.
链接取并集(旧链接保序在前, 新链接按记录顺序补后), 非链接文本(⚠ 待重新验证
标记与旧手工编号)原样保留; 需求侧单向旧链接不自动清除, 勾稽报告提示补
记录行回指后手工清理."
  (let* ((old-links (my/org-project--req-test-links (or old "")))
         (text (string-trim
                (replace-regexp-in-string org-link-bracket-re "" (or old ""))))
         (links (delete-dups (append old-links new-links))))
    (mapconcat #'identity
               (delq nil (append (when (org-string-nw-p text) (list text)) links))
               " ")))

(defun my/org-project-req-test-sync ()
  "勾稽需求↔测试/试验记录并回填 :TEST-CASE: (C-c P c).
从记录行「验证需求」列反向汇总每条需求被哪些记录验证, 回填其 :TEST-CASE:
 (并集去重, 逆序改写避免条目位置漂移), 并在 *需求测试勾稽* 列出缺口:
未覆盖需求、变更后待重新验证、单向关联、未关联需求的记录."
  (interactive)
  (unless (and (derived-mode-p 'org-mode) (buffer-file-name))
    (user-error "请在项目 org 文件中执行"))
  (let* ((file (buffer-file-name))
         (proj (or (my/org-project--config-name) (file-name-base file)))
         (reqs (my/org-project--req-entries))
         (records (my/org-project--record-entries))
         (req-ids (mapcar (lambda (e) (plist-get e :id)) reqs))
         (rec-ids (mapcar (lambda (e) (plist-get e :id)) records))
         (rec-refs (make-hash-table :test #'equal))   ; 记录ID -> 它引用的需求ID
         (ref-map (make-hash-table :test #'equal))    ; 需求ID -> 引用它的记录
         (inherited (seq-filter (lambda (e) (org-string-nw-p (plist-get e :inherited)))
                                reqs))
         (unlinked nil) (missing nil) (reverify nil) (oneway nil)
         (filled 0))
    (dolist (rec records)
      (let ((ids (delete-dups
                  (seq-filter (lambda (id) (member id req-ids))
                              (apply #'append
                                     (mapcar #'my/org-project--hash-link-targets
                                             (plist-get rec :cells)))))))
        (puthash (plist-get rec :id) ids rec-refs)
        (if (null ids)
            (push rec unlinked)
          (dolist (id ids)
            (puthash id (cons rec (gethash id ref-map)) ref-map)))))
    ;; 逆序回填: 改写靠后条目不影响靠前条目的 :pos
    (dolist (e (reverse reqs))
      (let* ((id (plist-get e :id))
             (old (or (plist-get e :test-case) ""))
             (new-links (mapcar
                         (lambda (r)
                           (my/org-project--record-link (plist-get r :id) file file))
                         (reverse (gethash id ref-map))))
             (merged (my/org-project--req-test-merge old new-links)))
        (unless (string= merged old)
          (save-excursion
            (goto-char (plist-get e :pos))
            (org-entry-put nil "TEST-CASE" merged))
          (setq filled (1+ filled)))
        (cond ((not (org-string-nw-p merged)) (push e missing))
              ((string-match-p (regexp-quote my/org-project--test-reverify-mark) merged)
               (push e reverify)))
        ;; 单向关联: 需求侧旧链接指向记录, 但记录行「验证需求」列未回指本需求
        (dolist (link (my/org-project--req-test-links old))
          (let ((target (my/org-project--link-target link)))
            (when (and target (member target rec-ids)
                       (not (member id (gethash target rec-refs))))
              (push (cons e target) oneway))))))
    (let ((buf (get-buffer-create "*需求测试勾稽*")))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "项目: %s (%s)\n\n" proj file))
          (insert (format (concat "勾稽统计: 需求 %d 条 · 有测试证据 %d · 未覆盖 %d"
                                  " · 待重新验证 %d · 单向关联 %d · 继承 %d"
                                  " · 记录 %d 条 · 未关联 %d · 回填 %d 条\n\n")
                          (length reqs) (- (length reqs) (length missing))
                          (length missing) (length reverify) (length oneway)
                          (length inherited)
                          (length records) (length unlinked) filled))
          (if (and (null missing) (null reverify) (null oneway) (null unlinked))
              (insert "勾稽缺口: (无)\n")
            (when missing
              (insert "未覆盖需求(无测试证据):\n")
              (dolist (e (reverse missing))
                (insert (format "  - %s %s\n" (plist-get e :id) (plist-get e :title)))))
            (when reverify
              (insert "\n变更后待重新验证(复验通过后删除 :TEST-CASE: 中的标记):\n")
              (dolist (e (reverse reverify))
                (insert (format "  - %s %s\n" (plist-get e :id) (plist-get e :title)))))
            (when oneway
              (insert "\n单向关联(需求侧链接了记录但记录行未回指, 在记录行「验证需求」列补链接后手工清理需求侧旧链接):\n")
              (dolist (p (reverse oneway))
                (insert (format "  - %s %s -> %s\n"
                                (plist-get (car p) :id) (plist-get (car p) :title)
                                (cdr p)))))
            (when unlinked
              (insert "\n未关联需求的记录:\n")
              (dolist (r (reverse unlinked))
                (insert (format "  - %s %s (%s)\n"
                                (plist-get r :id) (plist-get r :title)
                                (car (my/org-capture--record-spec (plist-get r :kind))))))))
          (when inherited
            (insert "\n继承自其他项目(核对源项目基线是否变更, :INHERITED: 链接直达源条目):\n")
            (dolist (e (reverse inherited))
              (insert (format "  - %s %s <- %s\n"
                              (plist-get e :id) (plist-get e :title)
                              (plist-get e :inherited)))))
          (let ((org-inhibit-startup t)) (org-mode))
          (goto-char (point-min))
          (setq buffer-read-only t)))
      (display-buffer buf))
    (message "需求-测试勾稽: 回填 %d 条, 未覆盖 %d, 待重新验证 %d, 单向关联 %d, 未关联记录 %d"
             filled (length missing) (length reverify) (length oneway) (length unlinked))))


;; ---- 需求继承 Requirement inheritance ----
;; 新项目借用旧项目时, 借的是源项目「已冻结的需求基线」而不是某个研制阶段:
;; 需求统一建档在「需求」章, :PHASE: 只是适用阶段标记. 继承在新项目方案设计
;; 阶段完成, 与新研需求一起圈进本项目基线; 测试证据不继承(源测试记录是源项目
;; 的证据), 导入条目 :TEST-CASE: 留空, 由 C-c P c 勾稽出缺口.

(defun my/org-project--req-entry-body (pos)
  "返回 POS 处需求条目正文 (属性抽屉之后到子树末尾, 去首尾空白)."
  (org-with-wide-buffer
   (save-excursion
     (goto-char pos)
     (org-back-to-heading t)
     (let ((beg (progn (org-end-of-meta-data) (point)))
           (end (save-excursion (org-end-of-subtree t t) (point))))
       (string-trim (buffer-substring-no-properties beg end))))))

(defun my/org-project--req-insert-entry (type text)
  "把需求条目文本 TEXT 插入「需求」章 TYPE 需求小节末尾 (无小节先创建).
TEXT 标题行以单个 * 开头, 插入时调整为小节下一层级 (骨架为 ****)."
  (goto-char (point-min))
  (unless (re-search-forward "^\\*\\* 需求[ \t]*$" nil t)
    (user-error "本文件无「需求」章"))
  (let* ((chapter-end (save-excursion (org-end-of-subtree t t) (point)))
         (section (concat (if (org-string-nw-p type) type "其他") "需求"))
         (sec-beg (save-excursion
                    (goto-char (point-min))
                    (when (re-search-forward
                           (concat "^\\*\\*\\* " (regexp-quote section) "[ \t]*$")
                           chapter-end t)
                      (line-beginning-position))))
         (sec-level (if sec-beg
                        (save-excursion (goto-char sec-beg) (org-outline-level))
                      3)))
    (if sec-beg
        (goto-char (save-excursion
                     (goto-char sec-beg)
                     (org-end-of-subtree t t)
                     (point)))
      (goto-char chapter-end)
      (insert "*** " section "\n"))
    (skip-chars-backward " \t\n")
    (insert "\n\n"
            (replace-regexp-in-string
             "\\`\\* " (concat (make-string (1+ sec-level) ?*) " ")
             text t t))))

(defun my/org-project-req-import ()
  "从源项目已冻结基线导入需求条目, 复制建档并留 :INHERITED: 溯源链接 (C-c P i).
借源项目「已冻结」的需求基线(默认最新), 不是某研制阶段的实时状态; 条目用本项目
编号重新建档: :SOURCE: 继承复用, :PHASE: 保留源值, :BASELINE:/:DESIGN:/:MODULE:/
:TEST-CASE: 清空待本项目重新圈定与验证, 正文原样复制; 源条目链接记入 :INHERITED:
 ([[file:相对路径::#源ID][源ID]], 点击直达源条目)."
  (interactive)
  (unless (and (derived-mode-p 'org-mode) (buffer-file-name))
    (user-error "请在项目 org 文件中执行"))
  (let* ((files (seq-filter (lambda (f) (not (string= (cdr f) (buffer-file-name))))
                            (my/org-project--files)))
         (choice (completing-read "源项目(配置项): " (mapcar #'car files) nil t))
         (src (cdr (assoc choice files)))
         (rows (with-current-buffer (find-file-noselect src)
                 (my/org-project--baseline-records-rows)))
         (frozen (seq-filter
                  (lambda (r) (string= (plist-get r :status)
                                       my/org-project--baseline-status-frozen))
                  rows))
         (cands (or frozen rows))
         (labels (mapcar (lambda (r)
                           (format "%s %s %s" (plist-get r :ver)
                                   (if (string= (plist-get r :status)
                                                my/org-project--baseline-status-frozen)
                                       "已冻结"
                                     (if (org-string-nw-p (plist-get r :status))
                                         (plist-get r :status) "无状态"))
                                   (plist-get r :date)))
                         cands)))
    (unless cands
      (user-error "源项目无 baseline-records 基线记录"))
    (let* ((default (car (last labels)))
           (vchoice (completing-read (format "基线版本(默认 %s): " default)
                                     labels nil t nil nil default))
           (ver (car (split-string vchoice " ")))
           (src-reqs (with-current-buffer (find-file-noselect src)
                       (seq-filter
                        (lambda (e)
                          (member ver (split-string (or (plist-get e :baseline) ""))))
                        (my/org-project--req-entries))))
           (rlab (lambda (e) (format "%s %s" (plist-get e :id) (plist-get e :title))))
           (sel (completing-read-multiple
                 (format "导入需求(%s, 空=取消, 逗号分隔多选): " ver)
                 (mapcar rlab src-reqs)))
           (picked (delq nil (mapcar (lambda (s)
                                       (seq-find (lambda (e)
                                                   (string= s (funcall rlab e)))
                                                 src-reqs))
                                     sel))))
      (unless picked
        (user-error "未选择需求, 取消导入"))
      (let ((created (format-time-string "[%Y-%m-%d %a]"))
            (rel (file-relative-name src (file-name-directory (buffer-file-name))))
            (src-code (file-name-base src))
            (n 0))
        (dolist (e picked)
          (let* ((type (if (org-string-nw-p (plist-get e :type))
                           (plist-get e :type) "其他"))
                 (letter (or (cdr (assoc type my/org-capture--req-id-letters)) "O"))
                 (new-id (my/org-capture--next-id (concat "REQ-" letter)))
                 (link (format "[[file:%s::#%s][%s]]" rel (plist-get e :id)
                               (my/org-project--id-short (plist-get e :id)
                                                         src-code)))
                 (body (with-current-buffer (find-file-noselect src)
                         (my/org-project--req-entry-body (plist-get e :pos))))
                 (text (concat "* DESIGN " (plist-get e :title) "\n"
                               "  :PROPERTIES:\n"
                               "  :CUSTOM_ID: " new-id "\n"
                               "  :TYPE: " type "\n"
                               "  :SOURCE: 继承复用\n"
                               "  :INHERITED: " link "\n"
                               "  :PHASE: " (or (plist-get e :phase) "") "\n"
                               "  :BASELINE:\n"
                               "  :DESIGN:\n"
                               "  :MODULE:\n"
                               "  :TEST-CASE:\n"
                               "  :CREATED: " created "\n"
                               "  :END:\n"
                               (if (org-string-nw-p body)
                                   (concat "\n" body "\n") ""))))
            (my/org-project--req-insert-entry type text)
            (setq n (1+ n))))
        (message "已从「%s」基线 %s 导入需求 %d 条 (:INHERITED: 溯源, 测试证据待重验证)"
                 choice ver n)))))


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
  "提示风险评估项 KIND (probability/impact), LABEL 为提示名, DEFAULT 为默认值; 记录取值供模板展开."
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
  "在「议题」章新建 REPORT 条目, 标题为 TITLE, 来源为 PHASE, 返回其 CUSTOM_ID.
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
        ;; 项目名/文件名须在切到 *项目风险* 缓冲区前取, 否则 buffer-file-name 为 nil
        (proj (or (my/org-project--config-name)
                  (file-name-base (buffer-file-name))))
        (file (buffer-file-name))
        (buf (get-buffer-create "*项目风险*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "项目: %s (%s)\n\n" proj file))
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
  "里程碑的判据清单(按编号列出), 基线冻结判据见 my/org-project--baseline-criteria.")

(defun my/org-project--milestone-criteria-text (key indent)
  "生成判据 checkbox 文本, KEY 为里程碑编号, INDENT 为缩进."
  (mapconcat (lambda (item) (concat indent "- [ ] " item "\n"))
             (cdr (assoc key my/org-project--milestone-criteria))
             ""))

(defun my/org-project--milestone-review-table-text (suffix indent)
  "生成 review-SUFFIX 名称的空评审表文本, SUFFIX 为表名后缀, INDENT 为缩进."
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
  "生成「里程碑」章完整子树文本: CODE 为项目代号, 汇总块 + 四个转段里程碑.
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
  "MILESTONE 是否未完成 (TODO 或未状态化)."
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
  "返回 MILESTONE 的 :REQUIRES: 中未完成的前置里程碑 ID 列表."
  (let (unmet)
    (dolist (id (split-string (or (plist-get milestone :requires) "") "[ ,]+" t))
      (let ((g (my/org-project--milestone-get id)))
        (when (or (null g) (my/org-project--milestone-open-p g))
          (push id unmet))))
    (nreverse unmet)))

(defun my/org-project--milestone-gaps (milestone)
  "返回 MILESTONE 的缺口描述列表 (判据未勾 + 前置里程碑未完成)."
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
  "返回 ID 对应里程碑的评审表数据行数."
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
  "确保 ID 对应里程碑下有 review-<后缀> 表, 缺失则在里程碑子树末尾补一张."
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
  "定位到 ID 对应里程碑的评审表 #+NAME 行, 缺表则补建."
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
  "MILESTONE 的补全显示文本."
  (format "%s %s [%s]"
          (plist-get milestone :suffix)
          (or (plist-get milestone :title) "")
          (or (plist-get milestone :todo) "无状态")))

(defun my/org-project--milestone-sort-key (milestone)
  "MILESTONE 的排序键: 按编号数字排序 (M1 < M2 ...)."
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
  "从 VER 的 RB<n> 形式提取 n; 非法版本返回 0."
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
FORCE 非 nil 跳过确认 (pe/圈定收尾联动), 评审结论为空时回填评审表该版本结论."
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
  "解冻阶段 STAGE 的最新基线: 置回「待评审」, FORCE 非 nil 跳过确认."
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
;; 测试/试验记录: 新记录由 pl 捕获按类型自动编号(代号_FT-001), `C-c P n' 给
;; 存量行补编号, `C-c P l' 插入记录引用链接, `C-c P t' 按编号跳转记录行.
;; 需求-测试勾稽: `C-c P c' 从记录行「验证需求」列回填 :TEST-CASE: 并列缺口
;; (未覆盖/待重新验证/单向关联/未关联记录).
;; 需求继承: `C-c P i' 从源项目已冻结基线导入需求(复制建档+:INHERITED: 溯源,
;; 测试证据不继承, :TEST-CASE: 留空待重验证).
;; 软件状态: `C-c P v' 登记修改项/测试项(从研制任务与测试记录多选, 补行+同步
;; 最新数据, 归属即引用); ps 捕获时也走同一套选择.
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
(global-set-key (kbd "C-c P n") #'my/org-project-record-number)
(define-key org-mode-map (kbd "C-c P n") #'my/org-project-record-number)
(global-set-key (kbd "C-c P t") #'my/org-project-record-goto)
(define-key org-mode-map (kbd "C-c P t") #'my/org-project-record-goto)
(global-set-key (kbd "C-c P l") #'my/org-project-record-link-insert)
(define-key org-mode-map (kbd "C-c P l") #'my/org-project-record-link-insert)
(global-set-key (kbd "C-c P c") #'my/org-project-req-test-sync)
(define-key org-mode-map (kbd "C-c P c") #'my/org-project-req-test-sync)
(global-set-key (kbd "C-c P i") #'my/org-project-req-import)
(define-key org-mode-map (kbd "C-c P i") #'my/org-project-req-import)
(global-set-key (kbd "C-c P v") #'my/org-project-version-register)
(define-key org-mode-map (kbd "C-c P v") #'my/org-project-version-register)

;; 风险汇总里的来源缺失标记、需求变更后的待重新验证标记用 error face 标红。
;; 用 face 符号而非写死颜色, 明暗主题切换后仍然有效 (与上面 face-override-spec 的处理同理)。
(font-lock-add-keywords 'org-mode '(("⚠ 未链接" 0 'error t)
                                    ("⚠ 待重新验证" 0 'error t)))

;; 风险台账视图：未闭环风险（待评估/措施中分组）。
;; RISK_LEVEL={.} 圈全部带等级的风险条目, /-RESOLVED 排除已了结,
;; 未状态化的存量条目视为未闭环(同 my/org-project--risk-open-p 的口径).
(add-to-list 'org-agenda-custom-commands
             '("R" "风险台账" tags "RISK_LEVEL={.}/-RESOLVED"
      ((org-agenda-overriding-header "风险台账（未闭环）")
       (org-super-agenda-groups
        '((:name "◎ 待评估" :todo "IDENT")
          (:name "◎ 措施中" :todo "MITIGATE")
          (:name "◎ 未状态化" :anything t)))))
             t)

;; 捕获收尾：需求变更移出基线、刷新项目动态块、评审后提示冻结基线
;; 注意写全 -hook 后缀: Emacs 31 的 add-hook 不再自动补 -hook
(add-hook 'org-capture-after-finalize-hook #'my/org-capture-req-change-finalize t)
(add-hook 'org-capture-after-finalize-hook #'my/org-capture-refresh-dblocks t)
(add-hook 'org-capture-after-finalize-hook #'my/org-capture-review-finalize t)
(add-hook 'org-capture-after-finalize-hook #'my/org-capture-sw-finalize t)

;; 记录链接按编号精确跳转: 软件状态「测试项」表的 [[file:..::编号][..]] 链接
;; 与记录行同文件且链接在前, org 文本搜索先命中链接自身落回原地;
;; 注册后 file 链接的 ::编号 搜索先按记录行编号匹配, 命中直达.
(add-hook 'org-execute-file-search-functions #'my/org-project--record-link-search)

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
               "  :TEST-CASE:\n"
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
               "  |--------+------+----------+----------|\n"
               "%(my/org-capture--sw-change-rows)\n"
               "  | 测试项 | 指标要求 | 实测结果 | 结论 |\n"
               "  |--------+----------+----------+------|\n"
               "%(my/org-capture--sw-test-rows)\n")
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
  "项目管理捕获模板（p 组），加载时插入 org-capture-templates.")

;; 插到「参考」组之前，保持 c → j → p → r → x 的菜单顺序
(let ((before (seq-take-while (lambda (tpl) (not (equal (car tpl) "r")))
                              org-capture-templates))
      (after (seq-drop-while (lambda (tpl) (not (equal (car tpl) "r")))
                             org-capture-templates)))
  (setq org-capture-templates
        (append before my/org-project--capture-templates after)))

(provide 'init-org-project)

;;; init-org-project.el ends here
