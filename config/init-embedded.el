;;; init-embedded.el --- 嵌入式开发与调试（STM32/DSP）配置. -*- lexical-binding: t -*-

;; Copyright (C) 2022~2026 王北洛

;; Author: 王北洛 <wbeiluo@gmail.com>
;; URL: https://github.com/wbeiluo/beiluo-emacs

;;; Commentary:
;;
;; 嵌入式开发与调试配置，按职责分层组织：
;;
;;  1. 调试        dape 嵌入式模板（GDB 原生 DAP，连 OpenOCD / JLink GDBServer）
;;  2. 构建        CMake + make/gmake（TI CCS 导出的 gmake 工程同样适用）
;;  3. 文件类型    链接脚本 / 汇编 / CubeMX 工程文件 / 固件 bin
;;  4. 快捷键      C-c d 命令前缀
;;
;; 外部依赖（按需安装，README「嵌入式调试」一节有详细说明）：
;;
;;  - arm-none-eabi-gcc + arm-none-eabi-gdb（GDB ≥ 14.1 才支持 DAP）
;;  - openocd 或 JLinkGDBServer（二选一，目标板的 GDBServer）
;;  - cmake、make（构建）
;;
;; 项目级参数（ELF 路径、GDB server 地址、初始命令等）通过工程根目录的
;; .dir-locals.el 配置，见 README。
;;
;;; Code:

;; 运行期依赖符号的前置声明：dape 在 dape 加载后、project/comint 按需加载时
;; 才定义，这里仅用于消除字节编译警告，不影响运行行为。
(defvar dape-configs)
(defvar dape-minibuffer-hint-ignore-properties)
(declare-function comint-check-proc "comint")
(declare-function dape-ensure-command "dape")
(declare-function dape-config-get "dape")
(declare-function dape-evaluate-expression "dape")
(declare-function dape-request "dape")
(declare-function dape--config "dape")
(declare-function dape--live-connection "dape")
(declare-function dape--live-connections "dape")
(declare-function dape--root-of "dape")
(declare-function project-root "project")

;;; 变量定义

(defgroup embedded nil
  "嵌入式开发与调试."
  :group 'tools
  :prefix "embedded-")

(defcustom embedded-gdb-program
  (if (executable-find "arm-none-eabi-gdb") "arm-none-eabi-gdb" "gdb")
  "嵌入式调试用的 GDB 可执行文件名.
默认优先用交叉工具链的 arm-none-eabi-gdb，没有则退回本机 gdb。"
  :type 'string)

(defcustom embedded-elf nil
  "项目固件 ELF 路径，nil 表示在工程内自动探测.
可在 .dir-locals.el 中按项目设置。"
  :type '(choice (const :tag "自动探测" nil) string))

(defcustom embedded-gdb-server-address nil
  "GDB server 地址（host:port），nil 时各配置用默认端口:
stm32-openocd 用 localhost:3333，stm32-jlink 用 localhost:2331，
gdb-remote 交互输入。可在 .dir-locals.el 中按项目设置。"
  :type '(choice (const :tag "用配置默认值" nil) string))

(defcustom embedded-dape-init-commands
  '("monitor reset halt" "load" "monitor reset halt")
  "会话首次停止后自动执行的 GDB 命令序列（dape）.
默认：复位停住 → 烧写固件 → 再次复位停住（标准的连接+烧写流程）。
设为 nil 可禁用自动执行（attach 时不动目标）。
可在 .dir-locals.el 中按项目覆盖，例如只复位不烧写：
  (embedded-dape-init-commands . (\"monitor reset halt\"))"
  :type '(choice (const :tag "禁用" nil) (repeat string)))

(defcustom embedded-openocd-args nil
  "启动 openocd 的参数列表，nil 时交互输入.
例如 (\"-f\" \"interface/stlink.cfg\" \"-f\" \"target/stm32f4x.cfg\")。
可在 .dir-locals.el 中按项目设置。"
  :type '(choice (const :tag "交互输入" nil) (repeat string)))

(defcustom embedded-jlink-args nil
  "JLinkGDBServer 启动参数列表，nil 时交互输入.
例如 (\"-device\" \"STM32F407VG\" \"-if\" \"SWD\" \"-speed\" \"4000\"
      \"-LocalhostOnly\")。
可在 .dir-locals.el 中按项目设置。"
  :type '(choice (const :tag "交互输入" nil) (repeat string)))

(defcustom embedded-flash-command "make flash"
  "外部烧写命令（走 make 目标，不经调试器），在工程根目录执行.
CCS/CMake 工程可在 .dir-locals.el 中改成自己的烧写目标。"
  :type 'string)

(defcustom embedded-make-program "make"
  "Make 可执行文件名（TI CCS 的 Windows 环境可设为 gmake）."
  :type 'string)

(defcustom embedded-parallel-jobs nil
  "编译并行度，nil 表示自动（Linux 用 nproc，Windows 用 4）."
  :type '(choice (const :tag "自动" nil) integer))

(defun embedded--string-list-p (value)
  "VALUE 是字符串列表时返回 t（nil 视为空列表）."
  (and (listp value) (seq-every-p #'stringp value)))

;; .dir-locals.el 里的项目变量：声明为安全，避免每次打开工程都询问
(dolist (spec '((embedded-elf stringp)
                (embedded-gdb-server-address stringp)
                (embedded-gdb-program stringp)
                (embedded-flash-command stringp)
                (embedded-make-program stringp)
                (embedded-openocd-args embedded--string-list-p)
                (embedded-jlink-args embedded--string-list-p)
                (embedded-dape-init-commands embedded--string-list-p)))
  (put (car spec) 'safe-local-variable (cadr spec)))

;;; 项目根目录与探测

(defun embedded--project-root ()
  "返回当前工程根目录."
  (if-let* ((proj (project-current nil)))
      (project-root proj)
    (or (locate-dominating-file default-directory "CMakeLists.txt")
        (locate-dominating-file default-directory ".git")
        default-directory)))

(defun embedded--read-elf ()
  "确定要调试的固件 ELF，返回绝对路径.
优先级：`embedded-elf'（dir-local）→ 工程内唯一 .elf → 交互选择。"
  (or (and (stringp embedded-elf)
           (not (string-empty-p embedded-elf))
           (expand-file-name embedded-elf))
      (let* ((root (embedded--project-root))
             (elfs (ignore-errors
                     (directory-files-recursively
                      root "\\.elf\\'" nil
                      (lambda (dir)
                        (not (member (file-name-nondirectory dir)
                                     '(".git" "node_modules" ".pio"))))))))
        (cond
         ((null elfs) (read-file-name "固件 ELF: " root))
         ((null (cdr elfs)) (car elfs))
         (t (expand-file-name
             (completing-read "选择 ELF: "
                              (mapcar (lambda (f) (file-relative-name f root)) elfs)
                              nil t nil nil
                              (file-relative-name (car elfs) root))
             root))))))

(defun embedded--read-target (default)
  "确定 GDB server 地址，dir-local 值优先.
`embedded-gdb-server-address' 优先，否则用 DEFAULT（各配置默认端口），
最后交互输入。"
  (or (and (stringp embedded-gdb-server-address)
           (not (string-empty-p embedded-gdb-server-address))
           embedded-gdb-server-address)
      default
      (read-string "GDB server (host:port): " "localhost:3333")))

;;; 构建：CMake + make/gmake

(use-package cmake-mode
  :ensure t
  :mode ("CMakeLists\\.txt\\'" . cmake-mode) ("\\.cmake\\'" . cmake-mode))

;; 安装后自动在 cmake-mode 中激活，无需 hook
(use-package cmake-font-lock
  :ensure t
  :defer t)

(defconst embedded--makefile-names
  '("Makefile" "GNUmakefile" "makefile" "MAKEFILE")
  "查找 makefile 时尝试的文件名.")

(defun embedded--cmake-build-dir (root)
  "返回 ROOT 下第一个含 CMakeCache.txt 的构建目录，无则 nil.
先看常见的 build/，再扫一级子目录。"
  (if (file-exists-p (expand-file-name "build/CMakeCache.txt" root))
      (expand-file-name "build" root)
    (seq-find (lambda (dir)
                (and (file-directory-p dir)
                     (file-exists-p (expand-file-name "CMakeCache.txt" dir))))
              (seq-filter #'file-directory-p
                          (directory-files root t
                                           directory-files-no-dot-files-regexp
                                           t)))))

(defun embedded--find-makefile-dir (root)
  "返回 ROOT 下可直接 make 的目录，无则 nil.
除根目录外，也识别 TI CCS 导出工程的 Debug/、Release/ 子目录。"
  (or (when (seq-find (lambda (name)
                        (file-exists-p (expand-file-name name root)))
                      embedded--makefile-names)
        root)
      (seq-find (lambda (dir)
                  (seq-find (lambda (name)
                              (file-exists-p (expand-file-name name dir)))
                            embedded--makefile-names))
                (list (expand-file-name "Debug" root)
                      (expand-file-name "Release" root)))))

(defun embedded--jobs ()
  "编译并行度参数.
`embedded-parallel-jobs' 未设置时 Linux 用 $(nproc)，Windows 用 4。"
  (format "%s" (or embedded-parallel-jobs
                   (if (eq system-type 'windows-nt) 4 "$(nproc)"))))

(defun embedded--compile-command (root)
  "根据工程结构生成 ROOT 的编译命令，无法识别时返回 nil."
  (if-let* ((cmake (embedded--cmake-build-dir root)))
      (format "cmake --build %s --parallel %s"
              (shell-quote-argument (file-relative-name cmake root))
              (embedded--jobs))
    (when-let* ((make-dir (embedded--find-makefile-dir root)))
      (format "%s -j%s -C %s"
              embedded-make-program
              (embedded--jobs)
              (shell-quote-argument (file-relative-name make-dir root))))))

(defun embedded-compile (arg)
  "编译当前嵌入式工程，结果在 compile-mode 缓冲中显示.
ARG 为非 nil（带前缀参数）时手动输入编译命令。
CMake 工程（有构建目录）用 cmake --build；否则找 Makefile/makefile
（含 TI CCS 的 Debug/makefile）用 make。"
  (interactive "P")
  (let* ((root (embedded--project-root))
         (default-command (embedded--compile-command root)))
    (let ((default-directory root)
          (command (if arg
                       (read-string "编译命令: " nil nil default-command)
                     (or default-command
                         (user-error "未识别到 CMake 构建目录或 Makefile")))))
      (compile command))))

(defun embedded-configure ()
  "生成 CMake 构建目录（cmake -S . -B build）."
  (interactive)
  (let ((root (embedded--project-root)))
    (unless (file-exists-p (expand-file-name "CMakeLists.txt" root))
      (user-error "项目根目录没有 CMakeLists.txt"))
    (let ((default-directory root))
      (compile "cmake -S . -B build"))))

(defun embedded-flash ()
  "执行 `embedded-flash-command'（默认 make flash）完成外部烧写.
命令可在 .dir-locals.el 中按项目覆盖。"
  (interactive)
  (let ((default-directory (embedded--project-root)))
    (compile embedded-flash-command)))

;;; 调试：dape 嵌入式配置（GDB 原生 DAP）

(defun embedded--ensure-gdb (config)
  "确认 CONFIG 使用的 GDB 存在且版本 >= 14.1（DAP 支持要求）."
  (dape-ensure-command config)
  (let* ((cmd (dape-config-get config 'command))
         (output (shell-command-to-string
                  (format "%s --version" (shell-quote-argument cmd))))
         (version (save-match-data
                    (if (string-match "GNU gdb \\(?:(.*) \\)?\\([0-9.]+\\)" output)
                        (string-to-number (match-string 1 output))
                      0))))
    (unless (>= version 14.1)
      (user-error "%s 版本为 %s，DAP 调试需要 GDB >= 14.1" cmd version))))

(defun embedded--dape-config (name default-address)
  "生成嵌入式 dape 配置 NAME，其默认 GDB server 地址为 DEFAULT-ADDRESS.
基于 GDB 原生 DAP（gdb --interpreter=dap），attach 到远程 GDBServer：
ELF、server 地址、初始命令分别读 dir-local 变量 `embedded-elf'、
`embedded-gdb-server-address'、`embedded-dape-init-commands'。"
  `(,name
    ensure embedded--ensure-gdb
    command embedded-gdb-program
    command-args ("--interpreter=dap")
    command-cwd dape-command-cwd
    embedded-init-commands embedded-dape-init-commands
    :request "attach"
    :program embedded--read-elf
    :target ,(lambda () (embedded--read-target default-address))))

(defun embedded--dape-send (conn commands)
  "按顺序向 CONN 发送 GDB 命令列表 COMMANDS（repl 上下文）.
失败时中断后续命令并提示。"
  (if-let* ((cmd (car commands)))
      (dape-request conn :evaluate
                    (list :expression cmd :context "repl")
                    (lambda (_body error)
                      (if error
                          (message "调试命令「%s」失败: %s" cmd error)
                        (embedded--dape-send conn (cdr commands)))))
    (message "嵌入式初始命令完成")))

(defvar embedded--dape-initialized nil
  "已执行过初始命令的 dape 会话（root connection）列表.")

(defun embedded--dape-auto-init ()
  "在 dape 会话首次停止时执行 `embedded-init-commands'（复位+烧写+复位）.
只在会话第一次 stopped 事件时触发，断点命中不再重复。"
  (let ((live (mapcar #'dape--root-of (dape--live-connections))))
    (setq embedded--dape-initialized
          (seq-remove (lambda (root) (not (memq root live)))
                      embedded--dape-initialized))
    (when-let* ((conn (dape--live-connection 'last t))
                (root (dape--root-of conn)))
      (unless (memq root embedded--dape-initialized)
        (when-let* ((cmds (plist-get (dape--config root)
                                     'embedded-init-commands)))
          (push root embedded--dape-initialized)
          (embedded--dape-send conn cmds))))))

(defun embedded-dape-init ()
  "对当前 dape 会话重发嵌入式初始命令（复位+烧写+复位）.
自动初始命令失败或目标跑飞后，用它重新拉回已知状态。"
  (interactive)
  (require 'dape)
  (let ((conn (dape--live-connection 'last t)))
    (unless conn (user-error "没有活动的 dape 调试会话"))
    (let ((cmds (plist-get (dape--config (dape--root-of conn))
                           'embedded-init-commands)))
      (unless (consp cmds)
        (user-error "未配置初始命令（embedded-dape-init-commands 为 nil）"))
      (embedded--dape-send conn cmds))))

(defvar embedded-dape-command-history nil
  "`embedded-dape-command' 的输入历史.")

(defun embedded-dape-command (command)
  "向当前 dape 会话发送任意 GDB 命令 COMMAND，结果显示在 dape REPL.
例如 monitor reset、info registers、x/16wx 0x20000000。"
  (interactive
   (list (read-string "GDB 命令: " "monitor reset halt"
                      'embedded-dape-command-history)))
  (require 'dape)
  (dape-evaluate-expression (dape--live-connection 'last t) command "repl"))

;; 注册到 dape：配置、自动初始命令钩子、minibuffer 提示过滤。
;; 必须在 dape 加载后进行，避免覆盖 dape 的 defcustom 默认值。
(with-eval-after-load 'dape
  (add-to-list 'dape-configs
               (embedded--dape-config 'stm32-openocd "localhost:3333"))
  (add-to-list 'dape-configs
               (embedded--dape-config 'stm32-jlink "localhost:2331"))
  (add-to-list 'dape-configs
               (embedded--dape-config 'gdb-remote nil))
  (add-hook 'dape-stopped-hook #'embedded--dape-auto-init)
  (add-to-list 'dape-minibuffer-hint-ignore-properties
               'embedded-init-commands))

;;; GDBServer 启动辅助

(defvar embedded-server-args-history nil
  "GDBServer 启动参数的输入历史.")

(defun embedded--server-start (program buffer-name args)
  "以 comint 启动 GDBServer PROGRAM，缓冲区 BUFFER-NAME，参数 ARGS.
已在运行则直接切换到该缓冲区。"
  (require 'comint)
  (let ((buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (unless (comint-check-proc buffer)
        (apply #'make-comint-in-buffer program buffer program nil args)))
    (pop-to-buffer buffer)))

(defun embedded--server-args (default prompt)
  "确定 GDBServer 参数，DEFAULT 优先否则读取 PROMPT 交互输入.
DEFAULT 通常来自 dir-local 或 defcustom。"
  (or default
      (split-string-and-unquote
       (read-string prompt nil 'embedded-server-args-history))))

(defun embedded-openocd-start (args)
  "启动 openocd，参数 ARGS 默认读 `embedded-openocd-args'.
例如 -f interface/stlink.cfg -f target/stm32f4x.cfg。"
  (interactive
   (list (embedded--server-args
          embedded-openocd-args
          "openocd 参数: ")))
  (unless (executable-find "openocd")
    (user-error "未找到 openocd（Arch: sudo pacman -S openocd）"))
  (embedded--server-start "openocd" "*embedded-openocd*" args))

(defun embedded-jlink-start (args)
  "启动 JLinkGDBServer，参数 ARGS 默认读 `embedded-jlink-args'."
  (interactive
   (list (embedded--server-args
          embedded-jlink-args
          "JLinkGDBServer 参数: ")))
  (unless (executable-find "JLinkGDBServer")
    (user-error "未找到 JLinkGDBServer（需安装 SEGGER J-Link 驱动）"))
  (embedded--server-start "JLinkGDBServer" "*embedded-jlink*" args))

(defun embedded-server-stop ()
  "停止本模块启动的 GDBServer（openocd / JLink）进程."
  (interactive)
  (dolist (name '("*embedded-openocd*" "*embedded-jlink*"))
    (when-let* ((buffer (get-buffer name)))
      (when-let* ((proc (get-buffer-process buffer)))
        (delete-process proc))
      (kill-buffer buffer)
      (message "已停止 %s" name))))

;;; 文件类型

;; 链接脚本（GNU ld）：.ld / .lds
(add-to-list 'auto-mode-alist '("\\.ld\\'" . ld-script-mode))
(add-to-list 'auto-mode-alist '("\\.lds\\'" . ld-script-mode))
;; 汇编：.S（预处理汇编）、.sx
(add-to-list 'auto-mode-alist '("\\.S\\'" . asm-mode))
(add-to-list 'auto-mode-alist '("\\.sx\\'" . asm-mode))
;; STM32CubeMX 工程文件
(add-to-list 'auto-mode-alist '("\\.ioc\\'" . conf-unix-mode))
;; 固件镜像：十六进制查看
(add-to-list 'auto-mode-alist '("\\.bin\\'" . hexl-mode))

(defun embedded-ti-cmd-mode (&rest _)
  "TI 链接命令文件（.cmd）模式，复用 `ld-script-mode'.
TI 的 MEMORY/SECTIONS 语法与 GNU ld 脚本接近。
不全局绑定 .cmd 后缀（与 Windows 批处理冲突），在 TI 工程的
.dir-locals.el 中配置：  ((\"*.cmd\" . embedded-ti-cmd-mode))"
  (ld-script-mode))

;;; 快捷键：C-c d 前缀

(defvar embedded-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "d") #'dape)                    ; 启动调试（选配置）
    (define-key map (kbd "r") #'embedded-dape-init)       ; 重发初始命令
    (define-key map (kbd "x") #'embedded-dape-command)    ; 任意 GDB 命令
    (define-key map (kbd "o") #'embedded-openocd-start)   ; 启动 openocd
    (define-key map (kbd "j") #'embedded-jlink-start)     ; 启动 JLinkGDBServer
    (define-key map (kbd "k") #'embedded-server-stop)     ; 停止 GDBServer
    (define-key map (kbd "b") #'embedded-compile)         ; 编译
    (define-key map (kbd "c") #'embedded-configure)       ; cmake 配置
    (define-key map (kbd "f") #'embedded-flash)           ; 外部烧写
    map)
  "嵌入式开发命令前缀.")

(global-set-key (kbd "C-c d") embedded-command-map)

(with-eval-after-load 'which-key
  (when (fboundp 'which-key-add-keymap-based-replacements)
    (which-key-add-keymap-based-replacements global-map "C-c d" "嵌入式")))

(provide 'init-embedded)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-embedded.el ends here
