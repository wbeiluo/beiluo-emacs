# AGENTS.md

Personal Emacs configuration (**BLEmacs**) in elisp only: no build system, no CI, no tests.
Verification = batch startup smoke test (below). Docs: `README.org` (Chinese, install steps).

## Layout and loading

- `init.el` is bootstrap only. It loads every module from `config/` via an **explicit ordered
  `(require 'init-*)` list**. Adding a new module means both creating `config/init-foo.el`
  (ending with `(provide 'init-foo)`) **and** inserting the `require` into `init.el` — nothing is
  auto-discovered. Order matters: `init-lib` provides dash/s/f/ht/ov/ts/hydra/posframe used later.
- Module file convention: `;;; init-foo.el --- ... -*- lexical-binding: t -*-` header, copyright
  block, Chinese commentary, `(provide 'init-foo)` + `;;; init-foo.el ends here` footer.
- `init-windows.el` is **Windows platform support** (windows-nt: PATH/MSYS2, clipboard coding);
  `init-window.el` is **window management**. Easy to confuse.
- Platform gates found in code: `display-graphic-p` (fonts, theme), `system-type 'windows-nt`
  (`init-windows.el`, parts of `init-fonts.el`), `gnu/linux` (`init-org.el`). The config targets
  both Arch Linux and Windows.
- `custom.el` is gitignored but loaded at the end of `init.el`. Machine-written by Custom — never
  hand-edit config there.

## Packages

- package.el + `use-package`, with `use-package-always-ensure nil` (`init-lib.el`): installable
  packages must declare `:ensure t` explicitly or they won't install on a fresh machine.
- ELPA archives are USTC China mirrors (`init-general.el`). `elpa/` is gitignored; packages are
  installed on first interactive start.
- `init-general.el` sets `package-enable-at-startup nil` — batch runs never see ELPA unless you
  activate packages yourself (see below).

## Verifying changes

```sh
emacs --batch -Q --eval '(require (quote package))' --eval '(package-activate-all)' \
      -l ~/.emacs.d/init.el
```

- Exit 0 = healthy. A load error aborts with a backtrace on stderr and exit 255.
- The `package-activate-all` prelude is **required**: batch mode does not auto-activate ELPA
  packages (interactive startup does). Without it you get bogus `Error (use-package): Cannot load
  <pkg>` warnings and a failure that says nothing about your change.
- Success prints nothing: startup messages are deliberately silenced (`with-temp-message` in
  `init.el`, `inhibit-message` in `init-general.el`). Rely on exit code/stderr, not printed output.
- Single module check: same prelude plus `-L ~/.emacs.d/config -l ~/.emacs.d/config/init-foo.el`
  (only works for modules whose deps are already loaded).

## Repo conventions

- Git tracks only `init.el`, `config/*.el`, `agents/*.md`, `README.org`, `logo.txt`, `.gitignore`,
  `LICENSE`.  Everything else at top level (`custom.el`, `recentf`, `places`, `history`, `elpa/`,
  `transient/`, …) is machine state and gitignored — never commit it.
- Commit messages are in Chinese (see `git log`); work happens on the `develop` branch.

## Runtime data and external dependencies

- Org data lives **outside the repo**: `~/Org` (agenda = `diary.org`, `plan.org`, `inbox.org`,
  `work.org` + everything under `~/Org/projects/`), denote notes in `~/Org/notes/`. LaTeX preview
  images cache to `~/Org/assets/org-latex-preview/`. Never assume org content is in-repo.
- System tools the config expects (README.org + code): `ripgrep`, `pyright` (lsp-pyright), `mpv`
  (EMMS), texlive + texlive-langchinese (org LaTeX export), and the fonts listed in `README.org`
  (pacman package names).
- AI setup (`init-ai.el`, gptel): backends for ChatGPT, Deepseek, and Xiaomi MIMO; default model
  `mimo-v2.6-flash`. API keys come from auth-source — never hardcode keys in config files.

## gptel / gptel-agent

Layered config in `init-ai.el`: L0 backends → L1 model profiles (`llm-fast|strong|reason|cheap`)
→ L2 personas (`gptel-directives`, generic voices only) → L3 task presets (`:parents` + inline
`:system` + tool whitelist).  Sub-agents live in `agents/*.md` as category agents mirroring the
preset task categories: `coder` (coding), `analyst` (explain/debugging/review, read-only),
`tech-researcher` (research), `writer` (writer/translate).  Presets are the in-session single-step
variant of a task, agents the autonomous multi-step variant; names may repeat (`writer`).

Rules that are easy to get wrong:

- **No sampling params in backend `:request-params`** (they override `gptel-temperature` and
  preset `:request-params` — `gptel--merge-plists` gives later plists precedence).
- **Agent files must not use `:parents`.** `gptel-agent-preset` is merged after parent expansion
  and wins over it (merge order: agent-file keys > `gptel-agent-preset` > `:parents`). Set an
  explicit `backend: <name>` + `model: <name>` pair instead. `backend:` is a backend *name string*
  resolved by `gptel-get-backend`; `model:` is interned to a symbol and must exist on that backend.
- **Plain YAML scalars cannot contain `: `** — use a block scalar (`description: >`).
- **One malformed agent file aborts registration of everything scanned after it** — hence the
  `condition-case` around `gptel-agent-update` in `init-ai.el`. After editing `agents/*.md`,
  re-run the batch smoke test (or `M-x gptel-agent-update`) and check `*Warnings*`.
- Agent body may reference templates `{{AGENTS}}` (other agents) and `{{SKILLS}}` (skill list);
  they are expanded at registration time, not at call time.
- `gptel-agent-dirs` is order-sensitive: later dirs override same-named earlier ones. `agents/` is
  appended to the end on purpose so local definitions win over package-builtin ones.
- If a prompt ever moves into `gptel-directives`, its name must not be an existing function name
  (`debug` is one — `:system 'debug` silently skips the directive lookup via `functionp`).

## Cross-module gotchas

- Org inactive timestamp is `C-c _`, not the default `C-c !`: flycheck's keymap prefix steals
  `C-c !` (minor-mode maps beat major-mode maps). See comments in `init-org.el`.
- Org table/date/tag faces are set via `face-override-spec`, not `set-face-attribute`, so they
  survive theme switches (theme changes reset face attributes). Colors stay with the theme.
- Completion is corfu + cape (`init-corfu.el`); `lsp-completion-provider` is `:none` and snippets
  come from tempel. Turning lsp completion back on will fight this setup.
