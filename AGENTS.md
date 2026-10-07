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
  `work.org` + everything under `~/Org/projects/`), denote notes in `~/Org/notes/`. Never assume
  org content is in-repo.
- System tools the config expects (README.org + code): `ripgrep`, `pyright` (lsp-pyright), `mpv`
  (EMMS), texlive + texlive-langchinese (org LaTeX export), and the fonts listed in `README.org`
  (pacman package names).
- AI setup (`init-ai.el`, gptel): backends for ChatGPT, Deepseek, and Xiaomi MIMO; default model
  `mimo-v2.6-flash`. API keys come from auth-source — never hardcode keys in config files.
- gptel-agent sub-agents live in `agents/*.md` (tracked): YAML front-matter + system prompt.  Plain
  YAML scalars cannot contain `: ` (use `description: >`), and `backend:` is not parsed into a
  backend object — use `model:` (models of the current backend) or `gptel-agent-preset`.  One
  malformed file aborts registration of everything scanned after it, hence the `condition-case`
  around `gptel-agent-update` in `init-ai.el`.
- gptel config in `init-ai.el` is layered: L0 backends → L1 model profiles (`llm-fast|strong|
  reason|cheap`) → L2 personas (`gptel-directives`, generic voices only) → L3 task presets
  (`:parents` + inline `:system` + tool whitelist).  Sub-agents live in `agents/*.md` as category
  agents mirroring the preset task categories: `coder` (coding), `analyst` (explain/debugging/review,
  read-only), `tech-researcher` (research), `writer` (writer/translate).  Presets are the
  in-session single-step variant of a task, agents the autonomous multi-step variant; names may
  repeat (`writer`).  Rules: no sampling params in backend `:request-params` (they override
  `gptel-temperature` and preset `:request-params` — `gptel--merge-plists` gives later plists
  precedence); agent files must not use `:parents` (overridden by `gptel-agent-preset`), use
  explicit `backend:` + `model:` pairs instead; if a prompt ever moves into `gptel-directives`,
  its name must not be an existing function name (`debug` is one — `:system 'debug` silently
  skips the directive lookup).
