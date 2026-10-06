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

- Git tracks only `init.el`, `config/*.el`, `README.org`, `logo.txt`, `.gitignore`, `LICENSE`.
  Everything else at top level (`custom.el`, `recentf`, `places`, `history`, `elpa/`, `transient/`,
  …) is machine state and gitignored — never commit it.
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
