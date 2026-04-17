set shell := ["bash", "-euo", "pipefail", "-c"]

export SHELLCHECK_EXCLUDES := "SC1090,SC2148,SC2034"
export LINT_EXCLUDE_DIRS := "emacs.d|helix|SpaceVim.d|screen-bin|screenrc|xmonad"

# List recipes
default:
    @just --list

# Run every lint step (what CI runs)
all: actionlint just-fmt shell nu nvim terminals structured tool-configs emacs

# Check that the Justfile itself is formatted (apply with `just --unstable --fmt`).
just-fmt:
    just --unstable --fmt --check >/dev/null

# Validate .github/workflows/*.yml
actionlint:
    actionlint

# Collect shell scripts the way CI does
_shell-files:
    #!/usr/bin/env bash
    git ls-files '*.sh' 'bashrc' 'bash_profile' 'bin/*' 'install' \
      | grep -Ev "^($LINT_EXCLUDE_DIRS)/" \
      | while read -r f; do
          if [[ "$f" == *.sh || "$f" == bashrc || "$f" == bash_profile || "$f" == install ]]; then
            echo "$f"
          elif file "$f" | grep -qE 'Bourne-Again shell script|POSIX shell script'; then
            echo "$f"
          fi
        done

# bash -n + shellcheck over every bash/sh script
shell:
    #!/usr/bin/env bash
    set -euo pipefail
    mapfile -t files < <(just _shell-files)
    printf 'checking %d scripts\n' "${#files[@]}"
    printf '%s\n' "${files[@]}" | xargs -r -I{} bash -n "{}"
    printf '%s\n' "${files[@]}" | xargs -r shellcheck --severity=warning --external-sources -e "$SHELLCHECK_EXCLUDES"

# shfmt diff (informational — does not fail)
shfmt:
    #!/usr/bin/env bash
    mapfile -t files < <(just _shell-files)
    printf '%s\n' "${files[@]}" | xargs -r shfmt -d -i 2 -ci || true

# Parse-check nushell config + autoload, then source config.nu
nu:
    #!/usr/bin/env bash
    set -euo pipefail
    mkdir -p "$HOME/.local/share/atuin" "$HOME/.cache/starship" "$HOME/.cache/zoxide"
    [ -e "$HOME/.local/share/atuin/init.nu" ] || : > "$HOME/.local/share/atuin/init.nu"
    [ -e "$HOME/.cache/starship/init.nu" ] || : > "$HOME/.cache/starship/init.nu"
    [ -e "$HOME/.cache/zoxide/init.nu" ] || : > "$HOME/.cache/zoxide/init.nu"
    while IFS= read -r f; do
      echo "checking $f"
      nu -c "source $PWD/$f"
    done < <(git ls-files 'nushell/**/*.nu' | grep -v '^nushell/nupm/')
    nu -c 'source nushell/config.nu; print ok'

# luac -p (luajit) parse-check every nvim lua file
nvim:
    #!/usr/bin/env bash
    set -euo pipefail
    fail=0
    while IFS= read -r f; do
      luajit -bl "$f" > /dev/null || { echo "parse error in $f"; fail=1; }
    done < <(git ls-files 'nvim/**/*.lua' | grep -v '^nvim/lazy-lock')
    exit "$fail"

# stylua --check (informational)
stylua:
    stylua --check nvim/ || true

# tmux / kitty / zellij config parse smoke
terminals:
    #!/usr/bin/env bash
    set -euo pipefail
    tmux -f tmux.conf new-session -d -s just-lint 'sleep 1'
    tmux kill-session -t just-lint 2>/dev/null || true
    kitty +runpy 'from kitty.config import load_config; load_config("kitty/kitty.conf"); print("ok")' \
      > /tmp/kitty.out 2> /tmp/kitty.err
    grep -q '^ok$' /tmp/kitty.out
    if grep -Ei '(Invalid|Bad key|Unknown key)' /tmp/kitty.err; then
      cat /tmp/kitty.err; exit 1
    fi
    zellij --config-dir zellij setup --check > /dev/null

# yaml / toml / json / gitconfig
structured: yaml toml json gitconfig

yaml:
    #!/usr/bin/env bash
    # Filter out paths that no longer exist (pending jj/git deletions).
    while IFS= read -r -d '' f; do
      [ -f "$f" ] && printf '%s\0' "$f"
    done < <(git ls-files -z '*.yml' '*.yaml' | grep -zEv "^($LINT_EXCLUDE_DIRS)/") \
      | xargs -0 -r yamllint -d '{extends: relaxed, rules: {line-length: disable}}'

toml:
    #!/usr/bin/env bash
    set -euo pipefail
    while IFS= read -r f; do
      python3 -c "import tomllib,sys; tomllib.load(open(sys.argv[1],'rb'))" "$f"
    done < <(git ls-files '*.toml' | grep -Ev "^($LINT_EXCLUDE_DIRS)/")

json:
    #!/usr/bin/env bash
    set -euo pipefail
    fail=0
    while IFS= read -r f; do
      jq empty "$f" || { echo "invalid JSON: $f"; fail=1; }
    done < <(git ls-files '*.json' | grep -Ev "^($LINT_EXCLUDE_DIRS|node_modules)/")
    exit "$fail"

gitconfig:
    git config --file gitconfig --list > /dev/null

# Emacs init smoke — strict, fails on any *Warnings* content. Uses the system `emacs` (must be 30.x).
emacs:
    #!/usr/bin/env bash
    set -euo pipefail
    mkdir -p "$HOME/org-roam"
    emacs --batch \
      --init-directory="$PWD/emacs.d" \
      -l "$PWD/emacs.d/early-init.el" \
      -l "$PWD/emacs.d/init.el" \
      -l "$PWD/emacs.d/ci/smoke.el"

# eslint + mypy config parse
tool-configs:
    #!/usr/bin/env bash
    set -euo pipefail
    if command -v eslint >/dev/null; then
      echo 'const x = 1;' > /tmp/dummy.js
      eslint --config eslintrc.js --print-config /tmp/dummy.js > /dev/null
    else
      echo "eslint not installed — skipping"
    fi
    if command -v mypy >/dev/null; then
      echo 'x: int = 1' > /tmp/dummy.py
      mypy --config-file mypy.ini /tmp/dummy.py
    else
      echo "mypy not installed — skipping"
    fi
