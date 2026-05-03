#!/usr/bin/env bash
# install.sh — symlink the Heroic save-sync hook scripts into ~/.local/bin/
# so Heroic's launcher (which references absolute paths in its config) can
# find them. Run once per machine, then run saves-setup.sh.
#
# Idempotent: safe to re-run.

set -euo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
TARGET_DIR="$HOME/.local/bin"

mkdir -p "$TARGET_DIR"

for f in heroic-pre.sh heroic-post.sh; do
    src="$HERE/$f"
    dst="$TARGET_DIR/$f"
    if [ ! -f "$src" ]; then
        echo "!!! missing source: $src" >&2
        exit 1
    fi
    ln -sfv "$src" "$dst"
done

cat <<EOF

Symlinks installed. Next:
  bash "$HERE/saves-setup.sh"

EOF
