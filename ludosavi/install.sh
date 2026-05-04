#!/usr/bin/env bash
# install.sh — symlink the Heroic save-sync hook scripts into ~/.local/bin/
# and the Ludusavi config into the flatpak's config dir, so Heroic's launcher
# (which references absolute paths) and Ludusavi can find them.
# Run once per machine, then run saves-setup.sh.
#
# Idempotent: safe to re-run.

set -euo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
BIN_DIR="$HOME/.local/bin"
LUDUSAVI_CONFIG_DIR="$HOME/.var/app/com.github.mtkennerly.ludusavi/config/ludusavi"

mkdir -p "$BIN_DIR" "$LUDUSAVI_CONFIG_DIR"

link() {
    local src="$1" dst="$2"
    if [ ! -f "$src" ]; then
        echo "!!! missing source: $src" >&2
        exit 1
    fi
    ln -sfv "$src" "$dst"
}

for f in heroic-pre.sh heroic-post.sh; do
    link "$HERE/$f" "$BIN_DIR/$f"
done

link "$HERE/ludusavi-config.yaml" "$LUDUSAVI_CONFIG_DIR/config.yaml"

cat <<EOF

Symlinks installed. Next:
  bash "$HERE/saves-setup.sh"

EOF
