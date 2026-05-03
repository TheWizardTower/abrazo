#!/usr/bin/env bash
# personal-backup.sh — rsync atuin, KWallet, and user fonts into Nextcloud.
# Each source is mirrored under ~/Nextcloud/personal-backup/<hostname>/<name>/
# so multiple machines don't overwrite each other.
#
# Idempotent. Safe to run unattended. Sources missing on this machine are
# skipped, not errors.

set -euo pipefail

DEST_BASE="$HOME/Nextcloud/personal-backup/$(hostname -s)"
mkdir -p "$DEST_BASE"

backup() {
    local name="$1" src="$2"
    if [ ! -e "$src" ]; then
        echo "skip $name (source missing: $src)"
        return 0
    fi
    local dest="$DEST_BASE/$name"
    mkdir -p "$dest"
    rsync -a --delete "$src/" "$dest/"
    echo "ok   $name -> $dest"
}

backup atuin-data    "$HOME/.local/share/atuin"
backup atuin-config  "$HOME/.config/atuin"
backup kwallet       "$HOME/.local/share/kwalletd"
backup fonts         "$HOME/.local/share/fonts"

echo "done at $(date '+%F %T')"
