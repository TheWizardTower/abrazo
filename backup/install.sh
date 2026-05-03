#!/usr/bin/env bash
# install.sh — symlink the personal-backup systemd user units into
# ~/.config/systemd/user/ and enable the timer.
# Idempotent.

set -euo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
TARGET="$HOME/.config/systemd/user"

mkdir -p "$TARGET"

for f in personal-backup.service personal-backup.timer; do
    ln -sfv "$HERE/$f" "$TARGET/$f"
done

systemctl --user daemon-reload
systemctl --user enable --now personal-backup.timer

echo
echo "Timer state:"
systemctl --user list-timers personal-backup.timer --no-pager
