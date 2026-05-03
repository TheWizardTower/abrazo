#!/usr/bin/env bash
# Heroic post-exit: backup save for the game that just closed.
set +e
TITLE="${HEROIC_GAME_TITLE:-}"
[ -z "$TITLE" ] && exit 0
echo "[$(date '+%H:%M:%S')] post-exit   $TITLE" >>"$HOME/.local/state/heroic-saves.log"
flatpak-spawn --host flatpak run com.github.mtkennerly.ludusavi \
    backup --force "$TITLE" >>"$HOME/.local/state/heroic-saves.log" 2>&1
exit 0
