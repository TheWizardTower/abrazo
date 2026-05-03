#!/usr/bin/env bash
# Heroic pre-launch: restore save for the game about to launch.
# Heroic exposes HEROIC_GAME_TITLE; we use flatpak-spawn to escape the
# Heroic sandbox and invoke the host's Ludusavi flatpak.
# Failures are non-fatal — never block a game launch.
set +e
TITLE="${HEROIC_GAME_TITLE:-}"
[ -z "$TITLE" ] && exit 0
echo "[$(date '+%H:%M:%S')] pre-launch  $TITLE" >>"$HOME/.local/state/heroic-saves.log"
flatpak-spawn --host flatpak run com.github.mtkennerly.ludusavi \
    restore --force "$TITLE" >>"$HOME/.local/state/heroic-saves.log" 2>&1
exit 0
