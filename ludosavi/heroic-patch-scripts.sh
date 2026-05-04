#!/usr/bin/env bash
# heroic-patch-scripts.sh — backfill pre/post script paths into all
# existing per-game Heroic configs. Heroic only copies its global default
# scripts into a game's config at install time, so games installed before
# you set the defaults are stuck with empty values.
#
# Idempotent. Skips entries that already have a non-empty value (so any
# per-game customization survives re-runs). Pass --force to overwrite.

set -euo pipefail

PRE="$HOME/.local/bin/heroic-pre.sh"
POST="$HOME/.local/bin/heroic-post.sh"
GAMES_CFG="$HOME/.var/app/com.heroicgameslauncher.hgl/config/heroic/GamesConfig"

FORCE=0
[ "${1:-}" = "--force" ] && FORCE=1

if pgrep -f 'com.heroicgameslauncher.hgl' >/dev/null; then
    echo "!!! Heroic is currently running. Close it before patching, or it'll overwrite our changes."
    exit 1
fi

if [ ! -d "$GAMES_CFG" ]; then
    echo "!!! Heroic GamesConfig dir not found: $GAMES_CFG" >&2
    exit 1
fi

python3 - "$GAMES_CFG" "$PRE" "$POST" "$FORCE" <<'PY'
import json, sys, os, glob

games_dir, pre, post, force_str = sys.argv[1:]
force = force_str == "1"

patched = skipped = touched_files = 0
for f in sorted(glob.glob(os.path.join(games_dir, '*.json'))):
    try:
        cfg = json.load(open(f))
    except Exception as e:
        print(f'  [skip] {os.path.basename(f)}: parse error {e}'); continue
    file_changed = False
    for k, v in cfg.items():
        if not isinstance(v, dict): continue
        for key, val in (('beforeLaunchScriptPath', pre), ('afterLaunchScriptPath', post)):
            cur = v.get(key, '')
            if force or cur == '' or cur is None:
                if cur != val:
                    v[key] = val
                    patched += 1
                    file_changed = True
            else:
                skipped += 1
    if file_changed:
        with open(f, 'w') as out:
            json.dump(cfg, out, indent=2)
        touched_files += 1

print(f'patched {patched} fields across {touched_files} files; skipped {skipped} (already set; use --force to overwrite)')
PY
