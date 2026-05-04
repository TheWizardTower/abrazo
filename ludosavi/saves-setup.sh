#!/usr/bin/env bash
# saves-setup.sh — wire Heroic save-sync via Ludusavi + Nextcloud.
# Idempotent: safe to re-run on Framework, XPS, or Steam Deck.

set -euo pipefail

NEXTCLOUD_URL="https://nextcloud.home.lab"
NEXTCLOUD_DIR="$HOME/Nextcloud"
SAVES_DIR="$NEXTCLOUD_DIR/heroic-saves"
SCRIPTS_DIR="$HOME/.local/bin"
PRE_SCRIPT="$SCRIPTS_DIR/heroic-pre.sh"
POST_SCRIPT="$SCRIPTS_DIR/heroic-post.sh"
LUDUSAVI_FLATPAK="com.github.mtkennerly.ludusavi"
NEXTCLOUD_FLATPAK="com.nextcloud.desktopclient.nextcloud"
HEROIC_FLATPAK="com.heroicgameslauncher.hgl"
HEROIC_ROOT="$HOME/.var/app/${HEROIC_FLATPAK}/config/heroic"
LUDUSAVI_CONFIG_DIR="$HOME/.var/app/${LUDUSAVI_FLATPAK}/config/ludusavi"

say() { printf '\n==> %s\n' "$*"; }
warn() { printf '\n!!! %s\n' "$*" >&2; }

ensure_flatpak() {
    local id="$1"
    if flatpak list --app --columns=application 2>/dev/null | grep -qx "$id"; then
        echo "    [ok] $id already installed"
    else
        echo "    [install] $id"
        flatpak install -y --noninteractive flathub "$id"
    fi
}

# --- 1. Sanity: Heroic must be installed (we don't install it here) ---
if ! flatpak list --app --columns=application 2>/dev/null | grep -qx "$HEROIC_FLATPAK"; then
    warn "Heroic flatpak ($HEROIC_FLATPAK) not found. Install Heroic first, then re-run."
    exit 1
fi

# --- 2. Install Ludusavi (flatpak); Nextcloud client must already be installed ---
say "Ensuring Ludusavi flatpak installed"
ensure_flatpak "$LUDUSAVI_FLATPAK"

# Heroic needs permission to talk to org.freedesktop.Flatpak so its pre/post
# hooks can use `flatpak-spawn --host` to invoke Ludusavi on the host side.
say "Granting Heroic permission to use the Flatpak portal"
flatpak override --user --talk-name=org.freedesktop.Flatpak "$HEROIC_FLATPAK"

if dpkg -s nextcloud-desktop >/dev/null 2>&1; then
    echo "    [ok] nextcloud-desktop (apt) installed"
    NEXTCLOUD_LAUNCH_HINT="nextcloud"
elif flatpak list --app --columns=application 2>/dev/null | grep -qx "$NEXTCLOUD_FLATPAK"; then
    echo "    [ok] $NEXTCLOUD_FLATPAK (flatpak) installed"
    NEXTCLOUD_LAUNCH_HINT="flatpak run $NEXTCLOUD_FLATPAK"
else
    warn "Nextcloud client not installed. Recommended: sudo apt install nextcloud-desktop"
    warn "  (apt version trusts host CAs directly; flatpak version fights with Qt cert paths)"
    exit 1
fi

# --- 3. Nextcloud client signed in? ---
if [ ! -d "$NEXTCLOUD_DIR" ]; then
    cat <<EOF

==> Nextcloud client not yet configured on this machine.
    Sign-in is GUI-only; finishing up requires three clicks:

    1) Launch:  $NEXTCLOUD_LAUNCH_HINT
    2) Sign in to: $NEXTCLOUD_URL
    3) Accept default sync folder: $NEXTCLOUD_DIR
    4) Wait for first sync to settle, then re-run this script.

EOF
    exit 0
fi

# --- 4. Saves folder ---
mkdir -p "$SAVES_DIR"
say "Saves dir ready: $SAVES_DIR"

# --- 5. Ludusavi config: point at Heroic + Nextcloud-synced backup target ---
mkdir -p "$LUDUSAVI_CONFIG_DIR"
CONFIG_FILE="$LUDUSAVI_CONFIG_DIR/config.yaml"
cat >"$CONFIG_FILE" <<EOF
manifest:
  enable: true
roots:
  - path: $HEROIC_ROOT
    store: heroic
backup:
  path: $SAVES_DIR
  format:
    chosen: simple
restore:
  path: $SAVES_DIR
EOF
say "Wrote Ludusavi config: $CONFIG_FILE"

# --- 6. Heroic pre/post hooks (managed by install.sh as symlinks) ---
if [ ! -x "$PRE_SCRIPT" ] || [ ! -x "$POST_SCRIPT" ]; then
    warn "Heroic hook scripts not found at $SCRIPTS_DIR. Run install.sh first."
    exit 1
fi
mkdir -p "$HOME/.local/state"
say "Heroic hooks present:"
echo "    pre:  $PRE_SCRIPT"
echo "    post: $POST_SCRIPT"
echo "    log:  $HOME/.local/state/heroic-saves.log"

# --- 7. Final manual step + first-machine seed reminder ---
cat <<EOF

==> Setup mostly done. Two manual steps remain:

  (A) In Heroic UI:  Settings -> Defaults (or per-game Advanced) ->
      Pre-launch script:  $PRE_SCRIPT
      Post-exit script:   $POST_SCRIPT

  (B) ON THE FIRST MACHINE ONLY (the one with current saves you trust):
      Before you launch any game, seed the backup with everything currently
      on disk so other machines don't restore an empty/stale snapshot:

        flatpak run $LUDUSAVI_FLATPAK backup --force

      Watch for errors; saves should appear under $SAVES_DIR shortly after.

EOF
