#!/usr/bin/env bash
# nc-unlock.sh — clear stale Nextcloud flatpak lockfile + runtime dir
# so the GUI can relaunch after an unclean exit.

set -eu

APP="com.nextcloud.desktopclient.nextcloud"

rm -f "$HOME/.var/app/${APP}/cache/tmp/kdsingleapp-merlin-3-nextcloud.lock"
rm -rf "/run/user/$(id -u)/.flatpak/${APP}"
echo "cleaned"
