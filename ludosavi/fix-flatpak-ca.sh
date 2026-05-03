#!/usr/bin/env bash
# fix-flatpak-ca.sh — inject the home-lab CA into the Nextcloud flatpak.
# Builds a combined bundle (runtime CAs + home-lab CA) and points the
# Nextcloud flatpak at it via SSL_CERT_FILE.

set -euo pipefail

APP="com.nextcloud.desktopclient.nextcloud"
LOCAL_CA="/home/merlin/Downloads/ca.crt"
BUNDLE_DIR="$HOME/.var/app/${APP}/data/certs"
BUNDLE_FILE="${BUNDLE_DIR}/bundle.crt"
SANDBOX_BUNDLE_PATH="/var/data/certs/bundle.crt"

if [ ! -f "$LOCAL_CA" ]; then
  echo "!!! local CA not at $LOCAL_CA — adjust path and re-run" >&2
  exit 1
fi

mkdir -p "$BUNDLE_DIR"

# Use the runtime's own CA bundle as base (it has more roots than the host's),
# then append our home-lab CA.
flatpak run --command=cat "$APP" /etc/ssl/certs/ca-certificates.crt >"$BUNDLE_FILE"
cat "$LOCAL_CA" >>"$BUNDLE_FILE"

flatpak override --user \
  --env="SSL_CERT_FILE=${SANDBOX_BUNDLE_PATH}" \
  "$APP"

echo "==> Combined bundle:  $BUNDLE_FILE  ($(wc -l <"$BUNDLE_FILE") lines)"
echo "==> Override applied. Restart the Nextcloud client (kill + relaunch)."
echo "    Verify with:  flatpak run --command=printenv $APP SSL_CERT_FILE"
