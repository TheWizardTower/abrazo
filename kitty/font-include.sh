#!/usr/bin/env sh
os=$(uname | tr A-Z a-z)
[ "$os" = darwin ] && os=macos
echo "include font-$os.conf"
