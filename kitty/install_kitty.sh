#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

export fileList=("current-theme" "kitty" "ligatures" "font-linux" "font-macos")

mkdir -p ~/.config/kitty/


for file in "${fileList[@]}"; do
	echo "File: ${file}"

	target="${HOME}/.config/kitty/${file}.conf"
	link="${SCRIPT_DIR}/${file}.conf"
	echo "Target:${target}"
	echo "Link:   ${link}"
	if [ ! -e "${target}" ]; then
	    ln --symbolic "${link}" "${target}"
	fi
done

if [ ! -e "${HOME}/.config/kitty/font-include.sh" ]; then
	ln --symbolic "${SCRIPT_DIR}/font-include.sh" "${HOME}/.config/kitty/font-include.sh"
fi
