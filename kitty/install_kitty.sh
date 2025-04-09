#!/usr/bin/env bash

set -euo pipefail

OS=$(uname | tr 'A-Z' 'a-z')

if [ ! -e "{HOME}/.config/kitty/font.conf" ]; then
	ln --symbolic "$(pwd)/font-${OS}.conf" "${HOME}/.config/kitty/font.conf"
fi

export fileList=("current-theme" "kitty" "ligatures")

mkdir -p ~/.config/kitty/


for file in ${fileList[@]}; do
	echo "File: ${file}"

	target="${HOME}/.config/kitty/${file}.conf"
	link="$(pwd)/${file}.conf"
	echo "Target:${target}"
	echo "Link:   ${link}"
	if [ ! -e "${target}" ]; then
	    ln --symbolic "${link}" "${target}"
	fi
done

