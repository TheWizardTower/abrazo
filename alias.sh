#!/usr/bin/env bash
set -euo pipefail

alias proot='while (! test -d ./.git) do cd ../; done'

if which flatpak && ! which nvim; then
	alias nvim="flatpak run io.neovim.nvim"
fi

