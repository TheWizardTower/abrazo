#!/usr/bin/env bash
set -euo pipefail

export REPO=https://github.com/neovim/nvim-lspconfig
export LOCAL_PATH="pack/nvim/start/nvim-lspconfig"

export STANDARD_CONFIG_DIR="${HOME}/.config/"
export FLATPAK_CONFIG_DIR="${HOME}/.var/app/io.neovim.nvim/config/"

if $(which nvim); then
    pushd "${STANDARD_CONFIG_DIR}"
else
    pushd "${FLATPAK_CONFIG_DIR}"
fi

pushd ~/.var/app/io.neovim.nvim/config/

# nvim exists; and is a symlink. This is the happy path, continue on.
if [ -L nvim ]; then
  echo "Symlink already created, continuing."
# nvim exists, is a standard dir
elif [ -d nvim ]; then
  UNIX_TIME=$(date '+%s')
  mv nvim "nvim_${UNIX_TIME}"
  ln -s ~/abrazo/nvim "$(pwd)/nvim"
fi

pushd nvim
if [ -d "${LOCAL_PATH}" ]; then
	pushd "${LOCAL_PATH}"
	git pull
	popd
else
	git clone "${REPO}" "${LOCAL_PATH}"
fi
popd

popd
