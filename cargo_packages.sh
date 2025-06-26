#!/usr/bin/env bash
set -euo pipefail


if ! which cargo; then
	curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
else
        echo "Cargo is already installed."
        rustup upgrade
fi

export PACKAGE_LIST="
  bat \
  erdtree \
  exa \
  fd-find \
  nu \
  procs \
  ripgrep \
  sd \
  tealdeer \
  tre \
  xh \
  zoxide \
  "

for package in $PACKAGE_LIST; do
	cargo install $package
done
