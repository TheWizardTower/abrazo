#!/usr/bin/env bash
set -euo pipefail

if ! which cargo; then
  curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
else
  echo "Cargo is already installed."
  rustup upgrade
fi

export PACKAGE_LIST="
  aichat \
  bat \
  code2prompt \
  erdtree \
  exa \
  fd-find \
  leadr \
  nu \
  procs \
  ripgrep \
  sd \
  tealdeer \
  tenere \
  tre \
  xh \
  yj \
  zoxide \
  "

for package in $PACKAGE_LIST; do
  cargo install $package
done
