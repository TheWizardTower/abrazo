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
  argc \
  bat \
  caargo-outdated \
  cargo-llvm-cov \
  code2prompt \
  erdtree \
  exa \
  fd-find \
  flamegraph \
  git-delta \
  leadr \
  mergiraf \
  nu \
  oha \
  pier \
  procs \
  ripgrep \
  sd \
  tealdeer \
  tenere \
  tokio-console
  tre \
  varisat-cli \
  xh \
  yj \
  zoxide \
  "

export LOCKED_PACKAGE_LIST="
  bacon-ls \
  difftastic \
  jj \
  jj-cli \
  zellij \
"

for package in $PACKAGE_LIST; do
  cargo install $package
done

for package in $LOCKED_PACKAGE_LIST; do
  cargo install --locked $package
done
