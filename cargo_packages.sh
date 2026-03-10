#!/usr/bin/env bash
# We actually don't want this, if one package fails, as it might if it is
# already installed, please carry on to the remaining ones.
# set -euo pipefail

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
  cargo-outdated \
  cargo-llvm-cov \
  code2prompt \
  fclones \
  fdupes \
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

# jj and jj-cli have to be in the same invocation, because reasons.
export LOCKED_PACKAGE_LIST="
  bacon-ls \
  cargo-audit \
  difftastic \
  jj jj-cli \
  zellij \
"

for package in $PACKAGE_LIST; do
  cargo install "$package"
done

for package in $LOCKED_PACKAGE_LIST; do
  cargo install --locked "$package"
done
