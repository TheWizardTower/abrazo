#!/usr/bin/env bash
# We don't fail on individual install errors — packages may already be
# installed at the requested version, and we want to continue.
# set -euo pipefail

if ! command -v cargo >/dev/null; then
  curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
else
  echo "Cargo is already installed."
  rustup update
fi

PACKAGE_LIST=(
  argc
  bacon
  bat
  cargo-llvm-cov
  cargo-outdated
  code2prompt
  erdtree
  eza
  fclones
  fd-find
  fdupes
  flamegraph
  git-delta
  leadr
  mergiraf
  mprocs
  netwatch-tui
  nu
  oha
  pier
  procs
  ripgrep
  sd
  tealdeer
  tenere
  tokio-console
  tre
  varisat-cli
  xh
  yj
  zoxide
)

# jj and jj-cli have to be in the same invocation, because reasons.
LOCKED_PACKAGE_LIST=(
  bacon-ls
  cargo-audit
  difftastic
  jj jj-cli
  zellij
)

cargo install "${PACKAGE_LIST[@]}"
cargo install --locked "${LOCKED_PACKAGE_LIST[@]}"
