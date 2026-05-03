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
  ast-grep
  atac
  bacon
  bandwhich
  bat
  bluetui
  bookrat
  cargo-binstall
  cargo-llvm-cov
  cargo-outdated
  cargo-selector
  carl
  code2prompt
  crates-tui
  darya
  diskonaut
  dotstate
  du-dust
  erdtree
  eva
  eza
  fclones
  fd-find
  fdupes
  flamegraph
  git-delta
  hoard-rs
  igrep
  jaq
  jjj
  jnv
  jsongrep
  kmon
  leadr
  llmfit
  mcp-cli
  mergiraf
  mprocs
  navi
  netwatch-tui
  nu
  oha
  oyo
  pier
  pik
  procs
  projectable
  purple-ssh
  repgrep
  rga
  ripgrep
  rustlens
  sd
  smartcat
  ssh-list
  tealdeer
  tenere
  tokio-console
  tre
  varisat-cli
  viddy
  xh
  yj
  zeitfetch
  zf
  zizmor
  zoxide
)

LOCKED_PACKAGE_LIST=(
  bacon-ls
  cargo-audit
  cargo-geiger
  cargo-seek
  difftastic
  filessh
  flamelens
  gitu
  jj-cli
  pueue
  zellij
)

BINSTALL_PACKAGES=(
  srgn
)

GIT_REPOS=(
  'https://github.com/bvaisvil/zenith.git'
  'https://github.com/quantumsheep/sshs'
)

cargo install "${PACKAGE_LIST[@]}"
cargo install --locked "${LOCKED_PACKAGE_LIST[@]}"
cargo binstall "${BINSTALL_PACKAGES[@]}"
cargo install --git "${GIT_REPOS[@]}"
cargo install parallel-disk-usage --bin pdu
