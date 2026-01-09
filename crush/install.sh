#!/usr/bin/env bash

mkdir -p ~/.local/share/crush/
ln -s ~/abrazo/crush/crush.json ~/.local/share/crush/crush.json

# Install uv, if not present.
if [ ! "$(which uv)" ]; then
  curl -LsSf https://astral.sh/uv/install.sh | sh
fi
if [ ! "$(which mcp-k8s)" ]; then
  go install github.com/silenceper/mcp-k8s/cmd/mcp-k8s@latest
fi

mkdir -p ~/git
pushd ~/git
git clone https://github.com/gpetraroli/mcp_pdf_reader.git
git clone https://github.com/oraios/serena
pushd serena
uv tool install ./
popd
popd
