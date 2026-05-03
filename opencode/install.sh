#!/usr/bin/env bash

# Install opencode
curl -fsSL https://opencode.ai/install | bash

# Install MCP servers referenced by opencode.json mcp config
uv tool install --upgrade serena-agent
uv tool install --upgrade kubernetes-mcp-server
uv tool install --upgrade postgres-mcp

# Install opencode shell strategy

if [ -d ~/.config/opencode/plugin/shell-strategy ]; then
  pushd ~/.config/opencode/plugin/shell-strategy || exit
  git pull
  popd || exit
else
  git clone https://github.com/JRedeker/opencode-shell-strategy.git ~/.config/opencode/plugin/shell-strategy
fi

ln -s ~/.config/opencode ~/abrazo/opencode
