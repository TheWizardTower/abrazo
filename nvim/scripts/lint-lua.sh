#!/bin/bash
set -euo pipefail

# Lint all Lua files in nvim/ directory
echo "Linting Neovim Lua configuration..."

cd "$(git rev-parse --show-toplevel)/nvim"

# Run luacheck with our config
luacheck . \
    --config .luacheckrc \
    --exclude lazy-lock \
    --max-line-length 120

echo "✅ All Lua files passed linting!"
