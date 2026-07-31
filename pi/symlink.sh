#!/bin/bash

set -euo pipefail

PI_DIR="$HOME/.pi/agent"
ABRAZO_PI_DIR="$HOME/abrazo/pi"

echo "Symlinking pi config from abrazo..."

# Create the target directory if it doesn't exist
mkdir -p "$PI_DIR/extensions"

# Remove existing files/dirs (with explicit paths)
if [ -L "$PI_DIR/auth.json" ] || [ -f "$PI_DIR/auth.json" ]; then
  rm -f "$PI_DIR/auth.json"
  echo "✓ Removed existing auth.json"
fi

if [ -d "$PI_DIR/extensions/llama-swap.ts" ] || [ -f "$PI_DIR/extensions/llama-swap.ts" ]; then
  rm -f "$PI_DIR/extensions/llama-swap.ts"
  echo "✓ Removed existing llama-swap.ts extension"
fi

# Create symlinks
ln -sf "$ABRAZO_PI_DIR/auth.json" "$PI_DIR/auth.json"
echo "✓ Symlinked auth.json -> $ABRAZO_PI_DIR/auth.json"

ln -sf "$ABRAZO_PI_DIR/extensions/llama-swap.ts" "$PI_DIR/extensions/llama-swap.ts"
echo "✓ Symlinked llama-swap.ts -> $ABRAZO_PI_DIR/extensions/llama-swap.ts"

echo ""
echo "Symlinks created successfully!"
echo ""
echo "Run 'pi --reload' or restart pi to apply changes."
