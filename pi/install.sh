#!/bin/bash

set -euo pipefail

PI_DIR="$HOME/.pi/agent"
ABRAZO_PI_DIR="$HOME/abrazo/pi"

echo "Installing pi configuration from abrazo..."

# Create the target directory if it doesn't exist
mkdir -p "$PI_DIR/extensions"

# Copy auth.json
if [ -f "$ABRAZO_PI_DIR/auth.json" ]; then
  cp "$ABRAZO_PI_DIR/auth.json" "$PI_DIR/auth.json"
  chmod 600 "$PI_DIR/auth.json"
  echo "✓ Copied auth.json"
else
  echo "⚠ No auth.json found in abrazo/pi, skipping"
fi

# Copy extensions
if [ -d "$ABRAZO_PI_DIR/extensions" ]; then
  cp -r "$ABRAZO_PI_DIR/extensions/"* "$PI_DIR/extensions/"
  echo "✓ Copied extensions"
else
  echo "⚠ No extensions directory found in abrazo/pi, skipping"
fi

echo ""
echo "Installation complete!"
echo ""
echo "To use, set your API key environment variable:"
echo '  export LLAMA_SWAP_API_KEY=dummy'
echo ""
echo "Then run pi with the llama-swap provider:"
echo '  pi --provider llama-swap --model Qwen3-Coder-Next-UD-Q8'
