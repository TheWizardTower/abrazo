#!/usr/bin/env bash
set -euo pipefail

echo "=== Installing OpenCode ==="
curl -fsSL https://opencode.ai/install | bash

echo "=== Installing MCP servers ==="
uv tool install --upgrade serena-agent
uv tool install --upgrade kubernetes-mcp-server
uv tool install --upgrade postgres-mcp

echo "=== Installing opencode shell strategy ==="
if [ -d ~/.config/opencode/plugin/shell-strategy ]; then
  pushd ~/.config/opencode/plugin/shell-strategy >/dev/null
  git pull --ff-only
  popd >/dev/null
else
  git clone https://github.com/JRedeker/opencode-shell-strategy.git ~/.config/opencode/plugin/shell-strategy
fi

echo "=== Setting up OpenCode config ==="
mkdir -p ~/.config/opencode
ln -sf ~/abrazo/opencode/opencode.json ~/.config/opencode/opencode.json

# ========================
# Daniel's Skill Repos
# ========================
echo "=== Installing Daniel's skill repositories ==="

SKILL_REPOS=(
  "https://github.com/pasunboneleve/a-philosophy-of-software-design-skills.git"
  "https://github.com/pasunboneleve/oiticica-style.git"
  "https://github.com/pasunboneleve/skills.git"
)

mkdir -p ~/git

for repo_url in "${SKILL_REPOS[@]}"; do
  repo_name=$(basename "$repo_url" .git)
  target_dir="$HOME/git/$repo_name"

  if [ -d "$target_dir" ]; then
    echo "Updating $repo_name..."
    pushd "$target_dir" >/dev/null
    git pull --ff-only
    popd >/dev/null
  else
    echo "Cloning $repo_name..."
    git clone "$repo_url" "$target_dir"
  fi

  # Run the link script if it exists
  if [ -f "$target_dir/scripts/link_skills.sh" ]; then
    echo "Running link_skills.sh for $repo_name..."
    pushd "$target_dir" >/dev/null
    bash scripts/link_skills.sh
    popd >/dev/null
  else
    echo "Warning: No link_skills.sh found in $repo_name"
  fi
done

echo "=== OpenCode + skills setup complete! ==="
echo "You can now start OpenCode and use skills like 'apsd-deep-modules', 'oiticica-concision', etc."
