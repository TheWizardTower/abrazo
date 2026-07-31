#!/usr/bin/env bash
set -euo pipefail

echo "=== Ensuring krew is installed ==="

if ! command -v kubectl-krew &>/dev/null && ! kubectl krew version &>/dev/null 2>&1; then
  echo "Installing krew..."
  (
    set -x
    cd "$(mktemp -d)"
    OS="$(uname | tr '[:upper:]' '[:lower:]')"
    ARCH="$(uname -m | sed -e 's/x86_64/amd64/' -e 's/\(arm\)\(64\)\?.*/\1\2/' -e 's/aarch64$/arm64/')"
    KREW="krew-${OS}_${ARCH}"
    curl -fsSLO "https://github.com/kubernetes-sigs/krew/releases/latest/download/${KREW}.tar.gz"
    tar zxvf "${KREW}.tar.gz"
    ./"${KREW}" install krew
  )
  # Make sure krew is on PATH for this session
  export PATH="${KREW_ROOT:-$HOME/.krew}/bin:$PATH"
else
  echo "krew already available."
fi

# Ensure PATH includes krew for the rest of the script
export PATH="${KREW_ROOT:-$HOME/.krew}/bin:$PATH"

echo ""
echo "=== Updating krew index ==="
kubectl krew update

echo ""
echo "=== Installing plugins (alphabetical order) ==="

# Your list + a few highly recommended ones.
# Comment out any you don't want.
PLUGINS=(
  access-matrix
  atlas
  cert-manager
  cleaner
  clog
  cnpg
  ctx
  deprecations
  doctor
  get-all
  ice
  images
  ktop
  neat
  node-ssm
  ns
  open-svc
  outdated
  pod-lens
  rbac-tool
  reap
  resource-capacity
  rolesum
  score
  sort-manifests
  starboard
  stern
  tree
  validate
  view-allocations
  view-secret
  viewnode
  vigil
  who-can
  whoami
)

for plugin in "${PLUGINS[@]}"; do
  echo "→ Installing ${plugin}..."
  if kubectl krew install "${plugin}" 2>/dev/null; then
    echo "  ✓ ${plugin} installed (or already present)"
  else
    echo "  ⚠ Could not install ${plugin} (may not exist in index or already installed)"
  fi
done

echo ""
echo "=== Installed kubectl plugins (via krew) ==="
kubectl krew list

echo ""
echo "=== Done ==="
echo "Remember to keep ~/.krew/bin in your PATH:"
echo '  export PATH="${KREW_ROOT:-$HOME/.krew}/bin:$PATH"'
echo ""
echo "KubeVigil MCP note:"
echo "  Use:  kubevigil mcp-server   or   kubectl-vigil mcp-server"
echo "  (not 'kubectl vigil mcp-server' under OpenCode)"
