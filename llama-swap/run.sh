#!/usr/bin/env bash
# Run llama-server from the llama-rocm container for smoke testing.
# Usage: bash ./run.sh <model.gguf> [extra llama-server args...]
#   model.gguf:  path relative to $MODELS_DIR (default ~/models) OR an absolute path
# Env overrides:
#   MODELS_DIR   host dir mounted read-only at /models (default: $HOME/models)
#   PORT         host port to publish (default: 8080)
#   IMAGE        container image tag (default: localhost/llama-rocm)
#   NAME         container name (default: llama-rocm-smoke)

set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "usage: $0 <model.gguf> [extra args...]" >&2
  exit 1
fi

MODEL_ARG="$1"; shift
MODELS_DIR="${MODELS_DIR:-$HOME/models}"
PORT="${PORT:-8080}"
IMAGE="${IMAGE:-localhost/llama-rocm}"
NAME="${NAME:-llama-rocm-smoke}"

# Resolve the model path: if absolute, remount its dir; else assume under MODELS_DIR.
if [[ "$MODEL_ARG" = /* ]]; then
  host_dir=$(dirname "$MODEL_ARG")
  in_container="/models/$(basename "$MODEL_ARG")"
else
  host_dir="$MODELS_DIR"
  in_container="/models/$MODEL_ARG"
fi

if [[ ! -d "$host_dir" ]]; then
  echo "model dir does not exist: $host_dir" >&2
  exit 1
fi
if [[ ! -f "$host_dir/$(basename "$MODEL_ARG")" ]]; then
  echo "model file not found: $host_dir/$(basename "$MODEL_ARG")" >&2
  exit 1
fi

echo ">> mounting $host_dir -> /models (ro)"
echo ">> model: $in_container"
echo ">> port:  $PORT"

exec podman run --rm -it \
  --name "$NAME" \
  --device=/dev/kfd --device=/dev/dri \
  --security-opt seccomp=unconfined \
  --ipc=host \
  -v "$host_dir":/models:ro \
  -p "$PORT":8080 \
  "$IMAGE" \
  -m "$in_container" \
  --host 0.0.0.0 --port 8080 \
  -ngl 99 \
  "$@"
