#!/usr/bin/env bash
# Build and (re)start the QUICKDRAW server on port 8080.
# Usage: ./serve.sh [port]
set -euo pipefail
cd "$(dirname "$0")"
PORT="${1:-8080}"

eval "$(opam env)" 2>/dev/null || true
dune build --profile release
mkdir -p static
install -m 644 _build/default/client/client.bc.js static/client.js

pkill -f "server.exe -port $PORT" 2>/dev/null || true
sleep 0.5

# keep the server alive across crashes; log to server.log
nohup bash -c "while true; do ./_build/default/server/server.exe -port $PORT -static static >> server.log 2>&1; echo \"server exited, restarting...\" >> server.log; sleep 1; done" > /dev/null 2>&1 &

sleep 1
curl -sf -o /dev/null "http://127.0.0.1:$PORT/" && echo "QUICKDRAW serving on port $PORT"
