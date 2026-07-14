#!/usr/bin/env bash
# Summon (or dismiss) companion bots to fill the lobby for testing.
#   ./bots.sh 14      start 14 bots (join as passive players, draw random
#                     shapes, rate randomly, keep playing until killed;
#                     self-expire after 2h)
#   ./bots.sh stop    kill the fleet
set -euo pipefail
cd "$(dirname "$0")"
if [ "${1:-}" = "stop" ]; then
  pkill -f "bot.exe .*-companion" 2>/dev/null && echo "bot fleet dismissed" || echo "no bots running"
  exit 0
fi
N="${1:-14}"
nohup ./_build/default/test/bot.exe -port 8080 -companion -n "$N" >> bots.log 2>&1 &
echo "$N companion bots joining (join the game FIRST if you want to be host; ./bots.sh stop to dismiss)"
