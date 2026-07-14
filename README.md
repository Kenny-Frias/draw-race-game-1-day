# draw-race-game-1-day
JSIP 1 Day Project

# QUICKDRAW — multiplayer drawing game

2–15 players get the same random word, draw it on an 80×60 pixel grid against
a shared timer (host picks the round length in the lobby, 0:30–5:00) (no live view of opponents), then rate each other's
drawings 1–5 stars. Score = aggregated star points (10 per star) + speed bonus
(seconds left when you submitted).

Drawing tools: a single PEN with a 1–5 thickness slider, flood FILL, ERASE,
UNDO, CLEAR, and a 15-color palette (full rainbow + basics). Results open
with the design's winner-reveal animation: drumroll + beeps, the #1 card
popping in under falling confetti, runner-up rows sliding in, and a REPLAY
button.

For testing at high player counts: `./bots.sh 14` fills the lobby with
passive bots that draw random shapes and rate randomly (`./bots.sh stop`
to dismiss). Join first if you want to be host.

Built from the `design_handoff_quickdraw` storyboard: all-OCaml, playable in
the browser from a single link.

## Architecture

```
shared/   protocol.ml   message types + game constants + design tokens,
                        sexp-serialized; compiled into BOTH sides so the
                        wire format can't drift
server/   server.ml     authoritative game server (cohttp-async +
                        cohttp_async_websocket): serves the client page/JS
                        and runs the state machine
                        Lobby -> WordReveal+Drawing -> Voting -> Results
client/   client.ml     the six screens, input handling, websocket
          draw.ml       Graphics-style primitives (fill_rect, draw_string,
                        shadow boxes) over an 800×600 canvas, compiled to
                        JS with js_of_ocaml
test/     bot.ml        end-to-end test: N websocket bots play a full round
```

One OCaml server binary serves everything; players just open the URL, enter a
name, and appear in the lobby. The wire protocol is s-expressions over a
single websocket: `join / ready / start / submit / rate / results`.

## Build & run

```sh
./serve.sh 8080        # dune build --profile release + start with auto-restart
```

then open `http://<host>:8080/`. The host (first player to join) starts the
round once 2+ players are ready.

## Test

```sh
dune build --profile release
./_build/default/test/bot.exe -port 8080 -n 3   # full round over real websockets
```

## Design

See `design_handoff_quickdraw/README.md` (in the design zip) for the full
handoff. Tokens: cream `#F5EAD8`, ink `#201E1D`, accent `#C67139`, sage
`#7A8A5E`; 2px ink borders with hard +3/+3 offset shadows; monospace
everywhere; the timer is the loudest element on the drawing screen.

Out of scope (per handoff): reconnect/rehydration, auth, live relay of
drawings mid-round. Sabotage + easter-egg layers were cut (optional in the
handoff).
