(* Shared protocol + game model for QUICKDRAW.
   Compiled into both the native server and the js_of_ocaml client, so the
   wire format (sexp strings over one websocket) can never drift. *)

open Sexplib0.Sexp_conv

(* -- Config constants (the design's "grid 80x60 - cell 8px" caption derives
      from these) -- *)

let cell_px = 8
let grid_cols = 640 / cell_px (* 80 *)
let grid_rows = 480 / cell_px (* 60 *)
let max_players = 8
let min_players = 2
let round_seconds = 90
let countdown_seconds = 3

(* easter-egg layer *)
let egg_points = 50 (* paint the hidden jackpot cell *)
let bonus_points = 10 (* first to paint with the broadcast bonus color *)
let wipe_count = 40 (* cells destroyed by the cursed-cell wipe *)
let bonus_period_s = 25 (* a fresh bonus color this often *)

(* Colors are carried as 0xRRGGBB ints, same convention as Graphics.rgb. *)

type color = int [@@deriving sexp]

let white = 0xFFFFFF

(* Grid serialized as a flat array, row-major: cols * rows ints. *)
type grid = color array [@@deriving sexp]

let empty_grid () = Array.make (grid_cols * grid_rows) white

type conn_state =
  | Connecting
  | Ready
[@@deriving sexp]

type player =
  { id : int
  ; name : string
  ; is_host : bool
  ; conn : conn_state
  }
[@@deriving sexp]

type submission =
  { player_id : int
  ; player_name : string
  ; grid : grid
  ; seconds_left : int (* banked speed bonus *)
  }
[@@deriving sexp]

type score_line =
  { s_player_id : int
  ; s_name : string
  ; votes : int
  ; speed : int
  ; egg : int (* jackpot secret cell *)
  ; bonus : int (* bonus-color claims *)
  ; total : int
  }
[@@deriving sexp]

(* the two hidden cells seeded each round *)
type secret_kind =
  | Jackpot
  | Cursed
[@@deriving sexp]

(* Client -> server *)
type client_msg =
  | Join of string (* requested name *)
  | Set_ready of bool
  | Start_round (* host only *)
  | Submit of grid * int (* grid, seconds left at submit *)
  | Rank of (int * int) list (* (player_id, rank 1..n-1) for each opponent *)
  | Next_round (* host, from results *)
  | Back_to_lobby (* host, from results *)
  | Lock_color of int * color (* sabotage: lock this color on that player *)
  | Hit_secret of secret_kind (* my paint just landed on a secret cell *)
  | Curse_wipe of int (* cursed-cell reward: wipe this player's cells *)
  | Claim_bonus (* I painted with the active bonus color *)
[@@deriving sexp]

(* Server -> client *)
type server_msg =
  | Joined of int (* your player id *)
  | Join_refused of string
  | Lobby of player list (* roster update; not a screen change *)
  | Go_lobby (* host sent everyone back to the lobby screen *)
  | Word_reveal of string * float (* word, draw deadline (unix epoch, after countdown) *)
  | Drawing_over (* timer hit zero: force submit *)
  | Vote_now of string * submission list (* word, everyone's drawings *)
  | Results of score_line list
  | Lock_offer of (int * string) list
    (* you submitted first: opponents (id, name) you may lock a color on *)
  | Color_locked of string * color
    (* locker's name, color unusable for the rest of the round *)
  | Secret_cells of int * int
    (* jackpot idx, cursed idx (flat grid indices); UI must keep them hidden *)
  | Jackpot_hit of string (* who found the jackpot (+egg_points), revealed now *)
  | Curse_offer of (int * string) list (* you hit the cursed cell: pick a victim *)
  | Wipe_cells of string * int (* wiper's name, how many of your cells to lose *)
  | Bonus_color of color * float (* bonus color + its expiry (unix epoch) *)
  | Bonus_claimed of string * color (* who banked it (+bonus_points) *)
[@@deriving sexp]

let string_of_client_msg m = Sexplib0.Sexp.to_string (sexp_of_client_msg m)

let client_msg_of_string s =
  client_msg_of_sexp (Parsexp.Single.parse_string_exn s)

let string_of_server_msg m = Sexplib0.Sexp.to_string (sexp_of_server_msg m)

let server_msg_of_string s =
  server_msg_of_sexp (Parsexp.Single.parse_string_exn s)

(* -- Design tokens (from the handoff README) -- *)

module Tokens = struct
  let ground = 0xF5EAD8 (* cream screen bg *)
  let ink = 0x201E1D (* text / borders *)
  let accent = 0xC67139 (* orange: primary buttons, timer *)
  let accent2 = 0x7A8A5E (* sage: confirm actions *)
  let muted = 0x9C8C72
  let disabled = 0xE8D9C0
  let yellow = 0xE8B23A
  let blue = 0x4A6FA5

  (* the 6 drawing palette colors, in swatch-grid order *)
  let palette = [| ink; accent; accent2; yellow; blue; white |]

  let win_w = 800
  let win_h = 600
end

(* human-readable palette color names (sabotage dialog, results) *)
let color_name (c : color) =
  if c = Tokens.ink then "INK"
  else if c = Tokens.accent then "ORANGE"
  else if c = Tokens.accent2 then "SAGE"
  else if c = Tokens.yellow then "YELLOW"
  else if c = Tokens.blue then "BLUE"
  else if c = white then "WHITE"
  else Printf.sprintf "#%06X" c
