(* Shared protocol + game model for QUICKDRAW.
   Compiled into both the native server and the js_of_ocaml client, so the
   wire format (sexp strings over one websocket) can never drift. *)

open Sexplib0.Sexp_conv

(* -- Config constants (the design's "grid 80x60 - cell 8px" caption derives
      from these) -- *)

let cell_px = 8
let grid_cols = 640 / cell_px (* 80 *)
let grid_rows = 480 / cell_px (* 60 *)
let max_players = 16
let min_players = 2

(* round length is host-adjustable in the lobby *)
let round_seconds = 90 (* default *)
let min_round_seconds = 30
let max_round_seconds = 300
let round_time_step = 15
let countdown_seconds = 3

(* star-rating vote: each player gives every other drawing 1..max_stars,
   worth star_points each in the final score *)
let max_stars = 5
let star_points = 10

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
  ; total : int
  }
[@@deriving sexp]

(* Client -> server *)
type client_msg =
  | Join of string (* requested name *)
  | Set_ready of bool
  | Set_round_time of int (* host only, seconds *)
  | Start_round (* host only *)
  | Submit of grid * int (* grid, seconds left at submit *)
  | Rate of (int * int) list (* (player_id, stars 1..5) for each opponent *)
  | Next_round (* host, from results *)
  | Back_to_lobby (* host, from results *)
[@@deriving sexp]

(* Server -> client *)
type server_msg =
  | Joined of int (* your player id *)
  | Join_refused of string
  | Lobby of player list * int (* roster + round seconds; not a screen change *)
  | Go_lobby (* host sent everyone back to the lobby screen *)
  | Word_reveal of string * float * int
    (* word, draw deadline (unix epoch, after countdown), round seconds *)
  | Drawing_over (* timer hit zero: force submit *)
  | Vote_now of string * submission list (* word, everyone's drawings *)
  | Results of score_line list
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

  (* drawing palette, in swatch-grid order (3 per row): the full rainbow
     plus the basics *)
  let palette =
    [| ink; 0x9C9C9C; white (* ink · gray · white *)
     ; 0xD64541; accent; yellow (* red · orange · yellow *)
     ; 0x4C9A3F; accent2; 0x7FB2D9 (* green · sage · sky *)
     ; blue; 0x3F3F74; 0x8E5DA2 (* blue · indigo · violet *)
     ; 0xE59CB4; 0x7A4E2D; 0xD9B98C (* pink · brown · tan *)
    |]

  let win_w = 800
  let win_h = 600
end
