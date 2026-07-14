(* QUICKDRAW browser client.
   All rendering through the Draw shim (Graphics-style primitives on a
   canvas); all layout follows the design storyboard, whose screens are shown
   at 60% scale — measurements here are the 100% values from the handoff. *)

open Js_of_ocaml
module P = Quickdraw_shared.Protocol
module T = Quickdraw_shared.Protocol.Tokens

let now_s () =
  let d = new%js Js.date_now in
  Js.float_of_number d##getTime /. 1000.

let fmt_clock secs =
  let s = max 0 secs in
  Printf.sprintf "%d:%02d" (s / 60) (s mod 60)

(* ---------- Client state ---------- *)

type vote_state =
  { v_word : string
  ; v_subs : P.submission list
  ; mutable v_selected : int option (* player_id of selected card *)
  ; mutable v_assigned : (int * int) list (* player_id -> rank *)
  ; mutable v_locked : bool
  }

type screen =
  | Connecting
  | Lobby_screen
  | Reveal of string * float (* word, drawing deadline *)
  | Drawing of string * float
  | Waiting
  | Voting of vote_state
  | Results of P.score_line list
  | Dead of string

let screen = ref Connecting
let players : P.player list ref = ref []
let my_id = ref (-1)
let grid = ref (P.empty_grid ())
let undo : P.color array list ref = ref []

type tool =
  | Pen1
  | Pen3
  | Fill
  | Erase

let tool = ref Pen1
let sel_color = ref 0 (* index into T.palette *)
let painting = ref false
let last_cell : (int * int) option ref = ref None

(* sabotage: color lock. [lock_ui] drives the picker shown to the first
   submitter (on the Waiting screen); [banned_color] is set on the victim
   for the remainder of the round; [lock_notice] shows the dialog briefly. *)
type lock_ui =
  | No_lock
  | Pick_target of (int * string) list
  | Pick_color of int * string (* chosen victim: id, name *)
  | Lock_sent of string * int (* victim name, color *)

let lock_ui = ref No_lock
let banned_color : int option ref = ref None
let lock_notice : (string * int * float) option ref = ref None
(* locker name, color, show-until *)

(* easter eggs: the two secret cells (flat grid indices; None once claimed
   locally or not in a round), the cursed-cell victim picker, the active
   bonus color, and a transient toast banner *)
let secret_jackpot : int option ref = ref None
let secret_cursed : int option ref = ref None
let curse_offer : (int * string) list option ref = ref None
let bonus_active : (int * float) option ref = ref None (* color, expiry *)
let bonus_sent = ref false (* claim already sent for this window *)
let toast : (string * float) option ref = ref None (* text, show-until *)

(* Cursed-cell payload, run on the VICTIM's client (the server never holds a
   grid mid-round, so we destroy our own cells on its instruction). Wipe [n]
   of my filled (non-white) cells; if fewer than [n] are filled, wipe them
   all. The caller clears the undo stack afterwards so this can't be undone. *)
let wipe_my_cells n =
  (* partial Fisher–Yates over the filled indices: exactly min n len distinct
     cells, uniformly, no rejection-sampling spin on sparse grids *)
  let filled = ref [] in
  Array.iteri (fun i c -> if c <> P.white then filled := i :: !filled) !grid;
  let arr = Array.of_list !filled in
  let len = Array.length arr in
  for k = 0 to min n len - 1 do
    let j = k + Random.int (len - k) in
    let t = arr.(k) in
    arr.(k) <- arr.(j);
    arr.(j) <- t;
    !grid.(arr.(k)) <- P.white
  done

let me () = List.find_opt (fun (p : P.player) -> p.id = !my_id) !players

let i_am_host () =
  match me () with Some p -> p.is_host | None -> false

let i_am_ready () =
  match me () with
  | Some { conn = P.Ready; _ } -> true
  | _ -> false

let ready_count () =
  List.length
    (List.filter (fun (p : P.player) -> match p.conn with P.Ready -> true | _ -> false)
       !players)

(* ---------- Websocket ---------- *)

let ws : WebSockets.webSocket Js.t option ref = ref None
let connected = ref false

let send (m : P.client_msg) =
  match !ws with
  | Some w when !connected -> w##send (Js.string (P.string_of_client_msg m))
  | _ -> ()

let submit_drawing seconds_left =
  send (Submit (!grid, seconds_left));
  screen := Waiting

let handle_server_msg (m : P.server_msg) =
  match m with
  | Joined id ->
    my_id := id;
    screen := Lobby_screen
  | Join_refused why -> screen := Dead why
  | Lobby ps -> players := ps
  | Go_lobby -> screen := Lobby_screen
  | Word_reveal (word, deadline) ->
    grid := P.empty_grid ();
    undo := [];
    tool := Pen1;
    sel_color := 0;
    lock_ui := No_lock;
    banned_color := None;
    lock_notice := None;
    secret_jackpot := None;
    secret_cursed := None;
    curse_offer := None;
    bonus_active := None;
    bonus_sent := false;
    toast := None;
    screen := Reveal (word, deadline)
  | Drawing_over ->
    (match !screen with
     | Drawing _ -> submit_drawing 0
     | _ -> ())
  | Vote_now (word, subs) ->
    screen
    := Voting
         { v_word = word
         ; v_subs = subs
         ; v_selected = None
         ; v_assigned = []
         ; v_locked = false
         }
  | Results lines -> screen := Results lines
  | Lock_offer targets -> lock_ui := Pick_target targets
  | Color_locked (who, color) ->
    banned_color := Some color;
    lock_notice := Some (who, color, now_s () +. 4.);
    (* if the stolen color is in hand, drop to the first legal swatch *)
    if T.palette.(!sel_color) = color
    then
      Array.iteri
        (fun i c ->
          if c <> color && T.palette.(!sel_color) = color then sel_color := i)
        T.palette
  | Secret_cells (j, cu) ->
    secret_jackpot := Some j;
    secret_cursed := Some cu
  | Jackpot_hit who ->
    toast
    := Some
         ( Printf.sprintf "%s found the secret cell! +%d" who P.egg_points
         , now_s () +. 4. )
  | Curse_offer targets -> curse_offer := Some targets
  | Wipe_cells (who, n) ->
    wipe_my_cells n;
    undo := []; (* sabotage survives UNDO *)
    toast
    := Some
         ( Printf.sprintf "%s CURSED you! %d cells wiped" who n
         , now_s () +. 4. )
  | Bonus_color (c, expires) ->
    bonus_active := Some (c, expires);
    bonus_sent := false
  | Bonus_claimed (who, color) ->
    bonus_active := None;
    toast
    := Some
         ( Printf.sprintf "%s banked the %s bonus! +%d" who
             (P.color_name color) P.bonus_points
         , now_s () +. 3. )

(* ---------- Grid editing ---------- *)

let gidx ~col ~row = (row * P.grid_cols) + col

(* every non-white write lands here: claim secret cells and the bonus color
   the instant the paint touches them (a tiny event, never the grid) *)
let cell_painted idx color =
  (match !secret_jackpot with
   | Some j when j = idx ->
     secret_jackpot := None;
     send (Hit_secret Jackpot)
   | _ -> ());
  (match !secret_cursed with
   | Some cu when cu = idx ->
     secret_cursed := None;
     send (Hit_secret Cursed)
   | _ -> ());
  match !bonus_active with
  | Some (bc, expires) when (not !bonus_sent) && bc = color
                            && now_s () < expires ->
    bonus_sent := true;
    send Claim_bonus
  | _ -> ()

let set_cell col row color =
  if col >= 0 && col < P.grid_cols && row >= 0 && row < P.grid_rows
  then (
    let idx = gidx ~col ~row in
    !grid.(idx) <- color;
    if color <> P.white then cell_painted idx color)

let stamp3 col row color =
  for dc = -1 to 1 do
    for dr = -1 to 1 do
      set_cell (col + dc) (row + dr) color
    done
  done

let flood_fill col row color =
  let target = !grid.(gidx ~col ~row) in
  if target <> color
  then (
    let q = Queue.create () in
    Queue.add (col, row) q;
    while not (Queue.is_empty q) do
      let c, r = Queue.pop q in
      if c >= 0 && c < P.grid_cols && r >= 0 && r < P.grid_rows
         && !grid.(gidx ~col:c ~row:r) = target
      then (
        set_cell c r color;
        Queue.add (c + 1, r) q;
        Queue.add (c - 1, r) q;
        Queue.add (c, r + 1) q;
        Queue.add (c, r - 1) q)
    done)

let push_undo () =
  undo := Array.copy !grid :: !undo;
  if List.length !undo > 60
  then undo := List.filteri (fun i _ -> i < 60) !undo

let pop_undo () =
  match !undo with
  | g :: rest ->
    grid := g;
    undo := rest
  | [] -> ()

let apply_at col row =
  match !tool with
  | Pen1 -> set_cell col row T.palette.(!sel_color)
  | Pen3 -> stamp3 col row T.palette.(!sel_color)
  | Erase -> stamp3 col row P.white
  | Fill -> () (* fill happens on mousedown only *)

(* DDA walk so fast drags leave no gaps *)
let walk (c0, r0) (c1, r1) f =
  let dc = c1 - c0
  and dr = r1 - r0 in
  let n = max (abs dc) (abs dr) in
  if n = 0
  then f c0 r0
  else
    for i = 0 to n do
      f (c0 + (dc * i / n)) (r0 + (dr * i / n))
    done

(* ---------- Layout constants (100% scale) ---------- *)

(* drawing screen *)
let dpad = 20
let toolbar_w = 106
let canvas_x = dpad + toolbar_w + 14 (* 140 *)
let canvas_y = 64
let canvas_w = P.grid_cols * P.cell_px (* 640 *)
let canvas_h = P.grid_rows * P.cell_px (* 480 *)

let cell_of_mouse mx my =
  let c = (int_of_float mx - canvas_x) / P.cell_px
  and r = (int_of_float my - canvas_y) / P.cell_px in
  if c >= 0 && c < P.grid_cols && r >= 0 && r < P.grid_rows
     && mx >= float_of_int canvas_x
     && my >= float_of_int canvas_y
  then Some (c, r)
  else None

(* ---------- UI helpers ---------- *)

(* hit regions rebuilt each frame; click dispatches on the topmost *)
let hits : (float * float * float * float * (unit -> unit)) list ref = ref []

let add_hit ~x ~y ~w ~h f =
  hits
  := (float_of_int x, float_of_int y, float_of_int w, float_of_int h, f) :: !hits

let dispatch_click mx my =
  let rec go = function
    | [] -> ()
    | (x, y, w, h, f) :: rest ->
      if mx >= x && mx <= x +. w && my >= y && my <= y +. h then f () else go rest
  in
  go !hits

(* all-caps bold button with the hard offset shadow; returns its width *)
let button ?(fill = T.accent) ?(text_color = 0xFFFFFF) ?(size = 20)
    ?(enabled = true) ~x ~y label on_click =
  let pad_x = 22 in
  let w = int_of_float (Draw.text_width ~size ~bold:true label) + (2 * pad_x) in
  let h = size + 20 in
  if enabled
  then (
    Draw.shadow_box ~off:4 ~x ~y ~w ~h ~fill ();
    Draw.text ~size ~bold:true ~color:text_color ~align:`Center
      ~x:(x + (w / 2)) ~y:(y + 10) label;
    add_hit ~x ~y ~w ~h on_click)
  else (
    Draw.shadow_box ~off:3 ~x ~y ~w ~h ~fill:T.disabled ();
    Draw.text ~size ~bold:true ~color:T.muted ~align:`Center
      ~x:(x + (w / 2)) ~y:(y + 10) label);
  w

(* letter-spaced text (canvas has no letter-spacing primitive) *)
let spaced_text ~size ~color ~spacing ~x ~y s =
  let cx = ref (float_of_int x) in
  String.iter
    (fun ch ->
      let cs = String.make 1 ch in
      Draw.text ~size ~bold:true ~color ~x:(int_of_float !cx) ~y cs;
      cx := !cx +. Draw.text_width ~size ~bold:true cs +. spacing)
    s

let string_upper = String.uppercase_ascii

(* scaled-down render of a grid into a box (voting thumbnails, previews) *)
let draw_grid_thumb ~x ~y ~w ~h (g : P.color array) =
  Draw.fill_rect ~x ~y ~w ~h P.white;
  let cw = float_of_int w /. float_of_int P.grid_cols
  and ch = float_of_int h /. float_of_int P.grid_rows in
  for row = 0 to P.grid_rows - 1 do
    for col = 0 to P.grid_cols - 1 do
      let c = g.(gidx ~col ~row) in
      if c <> P.white
      then
        Draw.fill_rectf
          ~x:(float_of_int x +. (float_of_int col *. cw))
          ~y:(float_of_int y +. (float_of_int row *. ch))
          ~w:(cw +. 0.5) ~h:(ch +. 0.5) c
    done
  done;
  Draw.draw_border ~x ~y ~w ~h ()

(* ---------- Screens ---------- *)

let render_lobby () =
  let px = 44 in
  spaced_text ~size:56 ~color:T.accent ~spacing:7. ~x:(px + 5) ~y:41 "QUICKDRAW";
  spaced_text ~size:56 ~color:T.ink ~spacing:7. ~x:px ~y:36 "QUICKDRAW";
  Draw.text ~size:18 ~bold:true ~color:T.accent2 ~x:px ~y:104
    (Printf.sprintf "draw fast · vote hard · %d–%d players" P.min_players
       P.max_players);
  (* player list box *)
  let bx = px
  and by = 140
  and bw = T.win_w - (2 * px)
  and bh = (P.max_players * 33) + 24 in
  Draw.fill_rect ~x:bx ~y:by ~w:bw ~h:bh P.white;
  Draw.draw_border ~x:bx ~y:by ~w:bw ~h:bh ();
  List.iteri
    (fun i slot ->
      let ry = by + 14 + (i * 33) in
      match slot with
      | Some (p : P.player) ->
        let is_ready = match p.conn with P.Ready -> true | _ -> false in
        let sq = if is_ready then T.accent2 else T.accent in
        Draw.fill_rect ~x:(bx + 18) ~y:(ry + 4) ~w:16 ~h:16 sq;
        Draw.draw_border ~x:(bx + 18) ~y:(ry + 4) ~w:16 ~h:16 ();
        let label =
          Printf.sprintf "P%d %s%s" (i + 1) p.name
            (if p.id = !my_id then " (you)" else "")
        in
        Draw.text ~size:19 ~x:(bx + 46) ~y:(ry + 2) label;
        let tag =
          if is_ready then (if p.is_host then "HOST · READY" else "READY")
          else if p.is_host then "HOST · JOINING"
          else "JOINING…"
        in
        let tc = if is_ready then T.ink else T.accent in
        let tw = int_of_float (Draw.text_width ~size:14 ~bold:true tag) + 16 in
        let tx = bx + bw - 18 - tw in
        Draw.draw_border ~color:tc ~x:tx ~y:ry ~w:tw ~h:24 ();
        Draw.text ~size:14 ~bold:true ~color:tc ~x:(tx + 8) ~y:(ry + 5) tag
      | None ->
        Draw.fill_rect ~x:(bx + 18) ~y:(ry + 4) ~w:16 ~h:16 T.disabled;
        Draw.draw_border ~color:T.muted ~x:(bx + 18) ~y:(ry + 4) ~w:16 ~h:16 ();
        Draw.text ~size:19 ~color:T.muted ~x:(bx + 46) ~y:(ry + 2)
          (Printf.sprintf "P%d — open slot —" (i + 1)))
    (List.init P.max_players (fun i -> List.nth_opt !players i));
  (* server address *)
  let sy = by + bh + 14 in
  Draw.text ~size:17 ~x:px ~y:(sy + 6) "server:";
  let host = Js.to_string Dom_html.window##.location##.host in
  let hw = int_of_float (Draw.text_width ~size:17 host) + 24 in
  Draw.fill_rect ~x:(px + 78) ~y:sy ~w:hw ~h:30 P.white;
  Draw.draw_border ~x:(px + 78) ~y:sy ~w:hw ~h:30 ();
  Draw.text ~size:17 ~x:(px + 90) ~y:(sy + 6) host;
  (* actions *)
  let byy = T.win_h - 80 in
  let can_start = i_am_host () && ready_count () >= P.min_players in
  let w1 =
    button ~x:px ~y:byy ~enabled:can_start "START ROUND" (fun () ->
      send Start_round)
  in
  Draw.text ~size:16 ~color:T.muted ~x:(px + w1 + 16) ~y:(byy + 12)
    "host only · needs 2+ ready";
  if i_am_ready ()
  then
    ignore
      (button ~fill:T.disabled ~text_color:T.ink ~x:(T.win_w - px - 150) ~y:byy
         "UNREADY" (fun () -> send (Set_ready false)))
  else
    ignore
      (button ~fill:T.accent2 ~x:(T.win_w - px - 160) ~y:byy "READY UP"
         (fun () -> send (Set_ready true)))

let render_reveal word deadline =
  let cx = T.win_w / 2 in
  Draw.text ~size:18 ~bold:true ~align:`Center ~x:cx ~y:150 "YOUR WORD IS";
  let wu = string_upper word in
  let ww = int_of_float (Draw.text_width ~size:50 ~bold:true wu) + 112 in
  Draw.shadow_box ~off:7 ~shadow:T.accent2 ~x:(cx - (ww / 2)) ~y:200 ~w:ww ~h:90
    ~fill:P.white ();
  Draw.text ~size:50 ~bold:true ~align:`Center ~x:cx ~y:220 wu;
  (* 3-2-1 countdown: current digit big in accent box, upcoming smaller *)
  let reveal_left = deadline -. float_of_int P.round_seconds -. now_s () in
  let cur = min P.countdown_seconds (max 1 (int_of_float (ceil reveal_left))) in
  let n_boxes = cur in
  let big = 74
  and small = 57
  and gap = 16 in
  let total = big + ((n_boxes - 1) * (small + gap)) in
  let x0 = cx - (total / 2) in
  let yy = 340 in
  Draw.fill_rect ~x:x0 ~y:yy ~w:big ~h:big T.accent;
  Draw.draw_border ~lw:4 ~x:x0 ~y:yy ~w:big ~h:big ();
  Draw.text ~size:40 ~bold:true ~color:0xFFFFFF ~align:`Center ~x:(x0 + (big / 2))
    ~y:(yy + 16) (string_of_int cur);
  for i = 1 to cur - 1 do
    let x = x0 + big + gap + ((i - 1) * (small + gap)) in
    let y = yy + ((big - small) / 2) in
    Draw.draw_border ~color:T.muted ~x ~y ~w:small ~h:small ();
    Draw.text ~size:27 ~bold:true ~color:T.muted ~align:`Center
      ~x:(x + (small / 2)) ~y:(y + 14) (string_of_int (cur - i))
  done;
  Draw.text ~size:17 ~color:T.muted ~align:`Center ~x:cx ~y:470
    "everyone gets the SAME word · drawing starts together"

let tool_buttons = [ "PEN·1", `T Pen1; "PEN·3", `T Pen3; "FILL", `T Fill
                   ; "ERASE", `T Erase; "UNDO", `A `Undo; "CLEAR", `A `Clear ]

let render_drawing word deadline =
  (* top bar *)
  Draw.text ~size:22 ~bold:true ~x:dpad ~y:(dpad - 4) (string_upper word);
  let secs_left = int_of_float (ceil (deadline -. now_s ())) in
  let tlabel = fmt_clock secs_left in
  let tw = int_of_float (Draw.text_width ~size:34 ~bold:true tlabel) + 34 in
  let sub_w = int_of_float (Draw.text_width ~size:20 ~bold:true "SUBMIT") + 44 in
  let sub_x = T.win_w - dpad - sub_w in
  let tx = sub_x - 16 - tw in
  Draw.shadow_box ~off:4 ~x:tx ~y:6 ~w:tw ~h:48 ~fill:T.accent ();
  Draw.text ~size:34 ~bold:true ~color:0xFFFFFF ~align:`Center ~x:(tx + (tw / 2))
    ~y:14 tlabel;
  ignore
    (button ~fill:T.accent2 ~x:sub_x ~y:8 "SUBMIT" (fun () ->
       submit_drawing (max 0 secs_left)));
  (* toolbar *)
  List.iteri
    (fun i (label, act) ->
      let y = canvas_y + (i * 44) in
      let selected =
        match act with `T t -> !tool = t | `A _ -> false
      in
      let fill = if selected then T.accent else P.white in
      let color =
        if selected then 0xFFFFFF
        else if String.equal label "CLEAR" then T.accent
        else T.ink
      in
      Draw.fill_rect ~x:dpad ~y ~w:toolbar_w ~h:36 fill;
      Draw.draw_border ~x:dpad ~y ~w:toolbar_w ~h:36 ();
      Draw.text ~size:15 ~bold:true ~color ~align:`Center
        ~x:(dpad + (toolbar_w / 2)) ~y:(y + 9) label;
      add_hit ~x:dpad ~y ~w:toolbar_w ~h:36 (fun () ->
        match act with
        | `T t -> tool := t
        | `A `Undo -> pop_undo ()
        | `A `Clear ->
          push_undo ();
          grid := P.empty_grid ()))
    tool_buttons;
  (* swatches: 2-column grid, bottom-aligned with the canvas *)
  let sw = (toolbar_w - 6) / 2
  and sh = 30
  and sgap = 6 in
  let rows = (Array.length T.palette + 1) / 2 in
  let sy0 = canvas_y + canvas_h - (rows * (sh + sgap)) + sgap in
  Array.iteri
    (fun i c ->
      let x = dpad + (i mod 2 * (sw + sgap))
      and y = sy0 + (i / 2 * (sh + sgap)) in
      let banned =
        match !banned_color with Some b -> b = c | None -> false
      in
      (* the live bonus color gets a loud yellow ring: paint with it first! *)
      (match !bonus_active with
       | Some (bc, expires) when bc = c && now_s () < expires && not banned ->
         Draw.draw_border ~lw:3 ~color:T.yellow ~x:(x - 6) ~y:(y - 6)
           ~w:(sw + 12) ~h:(sh + 12) ()
       | _ -> ());
      if !sel_color = i && not banned
      then Draw.draw_border ~lw:2 ~color:T.accent2 ~x:(x - 3) ~y:(y - 3)
             ~w:(sw + 6) ~h:(sh + 6) ();
      Draw.fill_rect ~x ~y ~w:sw ~h:sh c;
      if banned
      then (
        (* locked: wash out, diagonal hatch, accent × — and no hit region *)
        Draw.set_alpha 0.6;
        Draw.fill_rect ~x ~y ~w:sw ~h:sh P.white;
        Draw.set_alpha 1.0;
        let step = 7 in
        let d = ref step in
        while !d < sw + sh do
          Draw.line ~lw:1
            ~x1:(x + max 0 (!d - sh)) ~y1:(y + min !d sh)
            ~x2:(x + min !d sw) ~y2:(y + max 0 (!d - sw)) T.ink;
          d := !d + step
        done;
        Draw.line ~lw:3 ~x1:(x + 9) ~y1:(y + 7)
          ~x2:(x + sw - 9) ~y2:(y + sh - 7) T.accent;
        Draw.line ~lw:3 ~x1:(x + sw - 9) ~y1:(y + 7)
          ~x2:(x + 9) ~y2:(y + sh - 7) T.accent);
      Draw.draw_border ~x ~y ~w:sw ~h:sh ();
      if not banned then add_hit ~x ~y ~w:sw ~h:sh (fun () -> sel_color := i))
    T.palette;
  (* canvas *)
  Draw.fill_rect ~x:canvas_x ~y:canvas_y ~w:canvas_w ~h:canvas_h P.white;
  for row = 0 to P.grid_rows - 1 do
    for col = 0 to P.grid_cols - 1 do
      let c = !grid.(gidx ~col ~row) in
      if c <> P.white
      then
        Draw.fill_rect
          ~x:(canvas_x + (col * P.cell_px))
          ~y:(canvas_y + (row * P.cell_px))
          ~w:P.cell_px ~h:P.cell_px c
    done
  done;
  (* faint grid overlay *)
  Draw.set_alpha 0.07;
  let gc = T.ink in
  for col = 1 to P.grid_cols - 1 do
    Draw.line ~lw:1
      ~x1:(canvas_x + (col * P.cell_px)) ~y1:canvas_y
      ~x2:(canvas_x + (col * P.cell_px)) ~y2:(canvas_y + canvas_h) gc
  done;
  for row = 1 to P.grid_rows - 1 do
    Draw.line ~lw:1
      ~x1:canvas_x ~y1:(canvas_y + (row * P.cell_px))
      ~x2:(canvas_x + canvas_w) ~y2:(canvas_y + (row * P.cell_px)) gc
  done;
  Draw.set_alpha 1.0;
  Draw.draw_border ~x:canvas_x ~y:canvas_y ~w:canvas_w ~h:canvas_h ();
  Draw.text ~size:14 ~color:T.muted ~align:`Right
    ~x:(canvas_x + canvas_w - 8) ~y:(canvas_y + canvas_h - 22)
    (Printf.sprintf "grid %d×%d · cell %dpx" P.grid_cols P.grid_rows P.cell_px);
  Draw.text ~size:15 ~color:T.muted ~x:canvas_x ~y:(canvas_y + canvas_h + 12)
    "submit early to bank the remaining seconds as your speed bonus";
  (* bonus-color banner along the canvas top *)
  (match !bonus_active with
   | Some (bc, expires) when now_s () < expires ->
     let left = int_of_float (ceil (expires -. now_s ())) in
     let msg =
       Printf.sprintf "BONUS · first %s stroke +%d · %ds" (P.color_name bc)
         P.bonus_points left
     in
     let bw = int_of_float (Draw.text_width ~size:15 ~bold:true msg) + 28 in
     let bx = canvas_x + ((canvas_w - bw) / 2) in
     Draw.shadow_box ~off:3 ~x:bx ~y:(canvas_y + 6) ~w:bw ~h:30 ~fill:T.yellow ();
     Draw.text ~size:15 ~bold:true ~align:`Center ~x:(bx + (bw / 2))
       ~y:(canvas_y + 13) msg
   | _ -> ());
  (* transient toast (jackpot found / wiped / bonus banked) *)
  (match !toast with
   | Some (msg, until) when now_s () < until ->
     let w = int_of_float (Draw.text_width ~size:16 ~bold:true msg) + 36 in
     let x = canvas_x + ((canvas_w - w) / 2) in
     Draw.shadow_box ~off:3 ~x ~y:(canvas_y + 44) ~w ~h:34 ~fill:P.white ();
     Draw.text ~size:16 ~bold:true ~align:`Center ~x:(x + (w / 2))
       ~y:(canvas_y + 53) msg
   | _ -> ());
  (* sabotage dialog (design 1d), on top for a few seconds *)
  (match !lock_notice with
   | Some (who, color, until) when now_s () < until ->
     let cx = canvas_x + (canvas_w / 2) in
     let msg =
       Printf.sprintf "%s stole %s for the round!" who (P.color_name color)
     in
     let w = int_of_float (Draw.text_width ~size:18 ~bold:true msg) + 76 in
     let x = cx - (w / 2)
     and y = canvas_y + 150 in
     Draw.shadow_box ~off:5 ~x ~y ~w ~h:92 ~fill:P.white ();
     Draw.text ~size:22 ~bold:true ~color:T.accent ~align:`Center ~x:cx
       ~y:(y + 14) "! COLOR LOCKED !";
     Draw.text ~size:18 ~bold:true ~align:`Center ~x:cx ~y:(y + 52) msg
   | _ -> ());
  (* cursed-cell reward: modal victim picker (painting pauses while open) *)
  match !curse_offer with
  | None -> ()
  | Some targets ->
    let cx = T.win_w / 2 in
    let bw_of nm =
      int_of_float (Draw.text_width ~size:16 ~bold:true (string_upper nm)) + 32
    in
    let row_w =
      List.fold_left (fun a (_, nm) -> a + bw_of nm + 12) (-12) targets
    in
    let w = max 400 (row_w + 60) in
    let x = cx - (w / 2)
    and y = 200 in
    Draw.shadow_box ~off:5 ~x ~y ~w ~h:170 ~fill:P.white ();
    Draw.text ~size:22 ~bold:true ~color:T.accent ~align:`Center ~x:cx
      ~y:(y + 14) "! CURSED CELL !";
    Draw.text ~size:15 ~align:`Center ~x:cx ~y:(y + 46)
      (Printf.sprintf "wipe %d cells from someone's drawing:" P.wipe_count);
    let bx = ref (cx - (row_w / 2)) in
    List.iter
      (fun (tid, nm) ->
        let wb = bw_of nm in
        Draw.shadow_box ~off:3 ~x:!bx ~y:(y + 74) ~w:wb ~h:34 ~fill:T.accent ();
        Draw.text ~size:16 ~bold:true ~color:0xFFFFFF ~align:`Center
          ~x:(!bx + (wb / 2)) ~y:(y + 82) (string_upper nm);
        add_hit ~x:!bx ~y:(y + 74) ~w:wb ~h:34 (fun () ->
          send (Curse_wipe tid);
          curse_offer := None;
          toast := Some ("curse unleashed!", now_s () +. 2.5));
        bx := !bx + wb + 12)
      targets;
    let sk = int_of_float (Draw.text_width ~size:14 ~bold:true "SPARE EVERYONE") + 24 in
    Draw.draw_border ~x:(cx - (sk / 2)) ~y:(y + 126) ~w:sk ~h:28 ();
    Draw.text ~size:14 ~bold:true ~color:T.muted ~align:`Center ~x:cx
      ~y:(y + 133) "SPARE EVERYONE";
    add_hit ~x:(cx - (sk / 2)) ~y:(y + 126) ~w:sk ~h:28 (fun () ->
      curse_offer := None)

let render_waiting () =
  let cx = T.win_w / 2 in
  Draw.text ~size:34 ~bold:true ~align:`Center ~x:cx ~y:120 "SUBMITTED!";
  Draw.text ~size:18 ~color:T.muted ~align:`Center ~x:cx ~y:170
    "waiting for the other players...";
  draw_grid_thumb ~x:(cx - 160) ~y:220 ~w:320 ~h:240 !grid;
  (* toasts keep arriving after submit (jackpot found, bonus banked) *)
  (match !toast with
   | Some (msg, until) when now_s () < until ->
     let w = int_of_float (Draw.text_width ~size:16 ~bold:true msg) + 36 in
     Draw.shadow_box ~off:3 ~x:(cx - (w / 2)) ~y:66 ~w ~h:34 ~fill:P.white ();
     Draw.text ~size:16 ~bold:true ~align:`Center ~x:cx ~y:75 msg
   | _ -> ());
  (* sabotage picker: only the first submitter ever has lock_ui <> No_lock *)
  match !lock_ui with
  | No_lock -> ()
  | Pick_target targets ->
    Draw.text ~size:20 ~bold:true ~color:T.accent ~align:`Center ~x:cx ~y:478
      "FIRST TO SUBMIT — LOCK A COLOR!";
    Draw.text ~size:15 ~color:T.muted ~align:`Center ~x:cx ~y:506
      "pick a victim:";
    let bw_of nm =
      int_of_float (Draw.text_width ~size:16 ~bold:true (string_upper nm)) + 32
    in
    let total =
      List.fold_left (fun a (_, nm) -> a + bw_of nm + 12) (-12) targets
    in
    let x = ref (cx - (total / 2)) in
    List.iter
      (fun (tid, nm) ->
        let w = bw_of nm in
        Draw.shadow_box ~off:3 ~x:!x ~y:532 ~w ~h:34 ~fill:P.white ();
        Draw.text ~size:16 ~bold:true ~align:`Center ~x:(!x + (w / 2)) ~y:540
          (string_upper nm);
        let hx = !x in
        add_hit ~x:hx ~y:532 ~w ~h:34 (fun () ->
          lock_ui := Pick_color (tid, nm));
        x := !x + w + 12)
      targets
  | Pick_color (tid, nm) ->
    Draw.text ~size:20 ~bold:true ~color:T.accent ~align:`Center ~x:cx ~y:490
      (Printf.sprintf "LOCK WHICH COLOR ON %s?" (string_upper nm));
    let sw = 40
    and gap = 10 in
    let n = Array.length T.palette in
    let total = (n * sw) + ((n - 1) * gap) in
    Array.iteri
      (fun i c ->
        let x = cx - (total / 2) + (i * (sw + gap)) in
        Draw.fill_rect ~x ~y:528 ~w:sw ~h:sw c;
        Draw.draw_border ~x ~y:528 ~w:sw ~h:sw ();
        add_hit ~x ~y:528 ~w:sw ~h:sw (fun () ->
          send (Lock_color (tid, c));
          lock_ui := Lock_sent (nm, c)))
      T.palette
  | Lock_sent (nm, c) ->
    Draw.text ~size:18 ~bold:true ~color:T.accent2 ~align:`Center ~x:cx ~y:500
      (Printf.sprintf "you locked %s on %s!" (P.color_name c) (string_upper nm))

let render_voting (v : vote_state) =
  let px = 33 in
  Draw.text ~size:26 ~bold:true ~x:px ~y:24
    (Printf.sprintf "RANK THE %sS" (string_upper v.v_word));
  Draw.text ~size:16 ~color:T.muted ~align:`Right ~x:(T.win_w - px) ~y:32
    "click a drawing, then a rank";
  let n = List.length v.v_subs in
  let cols = if n <= 4 then n else (n + 1) / 2 in
  let card_w = min 170 ((T.win_w - (2 * px) - ((cols - 1) * 24)) / cols) in
  let card_h = card_w * 3 / 4 in
  let rows = (n + cols - 1) / cols in
  let total_w = (cols * card_w) + ((cols - 1) * 24) in
  let x0 = (T.win_w - total_w) / 2 in
  let row_h = card_h + 46 in
  let y0 = 90 + ((330 - (rows * row_h)) / 2) in
  List.iteri
    (fun i (s : P.submission) ->
      let x = x0 + (i mod cols * (card_w + 24))
      and y = y0 + (i / cols * row_h) in
      let mine = s.player_id = !my_id in
      if mine then Draw.set_alpha 0.45;
      draw_grid_thumb ~x ~y ~w:card_w ~h:card_h s.grid;
      if mine
      then (
        Draw.fill_rect ~x ~y:(y + card_h - 20) ~w:card_w ~h:20 T.ink;
        Draw.text ~size:14 ~bold:true ~color:0xFFFFFF ~align:`Center
          ~x:(x + (card_w / 2)) ~y:(y + card_h - 18) "YOURS";
        Draw.set_alpha 1.0)
      else (
        (* selection ring *)
        (match v.v_selected with
         | Some pid when pid = s.player_id ->
           Draw.draw_border ~lw:3 ~color:T.accent2 ~x:(x - 4) ~y:(y - 4)
             ~w:(card_w + 8) ~h:(card_h + 8) ()
         | _ -> ());
        (* rank badge, top-left corner *)
        let bs = 38 in
        let bx = x - 12
        and by = y - 12 in
        (match List.assoc_opt s.player_id v.v_assigned with
         | Some r ->
           Draw.fill_rect ~x:bx ~y:by ~w:bs ~h:bs T.accent;
           Draw.draw_border ~x:bx ~y:by ~w:bs ~h:bs ();
           Draw.text ~size:21 ~bold:true ~color:0xFFFFFF ~align:`Center
             ~x:(bx + (bs / 2)) ~y:(by + 8) (string_of_int r)
         | None ->
           Draw.fill_rect ~x:bx ~y:by ~w:bs ~h:bs T.disabled;
           Draw.draw_border ~x:bx ~y:by ~w:bs ~h:bs ();
           Draw.text ~size:21 ~bold:true ~align:`Center ~x:(bx + (bs / 2))
             ~y:(by + 8) "?");
        if not v.v_locked
        then
          add_hit ~x ~y ~w:card_w ~h:card_h (fun () ->
            v.v_selected <- Some s.player_id));
      Draw.text ~size:16 ~align:`Center ~x:(x + (card_w / 2)) ~y:(y + card_h + 8)
        (Printf.sprintf "%s · %s" s.player_name
           (fmt_clock (P.round_seconds - s.seconds_left))))
    v.v_subs;
  (* bottom: rank chips + lock in *)
  let n_opp = n - 1 in
  let cy = T.win_h - 78 in
  Draw.text ~size:18 ~x:px ~y:(cy + 12) "assign:";
  for r = 1 to n_opp do
    let x = px + 86 + ((r - 1) * 54) in
    let used = List.exists (fun (_, r') -> r' = r) v.v_assigned in
    Draw.fill_rect ~x ~y:cy ~w:43 ~h:43 (if used then T.accent else P.white);
    Draw.draw_border ~x ~y:cy ~w:43 ~h:43 ();
    Draw.text ~size:21 ~bold:true
      ~color:(if used then 0xFFFFFF else T.ink)
      ~align:`Center ~x:(x + 21) ~y:(cy + 10) (string_of_int r);
    if not v.v_locked
    then
      add_hit ~x ~y:cy ~w:43 ~h:43 (fun () ->
        match v.v_selected with
        | Some pid ->
          v.v_assigned
          <- (pid, r)
             :: List.filter (fun (p, r') -> p <> pid && r' <> r) v.v_assigned
        | None -> ())
  done;
  let all_assigned = List.length v.v_assigned = n_opp in
  if v.v_locked
  then
    Draw.text ~size:18 ~bold:true ~color:T.accent2 ~align:`Right
      ~x:(T.win_w - px) ~y:(cy + 12) "LOCKED · waiting…"
  else
    ignore
      (button ~fill:T.accent2 ~enabled:all_assigned ~x:(T.win_w - px - 140)
         ~y:cy "LOCK IN" (fun () ->
           v.v_locked <- true;
           send (Rank v.v_assigned)))

let render_results (lines : P.score_line list) =
  let px = 37 in
  Draw.text ~size:30 ~bold:true ~color:T.accent2 ~x:(px + 3) ~y:33
    "ROUND RESULTS";
  Draw.text ~size:30 ~bold:true ~x:px ~y:30 "ROUND RESULTS";
  let bw = T.win_w - (2 * px) in
  List.iteri
    (fun i (l : P.score_line) ->
      let y = 95 + (i * 48) in
      if i = 0
      then Draw.shadow_box ~off:4 ~shadow:T.accent ~x:px ~y ~w:bw ~h:40
             ~fill:P.white ()
      else (
        Draw.fill_rect ~x:px ~y ~w:bw ~h:40 P.white;
        Draw.draw_border ~x:px ~y ~w:bw ~h:40 ());
      Draw.text ~size:24 ~bold:true
        ~color:(if i = 0 then T.accent else T.ink)
        ~x:(px + 14) ~y:(y + 8)
        (Printf.sprintf "#%d" (i + 1));
      Draw.text ~size:18 ~bold:true ~x:(px + 74) ~y:(y + 11)
        (l.s_name ^ (if l.s_player_id = !my_id then " (you)" else ""));
      Draw.text ~size:16 ~color:T.muted ~align:`Right ~x:(px + bw - 110)
        ~y:(y + 12)
        (String.concat ""
           [ Printf.sprintf "votes %d + speed %d" l.votes l.speed
           ; (if l.egg > 0 then Printf.sprintf " + egg %d" l.egg else "")
           ; (if l.bonus > 0 then Printf.sprintf " + bonus %d" l.bonus else "")
           ]);
      Draw.text ~size:22 ~bold:true ~align:`Right ~x:(px + bw - 16) ~y:(y + 9)
        (string_of_int l.total))
    lines;
  let by = T.win_h - 80 in
  Draw.text ~size:15 ~color:T.muted ~x:px ~y:(by - 30)
    "score = votes + speed (seconds banked) + secret cell + bonus color";
  if i_am_host ()
  then (
    let w1 = button ~x:px ~y:by "NEXT ROUND" (fun () -> send Next_round) in
    ignore
      (button ~fill:T.disabled ~text_color:T.ink ~x:(px + w1 + 20) ~y:by
         "BACK TO LOBBY" (fun () -> send Back_to_lobby)))
  else
    Draw.text ~size:17 ~color:T.muted ~x:px ~y:(by + 12)
      "waiting for the host to start the next round..."

let render_message ?(sub = "") msg =
  let cx = T.win_w / 2 in
  Draw.text ~size:28 ~bold:true ~align:`Center ~x:cx ~y:250 msg;
  if sub <> ""
  then Draw.text ~size:17 ~color:T.muted ~align:`Center ~x:cx ~y:300 sub

(* ---------- Main loop ---------- *)

let render () =
  hits := [];
  Draw.clear ();
  (* reveal -> drawing transition is time-driven, synced by the deadline *)
  (match !screen with
   | Reveal (w, d) when now_s () >= d -. float_of_int P.round_seconds ->
     screen := Drawing (w, d)
   | Drawing (_, d) when now_s () > d +. 1.5 ->
     (* belt-and-braces: if Drawing_over got lost, self-submit *)
     submit_drawing 0
   | _ -> ());
  match !screen with
  | Connecting -> render_message "CONNECTING..."
  | Lobby_screen -> render_lobby ()
  | Reveal (w, d) -> render_reveal w d
  | Drawing (w, d) -> render_drawing w d
  | Waiting -> render_waiting ()
  | Voting v -> render_voting v
  | Results l -> render_results l
  | Dead why -> render_message ~sub:"reload the page to rejoin" why

let rec loop _t =
  render ();
  ignore
    (Dom_html.window##requestAnimationFrame (Js.wrap_callback loop))

(* ---------- Input wiring ---------- *)

(* mouse position in canvas coordinates, correcting for CSS scaling *)
let mouse_xy (e : Dom_html.mouseEvent Js.t) =
  let fl = Js.float_of_number in
  let rect = Draw.canvas##getBoundingClientRect in
  let left = fl rect##.left
  and right = fl rect##.right
  and top = fl rect##.top
  and bottom = fl rect##.bottom in
  let scale_x = float_of_int T.win_w /. (right -. left) in
  let scale_y = float_of_int T.win_h /. (bottom -. top) in
  ( (fl e##.clientX -. left) *. scale_x
  , (fl e##.clientY -. top) *. scale_y )

let on_mousedown e =
  let mx, my = mouse_xy e in
  (match !screen with
   | Drawing _ when Option.is_none !curse_offer ->
     (match cell_of_mouse mx my with
      | Some (c, r) ->
        push_undo ();
        (match !tool with
         | Fill -> flood_fill c r T.palette.(!sel_color)
         | _ ->
           painting := true;
           apply_at c r;
           last_cell := Some (c, r))
      | None -> ())
   | _ -> ());
  Js._true

let on_mousemove e =
  if !painting
  then (
    let mx, my = mouse_xy e in
    match cell_of_mouse mx my with
    | Some cell ->
      (match !last_cell with
       | Some prev -> walk prev cell (fun c r -> apply_at c r)
       | None -> apply_at (fst cell) (snd cell));
      last_cell := Some cell
    | None -> ());
  Js._true

let on_mouseup _e =
  painting := false;
  last_cell := None;
  Js._true

let on_click e =
  let mx, my = mouse_xy e in
  dispatch_click mx my;
  Js._true

(* touch position in canvas coordinates (phones opening the Slack link) *)
let touch_xy (e : Dom_html.touchEvent Js.t) =
  match Js.Optdef.to_option (e##.touches##item 0) with
  | None -> None
  | Some t ->
    let fl = Js.float_of_number in
    let rect = Draw.canvas##getBoundingClientRect in
    let left = fl rect##.left
    and right = fl rect##.right
    and top = fl rect##.top
    and bottom = fl rect##.bottom in
    let sx = float_of_int T.win_w /. (right -. left) in
    let sy = float_of_int T.win_h /. (bottom -. top) in
    Some ((fl t##.clientX -. left) *. sx, (fl t##.clientY -. top) *. sy)

let touch_start_paint (mx, my) =
  match !screen with
  | Drawing _ when Option.is_none !curse_offer ->
    (match cell_of_mouse mx my with
     | Some (c, r) ->
       push_undo ();
       (match !tool with
        | Fill -> flood_fill c r T.palette.(!sel_color)
        | _ ->
          painting := true;
          apply_at c r;
          last_cell := Some (c, r))
     | None -> ())
  | _ -> ()

let () =
  Random.self_init ();
  Draw.canvas##.onmousedown := Dom_html.handler on_mousedown;
  Draw.canvas##.onmousemove := Dom_html.handler on_mousemove;
  Dom_html.window##.onmouseup := Dom_html.handler on_mouseup;
  Draw.canvas##.onclick := Dom_html.handler on_click;
  (* touch: drags paint (taps already synthesize click events for buttons) *)
  ignore
    (Dom_html.addEventListener Draw.canvas Dom_html.Event.touchstart
       (Dom_html.handler (fun e ->
          (match touch_xy e with
           | Some p ->
             (* only swallow the event when it lands on the paint area,
                so taps on buttons still become clicks *)
             (match cell_of_mouse (fst p) (snd p) with
              | Some _ ->
                Dom.preventDefault e;
                touch_start_paint p
              | None -> ())
           | None -> ());
          Js._true))
       Js._false);
  ignore
    (Dom_html.addEventListener Draw.canvas Dom_html.Event.touchmove
       (Dom_html.handler (fun e ->
          if !painting
          then (
            Dom.preventDefault e;
            match touch_xy e with
            | Some (mx, my) ->
              (match cell_of_mouse mx my with
               | Some cell ->
                 (match !last_cell with
                  | Some prev -> walk prev cell (fun c r -> apply_at c r)
                  | None -> apply_at (fst cell) (snd cell));
                 last_cell := Some cell
               | None -> ())
            | None -> ());
          Js._true))
       Js._false);
  ignore
    (Dom_html.addEventListener Draw.canvas Dom_html.Event.touchend
       (Dom_html.handler (fun _ ->
          painting := false;
          last_cell := None;
          Js._true))
       Js._false);
  (* name, then connect *)
  let name =
    match
      Js.Opt.to_option
        (Dom_html.window##prompt
           (Js.string "QUICKDRAW - what's your name, drawer?")
           (Js.string ""))
    with
    | Some s ->
      let s = String.trim (Js.to_string s) in
      if s = "" then "anon" else s
    | None -> "anon"
  in
  let loc = Dom_html.window##.location in
  let proto =
    if Js.to_string loc##.protocol = "https:" then "wss://" else "ws://"
  in
  let url = proto ^ Js.to_string loc##.host ^ "/ws" in
  let sock = new%js WebSockets.webSocket (Js.string url) in
  ws := Some sock;
  sock##.onopen
  := Dom.handler (fun _ ->
       connected := true;
       send (Join name);
       Js._true);
  sock##.onmessage
  := Dom.handler (fun e ->
       (try handle_server_msg (P.server_msg_of_string (Js.to_string e##.data))
        with _ -> ());
       Js._true);
  sock##.onclose
  := Dom.handler (fun _ ->
       connected := false;
       (match !screen with
        | Dead _ -> ()
        | _ -> screen := Dead "DISCONNECTED");
       Js._true);
  ignore (Dom_html.window##requestAnimationFrame (Js.wrap_callback loop))
