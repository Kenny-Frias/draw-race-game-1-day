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
  ; mutable v_stars : (int * int) list (* player_id -> stars 1..5 *)
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

(* round length: the lobby setting (live, host-adjustable) and the value
   locked in for the round in progress *)
let lobby_round_secs = ref P.round_seconds
let cur_round_secs = ref P.round_seconds
let grid = ref (P.empty_grid ())
let undo : P.color array list ref = ref []

type tool =
  | Pen
  | Fill
  | Erase

let tool = ref Pen
let pen_size = ref 1 (* 1..5, set by the thickness slider *)
let sel_color = ref 0 (* index into T.palette *)
let painting = ref false
let dragging_slider = ref false
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

(* ---------- Winner-reveal animation state (design 2a) ---------- *)

type confetto =
  { c_x : float (* 0..1 of screen width *)
  ; c_w : int
  ; c_h : int
  ; c_color : int
  ; c_delay : float
  ; c_dur : float
  ; c_spin : float
  }

let results_anim_start = ref 0.
let confetti : confetto list ref = ref []

(* flat storyboard confetti colors: accent, sage, yellow, blue *)
let confetti_colors = [| T.accent; T.accent2; T.yellow; T.blue |]

let regen_confetti () =
  confetti
  := List.init 16 (fun _ ->
       { c_x = 0.04 +. Random.float 0.92
       ; c_w = 6 + Random.int 5
       ; c_h = 6 + Random.int 5
       ; c_color = confetti_colors.(Random.int (Array.length confetti_colors))
       ; c_delay = 1.8 +. Random.float 0.5
       ; c_dur = 2.2 +. Random.float 0.8
       ; c_spin = (if Random.bool () then 1. else -1.)
       })

(* drumroll beeps at rising pitch (the design's Graphics.sound), via
   Web Audio; silently a no-op if audio is unavailable/blocked *)
let audio_ctx : Js.Unsafe.any option ref = ref None

let get_audio () =
  match !audio_ctx with
  | Some a -> Some a
  | None ->
    (try
       let ctor =
         Js.Unsafe.pure_js_expr "(window.AudioContext||window.webkitAudioContext)"
       in
       let a = Js.Unsafe.new_obj ctor [||] in
       audio_ctx := Some a;
       Some a
     with _ -> None)

let drumroll_beeps () =
  try
    match get_audio () with
    | None -> ()
    | Some actx ->
      ignore (Js.Unsafe.meth_call actx "resume" [||]);
      let t0 : float = Js.Unsafe.get actx (Js.string "currentTime") in
      for i = 0 to 29 do
        (* 200 -> 800 Hz, one 50ms beep every 60ms for ~1.8s *)
        let freq = 200. +. (600. *. float_of_int i /. 29.) in
        let start = t0 +. (0.06 *. float_of_int i) in
        let osc = Js.Unsafe.meth_call actx "createOscillator" [||] in
        let gain = Js.Unsafe.meth_call actx "createGain" [||] in
        Js.Unsafe.set osc (Js.string "type") (Js.string "square");
        Js.Unsafe.set
          (Js.Unsafe.get osc (Js.string "frequency"))
          (Js.string "value") freq;
        Js.Unsafe.set
          (Js.Unsafe.get gain (Js.string "gain"))
          (Js.string "value") 0.03;
        ignore (Js.Unsafe.meth_call osc "connect" [| Js.Unsafe.inject gain |]);
        ignore
          (Js.Unsafe.meth_call gain "connect"
             [| Js.Unsafe.get actx (Js.string "destination") |]);
        ignore (Js.Unsafe.meth_call osc "start" [| Js.Unsafe.inject start |]);
        ignore
          (Js.Unsafe.meth_call osc "stop"
             [| Js.Unsafe.inject (start +. 0.05) |])
      done
  with _ -> ()

let start_results_anim () =
  results_anim_start := now_s ();
  regen_confetti ();
  drumroll_beeps ()

(* Cursed-cell payload, run on the VICTIM's client (the server never holds a
   grid mid-round, so we destroy our own cells on its instruction). Partial
   Fisher–Yates over the filled indices: exactly min n len distinct cells,
   uniformly. The caller clears the undo stack so this can't be undone. *)
let wipe_my_cells n =
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
  | Lobby (ps, secs) ->
    players := ps;
    lobby_round_secs := secs
  | Go_lobby -> screen := Lobby_screen
  | Word_reveal (word, deadline, secs) ->
    grid := P.empty_grid ();
    undo := [];
    tool := Pen;
    sel_color := 0;
    cur_round_secs := secs;
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
    := Voting { v_word = word; v_subs = subs; v_stars = []; v_locked = false }
  | Results lines ->
    start_results_anim ();
    screen := Results lines
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

(* n×n stamp centered on the cell (pen thickness) *)
let stamp n col row color =
  let lo = -((n - 1) / 2)
  and hi = n / 2 in
  for dc = lo to hi do
    for dr = lo to hi do
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
  | Pen -> stamp !pen_size col row T.palette.(!sel_color)
  | Erase -> stamp !pen_size col row P.white
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

(* pen thickness slider, under the PEN button *)
let slider_x = dpad
let slider_y = canvas_y + 44
let slider_w = toolbar_w
let slider_h = 36
let track_x0 = slider_x + 8
let track_x1 = slider_x + slider_w - 8

let in_slider mx my =
  mx >= float_of_int slider_x
  && mx <= float_of_int (slider_x + slider_w)
  && my >= float_of_int slider_y
  && my <= float_of_int (slider_y + slider_h)

let set_pen_size_from mx =
  let t =
    (mx -. float_of_int track_x0) /. float_of_int (track_x1 - track_x0)
  in
  let s = 1 + int_of_float (Float.round (t *. 4.)) in
  pen_size := max 1 (min 5 s)

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

(* font size at which a row of name buttons (32px padding, 12px gaps) fits
   in [avail] px — victim pickers stay on screen at any player count *)
let fit_row_size ~avail targets =
  let rec pick s =
    let row =
      List.fold_left
        (fun a (_, nm) ->
          a
          + int_of_float (Draw.text_width ~size:s ~bold:true (string_upper nm))
          + 32 + 12)
        (-12) targets
    in
    if row <= avail || s <= 9 then s else pick (s - 1)
  in
  pick 16

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
  (* player list box: one column up to 8 slots, two columns beyond *)
  let bx = px
  and by = 140
  and bw = T.win_w - (2 * px) in
  let two_col = P.max_players > 8 in
  let rows_per_col =
    if two_col then (P.max_players + 1) / 2 else P.max_players
  in
  let bh = (rows_per_col * 33) + 24 in
  let name_size = if two_col then 15 else 19
  and tag_size = if two_col then 11 else 14 in
  Draw.fill_rect ~x:bx ~y:by ~w:bw ~h:bh P.white;
  Draw.draw_border ~x:bx ~y:by ~w:bw ~h:bh ();
  if two_col
  then (
    Draw.set_alpha 0.25;
    Draw.line ~lw:2 ~x1:(bx + (bw / 2)) ~y1:(by + 8) ~x2:(bx + (bw / 2))
      ~y2:(by + bh - 8) T.muted;
    Draw.set_alpha 1.0);
  let colw = if two_col then (bw - 12) / 2 else bw in
  List.iteri
    (fun i slot ->
      let cx0 = bx + if two_col && i >= rows_per_col then colw + 12 else 0 in
      let ri = if two_col then i mod rows_per_col else i in
      let ry = by + 14 + (ri * 33) in
      match slot with
      | Some (p : P.player) ->
        let is_ready = match p.conn with P.Ready -> true | _ -> false in
        let sq = if is_ready then T.accent2 else T.accent in
        Draw.fill_rect ~x:(cx0 + 18) ~y:(ry + 4) ~w:16 ~h:16 sq;
        Draw.draw_border ~x:(cx0 + 18) ~y:(ry + 4) ~w:16 ~h:16 ();
        let label =
          Printf.sprintf "P%d %s%s" (i + 1) p.name
            (if p.id = !my_id then " (you)" else "")
        in
        let tag =
          if is_ready then (if p.is_host then "HOST · READY" else "READY")
          else if p.is_host then "HOST · JOINING"
          else "JOINING…"
        in
        let tc = if is_ready then T.ink else T.accent in
        let tw =
          int_of_float (Draw.text_width ~size:tag_size ~bold:true tag) + 14
        in
        let tx = cx0 + colw - 16 - tw in
        (* name shrinks to the space left of the status tag *)
        Draw.text
          ~size:
            (Draw.fit_size ~max_size:name_size
               ~max_w:(max 40 (tx - (cx0 + 46) - 8))
               label)
          ~x:(cx0 + 46) ~y:(ry + 4) label;
        Draw.draw_border ~color:tc ~x:tx ~y:ry ~w:tw ~h:24 ();
        Draw.text ~size:tag_size ~bold:true ~color:tc ~x:(tx + 7) ~y:(ry + 6)
          tag
      | None ->
        Draw.fill_rect ~x:(cx0 + 18) ~y:(ry + 4) ~w:16 ~h:16 T.disabled;
        Draw.draw_border ~color:T.muted ~x:(cx0 + 18) ~y:(ry + 4) ~w:16 ~h:16
          ();
        Draw.text ~size:name_size ~color:T.muted ~x:(cx0 + 46) ~y:(ry + 4)
          (Printf.sprintf "P%d — open slot —" (i + 1)))
    (List.init P.max_players (fun i -> List.nth_opt !players i));
  (* round time (host can adjust) *)
  let sy = by + bh + 14 in
  let tlabel = fmt_clock !lobby_round_secs in
  if i_am_host ()
  then (
    (* round time [-] 1:30 [+] stepper *)
    let step_w = 30
    and time_w = 70 in
    Draw.text ~size:16 ~color:T.muted ~x:px ~y:(sy + 7) "round time";
    let x0 =
      px + int_of_float (Draw.text_width ~size:16 "round time") + 14
    in
    let stepper ~x ~label ~enabled ~delta =
      Draw.fill_rect ~x ~y:sy ~w:step_w ~h:30 P.white;
      Draw.draw_border ~x ~y:sy ~w:step_w ~h:30 ();
      Draw.text ~size:20 ~bold:true
        ~color:(if enabled then T.ink else T.disabled)
        ~align:`Center ~x:(x + (step_w / 2)) ~y:(sy + 4) label;
      if enabled
      then
        add_hit ~x ~y:sy ~w:step_w ~h:30 (fun () ->
          send (Set_round_time (!lobby_round_secs + delta)))
    in
    stepper ~x:x0 ~label:"−"
      ~enabled:(!lobby_round_secs > P.min_round_seconds)
      ~delta:(-P.round_time_step);
    Draw.fill_rect ~x:(x0 + step_w + 6) ~y:sy ~w:time_w ~h:30 P.white;
    Draw.draw_border ~x:(x0 + step_w + 6) ~y:sy ~w:time_w ~h:30 ();
    Draw.text ~size:18 ~bold:true ~align:`Center
      ~x:(x0 + step_w + 6 + (time_w / 2)) ~y:(sy + 5) tlabel;
    stepper ~x:(x0 + step_w + time_w + 12) ~label:"+"
      ~enabled:(!lobby_round_secs < P.max_round_seconds)
      ~delta:P.round_time_step)
  else (
    let s = Printf.sprintf "round time · %s" tlabel in
    let sw = int_of_float (Draw.text_width ~size:16 s) + 20 in
    Draw.draw_border ~color:T.muted ~x:px ~y:sy ~w:sw ~h:30 ();
    Draw.text ~size:16 ~color:T.muted ~x:(px + 10) ~y:(sy + 6) s);
  (* actions: joining = ready, so the host just starts when 2+ are in *)
  let byy = T.win_h - 80 in
  let bw' =
    int_of_float (Draw.text_width ~size:20 ~bold:true "START ROUND") + 44
  in
  let bx' = (T.win_w - bw') / 2 in
  if i_am_host ()
  then (
    let can_start = List.length !players >= P.min_players in
    ignore
      (button ~x:bx' ~y:byy ~enabled:can_start "START ROUND" (fun () ->
         send Start_round));
    Draw.text ~size:14 ~color:T.muted ~align:`Center ~x:(T.win_w / 2)
      ~y:(byy + 48) "starts for everyone · needs 2+ players")
  else
    Draw.text ~size:17 ~color:T.muted ~align:`Center ~x:(T.win_w / 2)
      ~y:(byy + 12) "waiting for the host to start the round…"

let render_reveal word deadline =
  let cx = T.win_w / 2 in
  Draw.text ~size:18 ~bold:true ~align:`Center ~x:cx ~y:150 "YOUR WORD IS";
  let wu = string_upper word in
  let ww = int_of_float (Draw.text_width ~size:50 ~bold:true wu) + 112 in
  Draw.shadow_box ~off:7 ~shadow:T.accent2 ~x:(cx - (ww / 2)) ~y:200 ~w:ww ~h:90
    ~fill:P.white ();
  Draw.text ~size:50 ~bold:true ~align:`Center ~x:cx ~y:220 wu;
  (* 3-2-1 countdown: current digit big in accent box, upcoming smaller.
     Drawing starts at deadline - round length, so count down to that. *)
  let reveal_left = deadline -. float_of_int !cur_round_secs -. now_s () in
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

(* PEN (with slider under it), then the rest; slider occupies one slot *)
let tool_button ~y ~label ~selected ~warn on_click =
  let fill = if selected then T.accent else P.white in
  let color =
    if selected then 0xFFFFFF else if warn then T.accent else T.ink
  in
  Draw.fill_rect ~x:dpad ~y ~w:toolbar_w ~h:36 fill;
  Draw.draw_border ~x:dpad ~y ~w:toolbar_w ~h:36 ();
  Draw.text ~size:15 ~bold:true ~color ~align:`Center
    ~x:(dpad + (toolbar_w / 2)) ~y:(y + 9) label;
  add_hit ~x:dpad ~y ~w:toolbar_w ~h:36 on_click

let render_slider () =
  Draw.fill_rect ~x:slider_x ~y:slider_y ~w:slider_w ~h:slider_h P.white;
  Draw.draw_border ~x:slider_x ~y:slider_y ~w:slider_w ~h:slider_h ();
  let ty = slider_y + (slider_h / 2) in
  Draw.line ~lw:2 ~x1:track_x0 ~y1:ty ~x2:track_x1 ~y2:ty T.muted;
  for s = 1 to 5 do
    let x = track_x0 + ((s - 1) * (track_x1 - track_x0) / 4) in
    Draw.fill_rect ~x:(x - 1) ~y:(ty - 4) ~w:2 ~h:8 T.muted
  done;
  let kx = track_x0 + ((!pen_size - 1) * (track_x1 - track_x0) / 4) in
  Draw.fill_rect ~x:(kx - 5) ~y:(ty - 10) ~w:10 ~h:20 T.accent;
  Draw.draw_border ~x:(kx - 5) ~y:(ty - 10) ~w:10 ~h:20 ()

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
  (* toolbar: PEN + its thickness slider, then FILL / ERASE / UNDO / CLEAR *)
  tool_button ~y:canvas_y
    ~label:(Printf.sprintf "PEN·%d" !pen_size)
    ~selected:(!tool = Pen) ~warn:false
    (fun () -> tool := Pen);
  render_slider ();
  tool_button ~y:(canvas_y + 88) ~label:"FILL" ~selected:(!tool = Fill)
    ~warn:false
    (fun () -> tool := Fill);
  tool_button ~y:(canvas_y + 132) ~label:"ERASE" ~selected:(!tool = Erase)
    ~warn:false
    (fun () -> tool := Erase);
  tool_button ~y:(canvas_y + 176) ~label:"UNDO" ~selected:false ~warn:false
    (fun () -> pop_undo ());
  tool_button ~y:(canvas_y + 220) ~label:"CLEAR" ~selected:false ~warn:true
    (fun () ->
      push_undo ();
      grid := P.empty_grid ());
  (* swatches: 3-column grid, bottom-aligned with the canvas *)
  let sw = (toolbar_w - 8) / 3
  and sh = 26
  and sgap = 4 in
  let rows = (Array.length T.palette + 2) / 3 in
  let sy0 = canvas_y + canvas_h - (rows * (sh + sgap)) + sgap in
  Array.iteri
    (fun i c ->
      let x = dpad + (i mod 3 * (sw + sgap))
      and y = sy0 + (i / 3 * (sh + sgap)) in
      let banned =
        match !banned_color with Some b -> b = c | None -> false
      in
      (* the live bonus color gets a loud yellow ring: paint with it first! *)
      (match !bonus_active with
       | Some (bc, expires) when bc = c && now_s () < expires && not banned ->
         Draw.draw_border ~lw:3 ~color:T.yellow ~x:(x - 4) ~y:(y - 4)
           ~w:(sw + 8) ~h:(sh + 8) ()
       | _ -> ());
      if !sel_color = i && not banned
      then Draw.draw_border ~lw:2 ~color:T.accent2 ~x:(x - 2) ~y:(y - 2)
             ~w:(sw + 4) ~h:(sh + 4) ();
      Draw.fill_rect ~x ~y ~w:sw ~h:sh c;
      if banned
      then (
        (* locked: wash out, diagonal hatch, accent × — and no hit region *)
        Draw.set_alpha 0.6;
        Draw.fill_rect ~x ~y ~w:sw ~h:sh P.white;
        Draw.set_alpha 1.0;
        let step = 6 in
        let d = ref step in
        while !d < sw + sh do
          Draw.line ~lw:1
            ~x1:(x + max 0 (!d - sh)) ~y1:(y + min !d sh)
            ~x2:(x + min !d sw) ~y2:(y + max 0 (!d - sw)) T.ink;
          d := !d + step
        done;
        Draw.line ~lw:3 ~x1:(x + 7) ~y1:(y + 5)
          ~x2:(x + sw - 7) ~y2:(y + sh - 5) T.accent;
        Draw.line ~lw:3 ~x1:(x + sw - 7) ~y1:(y + 5)
          ~x2:(x + 7) ~y2:(y + sh - 5) T.accent);
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
    let bsize = fit_row_size ~avail:(T.win_w - 80) targets in
    let bw_of nm =
      int_of_float (Draw.text_width ~size:bsize ~bold:true (string_upper nm))
      + 32
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
        Draw.text ~size:bsize ~bold:true ~color:0xFFFFFF ~align:`Center
          ~x:(!bx + (wb / 2))
          ~y:(y + 74 + ((34 - bsize) / 2))
          (string_upper nm);
        add_hit ~x:!bx ~y:(y + 74) ~w:wb ~h:34 (fun () ->
          send (Curse_wipe tid);
          curse_offer := None;
          toast := Some ("curse unleashed!", now_s () +. 2.5));
        bx := !bx + wb + 12)
      targets;
    let sk =
      int_of_float (Draw.text_width ~size:14 ~bold:true "SPARE EVERYONE") + 24
    in
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
    let bsize = fit_row_size ~avail:(T.win_w - 40) targets in
    let bw_of nm =
      int_of_float (Draw.text_width ~size:bsize ~bold:true (string_upper nm))
      + 32
    in
    let total =
      List.fold_left (fun a (_, nm) -> a + bw_of nm + 12) (-12) targets
    in
    let x = ref (cx - (total / 2)) in
    List.iter
      (fun (tid, nm) ->
        let w = bw_of nm in
        Draw.shadow_box ~off:3 ~x:!x ~y:532 ~w ~h:34 ~fill:P.white ();
        Draw.text ~size:bsize ~bold:true ~align:`Center ~x:(!x + (w / 2))
          ~y:(532 + ((34 - bsize) / 2))
          (string_upper nm);
        add_hit ~x:!x ~y:532 ~w ~h:34 (fun () ->
          lock_ui := Pick_color (tid, nm));
        x := !x + w + 12)
      targets
  | Pick_color (tid, nm) ->
    let title = Printf.sprintf "LOCK WHICH COLOR ON %s?" (string_upper nm) in
    Draw.text
      ~size:(Draw.fit_size ~bold:true ~max_size:20 ~max_w:(T.win_w - 40) title)
      ~bold:true ~color:T.accent ~align:`Center ~x:cx ~y:484 title;
    (* all lockable colors (white is the eraser — not lockable) *)
    let sw = 30
    and gap = 6 in
    let lockable =
      Array.of_list
        (List.filter (fun c -> c <> P.white) (Array.to_list T.palette))
    in
    let n = Array.length lockable in
    let total = (n * sw) + ((n - 1) * gap) in
    Array.iteri
      (fun i c ->
        let x = cx - (total / 2) + (i * (sw + gap)) in
        Draw.fill_rect ~x ~y:518 ~w:sw ~h:sw c;
        Draw.draw_border ~x ~y:518 ~w:sw ~h:sw ();
        add_hit ~x ~y:518 ~w:sw ~h:sw (fun () ->
          send (Lock_color (tid, c));
          lock_ui := Lock_sent (nm, c)))
      lockable
  | Lock_sent (nm, c) ->
    Draw.text ~size:18 ~bold:true ~color:T.accent2 ~align:`Center ~x:cx ~y:500
      (Printf.sprintf "you locked %s on %s!" (P.color_name c) (string_upper nm))

(* star row overlaid on the bottom of a voting card; spacing shrinks with
   the card so it works at any player count *)
let star_band ~x ~y ~card_w ~rating ~clickable ~on_rate =
  let band_h = 26 in
  Draw.fill_rect ~x ~y ~w:card_w ~h:band_h P.white;
  Draw.draw_border ~x ~y ~w:card_w ~h:band_h ();
  let cy = y + (band_h / 2) in
  let spacing = min 24 ((card_w - 10) / P.max_stars) in
  let r = max 5 (spacing * 10 / 24) in
  for s = 1 to P.max_stars do
    let cx = x + (card_w / 2) + ((s - 3) * spacing) in
    if s <= rating
    then (
      Draw.fill_star ~cx ~cy ~r T.yellow;
      Draw.stroke_star ~lw:2 ~cx ~cy ~r T.ink)
    else Draw.stroke_star ~lw:2 ~cx ~cy ~r T.muted;
    if clickable
    then
      add_hit ~x:(cx - (spacing / 2)) ~y ~w:spacing ~h:band_h (fun () ->
        on_rate s)
  done

let render_voting (v : vote_state) =
  let px = 33 in
  Draw.text ~size:26 ~bold:true ~x:px ~y:24
    (Printf.sprintf "RATE THE %sS" (string_upper v.v_word));
  Draw.text ~size:16 ~color:T.muted ~align:`Right ~x:(T.win_w - px) ~y:32
    "give each drawing 1–5 stars";
  let n = List.length v.v_subs in
  let cols =
    if n <= 4 then n else if n <= 8 then (n + 1) / 2 else (n + 2) / 3
  in
  let rows = (n + cols - 1) / cols in
  (* card size limited by both width and the ~424px of vertical space *)
  let card_w =
    min
      (min 170 ((T.win_w - (2 * px) - ((cols - 1) * 24)) / cols))
      (((424 / rows) - 46) * 4 / 3)
  in
  let card_h = card_w * 3 / 4 in
  let total_w = (cols * card_w) + ((cols - 1) * 24) in
  let x0 = (T.win_w - total_w) / 2 in
  let row_h = card_h + 46 in
  let y0 = max 92 (90 + ((424 - (rows * row_h)) / 2)) in
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
        let rating =
          match List.assoc_opt s.player_id v.v_stars with
          | Some r -> r
          | None -> 0
        in
        star_band ~x ~y:(y + card_h - 26) ~card_w ~rating
          ~clickable:(not v.v_locked) ~on_rate:(fun stars ->
            v.v_stars
            <- (s.player_id, stars)
               :: List.filter (fun (p, _) -> p <> s.player_id) v.v_stars));
      let label =
        Printf.sprintf "%s · %s" s.player_name
          (fmt_clock (!cur_round_secs - s.seconds_left))
      in
      (* caption scales with the card, which shrinks with player count *)
      Draw.text
        ~size:(Draw.fit_size ~max_size:16 ~max_w:(card_w + 16) label)
        ~align:`Center ~x:(x + (card_w / 2)) ~y:(y + card_h + 8) label)
    v.v_subs;
  (* bottom: lock in once every drawing is rated *)
  let n_opp = n - 1 in
  let cy = T.win_h - 78 in
  let all_rated = List.length v.v_stars = n_opp in
  Draw.text ~size:17 ~color:T.muted ~x:px ~y:(cy + 12)
    (if all_rated then "all rated — lock it in!"
     else "rate every drawing to lock in");
  if v.v_locked
  then
    Draw.text ~size:18 ~bold:true ~color:T.accent2 ~align:`Right
      ~x:(T.win_w - px) ~y:(cy + 12) "LOCKED · waiting…"
  else
    ignore
      (button ~fill:T.accent2 ~enabled:all_rated ~x:(T.win_w - px - 140)
         ~y:cy "LOCK IN" (fun () ->
           v.v_locked <- true;
           send (Rate v.v_stars)))

(* Winner reveal (design 2a): drumroll -> #1 card pops with confetti ->
   remaining rows slide in -> buttons. All timings from the handoff. *)

let clamp01 x = if x < 0. then 0. else if x > 1. then 1. else x
let ease_out p = 1. -. ((1. -. p) *. (1. -. p))

(* qd-pop: scale .2 -> 1.15 (at 70%) -> 1. *)
let pop_scale p =
  let p = ease_out (clamp01 p) in
  if p < 0.7
  then 0.2 +. (0.95 *. (p /. 0.7))
  else 1.15 -. (0.15 *. ((p -. 0.7) /. 0.3))

let render_results (lines : P.score_line list) =
  let px = 37 in
  let t = now_s () -. !results_anim_start in
  (* -- drumroll (0..1.8s, fades out over 0.3s) -- *)
  if t < 2.1
  then (
    Draw.set_alpha (if t < 1.8 then 1.0 else clamp01 (1. -. ((t -. 1.8) /. 0.3)));
    let cx = T.win_w / 2 in
    Draw.text ~size:22 ~bold:true ~align:`Center ~x:cx ~y:230
      "AND THE WINNER IS…";
    (* shaking note glyph: rotate ±6° at ~5.5Hz around its bottom center *)
    let deg = -6. *. cos (2. *. Float.pi *. t /. 0.18) in
    Draw.rotated ~cx:(float_of_int cx) ~cy:350. ~deg (fun () ->
      Draw.text ~size:66 ~bold:true ~align:`Center ~x:cx ~y:280 "♬");
    Draw.set_alpha 1.0);
  (* -- title pops at 1.9s -- *)
  if t >= 1.9
  then (
    let p = clamp01 ((t -. 1.9) /. 0.4) in
    Draw.set_alpha (clamp01 (p *. 4.));
    let tw = Draw.text_width ~size:30 ~bold:true "ROUND RESULTS" in
    Draw.scaled
      ~cx:(float_of_int px +. (tw /. 2.)) ~cy:45. ~s:(pop_scale p)
      (fun () ->
        Draw.text ~size:30 ~bold:true ~color:T.accent2 ~x:(px + 3) ~y:33
          "ROUND RESULTS";
        Draw.text ~size:30 ~bold:true ~x:px ~y:30 "ROUND RESULTS");
    Draw.set_alpha 1.0);
  (* -- winner card pops at 2.0s -- *)
  (match lines with
   | winner :: _ when t >= 2.0 ->
     let p = clamp01 ((t -. 2.0) /. 0.5) in
     let cw = 460
     and ch = 112 in
     let cx = (T.win_w - cw) / 2
     and cy = 88 in
     Draw.set_alpha (clamp01 (p *. 4.));
     Draw.scaled
       ~cx:(float_of_int (T.win_w / 2))
       ~cy:(float_of_int cy +. (float_of_int ch /. 2.))
       ~s:(pop_scale p)
       (fun () ->
         Draw.shadow_box ~off:4 ~shadow:T.accent ~x:cx ~y:cy ~w:cw ~h:ch
           ~fill:P.white ();
         (* not spaced_text: it iterates bytes and would shred the ★ *)
         Draw.text ~size:20 ~bold:true ~color:T.accent ~align:`Center
           ~x:(T.win_w / 2) ~y:(cy + 12) "★ W I N N E R ★";
         let nm =
           winner.s_name
           ^ (if winner.s_player_id = !my_id then " (you)" else "")
         in
         (* name and itemization scale to the fixed card width *)
         let nsize = Draw.fit_size ~bold:true ~max_size:40 ~max_w:(cw - 36) nm in
         Draw.text ~size:nsize ~bold:true ~align:`Center ~x:(T.win_w / 2)
           ~y:(cy + 38 + ((40 - nsize) / 2))
           nm;
         let detail =
           String.concat ""
             [ Printf.sprintf "stars %d + speed %d" winner.votes winner.speed
             ; (if winner.egg > 0 then Printf.sprintf " + egg %d" winner.egg
                else "")
             ; (if winner.bonus > 0
                then Printf.sprintf " + bonus %d" winner.bonus
                else "")
             ; " = "
             ]
         in
         let total = string_of_int winner.total in
         let dsize =
           Draw.fit_size ~max_size:18 ~max_w:(cw - 30) (detail ^ total)
         in
         let dw = Draw.text_width ~size:dsize detail
         and tw = Draw.text_width ~size:dsize ~bold:true total in
         let x0 = (T.win_w / 2) - int_of_float ((dw +. tw) /. 2.) in
         let dy = cy + 84 + ((18 - dsize) / 2) in
         Draw.text ~size:dsize ~color:T.muted ~x:x0 ~y:dy detail;
         Draw.text ~size:dsize ~bold:true ~x:(x0 + int_of_float dw) ~y:dy
           total);
     Draw.set_alpha 1.0
   | _ -> ());
  (* -- rows #2.. slide in from the left (3.2s, 3.6s, then quick) -- *)
  let rest = match lines with [] -> [] | _ :: r -> r in
  let m = List.length rest in
  let bw = T.win_w - (2 * px) in
  let rh = if m <= 5 then 48 else min 40 (270 / max 1 m) in
  let box_h = rh - (if m <= 5 then 8 else 3) in
  (* text scales with the row box (at box_h = 40 these reproduce the old
     full-size values: 24/18/16/22), and centers vertically at any height *)
  let sz_rank = max 11 (box_h * 3 / 5)
  and sz_name = max 10 (box_h * 9 / 20)
  and sz_detail = max 9 (box_h * 2 / 5)
  and sz_total = max 11 (box_h * 11 / 20) in
  let ty sz = (box_h - sz) / 2 in
  let row_delay i = if i = 0 then 3.2 else 3.6 +. (0.15 *. float_of_int (i - 1)) in
  List.iteri
    (fun i (l : P.score_line) ->
      let p = ease_out (clamp01 ((t -. row_delay i) /. 0.35)) in
      if p > 0.
      then (
        let y = 225 + (i * rh) in
        let x = px + int_of_float (-40. *. (1. -. p)) in
        Draw.set_alpha p;
        Draw.fill_rect ~x ~y ~w:bw ~h:box_h P.white;
        Draw.draw_border ~x ~y ~w:bw ~h:box_h ();
        Draw.text ~size:sz_rank ~bold:true ~x:(x + 14) ~y:(y + ty sz_rank)
          (Printf.sprintf "#%d" (i + 2));
        let nm =
          l.s_name ^ (if l.s_player_id = !my_id then " (you)" else "")
        in
        Draw.text ~size:sz_name ~bold:true ~x:(x + 74) ~y:(y + ty sz_name) nm;
        let detail =
          String.concat ""
            [ Printf.sprintf "stars %d + speed %d" l.votes l.speed
            ; (if l.egg > 0 then Printf.sprintf " + egg %d" l.egg else "")
            ; (if l.bonus > 0 then Printf.sprintf " + bonus %d" l.bonus
               else "")
            ]
        in
        (* shrink the itemization so it never collides with the name *)
        let avail =
          bw - 110 - 74
          - int_of_float (Draw.text_width ~size:sz_name ~bold:true nm)
          - 12
        in
        let sz_detail =
          Draw.fit_size ~max_size:sz_detail ~max_w:(max 40 avail) detail
        in
        Draw.text ~size:sz_detail ~color:T.muted ~align:`Right
          ~x:(x + bw - 110) ~y:(y + ty sz_detail) detail;
        Draw.text ~size:sz_total ~bold:true ~align:`Right ~x:(x + bw - 16)
          ~y:(y + ty sz_total)
          (string_of_int l.total);
        Draw.set_alpha 1.0))
    rest;
  (* -- footnote + buttons last (>= 4s or after the final row) -- *)
  let buttons_at =
    Float.max 4. (row_delay (max 0 (m - 1)) +. 0.35)
  in
  let bp = ease_out (clamp01 ((t -. buttons_at) /. 0.35)) in
  if bp > 0.
  then (
    let by = T.win_h - 80 in
    let xoff = int_of_float (-40. *. (1. -. bp)) in
    Draw.set_alpha bp;
    Draw.text ~size:14 ~color:T.muted ~x:(px + xoff) ~y:(by - 26)
      (Printf.sprintf
         "score = stars (%d each) + speed (seconds left) + secret cell + bonus color"
         P.star_points);
    (if i_am_host ()
     then (
       let w1 =
         button ~x:(px + xoff) ~y:by "NEXT ROUND" (fun () -> send Next_round)
       in
       ignore
         (button ~fill:T.disabled ~text_color:T.ink ~x:(px + xoff + w1 + 20)
            ~y:by "BACK TO LOBBY" (fun () -> send Back_to_lobby)))
     else
       Draw.text ~size:17 ~color:T.muted ~x:(px + xoff) ~y:(by + 12)
         "waiting for the host to start the next round...");
    Draw.set_alpha 1.0);
  (* -- confetti on top (1.8s..~5.3s) -- *)
  List.iter
    (fun c ->
      let p = (t -. c.c_delay) /. c.c_dur in
      if p >= 0. && p <= 1.
      then
        Draw.rotated_rect
          ~cx:(c.c_x *. float_of_int T.win_w)
          ~cy:(-30. +. (p *. float_of_int (T.win_h + 80)))
          ~w:c.c_w ~h:c.c_h
          ~deg:(540. *. p *. c.c_spin)
          c.c_color)
    !confetti;
  (* -- replay (top right, always available) -- *)
  let rl = "↻ REPLAY" in
  let rw = int_of_float (Draw.text_width ~size:14 ~bold:true rl) + 20 in
  let rx = T.win_w - 16 - rw in
  Draw.shadow_box ~off:2 ~x:rx ~y:14 ~w:rw ~h:26 ~fill:T.disabled ();
  Draw.text ~size:14 ~bold:true ~align:`Center ~x:(rx + (rw / 2)) ~y:20 rl;
  add_hit ~x:rx ~y:14 ~w:rw ~h:26 start_results_anim

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
   | Reveal (w, d) when now_s () >= d -. float_of_int !cur_round_secs ->
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
     if in_slider mx my
     then (
       dragging_slider := true;
       set_pen_size_from mx)
     else (
       match cell_of_mouse mx my with
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
  if !dragging_slider
  then (
    let mx, _my = mouse_xy e in
    set_pen_size_from mx)
  else if !painting
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
  dragging_slider := false;
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
    if in_slider mx my
    then (
      dragging_slider := true;
      set_pen_size_from mx)
    else (
      match cell_of_mouse mx my with
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
             (* only swallow the event when it lands on the paint area or
                the slider, so taps on buttons still become clicks *)
             let on_paint =
               match cell_of_mouse (fst p) (snd p) with
               | Some _ -> true
               | None -> in_slider (fst p) (snd p)
             in
             if on_paint
             then (
               Dom.preventDefault e;
               touch_start_paint p)
           | None -> ());
          Js._true))
       Js._false);
  ignore
    (Dom_html.addEventListener Draw.canvas Dom_html.Event.touchmove
       (Dom_html.handler (fun e ->
          if !dragging_slider
          then (
            Dom.preventDefault e;
            match touch_xy e with
            | Some (mx, _) -> set_pen_size_from mx
            | None -> ())
          else if !painting
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
          dragging_slider := false;
          last_cell := None;
          Js._true))
       Js._false);
  (* name, then connect *)
  let name =
    match
      Js.Opt.to_option
        (Dom_html.window##prompt
           (Js.string "QUICKDRAW - Pick A Name")
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
