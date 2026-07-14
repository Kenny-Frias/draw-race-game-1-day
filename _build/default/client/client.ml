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
    screen := Reveal (word, deadline)
  | Drawing_over ->
    (match !screen with
     | Drawing _ -> submit_drawing 0
     | _ -> ())
  | Vote_now (word, subs) ->
    screen
    := Voting { v_word = word; v_subs = subs; v_stars = []; v_locked = false }
  | Results lines -> screen := Results lines

(* ---------- Grid editing ---------- *)

let gidx ~col ~row = (row * P.grid_cols) + col

let set_cell col row color =
  if col >= 0 && col < P.grid_cols && row >= 0 && row < P.grid_rows
  then !grid.(gidx ~col ~row) <- color

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
        !grid.(gidx ~col:c ~row:r) <- color;
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
        Draw.text ~size:name_size ~x:(cx0 + 46) ~y:(ry + 4) label;
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
  (* server address (left) · round time (right, host can adjust) *)
  let sy = by + bh + 14 in
  Draw.text ~size:17 ~x:px ~y:(sy + 6) "server:";
  let host = Js.to_string Dom_html.window##.location##.host in
  let hw = int_of_float (Draw.text_width ~size:17 host) + 24 in
  Draw.fill_rect ~x:(px + 78) ~y:sy ~w:hw ~h:30 P.white;
  Draw.draw_border ~x:(px + 78) ~y:sy ~w:hw ~h:30 ();
  Draw.text ~size:17 ~x:(px + 90) ~y:(sy + 6) host;
  let tlabel = fmt_clock !lobby_round_secs in
  if i_am_host ()
  then (
    (* [-] 1:30 [+] stepper *)
    let step_w = 30
    and time_w = 70 in
    let x0 = T.win_w - px - ((2 * step_w) + time_w + 12) in
    Draw.text ~size:16 ~color:T.muted ~align:`Right ~x:(x0 - 12) ~y:(sy + 7)
      "round time";
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
    let x0 = T.win_w - px - sw in
    Draw.draw_border ~color:T.muted ~x:x0 ~y:sy ~w:sw ~h:30 ();
    Draw.text ~size:16 ~color:T.muted ~x:(x0 + 10) ~y:(sy + 6) s);
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
      if !sel_color = i
      then Draw.draw_border ~lw:2 ~color:T.accent2 ~x:(x - 2) ~y:(y - 2)
             ~w:(sw + 4) ~h:(sh + 4) ();
      Draw.fill_rect ~x ~y ~w:sw ~h:sh c;
      Draw.draw_border ~x ~y ~w:sw ~h:sh ();
      add_hit ~x ~y ~w:sw ~h:sh (fun () -> sel_color := i))
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
    "submit early to bank the remaining seconds as your speed bonus"

let render_waiting () =
  let cx = T.win_w / 2 in
  Draw.text ~size:34 ~bold:true ~align:`Center ~x:cx ~y:120 "SUBMITTED!";
  Draw.text ~size:18 ~color:T.muted ~align:`Center ~x:cx ~y:170
    "waiting for the other players...";
  draw_grid_thumb ~x:(cx - 160) ~y:220 ~w:320 ~h:240 !grid

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
      Draw.text ~size:16 ~align:`Center ~x:(x + (card_w / 2)) ~y:(y + card_h + 8)
        (Printf.sprintf "%s · %s" s.player_name
           (fmt_clock (!cur_round_secs - s.seconds_left))))
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

let render_results (lines : P.score_line list) =
  let px = 37 in
  Draw.text ~size:30 ~bold:true ~color:T.accent2 ~x:(px + 3) ~y:33
    "ROUND RESULTS";
  Draw.text ~size:30 ~bold:true ~x:px ~y:30 "ROUND RESULTS";
  let bw = T.win_w - (2 * px) in
  (* rows compress to fit many players in the fixed window *)
  let n = List.length lines in
  let rh = if n <= 7 then 48 else max 24 (385 / n) in
  let box_h = rh - (if n <= 7 then 8 else 4) in
  let compact = rh < 40 in
  let sz_rank = if compact then 14 else 24
  and sz_name = if compact then 13 else 18
  and sz_detail = if compact then 12 else 16
  and sz_total = if compact then 14 else 22 in
  let ty extra = if compact then (box_h - 14) / 2 else extra in
  List.iteri
    (fun i (l : P.score_line) ->
      let y = 95 + (i * rh) in
      if i = 0
      then Draw.shadow_box ~off:4 ~shadow:T.accent ~x:px ~y ~w:bw ~h:box_h
             ~fill:P.white ()
      else (
        Draw.fill_rect ~x:px ~y ~w:bw ~h:box_h P.white;
        Draw.draw_border ~x:px ~y ~w:bw ~h:box_h ());
      Draw.text ~size:sz_rank ~bold:true
        ~color:(if i = 0 then T.accent else T.ink)
        ~x:(px + 14) ~y:(y + ty 8)
        (Printf.sprintf "#%d" (i + 1));
      Draw.text ~size:sz_name ~bold:true ~x:(px + 74) ~y:(y + ty 11)
        (l.s_name ^ (if l.s_player_id = !my_id then " (you)" else ""));
      Draw.text ~size:sz_detail ~color:T.muted ~align:`Right ~x:(px + bw - 110)
        ~y:(y + ty 12)
        (Printf.sprintf "stars %d + speed %d" l.votes l.speed);
      Draw.text ~size:sz_total ~bold:true ~align:`Right ~x:(px + bw - 16)
        ~y:(y + ty 9)
        (string_of_int l.total))
    lines;
  let by = T.win_h - 80 in
  Draw.text ~size:15 ~color:T.muted ~x:px ~y:(by - 30)
    (Printf.sprintf
       "score = star points (%d per star) + speed bonus (seconds left at submit)"
       P.star_points);
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
   | Drawing _ ->
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
  | Drawing _ ->
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
           (Js.string "Pick a Name")
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
