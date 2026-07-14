(* Graphics-style drawing primitives over an 800x600 HTML canvas.
   The native Graphics library the design targets has no browser build in
   this switch, so this is a thin shim exposing the same vocabulary:
   fill_rect / fill_circle / draw_string / moveto-lineto equivalents, plus
   the design system's bordered "shadow box" idiom (a second filled rect
   offset +3,+3 behind the box). Coordinates are top-left origin, y down. *)

open Js_of_ocaml
module T = Quickdraw_shared.Protocol.Tokens

(* jsoo 6 passes canvas numerics as Js.number Js.t *)
let n = Js.number_of_float
let fl = Js.float_of_number

let canvas : Dom_html.canvasElement Js.t =
  match Dom_html.getElementById_coerce "game" Dom_html.CoerceTo.canvas with
  | Some c -> c
  | None -> failwith "no #game canvas"

let ctx = canvas##getContext Dom_html._2d_

let css (color : int) =
  Js.string (Printf.sprintf "#%06X" (color land 0xFFFFFF))

let fill_rectf ~x ~y ~w ~h color =
  ctx##.fillStyle := css color;
  ctx##fillRect (n x) (n y) (n w) (n h)

let fill_rect ~x ~y ~w ~h color =
  fill_rectf
    ~x:(float_of_int x) ~y:(float_of_int y)
    ~w:(float_of_int w) ~h:(float_of_int h) color

(* 2px ink border, drawn inside the given rect *)
let draw_border ?(lw = 2) ?(color = T.ink) ~x ~y ~w ~h () =
  ctx##.strokeStyle := css color;
  ctx##.lineWidth := n (float_of_int lw);
  let o = float_of_int lw /. 2. in
  ctx##strokeRect
    (n (float_of_int x +. o)) (n (float_of_int y +. o))
    (n (float_of_int (w - lw))) (n (float_of_int (h - lw)))

let fill_circle ~cx ~cy ~r color =
  ctx##.fillStyle := css color;
  ctx##beginPath;
  ctx##arc (n (float_of_int cx)) (n (float_of_int cy)) (n (float_of_int r))
    (n 0.) (n (2. *. Float.pi)) Js._false;
  ctx##fill

let line ?(lw = 2) ~x1 ~y1 ~x2 ~y2 color =
  ctx##.strokeStyle := css color;
  ctx##.lineWidth := n (float_of_int lw);
  ctx##beginPath;
  ctx##moveTo (n (float_of_int x1)) (n (float_of_int y1));
  ctx##lineTo (n (float_of_int x2)) (n (float_of_int y2));
  ctx##stroke

let font ~size ~bold =
  Js.string
    (Printf.sprintf "%s%dpx 'Courier New', ui-monospace, monospace"
       (if bold then "bold " else "") size)

let text_width ?(size = 16) ?(bold = false) s =
  ctx##.font := font ~size ~bold;
  fl (ctx##measureText (Js.string s))##.width

(* [y] is the text's top edge (textBaseline=top keeps layout math simple) *)
let text ?(size = 16) ?(bold = false) ?(color = T.ink) ?(align = `Left) ~x ~y s =
  ctx##.font := font ~size ~bold;
  ctx##.fillStyle := css color;
  ctx##.textBaseline := Js.string "top";
  let xf =
    let w = fl (ctx##measureText (Js.string s))##.width in
    match align with
    | `Left -> float_of_int x
    | `Center -> float_of_int x -. (w /. 2.)
    | `Right -> float_of_int x -. w
  in
  ctx##fillText (Js.string s) (n xf) (n (float_of_int y))

(* The design-system box: hard offset shadow + fill + 2px ink border *)
let shadow_box ?(off = 3) ?(shadow = T.ink) ?(border = T.ink) ?(border_w = 2)
    ~x ~y ~w ~h ~fill () =
  fill_rect ~x:(x + off) ~y:(y + off) ~w ~h shadow;
  fill_rect ~x ~y ~w ~h fill;
  draw_border ~lw:border_w ~color:border ~x ~y ~w ~h ()

(* 5-pointed star (voting screen ratings) *)
let star_path ~cx ~cy ~r =
  ctx##beginPath;
  for i = 0 to 9 do
    let rr = if i mod 2 = 0 then float_of_int r else float_of_int r *. 0.45 in
    let a = (Float.pi *. float_of_int i /. 5.) -. (Float.pi /. 2.) in
    let x = float_of_int cx +. (rr *. cos a)
    and y = float_of_int cy +. (rr *. sin a) in
    if i = 0 then ctx##moveTo (n x) (n y) else ctx##lineTo (n x) (n y)
  done;
  ctx##closePath

let fill_star ~cx ~cy ~r color =
  star_path ~cx ~cy ~r;
  ctx##.fillStyle := css color;
  ctx##fill

let stroke_star ?(lw = 2) ~cx ~cy ~r color =
  star_path ~cx ~cy ~r;
  ctx##.strokeStyle := css color;
  ctx##.lineWidth := n (float_of_int lw);
  ctx##stroke

let clear () = fill_rect ~x:0 ~y:0 ~w:T.win_w ~h:T.win_h T.ground

let set_alpha a = ctx##.globalAlpha := n a
