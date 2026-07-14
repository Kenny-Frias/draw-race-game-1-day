(* End-to-end test bot: N bots connect over real websockets and play a full
   round (join -> ready -> start -> submit -> rank -> results). Exits 0 iff
   every bot reaches the Results screen. *)

open! Core
open! Async
module P = Quickdraw_shared.Protocol

let play ~uri ~name ~is_starter ~n_bots =
  let got_results = Ivar.create () in
  don't_wait_for
    (Deferred.ignore_m
       (Cohttp_async_websocket.Client.with_websocket_client uri
          ~f:(fun _resp ws ->
            let reader, writer = Websocket.pipes ws in
            let send m =
              Pipe.write_without_pushback_if_open writer
                (P.string_of_client_msg m)
            in
            send (Join name);
            let my_id = ref (-1) in
            let%bind () =
              Pipe.iter_without_pushback reader ~f:(fun s ->
                match P.server_msg_of_string s with
                | Joined id ->
                  printf "[%s] joined as #%d\n%!" name id;
                  my_id := id
                | Join_refused why -> printf "[%s] REFUSED: %s\n%!" name why
                | Lobby (ps, secs) ->
                  let ready = List.length ps in
                  printf "[%s] lobby: %d players (all ready), round=%ds\n%!"
                    name ready secs;
                  if is_starter && ready >= n_bots
                  then (
                    (* exercise the host round-time control: 90 -> 120s *)
                    if secs = P.round_seconds
                    then send (Set_round_time 120)
                    else if secs = 120 then send Start_round)
                | Word_reveal (w, dl, secs) ->
                  printf "[%s] word=%s deadline in %.1fs (round %ds)\n%!" name w
                    (dl -. Core_unix.gettimeofday ())
                    secs;
                  (* draw something: a few colored cells, then submit early *)
                  let g = P.empty_grid () in
                  Array.iteri g ~f:(fun i _ ->
                    if i % 97 = 0 then g.(i) <- P.Tokens.accent);
                  don't_wait_for
                    (let%map () = Clock.after (Time_float.Span.of_sec 1.) in
                     send (Submit (g, 42)))
                | Drawing_over -> printf "[%s] drawing over\n%!" name
                | Vote_now (w, subs) ->
                  printf "[%s] rating %d %ss\n%!" name (List.length subs) w;
                  let opponents =
                    List.filter subs ~f:(fun s -> s.P.player_id <> !my_id)
                  in
                  (* varied star ratings so aggregation is exercised *)
                  send
                    (Rate (List.mapi opponents ~f:(fun i s ->
                       s.P.player_id, (i mod P.max_stars) + 1)))
                | Results lines ->
                  printf "[%s] RESULTS: %s\n%!" name
                    (String.concat ~sep:" | "
                       (List.map lines ~f:(fun l ->
                          sprintf "%s votes=%d speed=%d egg=%d bonus=%d total=%d"
                            l.s_name l.votes l.speed l.egg l.bonus l.total)));
                  Ivar.fill_if_empty got_results ()
                | Secret_cells (j, cu) ->
                  printf "[%s] secrets: jackpot@%d cursed@%d\n%!" name j cu;
                  (* every bot races for the jackpot; the starter also pokes
                     the cursed cell to exercise the wipe path *)
                  send (Hit_secret Jackpot);
                  if is_starter then send (Hit_secret Cursed)
                | Jackpot_hit who ->
                  printf "[%s] jackpot found by %s (+%d)\n%!" name who
                    P.egg_points
                | Curse_offer targets ->
                  (match targets with
                   | (tid, tname) :: _ ->
                     printf "[%s] cursed cell! wiping %s\n%!" name tname;
                     send (Curse_wipe tid)
                   | [] -> ())
                | Wipe_cells (who, n) ->
                  printf "[%s] SABOTAGED: %s wiped %d of my cells\n%!" name
                    who n
                | Bonus_color (c, _expires) ->
                  printf "[%s] bonus color %s is up\n%!" name (P.color_name c)
                | Bonus_claimed (who, c) ->
                  printf "[%s] %s banked the %s bonus\n%!" name who
                    (P.color_name c)
                | Lock_offer targets ->
                  (* exercise the sabotage path: lock orange on someone *)
                  (match targets with
                   | (tid, tname) :: _ ->
                     printf "[%s] first to submit! locking ORANGE on %s\n%!"
                       name tname;
                     send (Lock_color (tid, P.Tokens.accent))
                   | [] -> ())
                | Color_locked (who, c) ->
                  printf "[%s] SABOTAGED: %s locked %s on me\n%!" name who
                    (P.color_name c)
                | Go_lobby -> ())
            in
            return ())));
  Ivar.read got_results

(* ---------- Companion mode: bots that fill the lobby for a human game ----------
   They never start rounds or touch settings; they join, draw random shapes,
   rate randomly, and keep playing round after round until killed. *)

let random_drawing () =
  let g = P.empty_grid () in
  let palette = P.Tokens.palette in
  let rand_color () = palette.(Random.int (Array.length palette)) in
  let set c r color =
    if c >= 0 && c < P.grid_cols && r >= 0 && r < P.grid_rows
    then g.((r * P.grid_cols) + c) <- color
  in
  (* a few filled rectangles *)
  for _ = 1 to 3 + Random.int 4 do
    let color = rand_color () in
    let c0 = Random.int P.grid_cols
    and r0 = Random.int P.grid_rows in
    let w = 4 + Random.int 20
    and h = 4 + Random.int 16 in
    for c = c0 to Int.min (P.grid_cols - 1) (c0 + w) do
      for r = r0 to Int.min (P.grid_rows - 1) (r0 + h) do
        set c r color
      done
    done
  done;
  (* a few thick strokes *)
  for _ = 1 to 2 + Random.int 4 do
    let color = rand_color () in
    let c0 = Random.int P.grid_cols
    and r0 = Random.int P.grid_rows
    and c1 = Random.int P.grid_cols
    and r1 = Random.int P.grid_rows in
    let n = Int.max 1 (Int.max (abs (c1 - c0)) (abs (r1 - r0))) in
    for i = 0 to n do
      let c = c0 + ((c1 - c0) * i / n)
      and r = r0 + ((r1 - r0) * i / n) in
      for dc = -1 to 1 do
        for dr = -1 to 1 do
          set (c + dc) (r + dr) color
        done
      done
    done
  done;
  g

let companion_play ~uri ~name =
  Deferred.ignore_m
    (Cohttp_async_websocket.Client.with_websocket_client uri
       ~f:(fun _resp ws ->
         let reader, writer = Websocket.pipes ws in
         let send m =
           Pipe.write_without_pushback_if_open writer (P.string_of_client_msg m)
         in
         send (Join name);
         let my_id = ref (-1) in
         Pipe.iter_without_pushback reader ~f:(fun s ->
           match P.server_msg_of_string s with
           | Joined id ->
             printf "[%s] in the lobby as #%d\n%!" name id;
             my_id := id
           | Word_reveal (_w, dl, secs) ->
             (* draw for a believable, varied amount of time *)
             let delay =
               3.
               +. Random.float (Float.min 15. (Float.of_int secs *. 0.6))
             in
             don't_wait_for
               (let%map () = Clock.after (Time_float.Span.of_sec delay) in
                let left =
                  Int.max 0 (Int.of_float (dl -. Core_unix.gettimeofday ()))
                in
                send (Submit (random_drawing (), left)))
           | Vote_now (_w, subs) ->
             let opps =
               List.filter subs ~f:(fun s -> s.P.player_id <> !my_id)
             in
             don't_wait_for
               (let%map () =
                  Clock.after (Time_float.Span.of_sec (1. +. Random.float 3.))
                in
                send
                  (Rate
                     (List.map opps ~f:(fun s ->
                        s.P.player_id, 1 + Random.int P.max_stars))))
           | _ -> ())))

let companion_names =
  [ "ziggy"; "pixel"; "doodle"; "scribble"; "crayon"; "smudge"; "inky"
  ; "sketchy"; "blotch"; "dotty"; "marker"; "stencil"; "easel"; "fresco"
  ; "gouache"; "mural"
  ]

let main ~port ~n_bots ~companion =
  Random.self_init ();
  let uri = Uri.of_string (sprintf "http://127.0.0.1:%d/ws" port) in
  if companion
  then (
    let plays =
      List.init n_bots ~f:(fun i ->
        let name =
          Option.value (List.nth companion_names i)
            ~default:(sprintf "bot%d" (i + 1))
        in
        companion_play ~uri ~name)
    in
    (* self-expire so no zombie fleet lingers *)
    match%bind
      Clock.with_timeout (Time_float.Span.of_hr 2.) (Deferred.all_unit plays)
    with
    | `Result () | `Timeout ->
      printf "companion bots done\n%!";
      return ())
  else (
    let results =
      List.init n_bots ~f:(fun i ->
        play ~uri ~name:(sprintf "bot%d" (i + 1)) ~is_starter:(i = 0) ~n_bots)
    in
    match%bind
      Clock.with_timeout (Time_float.Span.of_sec 30.)
        (Deferred.all_unit results)
    with
    | `Result () ->
      printf "ALL %d BOTS REACHED RESULTS - PASS\n%!" n_bots;
      return ()
    | `Timeout ->
      printf "TIMEOUT - FAIL\n%!";
      exit 1)

let () =
  Command.async ~summary:"quickdraw test bots"
    (let%map_open.Command port =
       flag "-port" (optional_with_default 8080 int) ~doc:"PORT server port"
     and n_bots = flag "-n" (optional_with_default 3 int) ~doc:"N bot count"
     and companion =
       flag "-companion" no_arg
         ~doc:
           " join as passive players: random drawings and ratings, never \
            start rounds, play forever"
     in
     fun () -> main ~port ~n_bots ~companion)
  |> Command_unix.run
