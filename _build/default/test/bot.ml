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
            send (Set_ready true);
            let my_id = ref (-1) in
            let%bind () =
              Pipe.iter_without_pushback reader ~f:(fun s ->
                match P.server_msg_of_string s with
                | Joined id ->
                  printf "[%s] joined as #%d\n%!" name id;
                  my_id := id
                | Join_refused why -> printf "[%s] REFUSED: %s\n%!" name why
                | Lobby ps ->
                  let ready =
                    List.count ps ~f:(fun p ->
                      match p.conn with P.Ready -> true | _ -> false)
                  in
                  printf "[%s] lobby: %d players, %d ready\n%!" name
                    (List.length ps) ready;
                  if is_starter && ready >= n_bots then send Start_round
                | Word_reveal (w, dl) ->
                  printf "[%s] word=%s deadline in %.1fs\n%!" name w
                    (dl -. Core_unix.gettimeofday ());
                  (* draw something: a few colored cells, then submit early *)
                  let g = P.empty_grid () in
                  Array.iteri g ~f:(fun i _ ->
                    if i % 97 = 0 then g.(i) <- P.Tokens.accent);
                  don't_wait_for
                    (let%map () = Clock.after (Time_float.Span.of_sec 1.) in
                     send (Submit (g, 42)))
                | Drawing_over -> printf "[%s] drawing over\n%!" name
                | Vote_now (w, subs) ->
                  printf "[%s] voting on %d %ss\n%!" name (List.length subs) w;
                  let opponents =
                    List.filter subs ~f:(fun s -> s.P.player_id <> !my_id)
                  in
                  send
                    (Rank (List.mapi opponents ~f:(fun i s ->
                       s.P.player_id, i + 1)))
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

let main ~port ~n_bots =
  let uri = Uri.of_string (sprintf "http://127.0.0.1:%d/ws" port) in
  let results =
    List.init n_bots ~f:(fun i ->
      play ~uri ~name:(sprintf "bot%d" (i + 1)) ~is_starter:(i = 0) ~n_bots)
  in
  match%bind
    Clock.with_timeout (Time_float.Span.of_sec 30.) (Deferred.all_unit results)
  with
  | `Result () ->
    printf "ALL %d BOTS REACHED RESULTS - PASS\n%!" n_bots;
    return ()
  | `Timeout ->
    printf "TIMEOUT - FAIL\n%!";
    exit 1

let () =
  Command.async ~summary:"quickdraw test bots"
    (let%map_open.Command port =
       flag "-port" (optional_with_default 8080 int) ~doc:"PORT server port"
     and n_bots = flag "-n" (optional_with_default 3 int) ~doc:"N bot count" in
     fun () -> main ~port ~n_bots)
  |> Command_unix.run
