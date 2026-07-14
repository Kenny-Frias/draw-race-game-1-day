(* QUICKDRAW authoritative game server.
   One process: serves the static client page + js over HTTP and runs the
   game state machine over a websocket at /ws.
   State machine: Lobby -> (WordReveal+Drawing, one deadline) -> Voting -> Results. *)

open! Core
open! Async
module P = Quickdraw_shared.Protocol

let words =
  [| "windmill"; "rocket"; "octopus"; "lighthouse"; "bicycle"; "snowman"
   ; "volcano"; "umbrella"; "castle"; "robot"; "giraffe"; "pizza"; "sailboat"
   ; "cactus"; "dragon"; "ladder"; "crown"; "anchor"; "butterfly"; "mushroom"
   ; "campfire"; "telescope"; "whale"; "tractor"; "igloo"; "scissors"
   ; "guitar"; "penguin"; "hamburger"; "spider"; "clock"; "kite"; "submarine"
   ; "palm tree"; "skateboard"; "toaster"; "banana"; "ghost"; "wizard"
   ; "pirate ship"; "balloon"; "ferris wheel"; "ice cream"; "treehouse"
   ; "waterfall"; "helicopter"; "dinosaur"; "mermaid"; "spaceship"; "tornado"
   ; "jellyfish"; "accordion"; "juggler"; "vampire"; "beehive"; "drawbridge"
   ; "porcupine"; "cannon"; "iceberg"; "scarecrow"; "fountain"; "chandelier"
   ; "kangaroo"; "barbecue"; "parachute"; "snail"; "trophy"; "walrus"
   ; "windmill"; "xylophone"; "yeti"; "zeppelin"; "avalanche"; "bulldozer"
   ; "carousel"; "dandelion"; "escalator"; "flamingo"; "gargoyle"; "hammock"
  |]

let now () = Core_unix.gettimeofday ()

(* ---------- Game state ---------- *)

type client =
  { mutable player : P.player
  ; send : string Pipe.Writer.t
  }

(* sabotage: the first player to submit earns a one-shot color lock *)
type lock_state =
  | Lock_unearned
  | Lock_offered of int (* player id who may lock *)
  | Lock_done

(* points earned mid-round; carried into Voting so scoring can see them *)
type awards =
  { jackpot_winner : int option (* pid who painted the jackpot cell *)
  ; bonus_won : (int * int) list (* pid -> accumulated bonus-color points *)
  }

type phase =
  | Lobby
  | Drawing of
      { word : string
      ; deadline : float
      ; secs : int (* this round's length *)
      ; participants : int list
      ; mutable subs : P.submission list
      ; mutable over_sent : bool
      ; mutable lock : lock_state
      ; jackpot_cell : int (* flat grid index, secret until hit *)
      ; cursed_cell : int
      ; mutable jackpot_winner : int option
      ; mutable cursed_hit : int option (* pid who earned the wipe *)
      ; mutable curse_used : bool
      ; mutable bonus : (P.color * float) option (* active color, expiry *)
      ; mutable bonus_won : (int * int) list
      }
  | Voting of
      { word : string
      ; participants : int list
      ; subs : P.submission list
      ; mutable ballots : (int * (int * int) list) list (* voter_id, ranks *)
      ; awards : awards
      }
  | Results of P.score_line list

let clients : client list ref = ref []
let phase : phase ref = ref Lobby
let next_id = ref 1
let round_token = ref 0
let round_secs = ref P.round_seconds (* host-adjustable in the lobby *)

let send_to (c : client) (m : P.server_msg) =
  Pipe.write_without_pushback_if_open c.send (P.string_of_server_msg m)

let broadcast (m : P.server_msg) =
  let s = P.string_of_server_msg m in
  List.iter !clients ~f:(fun c -> Pipe.write_without_pushback_if_open c.send s)

(* round-phase messages go only to that round's participants, so
   late joiners stay quietly in the lobby until the next round *)
let send_to_ids ids (m : P.server_msg) =
  let s = P.string_of_server_msg m in
  List.iter !clients ~f:(fun c ->
    if List.mem ids c.player.id ~equal:Int.equal
    then Pipe.write_without_pushback_if_open c.send s)

let roster () = List.map !clients ~f:(fun c -> c.player)
let broadcast_lobby () = broadcast (Lobby (roster (), !round_secs))

let find_client id = List.find !clients ~f:(fun c -> c.player.id = id)

let reassign_host () =
  if not (List.exists !clients ~f:(fun c -> c.player.is_host))
  then (
    match !clients with
    | c :: _ -> c.player <- { c.player with is_host = true }
    | [] -> ())

(* ---------- Scoring ---------- *)

let compute_scores (subs : P.submission list)
      (ballots : (int * (int * int) list) list) (awards : awards)
  : P.score_line list =
  (* aggregate star ratings: every voter gives each other drawing
     1..max_stars, each star worth star_points *)
  let vote_points = Hashtbl.create (module Int) in
  List.iter ballots ~f:(fun (voter, ratings) ->
    List.iter ratings ~f:(fun (pid, stars) ->
      if pid <> voter && List.exists subs ~f:(fun s -> s.P.player_id = pid)
      then (
        let stars = Int.max 1 (Int.min stars P.max_stars) in
        Hashtbl.update vote_points pid ~f:(fun v ->
          Option.value v ~default:0 + (stars * P.star_points)))));
  List.map subs ~f:(fun (s : P.submission) ->
    let votes = Option.value (Hashtbl.find vote_points s.player_id) ~default:0 in
    let speed = Int.max 0 (Int.min s.seconds_left P.max_round_seconds) in
    let egg =
      match awards.jackpot_winner with
      | Some w when w = s.player_id -> P.egg_points
      | _ -> 0
    in
    let bonus =
      Option.value
        (List.Assoc.find awards.bonus_won s.player_id ~equal:Int.equal)
        ~default:0
    in
    { P.s_player_id = s.player_id
    ; s_name = s.player_name
    ; votes
    ; speed
    ; egg
    ; bonus
    ; total = votes + speed + egg + bonus
    })
  |> List.sort ~compare:(fun a b -> Int.compare b.P.total a.P.total)

(* ---------- Phase transitions ---------- *)

let to_results participants (word : string) subs ballots awards =
  ignore word;
  let scores = compute_scores subs ballots awards in
  phase := Results scores;
  send_to_ids participants (Results scores)

let to_voting () =
  match !phase with
  | Drawing { word; subs; participants; jackpot_winner; bonus_won; _ } ->
    let awards = { jackpot_winner; bonus_won } in
    (match subs with
     | [] | [ _ ] ->
       (* not enough drawings survived; bail to results with what we have *)
       to_results participants word subs [] awards
     | _ ->
       phase := Voting { word; participants; subs; ballots = []; awards };
       send_to_ids participants (Vote_now (word, subs));
       (* a zombie connection must not hang the round: settle with whatever
          ballots arrived after a generous window *)
       let token = !round_token in
       upon (Clock.after (Time_float.Span.of_sec 75.)) (fun () ->
         match !phase with
         | Voting v when !round_token = token ->
           to_results v.participants v.word v.subs v.ballots v.awards
         | _ -> ()))
  | _ -> ()

let check_voting_done () =
  match !phase with
  | Voting { word; participants; subs; ballots; awards } ->
    (* every submitter who is still connected must have voted *)
    let expected =
      List.filter subs ~f:(fun s -> Option.is_some (find_client s.player_id))
    in
    let voted id = List.exists ballots ~f:(fun (v, _) -> v = id) in
    if List.for_all expected ~f:(fun s -> voted s.P.player_id)
    then to_results participants word subs ballots awards
  | _ -> ()

let check_drawing_done () =
  match !phase with
  | Drawing d ->
    let alive =
      List.filter d.participants ~f:(fun id -> Option.is_some (find_client id))
    in
    let submitted id = List.exists d.subs ~f:(fun s -> s.P.player_id = id) in
    if List.for_all alive ~f:submitted then to_voting ()
  | _ -> ()

let start_round () =
  (* everyone connected plays: joining the lobby is being ready *)
  let ready = !clients in
  if List.length ready >= P.min_players
  then (
    incr round_token;
    let token = !round_token in
    let word = words.(Random.int (Array.length words)) in
    let secs = !round_secs in
    let deadline = now () +. Float.of_int (P.countdown_seconds + secs) in
    let participants = List.map ready ~f:(fun c -> c.player.id) in
    (* two distinct hidden cells: the jackpot and the curse *)
    let n_cells = P.grid_cols * P.grid_rows in
    let jackpot_cell = Random.int n_cells in
    let cursed_cell =
      let c = ref (Random.int n_cells) in
      while !c = jackpot_cell do
        c := Random.int n_cells
      done;
      !c
    in
    phase
    := Drawing
         { word
         ; deadline
         ; secs
         ; participants
         ; subs = []
         ; over_sent = false
         ; lock = Lock_unearned
         ; jackpot_cell
         ; cursed_cell
         ; jackpot_winner = None
         ; cursed_hit = None
         ; curse_used = false
         ; bonus = None
         ; bonus_won = []
         };
    send_to_ids participants (Word_reveal (word, deadline, secs));
    send_to_ids participants (Secret_cells (jackpot_cell, cursed_cell));
    (* a fresh bonus color every bonus_period_s while the round runs; skip
       spawns too close to the deadline for anyone to react *)
    let bonus_colors =
      Array.filter P.Tokens.palette ~f:(fun c -> c <> P.white)
    in
    let rec spawn_bonus () =
      upon
        (Clock.after (Time_float.Span.of_sec (Float.of_int P.bonus_period_s)))
        (fun () ->
          match !phase with
          | Drawing d when !round_token = token ->
            if Float.(now () < d.deadline -. 10.)
            then (
              let c = bonus_colors.(Random.int (Array.length bonus_colors)) in
              let expires = now () +. Float.of_int P.bonus_period_s in
              d.bonus <- Some (c, expires);
              send_to_ids d.participants (Bonus_color (c, expires));
              spawn_bonus ())
          | _ -> ())
    in
    spawn_bonus ();
    (* at the deadline, tell laggards to force-submit; 3s grace, then move on *)
    upon
      (Clock.at (Time_float.of_span_since_epoch (Time_float.Span.of_sec deadline)))
      (fun () ->
        match !phase with
        | Drawing d when !round_token = token && not d.over_sent ->
          d.over_sent <- true;
          send_to_ids d.participants Drawing_over;
          upon (Clock.after (Time_float.Span.of_sec 3.)) (fun () ->
            match !phase with
            | Drawing _ when !round_token = token -> to_voting ()
            | _ -> ())
        | _ -> ()))

(* ---------- Sabotage: color lock ---------- *)

(* Authoritative gate for a Lock_color request. The server must not trust the
   client UI: decide here who may lock, whom, and which colors count. *)
let lock_request_ok ~(lock : lock_state) ~(participants : int list)
      ~(subs : P.submission list) ~(locker : int) ~(target_id : int)
      ~(color : P.color) : bool =
  (* the offer is the proof: only the offered player may lock, and it flips
     to Lock_done on success so it can't be spent twice *)
  (match lock with
   | Lock_offered id -> id = locker
   | Lock_unearned | Lock_done -> false)
  && target_id <> locker
  && List.mem participants target_id ~equal:Int.equal
  (* locking a finished player is a wasted shot — reject so the lock isn't
     burned on someone it can't affect *)
  && (not (List.exists subs ~f:(fun s -> s.P.player_id = target_id)))
  (* white is the eraser/background: locking it would be a non-move *)
  && color <> P.white
  && Array.mem P.Tokens.palette color ~equal:Int.equal

(* ---------- Per-client message handling ---------- *)

let handle_msg (c : client) (msg : P.client_msg) =
  match msg with
  | Join _ -> () (* only valid as first message *)
  | Set_round_time secs ->
    (match !phase with
     | Lobby when c.player.is_host ->
       round_secs
       := Int.max P.min_round_seconds (Int.min P.max_round_seconds secs);
       broadcast_lobby ()
     | _ -> ())
  | Start_round | Next_round ->
    (match !phase with
     | (Lobby | Results _) when c.player.is_host -> start_round ()
     | _ -> ())
  | Back_to_lobby ->
    (match !phase with
     | Results _ when c.player.is_host ->
       phase := Lobby;
       broadcast Go_lobby;
       broadcast_lobby ()
     | _ -> ())
  | Submit (grid, seconds_left) ->
    (match !phase with
     | Drawing d
       when List.mem d.participants c.player.id ~equal:Int.equal
            && (not (List.exists d.subs ~f:(fun s -> s.P.player_id = c.player.id)))
            && Array.length grid = P.grid_cols * P.grid_rows ->
       d.subs
       <- d.subs
          @ [ { P.player_id = c.player.id
              ; player_name = c.player.name
              ; grid
              ; seconds_left = Int.max 0 (Int.min seconds_left d.secs)
              }
            ];
       (* first submitter earns the color lock — offer the opponents
          who are still connected and still drawing *)
       (match d.lock with
        | Lock_unearned ->
          let targets =
            List.filter_map d.participants ~f:(fun id ->
              if id = c.player.id
                 || List.exists d.subs ~f:(fun s -> s.P.player_id = id)
              then None
              else
                Option.map (find_client id) ~f:(fun t -> id, t.player.name))
          in
          if not (List.is_empty targets)
          then (
            d.lock <- Lock_offered c.player.id;
            send_to c (Lock_offer targets))
        | Lock_offered _ | Lock_done -> ());
       check_drawing_done ()
     | _ -> ())
  | Lock_color (target_id, color) ->
    (match !phase with
     | Drawing d
       when lock_request_ok ~lock:d.lock ~participants:d.participants
              ~subs:d.subs ~locker:c.player.id ~target_id ~color ->
       d.lock <- Lock_done;
       (match find_client target_id with
        | Some target -> send_to target (Color_locked (c.player.name, color))
        | None -> ())
     | _ -> ())
  | Hit_secret kind ->
    (match !phase with
     | Drawing d
       when List.mem d.participants c.player.id ~equal:Int.equal
            && not (List.exists d.subs ~f:(fun s -> s.P.player_id = c.player.id))
       ->
       (match kind with
        | P.Jackpot ->
          (* first claim wins; announce immediately, pay at results *)
          if Option.is_none d.jackpot_winner
          then (
            d.jackpot_winner <- Some c.player.id;
            send_to_ids d.participants (Jackpot_hit c.player.name))
        | P.Cursed ->
          if Option.is_none d.cursed_hit
          then (
            d.cursed_hit <- Some c.player.id;
            let targets =
              List.filter_map d.participants ~f:(fun id ->
                if id = c.player.id
                   || List.exists d.subs ~f:(fun s -> s.P.player_id = id)
                then None
                else
                  Option.map (find_client id) ~f:(fun t -> id, t.player.name))
            in
            if not (List.is_empty targets)
            then send_to c (Curse_offer targets)))
     | _ -> ())
  | Curse_wipe target_id ->
    (match !phase with
     | Drawing d
       when (match d.cursed_hit with
             | Some pid -> pid = c.player.id
             | None -> false)
            && (not d.curse_used)
            && target_id <> c.player.id
            && List.mem d.participants target_id ~equal:Int.equal
            && not (List.exists d.subs ~f:(fun s -> s.P.player_id = target_id))
       ->
       d.curse_used <- true;
       (match find_client target_id with
        | Some t -> send_to t (Wipe_cells (c.player.name, P.wipe_count))
        | None -> ())
     | _ -> ())
  | Claim_bonus ->
    (match !phase with
     | Drawing d
       when List.mem d.participants c.player.id ~equal:Int.equal
            && not (List.exists d.subs ~f:(fun s -> s.P.player_id = c.player.id))
       ->
       (match d.bonus with
        | Some (color, expires) when Float.(now () < expires) ->
          (* first claim takes the window; a new color spawns later *)
          d.bonus <- None;
          d.bonus_won
          <- ( c.player.id
             , P.bonus_points
               + Option.value
                   (List.Assoc.find d.bonus_won c.player.id ~equal:Int.equal)
                   ~default:0 )
             :: List.Assoc.remove d.bonus_won c.player.id ~equal:Int.equal;
          send_to_ids d.participants (Bonus_claimed (c.player.name, color))
        | _ -> ())
     | _ -> ())
  | Rate ratings ->
    (match !phase with
     | Voting v when not (List.exists v.ballots ~f:(fun (id, _) -> id = c.player.id))
       ->
       v.ballots <- (c.player.id, ratings) :: v.ballots;
       check_voting_done ()
     | _ -> ())

let on_disconnect (c : client) =
  printf "leave: %s (#%d)\n%!" c.player.name c.player.id;
  clients := List.filter !clients ~f:(fun c' -> not (phys_equal c c'));
  reassign_host ();
  broadcast_lobby ();
  (* a departure may unblock a phase waiting on them *)
  check_drawing_done ();
  check_voting_done ()

let serve_client reader writer =
  match%bind Pipe.read reader with
  | `Eof -> return ()
  | `Ok first ->
    (match Or_error.try_with (fun () -> P.client_msg_of_string first) with
     | Ok (Join name) when List.length !clients < P.max_players ->
       let name =
         let n = String.strip name in
         if String.is_empty n then "anon" else String.prefix n 12
       in
       let id = !next_id in
       incr next_id;
       let is_host = not (List.exists !clients ~f:(fun c -> c.player.is_host)) in
       let c =
         { player = { P.id; name; is_host; conn = P.Ready }; send = writer }
       in
       clients := !clients @ [ c ];
       printf "join: %s (#%d)%s\n%!" name id (if is_host then " [host]" else "");
       send_to c (Joined id);
       broadcast_lobby ();
       let%bind () =
         Pipe.iter_without_pushback reader ~f:(fun s ->
           match Or_error.try_with (fun () -> P.client_msg_of_string s) with
           | Ok msg -> handle_msg c msg
           | Error _ -> ())
       in
       on_disconnect c;
       return ()
     | Ok (Join _) ->
       Pipe.write_without_pushback_if_open
         writer
         (P.string_of_server_msg (Join_refused "game is full (8 players)"));
       return ()
     | _ -> return ())

(* ---------- HTTP ---------- *)

let index_html =
  {|<!doctype html>
<html><head><meta charset="utf-8"><title>QUICKDRAW</title>
<style>
html,body{margin:0;height:100%;background:#f5ead8;display:flex;align-items:center;justify-content:center}
canvas{border:2px solid #201e1d;box-shadow:6px 6px 0 #201e1d;background:#f5ead8;max-width:min(96vw,800px);height:auto;touch-action:none}
</style></head>
<body><canvas id="game" width="800" height="600"></canvas>
<script src="client.js"></script></body></html>|}

let http_handler ~static ~body:_ _addr (req : Cohttp.Request.t) =
  let path = Uri.path (Cohttp.Request.uri req) in
  match path with
  | "/" | "/index.html" ->
    Cohttp_async.Server.respond_string
      ~headers:(Cohttp.Header.of_list [ "content-type", "text/html" ])
      index_html
  | "/client.js" ->
    Cohttp_async.Server.respond_with_file
      ~headers:(Cohttp.Header.of_list [ "content-type", "application/javascript" ])
      (Filename.concat static "client.js")
  | _ -> Cohttp_async.Server.respond_string ~status:`Not_found "not found"

let ws_handler ~inet:_ ~subprotocol:_ (_req : Cohttp.Request.t) =
  return
    (Cohttp_async_websocket.Server.On_connection.create (fun ws ->
       let reader, writer = Websocket.pipes ws in
       serve_client reader writer))

let main ~port ~static =
  Random.self_init ();
  let handler =
    Cohttp_async_websocket.Server.create
      ~non_ws_request:(http_handler ~static)
      ~should_process_request:(fun _ _ ~is_websocket_request:_ -> return (Ok ()))
      ws_handler
  in
  let%bind _server =
    Cohttp_async.Server.create_expert
      ~on_handler_error:`Ignore
      (Tcp.Where_to_listen.of_port port)
      (fun ~body addr req -> handler ~body addr req)
  in
  printf "QUICKDRAW server listening on port %d\n%!" port;
  Deferred.never ()

let () =
  Command.async
    ~summary:"QUICKDRAW multiplayer drawing game server"
    (let%map_open.Command port =
       flag "-port" (optional_with_default 8000 int) ~doc:"PORT listen port"
     and static =
       flag
         "-static"
         (optional_with_default "static" string)
         ~doc:"DIR directory containing client.js"
     in
     fun () -> main ~port ~static)
  |> Command_unix.run
