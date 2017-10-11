open Lwt.Infix

let flip f x y = f y x
let pi = 4.0 *. atan 1.0

let window = Dom_html.window
let str = Js.string
let alert s = window##alert (str s)

let ( %% ) n k =
  (* Fixes modulo for negative n *)
  if n < 0
  then k + n mod k
  else n mod k

let () = Random.self_init ()

let ( @> ) id coercion =
  try
    Dom_html.getElementById id
    |> coercion
    |> flip Js.Opt.get (fun () -> alert "Coercion impossible"; assert false)
  with Not_found ->
    Printf.ksprintf alert {|Élément "%s" introuvable|} id;
    raise Not_found

(*************************)
(* Behaviour of the game *)
(*************************)

type direction =
  | North
  | South
  | East
  | West

let opposite = function
  | North -> South
  | South -> North
  | West -> East
  | East -> West

type tile =
(*
  | Wall
  | Mouse
*)
  | Empty
  | SnakeHead of { from : direction }
  | SnakeBody of { from : direction; toward : direction }
  | SnakeTail of { toward : direction }
  | Apple

type state = {
  grid : tile array array;
  mutable direction : direction;
  mutable just_ate : bool;
  mutable head_pos : int * int;
  mutable tail_pos : int * int;
}

type move_result =
  | AteApple
  | AteItself
  | Nothing

let head_comes_from state =
  (* Return the direction from which the head comes *)
  let head_i, head_j = state.head_pos in
  match state.grid.(head_i).(head_j) with
  | SnakeHead { from } -> from
  | _ -> assert false

let body_goes_toward state (i, j) =
  (* Return the direction toward which or a body segment goes *)
  match state.grid.(i).(j) with
  | SnakeBody { toward; _ } -> toward
  | _ -> assert false

let tail_goes_toward state =
  (* Return the direction toward which the tail goes *)
  let (i, j) = state.tail_pos in
  match state.grid.(i).(j) with
  | SnakeTail { toward } -> toward
  | _ -> assert false

let next_position state (i, j) direction =
  let (i, j) = match direction with
    | North -> i - 1, j
    | South -> i + 1, j
    | East -> i, j - 1
    | West -> i, j + 1 in
  i %% Array.length state.grid,
  j %% Array.length state.grid.(0)

let move_head state =
  (* Moves the head one step forward. Returns the result of this move:
     either the snake ate an apple, or it ate itself, or nothing
     particular happened. This is supposed to be run after the tail
     moves, so eating it is losing. *)
  let i, j = state.head_pos in
  let from = head_comes_from state in

  (* Replace the head by a body segment *)
  state.grid.(i).(j) <- SnakeBody { from; toward = state.direction };

  (* Get the new position of the head *)
  let i', j' = next_position state (i, j) state.direction in

  (* Get the result and set just_ate  *)
  let result = match state.grid.(i').(j') with
    | Empty ->
      state.just_ate <- false;
      Nothing
    | SnakeTail _
    | SnakeBody _ -> AteItself
    | Apple ->
      state.just_ate <- true;
      AteApple
    | SnakeHead _ -> assert false
  in

  (* Move the head *)
  state.grid.(i').(j') <- SnakeHead { from = opposite state.direction };
  state.head_pos <- (i', j');

  (* Return the result of the move *)
  result

let move_tail state =
  let (i, j) = state.tail_pos in

  (* Remove the tail *)
  let tail_dir = tail_goes_toward state in
  state.grid.(i).(j) <- Empty;

  (* Create the new tail *)
  let (i', j') = next_position state (i, j) tail_dir in
  let toward = body_goes_toward state (i', j') in
  state.grid.(i').(j') <- SnakeTail { toward };
  state.tail_pos <- (i', j')

let rec spawn_apple state =
  (* Puts an apple on a board on a free position *)
  let i = Random.int (Array.length state.grid) in
  let j = Random.int (Array.length state.grid.(0)) in
  if state.grid.(i).(j) = Empty
  then state.grid.(i).(j) <- Apple
  else spawn_apple state

let update state =
  (* Updates one state: moves the head, possibly moves the tail and
     spawns an apple. Returns the result of the move. *)
  if not state.just_ate then move_tail state;
  let result = move_head state in
  if result = AteApple then spawn_apple state;
  result

let make_state ~rows ~cols =
  let grid = Array.make_matrix rows cols Empty in
  let i, j = rows / 2, cols / 2 in
  grid.(i).(j - 1) <- SnakeTail { toward = West };
  grid.(i).(j) <- SnakeBody { from = East; toward = West };
  grid.(i).(j + 1) <- SnakeHead { from = East };
  let state = {
    grid;
    direction = West;
    just_ate = false;
    head_pos = (i, j + 1);
    tail_pos = (i, j - 1);
  } in spawn_apple state;
  state

let register_direction direction state =
  (* Tries to register a new direction for the snake. If the head
     comes from this direction, does nothing. *)
  let head_direction = head_comes_from state in
  if direction <> head_direction
  then state.direction <- direction

(******************)
(* Graphical part *)
(******************)

let canvas_coords (i, j) =
  float (j * 10), float (i * 10)

let draw_head context _from x y =
  context##.fillStyle := str "#0000FF";
  context##fillRect x y 10. 10.

let draw_body context _from _toward x y =
  context##.fillStyle := str "#00FF00";
  context##fillRect x y 10. 10.

let draw_tail context _toward x y =
  context##.fillStyle := str "#000000";
  context##fillRect x y 10. 10.

let draw_apple context x y =
  context##.fillStyle := str "#FF0000";
  context##fillRect x y 10. 10.

let draw_tile context i j tile =
  let x, y = canvas_coords (i, j) in
  match tile with
  | SnakeHead { from } -> draw_head context from x y
  | SnakeBody { from; toward } -> draw_body context from toward x y
  | SnakeTail { toward } -> draw_tail context toward x y
  | Apple -> draw_apple context x y
  | Empty -> ()

let draw_state context state =
  let rows = Array.length state.grid in
  let cols = Array.length state.grid.(0) in
  let x, y = canvas_coords (rows, cols) in
  context##clearRect 0. 0. x y;
  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      draw_tile context i j state.grid.(i).(j)
    done
  done;
  context##fill

let rec main_loop context state =
  Lwt_js.sleep 0.05 >>= fun () ->
  let result = update state in
  draw_state context state;
  if result <> AteItself
  then main_loop context state
  else (alert "Perdu"; Lwt.return ())

let change_direction state = function
  | "ArrowRight" -> register_direction West state
  | "ArrowLeft" -> register_direction East state
  | "ArrowUp" -> register_direction North state
  | "ArrowDown" -> register_direction South state
  | _ -> ()

let control_loop state =
  Lwt_js_events.keypresses
    window
    (fun ev _thread ->
       let key_pressed =
         Js.Optdef.get ev##.key
           (fun () -> alert "oups"; assert false) in
       change_direction state (Js.to_string key_pressed);
       Lwt.return ())

let display_keyboard () =
  let key_elt = Dom_html.getElementById "key" in
  Lwt_js_events.keypresses
    window
    (fun ev _thread ->
       let key_pressed =
         Js.Optdef.get ev##.key
           (fun () ->
              key_elt##.innerHTML := (str "oups");
              assert false) in
       key_elt##.innerHTML := key_pressed;
       Lwt.return ())

let _ : unit Lwt.t =
  let%lwt () = Lwt_js_events.domContentLoaded () in
  let canvas = "canvas" @> Dom_html.CoerceTo.canvas in
  let context = canvas##getContext Dom_html._2d_ in
  let state = make_state ~rows:50 ~cols:100 in
  Lwt.join [
    main_loop context state;
    control_loop state;
    display_keyboard ();
  ]


(*
Local Variables:
compile-command: "ocamlfind ocamlc -package js_of_ocaml-lwt -package js_of_ocaml.ppx \
 -package lwt.ppx \
 -linkpkg -o snake.bytes snake.ml; js_of_ocaml snake.bytes"
End:
*)
