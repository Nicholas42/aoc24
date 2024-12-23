open Aoc24


let () = Printexc.record_backtrace true
type remote_press = Left | Right | Up | Down | A

let remote_presses = [ Left; Right; Up; Down; A ]

module PositionTable = CCHashtbl.Make (struct
  type t = position

  let equal = ( = )
  let hash = Hashtbl.hash
end)

let dir_to_c = function
  | Left -> '<'
  | Right -> '>'
  | Up -> '^'
  | Down -> 'v'
  | A -> 'A'

type keypad_press =
  | Num0
  | Num1
  | Num2
  | Num3
  | Num4
  | Num5
  | Num6
  | Num7
  | Num8
  | Num9
  | NumA

let num_of_char = function
  | '0' -> Some Num0
  | '1' -> Some Num1
  | '2' -> Some Num2
  | '3' -> Some Num3
  | '4' -> Some Num4
  | '5' -> Some Num5
  | '6' -> Some Num6
  | '7' -> Some Num7
  | '8' -> Some Num8
  | '9' -> Some Num9
  | 'A' -> Some NumA
  | _ -> None

let dir_of_char = function
  | '<' -> Some Left
  | '>' -> Some Right
  | '^' -> Some Up
  | 'v' -> Some Down
  | 'A' -> Some A
  | _ -> None

let keypad_char = [ "789"; "456"; "123"; " 0A" ] |> Matrix.from_input
let keypad = Matrix.map num_of_char keypad_char
let remotepad_char = [ " ^A"; "<v>" ] |> Matrix.from_input
let remotepad = Matrix.map dir_of_char remotepad_char

let keypad_A =
  Matrix.find_pos (( = ) (Some NumA)) keypad
  |> CCOption.get_exn_or "No A on keypad"
  |> fst

let remotepad_A =
  Matrix.find_pos (( = ) (Some A)) remotepad
  |> CCOption.get_exn_or "No A on remotepad"
  |> fst

let move_to_pos = function
  | Left -> { x = ~-1; y = 0 }
  | Right -> { x = 1; y = 0 }
  | Up -> { x = 0; y = ~-1 }
  | Down -> { x = 0; y = 1 }
  | A -> { x = 0; y = 0 }

let apply_move kp current move =
  let delta = move_to_pos move in
  let next = IntPosition.add current delta in
  Matrix.get_opt kp next |> CCOption.flatten |> CCOption.map (CCFun.const next)
  |> CCFun.tap (fun res -> if res = Some {x=0;y=0}then failwith "moved to emtpy")

(* let get_all_paths matrix src_pos tgt_pos = *)
(*   let diff = IntPosition.diff tgt_pos src_pos in *)
(*   let hor_move = if diff.x < 0 then Left else Right in *)
(*   let vert_move = if diff.y < 0 then Up else Down in *)
(*   let one_move = *)
(*     CCList.append *)
(*       (CCList.repeat (abs diff.x) [ hor_move ]) *)
(*       (CCList.repeat (abs diff.y) [ vert_move ]) *)
(*   in *)
(*   let x_first_corner = { src_pos with x = tgt_pos.x } in *)
(*   let y_first_corner = { src_pos with y = tgt_pos.y } in *)
(*   match *)
(*     (Matrix.get_exn matrix x_first_corner, Matrix.get_exn matrix y_first_corner) *)
(*   with *)
(*   | ' ', ' ' -> failwith "Both ways over space shouldn't happen" *)
(*   | ' ', _ -> [ CCList.rev one_move ] *)
(*   | _, ' ' -> [ one_move ] *)
(*   | _, _ -> [ one_move; CCList.rev one_move ] *)

(* let get_all_paths_from_value matrix src tgt = *)
(*   let src_pos = *)
(*     Matrix.find_pos (( = ) src) matrix |> CCOption.get_exn_or "no source" |> fst *)
(*   in *)
(*   let tgt_pos = *)
(*     Matrix.find_pos (( = ) tgt) matrix |> CCOption.get_exn_or "no target" |> fst *)
(*   in *)
(*   get_all_paths matrix src_pos tgt_pos *)

(* let snoc l e = CCList.rev l |> CCList.cons e |> CCList.rev *)
(*     +---+---+ *)
(*     | ^ | A | *)
(* +---+---+---+ *)
(* | < | v | > | *)
(* +---+---+---+ *)

(* let apply_remote instruction current = *)
(*   match (current, instruction) with *)
(*   | x, A -> x *)
(*   | Left, Right -> Down *)
(*   | Down, Left -> Left *)
(*   | Down, Right -> Right *)
(*   | Down, Up -> Up *)
(*   | Right, Left -> Down *)
(*   | Right, Up -> A *)
(*   | Up, Down -> Down *)
(*   | Up, Right -> A *)
(*   | A, Left -> Up *)
(*   | A, Down -> Right *)
(*   | _, _ -> failwith "I cannot go there" *)
(* *)
(* let apply_keypad instruction current = *)
(*   match (current, instruction) with *)
(*   | x, A -> x *)

let rec apply_recursive ?(last_is_keypad = true) (state : position list)
    (instruction : remote_press) =
  match state with
  | [] -> failwith "shouldn't make this list empty"
  | [ last ] when last_is_keypad ->
      apply_move keypad last instruction |> CCOption.map CCList.return
  | pos :: rest ->
      if instruction = A && rest <> [] then
        apply_recursive ~last_is_keypad rest
          (Matrix.get_exn remotepad pos
          |> CCOption.get_exn_or "invalid instruction")
        |> CCOption.map (CCList.cons pos)
      else
        apply_move remotepad pos instruction |> CCOption.map (CCList.cons' rest)

let graph ?(last_is_keypad = true) () =
  CCGraph.make (fun v ->
      CCList.filter_map
        (fun dir ->
          apply_recursive ~last_is_keypad v dir
          |> CCOption.map (fun n -> (dir, n)))
        remote_presses
      |> CCList.to_iter)

(* *)
(* let rec reverse_path ?(stack = []) = function *)
(*   | [] -> stack *)
(*   | Left :: rest -> reverse_path ~stack:(Right :: stack) rest *)
(*   | Right :: rest -> reverse_path ~stack:(Left :: stack) rest *)
(*   | Up :: rest -> reverse_path ~stack:(Down :: stack) rest *)
(*   | Down :: rest -> reverse_path ~stack:(Up :: stack) rest *)
(*   | A :: _ -> failwith "Cannot reverse with press" *)
(* *)
(* let rec get_remote_paths source target = *)
(*   match (source, target) with *)
(*   | x, y when x = y -> [] *)
(*   | Left, Right -> [ [ Right; Right ] ] *)
(*   | Left, Up -> [ [ Right; Up ] ] *)
(*   | Left, Down -> [ [ Right ] ] *)
(*   | Left, A -> [ [ Right; Right; Up ] ] *)
(*   | Right, Up -> [ [ Left; Up ]; [ Up; Left ] ] *)
(*   | Right, Down -> [ [ Left ] ] *)
(*   | Right, A -> [ [ Up ] ] *)
(*   | Up, Down -> [ [ Down ] ] *)
(*   | Up, A -> [ [ Right ] ] *)
(*   | Down, A -> [ [ Right; Up ]; [ Up; Right ] ] *)
(*   | _, _ -> CCList.map reverse_path @@ get_remote_paths target source *)

let start depth =
  keypad_A :: CCList.replicate (depth - 1) remotepad_A |> CCList.rev

let get_target depth c =
  num_of_char c
  |> CCFun.tap (fun res -> if res = None then failwith "Invalid digit")
  |> fun n ->
  Matrix.find_pos (( = ) n) keypad
  |> CCOption.get_exn_or "digit not on keypad"
  |> fst
  |> CCList.cons' (CCList.replicate (depth - 1) remotepad_A)
  |> CCList.rev

let get_target_pure_remote depth c =
  dir_of_char c
  |> CCFun.tap (fun res -> if res = None then failwith "Invalid dir")
  |> fun d ->
  Matrix.find_pos (( = ) d) remotepad
  |> CCOption.get_exn_or "dir not on remotepad"
  |> fst
  |> CCList.cons' (CCList.replicate (depth - 1) remotepad_A)
  |> CCList.rev

let node_to_str node =
  CCList.map (Matrix.get_exn remotepad_char) node |> CCString.of_list

let _print_path path =
  CCList.map (fun (_, e, _) -> dir_to_c e) path
  |> CCList.cons ' ' |> CCString.of_list |> print_endline;
  CCList.map (fun (_, _, v) -> node_to_str v) path
  |> CCList.cons (CCList.hd path |> fun (v, _, _) -> node_to_str v)
  |> Matrix.from_input |> Matrix.transpose |> Matrix.print print_char

let get_path g start target =
  CCGraph.Traverse.dijkstra
    ~tbl:(CCGraph.mk_table ~eq:( = ) 10)
    ~graph:g
    (CCGraph.Iter.return start)
  |> CCGraph.Iter.filter_map (fun ((v, _, _) as tup) ->
         if v = target then Some tup else None)
  |> Iter.head
  |> CCOption.get_exn_or "Cannot reach target"

let type_one depth start c =
  let target = get_target depth c in
  get_path (graph()) start target |> fun (_, d, _) -> (d + 1, target)

let type_many depth start code =
  CCString.fold_left
    (fun (dist, last) c -> type_one depth last c |> CCPair.map_fst (( + ) dist))
    (0, start) code
  |> fst

let eval_code depth start code =
  let value =
    CCString.rdrop_while (CCFun.negate is_digit) code |> int_of_string
  in
  let dist = type_many depth start code in
  CCFormat.printf "%d * %d = %d (D=%d)\n" value dist (value * dist) depth;
  dist * value

let check_path depth start code =
  let target = get_target_pure_remote depth code in
  get_path (graph ~last_is_keypad:false ()) start target |> fun (_, _, p) ->
  _print_path (CCList.rev ((target, A, target) :: p));
  target

let _part1 input =
  let depth = 3 in
  let start = start depth in
  CCList.fold_left (fun acc code -> acc + eval_code depth start code) 0 input

let part2 input =
  input |> ignore;
  let depth = 2 in
  let inp = "<" in
  CCString.fold_left
    (fun acc c -> check_path depth acc c)
    (CCList.replicate depth remotepad_A)
    inp
  |> ignore;
  1

(* CCList.range 3 7 *)
(* |> CCList.map (fun depth -> *)
(*        let start = start depth in *)
(*        CCList.fold_left *)
(*          (fun acc code -> acc + eval_code depth start code) *)
(*          0 input) *)
(* |> CCList.hd *)

let () =
  let input = get_input () in
  (* part1 input |> print_int_nl; *)
  part2 input |> print_int_nl
