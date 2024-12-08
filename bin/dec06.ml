open Aoc24

type direction = Up | Right | Down | Left
type guard = { pos : position; facing : direction }

module PosSet = CCSet.Make (struct
  type t = position

  let compare = compare
end)

module GuardSet = CCSet.Make (struct
  type t = guard

  let compare = compare
end)

let c_to_dir = function
  | '^' -> Up
  | '>' -> Right
  | 'v' -> Down
  | '<' -> Left
  | _ -> failwith "Not a direction"

let find_guard mat : guard =
  Matrix.find_pos (fun c -> c = '^' || c = '>' || c = 'v' || c = '<') mat
  |> CCOption.get_exn_or "No guard found"
  |> fun (pos, c) -> { pos; facing = c_to_dir c }

let go { pos = { x; y }; facing } =
  match facing with
  | Up -> { pos = { x; y = y - 1 }; facing }
  | Right -> { pos = { x = x + 1; y }; facing }
  | Down -> { pos = { x; y = y + 1 }; facing }
  | Left -> { pos = { x = x - 1; y }; facing }

let turn { pos; facing } =
  match facing with
  | Up -> { pos; facing = Right }
  | Right -> { pos; facing = Down }
  | Down -> { pos; facing = Left }
  | Left -> { pos; facing = Up }

let step field guard =
  let ({ pos; _ } as next) = go guard in
  let maybe_obstacle = Matrix.get_opt field pos in
  match maybe_obstacle with
  | None -> None
  | Some '#' -> Some (turn guard)
  | _ -> Some next

let rec run_steps field guard positions =
  let positions = PosSet.add guard.pos positions in
  let next_guard = step field guard in
  match next_guard with
  | None -> positions
  | Some g -> run_steps field g positions

let rec run_steps_loop obstacles guard guards =
  let guards = GuardSet.add guard guards in
  let next_guard = step obstacles guard in
  match next_guard with
  | None -> false
  | Some g ->
      if GuardSet.mem g guards then true else run_steps_loop obstacles g guards

let has_loop_with field guard extra =
  if Matrix.get_exn field extra <> '.' then false
  else (
    Matrix.set_exn field extra '#';
    let result = run_steps_loop field guard GuardSet.empty in
    Matrix.set_exn field extra '.';
    result)

let part1 field guard = run_steps field guard PosSet.empty |> PosSet.cardinal

let part2 field guard =
  Matrix.count_pos (fun pos _ -> has_loop_with field guard pos) field

let () =
  let input = get_input () in
  let field = Matrix.from_input input in
  let guard = find_guard field in
  part1 field guard |> print_int_nl;
  part2 field guard |> print_int_nl
