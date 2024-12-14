open Aoc24

type robot = { p : position; v : position }

let width = 101
let height = 103

let read_robots line =
  match parse_all (extract_all integer) line with
  | [ px; py; vx; vy ] -> { p = { x = px; y = py }; v = { x = vx; y = vy } }
  | _ -> failwith "Not 4 numbers on a line"

let step_robot ?(times = 1) ({ p; v } : robot) =
  {
    p =
      { x = (p.x + (times * v.x)) % width; y = (p.y + (times * v.y)) % height };
    v;
  }

let get_quadrant { p; _ } =
  match (BatInt.ord p.x (width / 2), BatInt.ord p.y (height / 2)) with
  | Eq, _ | _, Eq -> None
  | Lt, Lt -> Some 0
  | Lt, Gt -> Some 1
  | Gt, Lt -> Some 2
  | Gt, Gt -> Some 3

let make_robot_matrix robots =
  let m = Matrix.make width height false in
  CCList.iter (fun { p; _ } -> Matrix.set_exn m p true) robots;
  m

let rec set_all_connected (robots : bool Matrix.t) (islands : int Matrix.t)
    island start =
  if Matrix.get_opt robots start = Some true && Matrix.get_exn islands start = 0
  then (
    Matrix.set_exn islands start island;
    IntPosition.orthogonal_neighbors start
    |> CCList.iter (set_all_connected robots islands island))
  else ()

let mark_islands (robots : bool Matrix.t) (islands : int Matrix.t)
    last_island_number pos is_robot =
  match is_robot with
  | false -> last_island_number
  | true when Matrix.get_exn islands pos <> 0 -> last_island_number
  | _ ->
      set_all_connected robots islands (last_island_number + 1) pos;
      last_island_number + 1

let find_biggest_marked islands =
  let max_num = Matrix.fold_pos (fun sofar _ v -> max sofar v) 0 islands in
  let counts = CCArray.make (max_num + 1) 0 in
  Matrix.fold_pos
    (fun () _ v -> if v > 0 then counts.(v) <- counts.(v) + 1)
    () islands;
  CCArray.max_exn compare counts

let find_biggest_island robots =
  let robots = make_robot_matrix robots in
  let islands = Matrix.make width height 0 in
  Matrix.fold_pos
    (fun acc pos v -> mark_islands robots islands acc pos v)
    0 robots
  |> ignore;
  find_biggest_marked islands

let print_robots robots =
  make_robot_matrix robots
  |> Matrix.print (fun r -> if r then print_char '#' else print_char ' ')

let part1 input =
  CCList.fold_left (fun acc line -> read_robots line :: acc) [] input
  |> CCList.map (step_robot ~times:100)
  |> CCList.filter_map get_quadrant
  |> CCList.group_by |> CCList.map CCList.length |> CCList.reduce_exn ( * )

let part2 input =
  let robots =
    CCList.fold_left (fun acc line -> read_robots line :: acc) [] input
  in
  let result =
    CCSeq.ints 0 |> CCSeq.take 10000
    |> CCSeq.drop_while (fun steps ->
           5
           * (CCList.map (step_robot ~times:steps) robots |> find_biggest_island)
           <= 2 * CCList.length robots)
    |> CCSeq.head_exn
  in
  CCList.map (step_robot ~times:result) robots |> print_robots;
  result

let () =
  let input = get_input () in
  part1 input |> print_int_nl;
  part2 input |> print_int_nl
