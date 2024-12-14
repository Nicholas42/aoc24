open Aoc24

type robot = { p : position; v : position }

module PositionSet = CCHashSet.Make (struct
  type t = position

  let equal = ( = )
  let hash = CCHash.poly
end)

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

let make_robot_graph positionSet =
  CCGraph.make (fun p ->
      IntPosition.orthogonal_neighbors p
      |> CCList.filter (fun n -> PositionSet.mem positionSet n)
      |> CCList.map (fun n -> ((p, n), n))
      |> CCList.to_iter)

let find_components robots =
  let tbl = CCGraph.mk_table ~eq:( = ) (CCList.length robots) in
  let positions = CCList.map (fun r -> r.p) robots in
  let graph = positions |> PositionSet.of_list |> make_robot_graph in
  CCGraph.scc ~tbl ~graph (CCList.to_iter positions)

let make_robot_matrix robots =
  let m = Matrix.make width height false in
  CCList.iter (fun { p; _ } -> Matrix.set_exn m p true) robots;
  m

let find_biggest_island robots =
  find_components robots
  |> CCGraph.Iter.map CCList.length
  |> CCGraph.Iter.fold max 0

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
    CCSeq.ints 0
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
