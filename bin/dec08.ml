open Aoc24
open CCSeq.Infix
open IntPosition

type antenna = { frequency : char; pos : position }

let find_antennae input =
  fold_input
    (fun sofar pos c ->
      if c <> '.' then { frequency = c; pos } :: sofar else sofar)
    [] input

let nodes_for a b : position list =
  let inc = diff a.pos b.pos in
  [ add a.pos inc; diff b.pos inc ]

let calc_in_line_x a b width y : position list =
  if a.pos.y = b.pos.y && a.pos.y = y then
    0 --^ width |> CCSeq.map (fun x : position -> { x; y }) |> CCList.of_seq
  else
    let inc = to_float (diff a.pos b.pos) in
    let factor = float_of_int (y - a.pos.y) /. inc.y in
    let expected_x = (inc.x *. factor) +. float_of_int a.pos.x in
    if Float.is_integer expected_x then [ { x = int_of_float expected_x; y } ]
    else []

let nodes_in_line a b width height : position list =
  0 --^ height |> CCList.of_seq |> CCList.flat_map (calc_in_line_x a b width)

let collect_nodes antennae =
  CCList.diagonal antennae
  |> CCList.fold_left (fun acc (a, b) -> nodes_for a b @ acc) []

let collect_nodes_in_line width height antennae =
  CCList.diagonal antennae
  |> CCList.fold_left
       (fun acc (a, b) -> nodes_in_line a b width height @ acc)
       []

let filter_valid width height input =
  CCList.sort_uniq ~cmp:compare input
  |> CCList.count (fun ({ x; y } : position) ->
         x >= 0 && x < width && y >= 0 && y < height)

let part1 grouped width height =
  CCList.flat_map collect_nodes grouped |> filter_valid width height

let part2 grouped width height =
  CCList.flat_map (collect_nodes_in_line width height) grouped
  |> filter_valid width height

let () =
  let input = get_input () in
  let antennae = find_antennae input in
  let width = CCList.hd input |> CCString.length in
  let height = CCList.length input in
  let grouped =
    CCList.group_by
      ~hash:(fun a -> Hashtbl.hash a.frequency)
      ~eq:(fun a b -> a.frequency == b.frequency)
      antennae
  in
  part1 grouped width height |> print_int;
  print_newline ();
  part2 grouped width height |> print_int;
  print_newline ()
