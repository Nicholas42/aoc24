open Aoc24
open CCSeq.Infix

type position = Matrix.position
type antenna = { frequency : char; pos : position }

let find_antennae input =
  CCList.foldi
    (fun init y row ->
      CCString.foldi
        (fun sofar x c ->
          if c <> '.' then { frequency = c; pos = { x; y } } :: sofar else sofar)
        init row)
    [] input

let nodes_for a b : position list =
  let diff_x = a.pos.x - b.pos.x in
  let diff_y = a.pos.y - b.pos.y in
  [
    { x = a.pos.x + diff_x; y = a.pos.y + diff_y };
    { x = b.pos.x - diff_x; y = b.pos.y - diff_y };
  ]

let calc_in_line_x a b width y : position list =
  if a.pos.y = b.pos.y && a.pos.y = y then
    0 --^ width |> CCSeq.map (fun x : position -> { x; y }) |> CCList.of_seq
  else
    let diff_x = float_of_int @@ (a.pos.x - b.pos.x) in
    let diff_y = float_of_int @@ (a.pos.y - b.pos.y) in
    let factor = float_of_int (y - a.pos.y) /. diff_y in
    let expected_x = (diff_x *. factor) +. float_of_int a.pos.x in
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
  let input = get_input in
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
