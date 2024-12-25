open Aoc24

type entityTyp = Key | Lock
type entity = { typ : entityTyp; heights : int array }

let read_entity mat =
  let heights = Array.make (Matrix.width mat) ~-1 in
  Matrix.fold_pos
    (fun () { x; _ } c -> if c = '#' then heights.(x) <- heights.(x) + 1)
    () mat;
  if Matrix.get_2d_opt mat 0 0 = Some '#' then { typ = Lock; heights }
  else { typ = Key; heights }

let read_all_entities input =
  CCList.group_succ ~eq:(fun l1 l2 -> l1 <> "" && l2 <> "") input
  |> CCList.filter (( <> ) [ "" ]) |> CCList.map Matrix.from_input |> fun as_mats  ->
  (CCList.hd as_mats |> Matrix.height |> fun h -> h - 2,  CCList.map read_entity as_mats)

let part1 height input=
  let keys, locks = CCList.partition_filter_map (fun {typ; heights} -> match typ with |Key -> `Left heights |Lock -> `Right heights) input in
  CCList.product (fun key lock -> Array.for_all2 (fun k l -> k+l <= height) key lock) keys locks |> CCList.count CCFun.id

let part2 = print_endline "Hello part2!"

let () =
  let height, input = get_input() |> read_all_entities in
  part1 height input |> print_int_nl;
  part2
