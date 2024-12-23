open Aoc24

module PositionSet = CCHashSet.Make (struct
  type t = position

  let equal = ( = )
  let hash = Hashtbl.hash
end)

let width = 71
let height = 71
let start = CCGraph.Iter.return { x = 0; y = 0 }
let target = { x = width - 1; y = height - 1 }

let read_input input =
  CCList.map (parse_all (extract_all integer)) input
  |> CCList.map (function
       | [ x; y ] -> { x; y }
       | _ -> failwith "invalid number of ints on a line")

let graph obstacles pos =
  IntPosition.orthogonal_neighbors pos
  |> CCList.filter (fun { x; y } -> x >= 0 && y >= 0 && x < width && y < height)
  |> CCList.filter (CCFun.negate @@ PositionSet.mem obstacles)
  |> CCList.map (fun n -> ((), n))
  |> CCList.to_iter

let mk_tbl () = CCGraph.mk_table ~eq:( = ) ~hash:Hashtbl.hash (height * width)

let rec bpartition_search ~lb ~ub f =
  if ub - lb <= 2 then if f (lb + 1) then ub else lb + 1
  else
    let mid = (ub + lb) / 2 in
    if f mid then bpartition_search ~lb:mid ~ub f
    else bpartition_search ~lb ~ub:mid f

let part1 input =
  let obstacles = CCList.take 1024 input |> PositionSet.of_list in
  CCGraph.Traverse.dijkstra ~tbl:(mk_tbl ()) ~dist:(CCFun.const 1)
    ~graph:(graph obstacles) start
  |> CCGraph.Iter.fold (fun acc (v, d, _) -> if v = target then d else acc) 0

let has_path_to_target obstacles =
  CCGraph.Traverse.dfs ~tbl:(mk_tbl ()) ~graph:(graph obstacles) start
  |> CCGraph.Iter.fold (fun acc v -> acc || v = target) false

let prefix_path_test obstacles i =
  CCList.take i obstacles |> PositionSet.of_list |> has_path_to_target

let part2 obstacles =
  bpartition_search ~lb:0 ~ub:(CCList.length obstacles)
    (prefix_path_test obstacles)
  |> Int.pred
  |> CCFun.flip CCList.get_at_idx obstacles
  |> CCOption.get_exn_or "No blocking byte found"

let () =
  let input = get_input () |> read_input in
  part1 input |> print_int_nl;
  part2 input |> fun { x; y } -> CCFormat.printf "%d,%d\n" x y
