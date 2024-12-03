open Aoc24

let line_parser = integer <* whitespace &> integer

let part1 input =
  List.map (parse_all line_parser) input
  |> List.split
  |> BatTuple.Tuple2.mapn @@ List.sort compare
  |> uncurry @@ List.map2 dist
  |> List.sum

let sim_score comp_list reference =
  reference * BatList.count_matching (( = ) reference) comp_list

let part2 input =
  List.map (parse_all line_parser) input |> List.split |> fun (lhs, rhs) ->
  List.map (sim_score rhs) lhs |> List.sum

let () =
  let input = get_input in
  part1 input |> print_anything;
  part2 input |> print_anything
