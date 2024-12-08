open Aoc24

let line_parser = integer <* whitespace &> integer

let part1 input =
  List.map (parse_all line_parser) input
  |> List.split
  |> CCPair.map_same @@ List.sort compare
  |> CCFun.uncurry @@ CCList.map2 dist
  |> sum

let sim_score comp_list reference =
  reference * CCList.count (( = ) reference) comp_list

let part2 input =
  List.map (parse_all line_parser) input |> List.split |> fun (lhs, rhs) ->
  List.map (sim_score rhs) lhs |> sum

let () =
  let input = get_input () in
  part1 input |> print_int;
  print_newline ();
  part2 input |> print_int;
  print_newline ()
