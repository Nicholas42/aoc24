open Aoc24

let line_parser = both (integer <* whitespace) integer

let parse_line parser arg =
  match parse_string ~consume:All parser arg with
  | Ok l -> l
  | Error msg -> failwith msg

let part1 input =
  Enum.map (parse_line line_parser) input
  |> BatList.of_enum |> BatList.split
  |> map_pair @@ BatList.sort ( - )
  |> uncurry BatList.combine
  |> BatList.map (fun (a, b) -> a - b |> abs)
  |> List.sum

let sim_score comp_list reference =
  reference * BatList.count_matching (( = ) reference) comp_list

let part2 input =
  Enum.map (parse_line line_parser) input |> BatList.of_enum |> BatList.split
  |> fun (lhs, rhs) -> BatList.map (sim_score rhs) lhs |> BatList.sum

let () =
  let in1, in2 = get_input_twice in
  part1 in1 |> print_int;
  print_newline ();
  part2 in2 |> print_int;
  print_newline ()
