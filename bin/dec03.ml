open Aoc24

type instruction = Mul of int | Do | Dont

let mul_parser =
  string "mul(" *> (both integer @@ (string "," *> integer)) <* string ")"
  >>| fun (l, r) -> l * r

let parse_line parser line =
  match parse_string ~consume:All (extract_all parser) line with
  | Ok l -> BatList.filter_map identity l
  | Error msg -> failwith msg

let instruction_parser =
  mul_parser
  >>| (fun result -> Mul result)
  <|> (string "do()" >>| fun _ -> Do)
  <|> (string "don't()" >>| fun _ -> Dont)

let rec enabled_sum acc enabled l =
  match l with
  | [] -> acc
  | Do :: rest -> enabled_sum acc true rest
  | Dont :: rest -> enabled_sum acc false rest
  | Mul value :: rest ->
      let real_val = if enabled then value else 0 in
      enabled_sum (acc + real_val) enabled rest

let part1 input =
  BatEnum.map (parse_line mul_parser) input
  |> BatList.of_enum |> BatList.flatten |> BatList.sum |> print_int

let part2 input =
  BatEnum.map (parse_line instruction_parser) input
  |> BatList.of_enum |> BatList.flatten |> enabled_sum 0 true |> print_int

let () =
  let in1, in2 = get_input_twice in
  part1 in1;
  print_newline ();
  part2 in2;
  print_newline ();
