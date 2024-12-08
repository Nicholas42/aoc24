open Aoc24

type instruction = Mul of int | Do | Dont

let mul_parser =
  string "mul(" *> integer
  &> string "," *> integer
  <* string ")" >>| CCFun.uncurry ( * )

let parse_line parser line = parse_all (extract_all parser) line

let instruction_parser =
  mul_parser
  >>| (fun result -> Mul result)
  <|> string "do()" *> return Do
  <|> string "don't()" *> return Dont

let enabled_sum l =
  CCList.fold_left
    (fun (acc, enabled) inst ->
      match inst with
      | Do -> (acc, true)
      | Dont -> (acc, false)
      | Mul value -> ((acc + if enabled then value else 0), enabled))
    (0, true) l
  |> fst

let part1 (input : string list) =
  CCList.map (parse_line mul_parser) input |> CCList.flatten |> sum

let part2 input =
  CCList.map (parse_line instruction_parser) input
  |> CCList.flatten |> enabled_sum

let () =
  let input = get_input () in
  part1 input |> print_int_nl;
  part2 input |> print_int_nl
