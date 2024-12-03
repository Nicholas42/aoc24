open Aoc24

type instruction = Mul of int | Do | Dont

let mul_parser =
  string "mul(" *> integer
  &> string "," *> integer
  <* string ")" >>| uncurry ( * )

let parse_line parser line = parse_all (extract_all parser) line

let instruction_parser =
  mul_parser
  >>| (fun result -> Mul result)
  <|> string "do()" *> return Do
  <|> string "don't()" *> return Dont

let rec enabled_sum ?(acc = 0) ?(enabled = true) l =
  match l with
  | [] -> acc
  | Do :: rest -> enabled_sum ~acc ~enabled:true rest
  | Dont :: rest -> enabled_sum ~acc ~enabled:false rest
  | Mul value :: rest ->
      enabled_sum ~acc:(acc + if enabled then value else 0) ~enabled rest

let part1 (input : string list) =
  List.map (parse_line mul_parser) input |> List.flatten |> List.sum

let part2 input =
  List.map (parse_line instruction_parser) input |> List.flatten |> enabled_sum

let () =
  let input = get_input in
  part1 input |> print_anything;
  part2 input |> print_anything
