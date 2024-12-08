open Aoc24

type monotony = Increasing | Decreasing | Indetermined

let line_parser = extract_all integer

let rec check_list monotony list =
  match list with
  | [] -> true
  | head :: (twilek :: _ as tail) -> (
      match (BatInt.ord twilek head, monotony) with
      | Eq, _ -> false
      | Gt, Decreasing -> false
      | Lt, Increasing -> false
      | _ when dist twilek head > 3 -> false
      | Gt, _ -> check_list Increasing tail
      | Lt, _ -> check_list Decreasing tail)
  | _ -> true

let rec iterate_lenient heads = function
  | twilek :: rest ->
      if check_list Indetermined (heads @ rest) then true
      else iterate_lenient (back_cons heads twilek) rest
  | [] -> false

let check_lenient list =
  if check_list Indetermined list then true else iterate_lenient [] list

let part1 input =
  input
  |> CCList.map @@ parse_all line_parser
  |> CCList.count @@ check_list Indetermined

let part2 input =
  input |> CCList.map @@ parse_all line_parser |> CCList.count check_lenient

let () =
  let input = get_input () in
  part1 input |> print_int_nl;
  part2 input |> print_int_nl
