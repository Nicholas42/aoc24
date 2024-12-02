open Aoc24

type monotony = Increasing | Decreasing | Indetermined

let line_parser = Angstrom.many (integer <* whitespace)

let parse_line line =
  match parse_string ~consume:Prefix line_parser line with
  | Ok l -> l
  | Error msg -> failwith msg

let rec check_list monotony list =
  match list with
  | [] -> true
  | head :: sec :: tail ->
      if head == sec then false
      else if monotony == Increasing && head > sec then false
      else if monotony == Decreasing && head < sec then false
      else if abs (sec - head) > 3 then false
      else
        check_list (if head > sec then Decreasing else Increasing) (sec :: tail)
  | _ -> true

let rec iterate_lenient heads tails =
  match tails with
  | twilek :: rest ->
      if check_list Indetermined (heads @ rest) then true
      else iterate_lenient (heads @ [ twilek ]) rest
  | [] -> false

let check_lenient list =
  if check_list Indetermined list then true else iterate_lenient [] list

let part1 input =
  input |> Enum.map parse_line
  |> Enum.map @@ check_list Indetermined
  |> BatEnum.filter identity |> BatEnum.hard_count

let part2 input =
  input |> Enum.map parse_line |> Enum.map check_lenient
  |> BatEnum.filter identity |> BatEnum.hard_count
(* Enum.map parse_line line |> BatList.of_enum |> print_list *)

let () =
  let in1, in2 = get_input_twice in
  part1 in1 |> print_int;
  print_endline "";
  part2 in2 |> print_int;
  print_endline ""
