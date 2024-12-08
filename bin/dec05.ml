open Aoc24
open CCFun.Infix

let parse_order_rule = integer <* string "|" &> integer
let parse_update = extract_all integer

let is_before rules before after =
  CCList.sorted_mem ~cmp:compare (before, after) rules

let rec is_correct_single rules page = function
  | [] -> true
  | hd :: tl ->
      if is_before rules hd page then false else is_correct_single rules page tl

let rec is_correct rules = function
  | [] -> true
  | hd :: tl -> is_correct_single rules hd tl && is_correct rules tl

let parse_input lines =
  let rules, updates = CCList.take_drop_while (fun x -> x <> "") lines in
  ( CCList.map (parse_all parse_order_rule) rules
    |> CCList.sort_uniq ~cmp:compare,
    CCList.map (parse_all parse_update) (CCList.drop 1 updates) )

let get_middle update =
  let len = CCList.length update in
  CCList.get_at_idx_exn (len / 2) update

let order_func rules lhs rhs =
  if lhs = rhs then 0
  else if is_before rules rhs lhs then -1
  else if is_before rules lhs rhs then 1
  else failwith (CCFormat.sprintf "Incomparable: %d and %d" lhs rhs)

let part1 input =
  let rules, updates = parse_input input in
  CCList.filter (is_correct rules) updates |> CCList.map get_middle |> sum

let part2 input =
  let rules, updates = parse_input input in
  CCList.filter (CCFun.negate @@ is_correct rules) updates
  |> CCList.map (get_middle % CCList.sort (order_func rules))
  |> sum

let () =
  let input = get_input () in
  part1 input |> print_int |> print_newline;
  part2 input |> print_int |> print_newline
