open Aoc24

let parse_words = extract_all (take_while1 is_alpha)

let parse_input = function
  | words :: _ :: tests -> (parse_all parse_words words, tests)
  | _ -> failwith "Unexpected input"

module Cache = CCHashtbl.Make (struct
  type t = string

  let equal = ( = )
  let hash = CCHash.string
end)

let memo_rec f =
  let cache = Cache.create 16 in
  let rec g x =
    match Cache.get cache x with
    | Some r -> r
    | None ->
        let y = f g x in
        Cache.add cache x y;
        y
  in
  g

let grep_memo words =
  let grep self input =
    if input = "" then true
    else
      CCList.find_opt
        (fun word ->
          match CCString.chop_prefix ~pre:word input with
          | None -> false
          | Some chopped -> self chopped)
        words
      |> CCOption.is_some
  in
  memo_rec grep

let find_all_memo words =
  let find_all self input =
    if input = "" then 1
    else
      CCList.filter_map
        (fun word ->
          match CCString.chop_prefix ~pre:word input with
          | None -> None
          | Some chopped -> Some (self chopped))
        words
      |> sum
  in
  memo_rec find_all

let part1 words tests = CCList.count (grep_memo words) tests
let part2 words tests = CCList.map (find_all_memo words) tests |> sum

let () =
  let words, tests = get_input () |> parse_input in
  part1 words tests |> print_int_nl;
  part2 words tests |> print_int_nl
