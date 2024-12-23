open Aoc24

type pattern = int * int * int * int

module PatternMap = CCHashtbl.Make (struct
  type t = pattern

  let hash = CCHash.quad CCHash.int CCHash.int CCHash.int CCHash.int
  let equal (a, b, c, d) (w, x, y, z) = a == w && b == x && c == y && d == z
end)

type bid = { price : int; diff : int }

let two24 = Int.shift_left 1 24
let mix = Int.logxor
let prune = Int.logand (two24 - 1)
let lshift = CCFun.flip Int.shift_left
let rshift = CCFun.flip Int.shift_right_logical
let step1 secret = secret |> lshift 6 |> mix secret |> prune
let step2 secret = secret |> rshift 5 |> mix secret |> prune
let step3 secret = secret |> lshift 11 |> mix secret |> prune
let step secret = secret |> step1 |> step2 |> step3
let stepn ~n start = CCFun.iterate n step start
let last_digit secret = secret % 10

let collect_differences ~n start =
  CCFun.iterate n
    (fun (secret, differences) ->
      let next_secret = step secret in
      ( next_secret,
        {
          price = last_digit next_secret;
          diff = last_digit next_secret - last_digit secret;
        }
        :: differences ))
    (start, [])
  |> snd |> CCList.rev

let collect_patterns differences =
  CCList.sublists_of_len ~offset:1 4 differences
  |> CCList.rev |> CCList.to_iter
  |> Iter.map (function
       | [ a; b; c; d ] -> ((a.diff, b.diff, c.diff, d.diff), d.price)
       | _ -> failwith "List has not length 4")
  |> PatternMap.of_iter

let sum_results pattern_maps pattern =
  CCList.map
    (fun pmap -> PatternMap.get_or pmap pattern ~default:0)
    pattern_maps
  |> sum

let part1 input = input |> CCList.map (stepn ~n:2000) |> sum

let part2 input =
  let patterns =
    CCList.map (collect_differences ~n:2000) input
    |> CCList.map collect_patterns
  in
  CCList.to_iter patterns |> Iter.map PatternMap.keys |> Iter.flatten
  |> Iter.sort_uniq
  |> Iter.map (fun pattern -> sum_results patterns pattern)
  |> Iter.max
  |> CCOption.get_exn_or "we have no patterns"

let () =
  let input = get_input () |> CCList.map int_of_string in
  part1 input |> print_int_nl;
  part2 input |> print_int_nl
