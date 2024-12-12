open Aoc24
module Counter = CCHashtbl.Make (Int)

let count_numbers numbers =
  let result = Counter.create (CCList.length numbers) in
  CCList.iter (Counter.incr result) numbers;
  result

let redistribute table ~amount ~source ~targets =
  CCList.iter (Counter.incr ~by:amount table) targets;
  Counter.decr ~by:amount table source

let blink_once number =
  if number = 0 then [ 1 ]
  else
    let { exponent; _ } = log_ceil ~base:10 number in
    if is_even exponent then
      let { quotient; remainder } = divmod number @@ pow 10 (exponent / 2) in
      [ quotient; remainder ]
    else [ number * 2024 ]

let rec blink_n_times (numbers : int Counter.t) = function
  | 0 -> numbers
  | n ->
      Counter.map_list (fun k v -> (k, v, blink_once k)) numbers
      |> CCList.iter (fun (k, v, targets) ->
             redistribute numbers ~amount:v ~source:k ~targets);
      blink_n_times numbers (n - 1)

let part1 numbers =
  blink_n_times (count_numbers numbers) 25 |> Counter.values_list |> sum

let part2 numbers =
  blink_n_times (count_numbers numbers) 75 |> Counter.values_list |> sum

let () =
  let input = get_input () |> CCList.hd |> parse_all (extract_all integer) in
  part1 input |> print_int_nl;
  part2 input |> print_int_nl
