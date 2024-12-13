open Aoc24
open Q

type claw = { prize : qPosition; a : qPosition; b : qPosition }

let is_int x = Z.equal (Q.den x) Z.one

let read_claws (input : string list) =
  CCList.fold_right
    (fun line acc -> parse_all (extract_all integer) line :: acc)
    input []
  |> CCList.filter_map (function
       | [ x; y ] -> Some { x = Q.of_int x; y = Q.of_int y }
       | _ -> None)
  |> CCList.sublists_of_len 3
  |> CCList.map (function
       | [ a; b; prize ] -> { a; b; prize }
       | _ -> failwith "Invalid sublist length")

let big_num = Q.of_int 10000000000000
let add_big { x; y } = { x = x + big_num; y = y + big_num }

let get_factors { a; b; prize } =
  let det = (a.x * b.y) - (a.y * b.x) in
  if det = Q.zero then failwith "not invertible"
  else
    let rhs = { x = prize.x / det; y = prize.y / det } in
    ((rhs.x * b.y) - (b.x * rhs.y), (rhs.y * a.x) - (rhs.x * a.y))

let evaluate (m, n) = if is_int m && is_int n then m + m + m + n else zero

let part1 claws =
  CCList.map get_factors claws |> CCList.map evaluate |> CCList.reduce_exn ( + )

let part2 claws =
  CCList.map (fun c -> { c with prize = add_big c.prize }) claws |> part1

let () =
  let claws = get_input () |> read_claws in
  part1 claws |> print;
  print_newline ();
  part2 claws |> print;
  print_newline ()
