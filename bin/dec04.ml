open Aoc24

let xmas = [ 'X'; 'M'; 'A'; 'S' ]
let zero = { x = 0; y = 0 }

let all_directions () =
  CCSeq.(product (~-1 -- 1) (~-1 -- 1))
  |> CCSeq.filter_map (fun (x, y) ->
         if x == 0 && y == 0 then None else Some { x; y })

let diagonals = [ { x = 1; y = 1 }; { x = -1; y = 1 } ]
let reverse pos = IntPosition.diff zero pos
let go = IntPosition.add

let rec check_xmas field position direction = function
  | [] -> true
  | hd :: tl ->
      if Some hd <> Matrix.get_opt field position then false
      else check_xmas field (go position direction) direction tl

let check_mas field position direction =
  match
    ( Matrix.get_opt field @@ go position direction,
      Matrix.get_opt field @@ go position @@ reverse direction )
  with
  | Some 'M', Some 'S' | Some 'S', Some 'M' -> true
  | _ -> false

let check_x_mas field position =
  CCList.for_all (check_mas field position) diagonals

let positions input =
  CCSeq.(
    product (0 --^ Matrix.width input) (0 --^ Matrix.height input)
    |> map IntPosition.from_pair)

let part1 (input : char Matrix.t) =
  positions input
  |> CCSeq.map_product
       (fun dir pos -> check_xmas input pos dir xmas)
       (all_directions ())
  |> CCSeq.filter CCFun.id |> CCSeq.length

let part2 (input : char Matrix.t) =
  positions input
  |> CCSeq.filter (fun pos -> Matrix.get_opt input pos = Some 'A')
  |> CCSeq.map (check_x_mas input)
  |> CCSeq.filter CCFun.id |> CCSeq.length

let () =
  let input = get_input () |> Matrix.from_input in
  part1 input |> print_int_nl;
  part2 input |> print_int_nl
