open Aoc24

let xmas = [ 'X'; 'M'; 'A'; 'S' ]

type direction = { dx : int; dy : int }
type position = { x : int; y : int }

let pos_from_pair (x, y) = { x; y }

let all_directions () =
  CCSeq.(product (~-1 -- 1) (~-1 -- 1))
  |> CCSeq.filter_map (fun (x, y) ->
         if x == 0 && y == 0 then None else Some { dx = x; dy = y })

let diagonals = [ { dx = 1; dy = 1 }; { dx = -1; dy = 1 } ]
let reverse { dx; dy } = { dx = 0 - dx; dy = 0 - dy }
let go { x; y } { dx; dy } = { x = x + dx; y = y + dy }

let get_2d array pos =
  Array.get_opt array pos.y |> fun o ->
  Option.bind o (fun s -> String.get_opt s pos.x)

let rec check_xmas field position direction = function
  | [] -> true
  | hd :: tl ->
      if Some hd <> get_2d field position then false
      else check_xmas field (go position direction) direction tl

let check_mas field position direction =
  match
    ( get_2d field @@ go position direction,
      get_2d field @@ go position @@ reverse direction )
  with
  | Some 'M', Some 'S' | Some 'S', Some 'M' -> true
  | _ -> false

let check_x_mas field position =
  CCList.for_all (check_mas field position) diagonals

let positions input =
  let height = Array.length input in
  let width = String.length input.(0) in
  CCSeq.(product
    (0 --^ width)
    (0 --^ height)
  |> map pos_from_pair)

let part1 input =
  positions input
  |> CCSeq.product (all_directions ())
  |> CCSeq.map (fun (dir, pos) -> check_xmas input pos dir xmas)
  |> CCSeq.filter CCFun.id |> CCSeq.length

let part2 input =
  positions input
  |> CCSeq.filter (fun pos -> get_2d input pos = Some 'A')
  |> CCSeq.map (check_x_mas input)
  |> CCSeq.filter CCFun.id |> CCSeq.length

let () =
  let input = get_input () |> Array.of_list in
  part1 input |> print_anything;
  part2 input |> print_anything
