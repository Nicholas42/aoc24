open Aoc24

let rec follow_trail field start : position list =
  let cur_height = Matrix.get_exn field start in
  if cur_height = 9 then [ start ]
  else
    IntPosition.orthogonal_neighbors start
    |> CCList.flat_map (fun n ->
           match Matrix.get_opt field n with
           | None -> []
           | Some x when x = cur_height + 1 -> follow_trail field n
           | _ -> [])

let collect_trails field start =
  match Matrix.get_opt field start with
  | Some 0 ->
      follow_trail field start |> CCList.sort_uniq ~cmp:compare |> CCList.length
  | _ -> 0

let collect_distinct_trails field start =
  match Matrix.get_opt field start with
  | Some 0 -> follow_trail field start |> CCList.length
  | _ -> 0

let part1 input =
  Matrix.fold_pos (fun acc pos _ -> acc + collect_trails input pos) 0 input

let part2 input =
  Matrix.fold_pos
    (fun acc pos _ -> acc + collect_distinct_trails input pos)
    0 input

let () =
  let input = get_input () |> Matrix.from_input |> Matrix.map digit_of_char in
  part1 input |> print_int_nl;
  part2 input |> print_int_nl
