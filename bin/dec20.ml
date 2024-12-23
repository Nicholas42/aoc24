open Aoc24

let unit_circle ~radius =
  CCList.product
    (fun x y -> { x; y })
    (CCList.range ~-radius radius)
    (CCList.range ~-radius radius)
  |> CCList.filter (fun p -> IntPosition.manhattan p <= radius)

let find_pos matrix searched =
  Matrix.find_pos (( = ) searched) matrix
  |> CCOption.get_exn_or (CCFormat.sprintf "No %c found" searched)
  |> fst

let pos_tbl size = CCGraph.mk_table ~eq:( = ) ~hash:Hashtbl.hash size

let get_dist_from matrix pos =
  let num_nodes = Matrix.width matrix * Matrix.height matrix in
  let graph =
    CCGraph.make (fun pos ->
        IntPosition.orthogonal_neighbors pos
        |> CCList.filter (fun p ->
               match Matrix.get_opt matrix p with
               | None | Some '#' -> false
               | Some '.' | Some 'S' | Some 'E' -> true
               | Some x -> failwith (CCFormat.sprintf "Invalid field %c" x))
        |> CCList.map (fun pos -> ((), pos))
        |> CCList.to_iter)
  in
  CCGraph.Traverse.dijkstra ~tbl:(pos_tbl num_nodes) ~dist:(CCFun.const 1)
    ~graph (CCGraph.Iter.return pos)

let () = Printexc.record_backtrace true

let calc_distances matrix =
  let dist_matrix =
    Matrix.make (Matrix.width matrix) (Matrix.height matrix) None
  in
  get_dist_from matrix (find_pos matrix 'S')
  |> CCGraph.Iter.iter (fun (p, d, _) ->
         Matrix.update dist_matrix (fun _ -> Some (d, 0)) p);
  get_dist_from matrix (find_pos matrix 'E')
  |> CCGraph.Iter.iter (fun (p, d, _) ->
         Matrix.update dist_matrix
           (CCOption.map (CCPair.map_snd (CCFun.const d)))
           p);
  dist_matrix

let count_shortcuts ?(length = 2) max_dist
    (distances : (int * int) option Matrix.t) =
  let circle = unit_circle ~radius:length in
  Matrix.fold_pos
    (fun acc pos origin_distances ->
      acc
      +
      match origin_distances with
      | Some (from_s, _) ->
          CCList.map (IntPosition.add pos) circle
          |> CCList.filter_map (fun other_pos ->
                 Matrix.get_opt distances other_pos
                 |> CCOption.flatten
                 |> CCOption.map (fun (_, to_end) -> from_s + to_end + (IntPosition.manhattan @@ IntPosition.diff pos other_pos)))
          |> CCList.count (fun full_dist -> full_dist <= max_dist)
      | None -> 0)
    0 distances

let count_short_shortcuts length matrix =
  let distances = calc_distances matrix in
  let start = find_pos matrix 'S' in
  let min_dist =
    Matrix.get_exn distances start
    |> CCOption.get_exn_or "Start has invalid distance"
    |> CCFun.uncurry ( + )
  in
  count_shortcuts ~length (min_dist - 100) distances

let part1 = count_short_shortcuts 2
let part2 = count_short_shortcuts 20

let () =
  let matrix = get_input () |> Matrix.from_input in
  part1 matrix |> print_int_nl;
  part2 matrix |> print_int_nl
