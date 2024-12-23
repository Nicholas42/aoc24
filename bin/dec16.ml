open Aoc24

type vertex = { position : position; direction : direction }
type edge = { src : vertex; tgt : vertex }
type field = Wall | Empty | Start | End

let directions = [ Left; Right; Up; Down ]

let nodes_from_pos position =
  CCList.map (fun dir -> { position; direction = dir }) directions

let get_neighbors matrix { position; direction } =
  { position = IntPosition.move position direction; direction }
  :: nodes_from_pos position
  |> CCList.filter (fun v ->
         match Matrix.get_opt matrix v.position with
         | None | Some Wall -> false
         | _ -> true)

let graph_from_matrix matrix : (vertex, edge) CCGraph.t =
  CCGraph.make (fun v ->
      get_neighbors matrix v
      |> CCList.map (fun n -> ({ src = v; tgt = n }, n))
      |> CCList.to_iter)

let distance
    { src = { direction = src_dir; _ }; tgt = { direction = tgt_dir; _ } } =
  match (src_dir, tgt_dir) with
  | x, y when x = y -> 1
  | x,y when y = rev_dir x  -> 2000
  | _ -> 1000

let find_start matrix =
  Matrix.find_pos (( = ) Start) matrix |> CCOption.get_exn_or "No Start"
  |> fun (p, _) -> { position = p; direction = Right }

let get_all_ends matrix =
  Matrix.find_pos (( = ) End) matrix |> CCOption.get_exn_or "No End"
  |> fun (p, _) -> nodes_from_pos p

let read_matrix input =
  Matrix.from_input input
  |> Matrix.map (function
       | '#' -> Wall
       | '.' -> Empty
       | 'E' -> End
       | 'S' -> Start
       | _ -> failwith "invalid field")

let compute_shortest_paths matrix sources =
  let g = graph_from_matrix matrix in
  CCGraph.Traverse.dijkstra
    ~tbl:(CCGraph.mk_table ~eq:( = ) ~hash:Hashtbl.hash 100)
    ~dist:distance ~graph:g sources

let part1 matrix =
  compute_shortest_paths matrix @@ CCGraph.Iter.return @@ find_start matrix
  |> CCGraph.Iter.filter_map (fun (vertex, d, _) ->
         if Matrix.get_opt matrix vertex.position = Some End then Some d
         else None)
  |> CCGraph.Iter.to_list |> CCList.head_opt
  |> CCOption.get_exn_or "No end in sight"

module VertexTable = CCHashtbl.Make (struct
  type t = vertex

  let equal = ( = )
  let hash = Hashtbl.hash
end)

let match_nodes l m =
  let tbl = VertexTable.create 100 in
  CCGraph.Iter.iter (fun (v, d, _) -> VertexTable.add tbl v d) l;
  CCGraph.Iter.iter (fun (v, d, _) -> VertexTable.incr ~by:d tbl {position=v.position;direction=rev_dir v.direction}) m;
  tbl

let find_all_minima tbl =
  VertexTable.fold
    (fun v d (min_dist, min_vertices) ->
      if d < min_dist then (d, [ v ])
      else if d > min_dist then (min_dist, min_vertices)
      else (min_dist, v :: min_vertices))
    tbl (Int.max_int, [])
  |> snd

let part2 matrix =
  let start_distances =
    compute_shortest_paths matrix @@ CCGraph.Iter.return @@ find_start matrix
  in
  let end_distances =
    compute_shortest_paths matrix @@ CCList.to_iter @@ get_all_ends matrix
  in
  match_nodes start_distances end_distances
  |> find_all_minima
  |> CCList.sort_uniq ~cmp:(fun l r -> compare l.position r.position)
  |> CCList.length

let () =
  let matrix = get_input () |> read_matrix in
  part1 matrix |> print_int_nl;
  part2 matrix |> print_int_nl
