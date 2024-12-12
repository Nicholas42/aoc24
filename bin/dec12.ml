open Aoc24

type fence = { around_plot : position; delta : position }
type region_info = { area : int; fences : fence list }

let region_cost { area; fences } = area * CCList.length fences

let fence_between ~inside ~outside =
  { around_plot = inside; delta = IntPosition.diff inside outside }

let rec collect_fences field visited start cur_plot : region_info =
  if Matrix.get_opt visited start = Some true then { area = 0; fences = [] }
  else (
    Matrix.set_exn visited start true;
    IntPosition.orthogonal_neighbors start
    |> CCList.fold_left
         (fun { area; fences } neighbor ->
           let { area = additional_area; fences = additional_fences } =
             if Matrix.get_opt field neighbor = Some cur_plot then
               collect_fences field visited neighbor cur_plot
             else
               {
                 area = 0;
                 fences = [ fence_between ~inside:start ~outside:neighbor ];
               }
           in
           {
             area = area + additional_area;
             fences = CCList.append additional_fences fences;
           })
         { area = 1; fences = [] })

let count_connected dir row_or_col =
  CCList.map dir row_or_col
  |> CCList.sort_uniq ~cmp:compare
  |> CCList.fold_left
       (fun (num_cc, last) next ->
         match last with
         | Some x when x <> next - 1 -> (num_cc + 1, Some next)
         | _ -> (num_cc, Some next))
       (1, None)
  |> fst

let find_sides_of_direction delta positions =
  let changing_dir, constant_dir =
    if delta.y = 0 then (get_x, get_y) else (get_y, get_x)
  in
  group_on changing_dir positions
  |> CCList.map (count_connected constant_dir)
  |> sum

let find_sides fences =
  group_on (fun f -> f.delta) fences
  |> CCList.map (fun group ->
         let delta = (CCList.hd group).delta in
         CCList.map (fun f -> f.around_plot) group
         |> find_sides_of_direction delta)
  |> sum

let reduced_cost { area; fences } = area * find_sides fences

let part1 field =
  let visited = Matrix.make (Matrix.width field) (Matrix.height field) false in
  Matrix.fold_pos
    (fun acc pos cur_plot ->
      acc + (region_cost @@ collect_fences field visited pos cur_plot))
    0 field

let part2 field =
  let visited = Matrix.make (Matrix.width field) (Matrix.height field) false in
  Matrix.fold_pos
    (fun acc pos cur_plot ->
      acc + (reduced_cost @@ collect_fences field visited pos cur_plot))
    0 field

let () =
  let field = get_input () |> Matrix.from_input in
  part1 field |> print_int_nl;
  part2 field |> print_int_nl
