open Aoc24
module StringSet = CCHashSet.Make (String)
module StringTbl = CCHashtbl.Make (String)

let add_edge tbl s t =
  StringTbl.update tbl
    ~f:(fun _ -> function
      | Some set ->
          StringSet.insert set t;
          Some set
      | None -> Some (StringSet.singleton t))
    ~k:s

let split_line line =
  String.split_on_char '-' line |> function
  | [ s; t ] -> (s, t)
  | _ -> failwith "Line doesn't contain exactly two nodes"

let read_tbl input =
  let result = StringTbl.create (CCList.length input) in
  CCList.iter
    (fun line ->
      split_line line |> fun (s, t) ->
      add_edge result s t;
      add_edge result t s)
    input;
  result

let is_connected tbl s t =
  StringTbl.get tbl s
  |> CCOption.map (fun set -> StringSet.mem set t)
  |> CCOption.get_or ~default:false

let find_triangles tbl =
  let is_valid_triangle a b c =
    a < b && b < c
    && (a.[0] = 't' || b.[0] = 't' || c.[0] = 't')
    && is_connected tbl b c
  in
  StringTbl.fold (fun k neighbors acc ->
      Iter.product (StringSet.to_iter neighbors) (StringSet.to_iter neighbors)
      |> Iter.filter_count (CCFun.uncurry (is_valid_triangle k))
      |> ( + ) acc) tbl 0

let largest_clique tbl =
  let rec largest_clique_aux tbl inside outside = function
    | [] -> inside
    | head :: tail ->
        let can_join = CCList.for_all (is_connected tbl head) inside in
        let with_head =
          if can_join then largest_clique_aux tbl (head :: inside) outside tail
          else []
        in
        let without_head =
          largest_clique_aux tbl inside (head :: outside) tail
        in
        if CCList.length with_head > CCList.length without_head then with_head
        else without_head
  in
  largest_clique_aux tbl [] [] (StringTbl.keys_list tbl)

let part1 tbl = find_triangles tbl
let part2 tbl = largest_clique tbl |> CCList.sort compare |> CCString.concat ","

let () =
  let tbl = get_input () |> read_tbl in
  part1 tbl |> print_int_nl;
  part2 tbl |> print_endline
