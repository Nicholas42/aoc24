open Aoc24

type operation = AND | OR | XOR
type computation = { inputs : string * string; op : operation }

module StringMap = CCHashtbl.Make (String)
module StringSet = CCHashSet.Make (String)

let op_of_string = function
  | "XOR" -> XOR
  | "AND" -> AND
  | "OR" -> OR
  | any -> failwith (CCFormat.sprintf "Not an operation: %s" any)

let parse_comp line =
  CCString.split_on_char ' ' line |> function
  | [ lhs; op; rhs; _arrow; target ] ->
      (target, { inputs = (lhs, rhs); op = op_of_string op })
  | _ -> failwith "Invalid line length"

let parse_start line =
  parse_all
    (Angstrom.take_till (( = ) ':')
    <* Angstrom.skip_while (fun c -> c <> '0' && c <> '1')
    &> integer)
    line

let parse_input input =
  let start, nodes = CCList.take_drop_while (( <> ) "") input in
  let values = CCList.map parse_start start |> StringMap.of_list in
  let computations =
    CCList.tl nodes |> CCList.map parse_comp |> StringMap.of_list
  in
  (values, computations)

let do_compute = function
  | AND -> Int.logand
  | OR -> Int.logor
  | XOR -> Int.logxor

let rec compute values computations key =
  StringMap.get_or_add values
    ~f:(fun k ->
      let comp =
        StringMap.get computations k
        |> CCOption.get_exn_or (CCFormat.sprintf "No computation for %s" k)
      in
      let lhs, rhs =
        CCPair.map_same (compute values computations) comp.inputs
      in
      do_compute comp.op lhs rhs)
    ~k:key

let compute_final_state values computations =
  let values_copy = StringMap.copy values in
  StringMap.keys_list computations
  |> CCList.filter (String.starts_with ~prefix:"z")
  |> CCList.iter (fun key -> compute values_copy computations key |> ignore);
  values_copy

let rec collect_influences collection computations key =
  match StringMap.get computations key with
  | None -> collection
  | Some { inputs = lhs, rhs; _ } ->
      if StringSet.mem collection key then collection
      else (
        StringSet.insert collection key;
        collect_influences collection computations lhs |> ignore;
        collect_influences collection computations rhs)

let of_big_endian = CCList.fold_left (fun acc bit -> (acc * 2) + bit) 0
let diffs lhs rhs = CCList.foldi2 (fun acc i l r -> if l = r then acc else i::acc) [] lhs rhs |> CCList.map ((-) (CCList.length lhs -1 ))

let to_big_endian start =
  CCList.unfold (fun i -> if i = 0 then None else Some (i % 2, i / 2)) start
  |> CCList.rev

let extract_number ~prefix values =
  StringMap.to_list values
  |> CCList.filter (fun (k, _) -> String.starts_with ~prefix k)
  |> CCList.sort CCOrd.(pair (opp string) int)
  |> CCList.map snd

let part1 values computations =
  compute_final_state values computations
  |> extract_number ~prefix:"z" |> of_big_endian

let part2 values computations =
  let result = compute_final_state values computations in

  let x = extract_number ~prefix:"x" values |> of_big_endian in
  let y = extract_number ~prefix:"y" values |> of_big_endian in
  let expected = to_big_endian (x + y) in
  let actual = extract_number ~prefix:"z" result in
  (* let differences = diffs expected actual |> CCList.map (fu *)
  CCList.map char_of_digit expected |> CCString.of_list |> print_endline;
  CCList.map char_of_digit actual |> CCString.of_list |> print_endline;
  collect_influences |> ignore;
  diffs expected actual |> CCList.map (fun i -> CCFormat.sprintf "z<%02d>" i) |> CCString.concat ", " |> print_endline


let () =
  let values, computations = get_input () |> parse_input in
  part1 (StringMap.copy values) computations |> print_int_nl;
  part2 values computations
