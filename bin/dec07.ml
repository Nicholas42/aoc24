open Aoc24

type equation = { result : Z.t; args : Z.t list }

let read_equation =
  integerZ <* string ": " &> extract_all integerZ >>| fun (res, args) ->
  { result = res; args }

let combine lhs rhs = Z.of_string (Z.to_string lhs ^ Z.to_string rhs)

let read_all input =
  CCList.map
    (fun line ->
      match parse_string ~consume:All read_equation line with
      | Ok result -> result
      | Error msg -> failwith msg)
    input

let rec try_eval interim_result { result : Z.t; args : Z.t list } ops =
  if interim_result > Some result then false
  else
    match args with
    | [] -> CCOption.get_or ~default:Z.zero interim_result = result
    | hd :: tl -> (
        let next_eq = { result; args = tl } in
        match interim_result with
        | None -> try_eval (Some hd) next_eq ops
        | Some res ->
            CCList.exists
              (fun op -> try_eval (CCOption.some @@ op res hd) next_eq ops)
              ops)

let sum_valid equations ops =
  CCList.filter
    (fun eq -> try_eval None { result = eq.result; args = eq.args } ops)
    equations
  |> CCList.fold_left (fun acc eq -> Z.add acc eq.result) Z.zero

let part1 equations = sum_valid equations [ Z.add; Z.mul ]
let part2 equations = sum_valid equations [ Z.add; Z.mul; combine ]

let () =
  let input = get_input () in
  let equations = read_all input in
  part1 equations |> Z.print;
  print_newline ();
  part2 equations |> Z.print;
  print_newline ()
