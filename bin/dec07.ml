open Aoc24

type equation = { result : int; args : int list }

let read_equation =
  integer <* string ": " &> extract_all integer >>| fun (res, args) ->
  { result = res; args }

let combine lhs rhs = int_of_string (Int.to_string lhs ^ Int.to_string rhs)

let read_all input =
  CCList.map
    (fun line ->
      match parse_string ~consume:All read_equation line with
      | Ok result -> result
      | Error msg -> failwith msg)
    input

let rec try_eval interim_result { result; args } ops =
  if interim_result > Some result then false
  else
    match args with
    | [] -> CCOption.get_or ~default:0 interim_result = result
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
  |> CCList.map (fun eq -> eq.result)
  |> sum

let part1 equations = sum_valid equations [ ( + ); ( * ) ]
let part2 equations = sum_valid equations [ ( + ); ( * ); combine ]

let () =
  let input = get_input () in
  let equations = read_all input in
  part1 equations |> print_int;
  print_newline ();
  part2 equations |> print_int;
  print_newline ()
