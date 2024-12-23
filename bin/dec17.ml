open Aoc24

type state = {
  a : int;
  b : int;
  c : int;
  code : int array;
  rev_listing : int list;
      (* reversed code as a list, this is useful to have and code doesn't change *)
  ip : int;
  output : int list;
}

let extract_int line = parse_all (skip_till integer) line

let read_program input =
  match input with
  | [ a_reg; b_reg; c_reg; _; code ] ->
      let listing = parse_all (extract_all integer) code in
      {
        a = extract_int a_reg;
        b = extract_int b_reg;
        c = extract_int c_reg;
        code = listing |> CCArray.of_list;
        rev_listing = listing |> CCList.rev;
        ip = 0;
        output = [];
      }
  | _ -> failwith "Invalid executable format"

let read_literal state = state.code.(state.ip + 1)

let read_combo state =
  match read_literal state with
  | x when x < 4 -> x
  | 4 -> state.a
  | 5 -> state.b
  | 6 -> state.c
  | _ -> failwith "Invalid operand"

let step ?(by = 2) state = { state with ip = state.ip + by }

let dv state =
  let numerator = state.a in
  let denominator = read_combo state |> pow 2 in
  numerator / denominator

let adv state = { state with a = dv state } |> step
let bxl state = { state with b = state.b lxor read_literal state } |> step
let bst state = { state with b = read_combo state % 8 } |> step

let jnz state =
  if state.a = 0 then state |> step else { state with ip = read_literal state }

let bxc state = { state with b = state.b lxor state.c } |> step

let out state =
  { state with output = (read_combo state % 8) :: state.output } |> step

let bdv state = { state with b = dv state } |> step
let cdv state = { state with c = dv state } |> step

let run_step state =
  if state.ip >= CCArray.length state.code then None
  else
    (match state.code.(state.ip) with
    | 0 -> adv
    | 1 -> bxl
    | 2 -> bst
    | 3 -> jnz
    | 4 -> bxc
    | 5 -> out
    | 6 -> bdv
    | 7 -> cdv
    | _ -> failwith "Invalid opcode")
      state
    |> CCOption.some

let rec run_program state =
  match run_step state with
  | None -> state
  | Some new_state -> run_program new_state

let print_list l =
  CCList.map char_of_digit l |> CCList.intersperse ',' |> CCList.iter print_char

let rec check_first_out ~expected state =
  match run_step state with
  | None -> failwith "Terminated before we got an output"
  | Some { output = [ out_hd ]; _ } -> out_hd = expected
  | Some new_state -> check_first_out ~expected new_state

let rec find_quine index state =
  if index < 0 then Some state
  else
    let expected = state.code.(index) in
    CCSeq.range 0 8
    |> CCSeq.filter_map (fun digit ->
           let to_test = { state with a = (state.a * 8) + digit } in
           if check_first_out ~expected to_test then
             find_quine (index - 1) to_test
           else None)
    |> CCSeq.head

let check_quine state =
  run_program state |> fun x ->
  if x.output = x.rev_listing then state.a else failwith "This quine is invalid"

let part1 input =
  read_program input |> run_program |> fun x -> x.output |> CCList.rev

let part2 input =
  let state = { (read_program input) with a = 0 } in
  find_quine (CCArray.length state.code - 1) state
  |> CCOption.get_exn_or "No quine found"
  |> check_quine

let () =
  let input = get_input () in
  part1 input |> print_list |> print_newline;
  part2 input |> print_int_nl
