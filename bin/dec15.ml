open Aoc24

let split_input input =
  let matrix_input, instruction_input =
    CCList.take_drop_while (fun line -> line <> "") input
  in
  ( Matrix.from_input matrix_input,
    fold_input (fun acc _ c -> direction_of_char c :: acc) [] instruction_input
    |> CCList.rev )

let widen_matrix matrix =
  Matrix.init
    (2 * Matrix.width matrix)
    (Matrix.height matrix)
    (fun y x ->
      match Matrix.get_2d_exn matrix (x / 2) y with
      | 'O' when is_even x -> '['
      | 'O' -> ']'
      | '@' when is_even x -> '@'
      | '#' -> '#'
      | _ -> '.')

let find_robot matrix =
  Matrix.find_pos (( = ) '@') matrix
  |> CCOption.get_exn_or "Robot not found"
  |> fst

let exec_move matrix old_position new_position =
  Matrix.set_exn matrix new_position (Matrix.get_exn matrix old_position);
  Matrix.set_exn matrix old_position '.'

let rec move_object matrix position direction =
  let new_position = IntPosition.move position direction in
  match Matrix.get_exn matrix new_position with
  | '.' ->
      exec_move matrix position new_position;
      true
  | '#' -> false
  | 'O' | '[' | ']' ->
      if move_object matrix new_position direction then (
        exec_move matrix position new_position;
        true)
      else false
  | _ -> failwith "Invalid field"

let rec check_vertical_move matrix position direction =
  let new_position = IntPosition.move position direction in
  match Matrix.get_exn matrix new_position with
  | '.' -> true
  | '#' -> false
  | '[' ->
      check_vertical_move matrix new_position direction
      && check_vertical_move matrix
           (IntPosition.move new_position Right)
           direction
  | ']' ->
      check_vertical_move matrix new_position direction
      && check_vertical_move matrix
           (IntPosition.move new_position Left)
           direction
  | _ -> failwith "Invalid field"

let rec do_vertical_move matrix position direction =
  let new_position = IntPosition.move position direction in
  (match Matrix.get_exn matrix new_position with
  | '.' -> ()
  | '[' ->
      do_vertical_move matrix new_position direction;
      let new_right = IntPosition.move new_position Right in
      do_vertical_move matrix new_right direction
  | ']' ->
      do_vertical_move matrix new_position direction;
      let new_left = IntPosition.move new_position Left in
      do_vertical_move matrix new_left direction
  | _ -> failwith "Invalid field");
  exec_move matrix position new_position

let move_wide_object matrix position direction =
  if Matrix.get_exn matrix position <> '@' then failwith "no robot at pos";
  match direction with
  | Left | Right ->
      if move_object matrix position direction then
        IntPosition.move position direction
      else position
  | Up | Down ->
      if check_vertical_move matrix position direction then (
        do_vertical_move matrix position direction;
        IntPosition.move position direction)
      else position

let gps_score { x; y } obj = if obj = 'O' || obj = '[' then (100 * y) + x else 0

let matrix_score =
  Matrix.fold_pos (fun acc pos obj -> acc + gps_score pos obj) 0

let part1 matrix instructions =
  let robot = find_robot matrix in
  CCList.fold_left
    (fun rob inst ->
      if move_object matrix rob inst then IntPosition.move rob inst else rob)
    robot instructions
  |> ignore;
  (* Matrix.print print_char matrix; *)
  Matrix.fold_pos (fun acc pos obj -> acc + gps_score pos obj) 0 matrix

let part2 matrix instructions =
  let robot = find_robot matrix in
  CCList.fold_left (move_wide_object matrix) robot instructions |> ignore;
  (* Matrix.print print_char matrix; *)
  matrix_score matrix

let () =
  let matrix, instructions = get_input () |> split_input in
  let wide_matrix = widen_matrix matrix in
  part1 matrix instructions |> print_int_nl;
  part2 wide_matrix instructions |> print_int_nl
