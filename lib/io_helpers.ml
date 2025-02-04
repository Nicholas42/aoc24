open Position

let print_anything a =
  print_string @@ BatPervasives.dump a;
  print_newline ()

let digit_of_char c = int_of_char c - int_of_char '0'
let char_of_digit d = char_of_int (d + int_of_char '0')
let get_input () = CCIO.(with_in Sys.argv.(1) read_lines_l)

let print_int_nl x =
  print_int x;
  print_newline ()

let fold_input (func : 'a -> position -> char -> 'a) (init : 'a)
    (input : string list) : 'a =
  CCList.foldi
    (fun outer_acc y row ->
      CCString.foldi
        (fun inner_acc x c -> func inner_acc { x; y } c)
        outer_acc row)
    init input
