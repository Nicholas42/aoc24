let print_anything a =
  print_string @@ BatPervasives.dump a;
  print_newline ()

let get_input () = CCIO.(with_in Sys.argv.(1) read_lines_l)
