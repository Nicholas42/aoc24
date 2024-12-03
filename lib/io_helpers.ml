open Batteries

let print_anything a =
  print_string @@ dump a;
  print_newline ()

let get_input = File.lines_of Sys.argv.(1) |> BatList.of_enum
