open Batteries

let print_list l =
  print_string "[";
  List.iter (fun x -> dump x |> Printf.printf "%s ") l;
  print_string "\b]\n"

let get_input_twice = BatFile.lines_of Sys.argv.(1) |> Enum.dup
