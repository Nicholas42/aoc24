open Angstrom
open Functional_helpers

let is_digit = function '0' .. '9' -> true | _ -> false

let is_whitespace = function
  | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
  | _ -> false

let whitespace = take_while is_whitespace

let string_peek expected =
  peek_string (String.length expected) >>= fun actual ->
  if actual = expected then advance 1 *> return (Some expected)
  else fail @@ Printf.sprintf "Expected string '%s', found '%s'" expected actual

let digit = satisfy is_digit >>| String.make 1 >>| int_of_string
let integer = take_while1 is_digit >>| int_of_string
let integerZ = take_while1 is_digit >>| Z.of_string

let extract_all parser =
  many (parser >>| (fun x -> Some x) <|> any_char *> return None) >>| CCList.keep_some

let ( &> ) = both

let parse_all parser line =
  match parse_string ~consume:All parser line with
  | Ok result -> result
  | Error msg -> failwith msg
