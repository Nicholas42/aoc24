open Angstrom

let is_digit = function '0' .. '9' -> true | _ -> false

let is_whitespace = function
  | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
  | _ -> false

let whitespace = take_while is_whitespace

let string_peek expected =
  peek_string (String.length expected) >>= fun actual ->
  if actual = expected then advance 1 *> return (Some expected) else fail "nope"

let digit = satisfy is_digit >>| String.make 1 >>| int_of_string

let integer = take_while1 is_digit >>| int_of_string

let extract_all parser =
  many (parser >>| (fun x -> Some x) <|> any_char *> return None)
