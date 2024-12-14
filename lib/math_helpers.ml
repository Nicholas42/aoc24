let dist a b = a - b |> abs
let is_even x = x mod 2 = 0
let is_odd = CCFun.negate is_even

type divmod_result = { quotient : int; remainder : int }

let divmod dividend divisor =
  { quotient = dividend / divisor; remainder = dividend mod divisor }

type log_result = { exponent : int; result : int }

let rec num_len ?(result = 0) number =
  if number = 0 then result else num_len (number / 10) ~result:(result + 1)

let log_ceil ~base number =
  let rec log_ceil_impl inner_number { exponent; result } =
    if inner_number = 0 then { exponent; result }
    else log_ceil_impl (inner_number / base) { exponent = exponent + 1; result }
  in
  log_ceil_impl number { exponent = 0; result = 1 }

let log_floor ~base number =
  match log_ceil ~base number with
  | { exponent = 0; result = 1 } -> { exponent = 0; result = 1 }
  | { exponent; result } -> { exponent = exponent - 1; result = result / base }

let square x = x * x

let rec pow base exp =
  match exp with
  | 0 -> 1
  | _ when is_even exp ->
      let half_pow = pow base (exp / 2) in
      half_pow * half_pow
  | _ -> base * pow base (exp - 1)

let ( % ) x y = ((x mod y) + y ) mod y
