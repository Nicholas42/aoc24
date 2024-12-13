module type NumberType = sig
  type t

  val add : t -> t -> t
  val sub : t -> t -> t
  val abs : t -> t
  val zero : t
  val one : t
  val to_float : t -> float
  val to_int : t -> int
  val to_z : t -> Z.t
end

type 'a gen_position = { x : 'a; y : 'a }

module type PositionInterface = sig
  type number_type
  type t

  val diff : t -> t -> t
  val add : t -> t -> t
  val manhattan : t -> number_type
  val to_float : t -> float gen_position
  val to_int : t -> int gen_position
  val to_z : t -> Z.t gen_position
  val from_pair : number_type * number_type -> t
  val orthogonal_neighbors : t -> t list
  val diagonal_neighbors : t -> t list

  val in_rec :
    ?lower_bound_x:number_type ->
    ?lower_bound_y:number_type ->
    number_type ->
    number_type ->
    t ->
    bool
end

module MakePosition (N : NumberType) :
  PositionInterface with type number_type := N.t and type t := N.t gen_position =
struct
  let ( - ) = N.sub
  let ( + ) = N.add
  let diff { x = lx; y = ly } { x = rx; y = ry } = { x = lx - rx; y = ly - ry }
  let add { x = lx; y = ly } { x = rx; y = ry } = { x = lx + rx; y = ly + ry }
  let manhattan { x; y } = N.abs x + N.abs y
  let to_int { x; y } = { x = N.to_int x; y = N.to_int y }
  let to_float { x; y } = { x = N.to_float x; y = N.to_float y }
  let to_z { x; y } = { x = N.to_z x; y = N.to_z y }
  let from_pair (x, y) = { x; y }

  let orthogonal_neighbors { x; y } =
    [
      { x = x + N.one; y };
      { x = x - N.one; y };
      { x; y = y + N.one };
      { x; y = y - N.one };
    ]

  let diagonal_neighbors { x; y } =
    [
      { x = x + N.one; y = y + N.one };
      { x = x - N.one; y = y + N.one };
      { x = x + N.one; y = y - N.one };
      { x = x - N.one; y = y - N.one };
    ]

  let in_rec ?(lower_bound_x = N.zero) ?(lower_bound_y = N.zero) upper_bound_x
      upper_bound_y { x; y } =
    lower_bound_x <= x && lower_bound_y <= y && x < upper_bound_x
    && y < upper_bound_y
end

module IntPosition = MakePosition (struct
  include Int

  let to_int = CCFun.id
  let to_z = Z.of_int
end)

module FloatPosition = MakePosition (struct
  include Float

  let to_float = CCFun.id
  let to_z = Z.of_float
end)

module ZPosition = MakePosition (struct
  include Z

  let to_z = CCFun.id
end)

module QPosition = MakePosition (struct
  include Q

  let to_z x = Q.to_int x |> Z.of_int
end)

type position = int gen_position
type fPosition = float gen_position
type zPosition = Z.t gen_position
type qPosition = Q.t gen_position

let get_x { x; _ } = x
let get_y { y; _ } = y
