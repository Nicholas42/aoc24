open Position

module Matrix : sig
  type 'a t = 'a array array

  val get_opt : 'a t -> position -> 'a option
  val get_exn : 'a t -> position -> 'a
  val get_2d_opt : 'a t -> int -> int -> 'a option
  val get_2d_exn : 'a t -> int -> int -> 'a
  val set_safe : 'a t -> position -> 'a -> bool
  val set_exn : 'a t -> position -> 'a -> unit
  val set_2d_safe : 'a t -> int -> int -> 'a -> bool
  val set_2d_exn : 'a t -> int -> int -> 'a -> unit
  val make : int -> int -> 'a -> 'a t
  val init : int -> int -> (int -> int -> 'a) -> 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val mapi : (int -> int -> 'a -> 'b) -> 'a t -> 'b t
  val height : 'a t -> int
  val width : 'a t -> int
  val from_input : string list -> char t
  val find_pos : ('a -> bool) -> 'a t -> (position * 'a) option
  val count_pos : (position -> 'a -> bool) -> 'a t -> int
  val print : ('a -> unit) -> 'a t -> unit
  val fold_pos : ('b -> position -> 'a -> 'b) -> 'b -> 'a t -> 'b
end = struct
  type 'a t = 'a array array

  let height mat = CCArray.length mat
  let width mat = if height mat = 0 then 0 else CCArray.length mat.(0)

  let get_2d_opt mat x y =
    CCArray.get_safe mat y
    |> CCOption.flat_map (fun row -> CCArray.get_safe row x)

  let get_2d_exn mat x y = mat.(y).(x)
  let get_opt mat { x; y } = get_2d_opt mat x y
  let get_exn mat { x; y } = get_2d_exn mat x y
  let set_2d_exn mat x y v = mat.(y).(x) <- v

  let set_2d_safe mat x y v =
    if 0 <= x && x < width mat && 0 <= y && y < height mat then (
      set_2d_exn mat x y v;
      true)
    else false

  let set_exn mat { x; y } = set_2d_exn mat x y
  let set_safe mat { x; y } = set_2d_safe mat x y
  let make width height v = CCArray.make_matrix height width v
  let init width height f = CCArray.init_matrix height width f
  let map f mat = CCArray.(map (fun row -> map f row) mat)

  let mapi f mat =
    CCArray.(mapi (fun y row -> mapi (fun x c -> f x y c) row)) mat

  let from_input input =
    let height = CCList.length input in
    let width = CCString.length (CCList.hd input) in
    init width height (fun y x ->
        CCList.get_at_idx_exn y input |> fun row -> CCString.get row x)

  let find_pos f mat =
    CCArray.find_mapi
      (fun y row ->
        CCArray.find_idx f row |> function
        | Some (x, v) -> Some ({ x; y }, v)
        | None -> None)
      mat

  let count_pos f mat =
    CCArray.foldi
      (fun row_acc y row ->
        CCArray.foldi
          (fun acc x v -> if f { x; y } v then acc + 1 else acc)
          row_acc row)
      0 mat

  let print printer mat =
    CCArray.iter
      (fun row ->
        CCArray.iter printer row;
        print_newline ())
      mat

  let fold_pos f init m =
    CCArray.foldi
      (fun row_acc y row ->
        CCArray.foldi (fun acc x v -> f acc { x; y } v) row_acc row)
      init m
end
