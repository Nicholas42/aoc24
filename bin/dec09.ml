open Aoc24

type file = { size : int; id : int }
type block = Free of int | File of file

let read_line line =
  parse_all (extract_all digit) line
  |> CCList.mapi (fun i d ->
         if i mod 2 = 0 then File { size = d; id = i / 2 } else Free d)

(* Is start + (start + 1) + ... + (start + length - 1) *)
let gauss start length =
  (((start + length) * (start + length - 1)) - (start * (start - 1))) / 2

let rec fqueue_concat = function
  | [] -> CCFQueue.empty
  | front :: rest -> CCFQueue.append front (fqueue_concat rest)

let rec move_forward queue ({ size; id } as last_elem) new_queue
    handle_too_small =
  let opt = CCFQueue.take_front queue in
  match opt with
  | None -> (new_queue, File { size; id })
  | Some (front, rest) -> (
      match front with
      | Free free_size ->
          if free_size >= size then
            ( fqueue_concat
                [
                  new_queue;
                  CCFQueue.doubleton
                    (File { size; id })
                    (Free (free_size - size));
                  rest;
                ],
              Free size )
          else handle_too_small rest last_elem free_size new_queue
      | File _ ->
          move_forward rest last_elem
            (CCFQueue.snoc new_queue front)
            handle_too_small)

let rec too_small_partial rest { size; id } free_size new_queue =
  move_forward rest
    { size = size - free_size; id }
    (CCFQueue.snoc new_queue (File { size = free_size; id }))
    too_small_partial

let rec too_small_whole rest last_elem free_size new_queue =
  move_forward rest last_elem
    (CCFQueue.snoc new_queue (Free free_size))
    too_small_whole

let check_sum l =
  CCFQueue.to_list l
  |> CCList.fold_left
       (fun (cum_size, result) block ->
         match block with
         | Free size -> (cum_size + size, result)
         | File { size; id } ->
             (cum_size + size, result + (id * gauss cum_size size)))
       (0, 0)
  |> snd

let rec defrag front_end back_end handler =
  match CCFQueue.take_back front_end with
  | None -> back_end
  | Some (front, (Free _ as free)) ->
      defrag front (CCFQueue.cons free back_end) handler
  | Some (front, File { size; id }) ->
      let new_front, new_last =
        move_forward front { size; id } CCFQueue.empty handler
      in
      defrag new_front (CCFQueue.cons new_last back_end) handler

let part1 input =
  let files = read_line input |> CCFQueue.of_list in
  defrag files CCFQueue.empty too_small_partial |> check_sum

let part2 input =
  let files = read_line input |> CCFQueue.of_list in
  let res = defrag files CCFQueue.empty too_small_whole in
  check_sum res

let () =
  let input = get_input () |> CCList.hd in
  part1 input |> print_int_nl;
  part2 input |> print_int_nl
