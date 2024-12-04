open Batteries

let filter_some l = BatList.filter_map identity l
let back_cons l e = l @ [ e ]

module String = struct
  include Stdlib.String
  let get_opt container index =
    if index < 0 || index >= length container then None
    else Some (get container index)
end

module Array = struct
  include Stdlib.Array
  let get_opt container index =
    if index < 0 || index >= length container then None
    else Some (get container index)
end
