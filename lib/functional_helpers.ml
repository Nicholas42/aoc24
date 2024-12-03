open Batteries

let filter_some l = BatList.filter_map identity l
let back_cons l e = l @ [ e ]
