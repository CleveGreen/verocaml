type node = Empty | Node of node
let checked () =
  let local (value : node [@finite]) = value in
  local Empty
