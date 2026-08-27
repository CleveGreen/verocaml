type node = Empty | Node of int * node

let checked (value : node [@finite]) = ()

let caller (value : node) =
  checked value
