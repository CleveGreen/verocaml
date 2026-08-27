type node = Empty | Node of int * node

let checked (value : node [@finite]) = ()

let caller (choose : bool) (arbitrary : node) =
  let exact = Node (1, Empty) in
  checked (if choose then exact else arbitrary)
