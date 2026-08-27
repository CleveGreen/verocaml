type node = Empty | Node of int * node

let consume (_xs : node [@finite]) : unit = ()

let wrap (arbitrary : node) : node =
  Node (0, arbitrary)

let use arbitrary =
  consume (wrap arbitrary)
