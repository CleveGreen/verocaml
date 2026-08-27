type node = Empty | Node of int * node

let consume (_xs : node [@finite]) : unit = ()

let choose flag (finite : node [@finite]) (arbitrary : node) : node =
  if flag then finite else arbitrary

let use flag finite arbitrary =
  consume (choose flag finite arbitrary)
