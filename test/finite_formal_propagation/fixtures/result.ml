type node = Empty | Node of node
let checked () : (node [@finite]) = Empty
