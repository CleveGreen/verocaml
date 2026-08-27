type node = Empty | Node of int * node
let checked (left : node) (right : node [@finite]) = ()
