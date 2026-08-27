type node = Empty | Node of int * node
val checked : (node [@finite]) -> node -> unit
