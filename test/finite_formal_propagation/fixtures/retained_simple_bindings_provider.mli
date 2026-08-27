type node = Empty | Node of int * node
val node_length : node -> int
val acceptable : node -> bool
val checked : (node [@finite]) -> (node [@finite]) -> int
