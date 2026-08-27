type node = Empty | Node of node
val checked : (node [@finite]) -> (node [@finite]) -> unit
