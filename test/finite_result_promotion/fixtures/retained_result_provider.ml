type node = Empty | Node of int * node

let make value = Node (value, Empty)
