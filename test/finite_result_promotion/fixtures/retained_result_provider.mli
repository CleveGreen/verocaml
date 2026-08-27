type node = Empty | Node of int * node

val make : int -> node
