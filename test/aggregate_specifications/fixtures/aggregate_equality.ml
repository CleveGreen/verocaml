type value = Value of int

let equal (left : value) right = left = right [@@verocaml.spec]
