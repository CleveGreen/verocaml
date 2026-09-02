type t = int
[@@verocaml.logical_sort]

let of_string value = int_of_string value
[@@verocaml.integer_literal]
