[@@@verocaml.verify]

let optional_specification ~first ?(second : int option @ once) () =
  Curried_legacy.optional ~first ?second ()
[@@verocaml.external_specification]
