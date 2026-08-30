type 'a option_specification = 'a option
[@@verocaml.external_type_specification]

type 'a box_specification = 'a External_types.box
[@@verocaml.external_type_specification]

let choose ?(value = 0) () = value

let selected () = choose ~value:3 ()
