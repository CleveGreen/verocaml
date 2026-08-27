type 'a box = Box of 'a

[%%verocaml.symbolic val integer_image : int -> int]
[%%verocaml.symbolic val boolean_image : bool -> bool]
[%%verocaml.symbolic val box_image : 'a box -> 'a box]

let integer_congruence (value : int) : int =
  [%verocaml.ensures fun _ -> integer_image value = integer_image value];
  value

let boolean_congruence (value : bool) : bool =
  [%verocaml.ensures fun _ -> boolean_image value = boolean_image value];
  value

let aggregate_congruence (value : int box) : int box =
  [%verocaml.ensures fun _ -> box_image value = box_image value];
  value
