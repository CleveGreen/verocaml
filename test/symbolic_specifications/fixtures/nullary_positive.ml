type 'a box = Box of 'a

[%%verocaml.symbolic val integer_constant : int]
[%%verocaml.symbolic val boolean_constant : bool]
[%%verocaml.symbolic val box_constant : int box]

let integer_identity (value : int) : int =
  [%verocaml.ensures fun _ -> integer_constant = integer_constant];
  value

let boolean_identity (value : int) : int =
  [%verocaml.ensures fun _ -> boolean_constant = boolean_constant];
  value

let aggregate_identity (value : int) : int =
  [%verocaml.ensures fun _ -> box_constant = box_constant];
  value
