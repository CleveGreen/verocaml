[%%verocaml.symbolic val tuple_image : (int * int) -> int]

let rejected (value : int) : int =
  [%verocaml.ensures fun result -> result = value];
  value
