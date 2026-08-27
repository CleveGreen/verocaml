[%%verocaml.symbolic val choose : 'a -> 'a]

let abstract_identity (value : 'a) : 'a =
  [%verocaml.ensures fun _ -> choose value = choose value];
  value

let integer_identity (value : int) : int =
  [%verocaml.ensures fun _ -> choose value = choose value];
  value

let boolean_identity (value : bool) : bool =
  [%verocaml.ensures fun _ -> choose value = choose value];
  value
