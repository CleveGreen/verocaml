let positive (value : int) : bool = value > 0

let preserve (value : int) : int =
  [%verocaml.ensures fun _ -> positive value];
  value
