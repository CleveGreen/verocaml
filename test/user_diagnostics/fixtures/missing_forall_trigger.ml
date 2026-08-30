let observed (_value : int) : bool = true [@@verocaml.spec]

let preserve (value : int) : int =
  [%verocaml.requires forall (fun (candidate : int) -> observed candidate)];
  value
