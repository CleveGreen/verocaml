[%%verocaml.symbolic val observed : int -> bool]

let bad x =
  [%verocaml.requires
    forall (fun (y : int) -> ((y = y) [@trigger]))];
  x
