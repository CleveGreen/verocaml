[%%verocaml.symbolic val observed : int -> bool]

let bad x =
  [%verocaml.requires
    forall (fun (y : int) ->
      ((observed y) [@trigger]) && ((observed y) [@trigger]))];
  x
