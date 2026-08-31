[%%verocaml.symbolic val produce : int -> unit]

let bad x =
  [%verocaml.requires
    forall (fun (y : int) -> ((produce y) [@trigger]) = ())];
  x
