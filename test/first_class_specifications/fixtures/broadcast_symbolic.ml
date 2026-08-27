[%%verocaml.symbolic val observe : (int -> int) -> int]
[%%verocaml.symbolic val make : int -> int -> int]
[%%verocaml.symbolic val image : ('a -> 'b) -> 'a -> 'b]

let increment (value : int) : int = value + 1 [@@verocaml.spec]

let apply_reflexive (f : 'a -> 'b) (value : 'a) : unit =
  [%verocaml.ensures fun _ -> (f value [@trigger]) = f value] ; ()
[@@verocaml.proof] [@@verocaml.broadcast]

let image_axiom (f : 'a -> 'b) (value : 'a) : unit =
  [%verocaml.ensures fun _ -> (f value [@trigger]) = image f value] ; ()
[@@verocaml.axiom] [@@verocaml.broadcast]

[@@@verocaml.activate [ apply_reflexive; image_axiom ]]

let symbolic_functions (seed : int) (value : int) : int =
  [%verocaml.assert observe increment = observe increment];
  [%verocaml.assert (make seed) value = (make seed) value];
  [%verocaml.assert
    let f = increment in
    image f value = f value];
  [%verocaml.ensures fun result -> result = seed];
  seed
