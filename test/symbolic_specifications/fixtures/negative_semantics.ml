[%%verocaml.symbolic val image : int -> int]

let false_injectivity (left : int) (right : int) : unit =
  [%verocaml.requires left <> right];
  [%verocaml.ensures fun _ -> image left <> image right];
  ()
