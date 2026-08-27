type 'a seq = Nil | Cons of 'a * 'a seq

let equivalent ~(left : 'a seq) ~(right : 'a seq) : bool = left = right
[@@verocaml.spec]

let instantiate (xs : int seq) : bool = equivalent ~left:xs ~right:xs
[@@verocaml.spec]

let labelled_spec_application_is_supported (xs : int seq) : unit =
  [%verocaml.assert instantiate xs]
[@@verocaml.proof]
