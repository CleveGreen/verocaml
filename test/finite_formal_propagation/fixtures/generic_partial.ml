type 'a seq = Nil | Cons of 'a * 'a seq

let equivalent (left : 'a seq) (right : 'a seq) : bool = left = right
[@@verocaml.spec]

let instantiate (xs : int seq) : bool =
  let compare = equivalent xs in
  compare xs
[@@verocaml.spec]

let partial_spec_application_is_supported (xs : int seq) : unit =
  [%verocaml.assert instantiate xs]
[@@verocaml.proof]
