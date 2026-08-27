let same value = value = value [@@verocaml.spec]

let reflexive value : unit =
  [%verocaml.assert same value];
  [%verocaml.ensures fun _result -> value = value];
  ()
[@@verocaml.proof]

let int_case (value : int) : unit =
  [%verocaml.assert same value];
  ()
[@@verocaml.proof]

let bool_case (value : bool) : unit =
  [%verocaml.assert same value];
  ()
[@@verocaml.proof]
