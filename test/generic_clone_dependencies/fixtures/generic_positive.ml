type 'a choice = Neither | One of 'a | Two of 'a * 'a

let id value = value

let choose flag left right = if flag then left else right

let labelled ~first ~second ~flag =
  if flag then first else second

let spec_identity (value : 'a) : bool = value = value
[@@verocaml.spec]

let spec_choice_identity (value : 'a choice) : bool = value = value
[@@verocaml.spec]

let prove_identity (value : 'a) : unit =
  [%verocaml.ensures fun _result -> spec_identity value];
  ()
[@@verocaml.proof]

let prove_choice_identity (value : 'a choice) : unit =
  [%verocaml.ensures fun _result -> spec_choice_identity value];
  ()
[@@verocaml.proof]

let use_int (value : int) =
  labelled ~second:value ~flag:true ~first:(choose true (id value) value)

let use_bool (value : bool) =
  labelled ~second:value ~flag:true ~first:(choose true (id value) value)

let int_spec_control (value : int) : bool = spec_identity value
[@@verocaml.spec]

let bool_spec_control (value : bool) : bool = spec_identity value
[@@verocaml.spec]

let int_proof_control (value : int) : unit = prove_identity value
[@@verocaml.proof]

let bool_proof_control (value : bool) : unit = prove_identity value
[@@verocaml.proof]
