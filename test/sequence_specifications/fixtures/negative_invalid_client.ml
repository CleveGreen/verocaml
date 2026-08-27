let integer_sequence size offset =
  init size (fun index -> offset + index)
[@@verocaml.spec]

let logical_update sequence index value = update sequence index value
[@@verocaml.spec]

let logical_subrange sequence lower upper =
  subrange sequence lower upper
[@@verocaml.spec]

let invalid_init_length_is_not_constrained () : unit =
  [%verocaml.activate [axiom_length_domain]
    ([%verocaml.assert length (integer_sequence (-1) 0) = -1]; ())]
[@@verocaml.proof]

let invalid_empty_get_is_not_constrained () : unit =
  [%verocaml.activate [axiom_length_domain]
    ([%verocaml.assert get (empty : int t) 0 = 123]; ())]
[@@verocaml.proof]

let invalid_upper_get_is_not_constrained () : unit =
  [%verocaml.activate [axiom_length_domain]
    ([%verocaml.assert get (integer_sequence 2 10) 2 = 12]; ())]
[@@verocaml.proof]

let invalid_update_is_not_constrained () : unit =
  [%verocaml.activate [axiom_length_domain]
    ([%verocaml.assert
       length (logical_update (empty : int t) 0 9) = 0];
     ())]
[@@verocaml.proof]

let invalid_subrange_is_not_constrained () : unit =
  [%verocaml.activate [axiom_length_domain]
    ([%verocaml.assert
       length (logical_subrange (integer_sequence 3 0) (-1) 1) = 2];
     ())]
[@@verocaml.proof]
