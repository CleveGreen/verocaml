type 'a box = Box of 'a

let option_is_some (value : 'a option) : bool =
  match value with None -> false | Some _ -> true
[@@verocaml.spec]

[%%verocaml.symbolic val option_unwrap : 'a option -> 'a]

let axiom_option_unwrap (value : 'a option) : unit =
  [%verocaml.requires option_is_some value];
  [%verocaml.ensures fun _ ->
    match value with
    | None -> false
    | Some payload -> payload = option_unwrap value];
  ()
[@@verocaml.axiom]

[%%verocaml.symbolic
  val nested_unwrap : ((('a box option) option, 'error box option) result) -> 'a]

let axiom_nested_unwrap
    (value : (('a box option) option, 'error box option) result) : unit =
  [%verocaml.ensures fun _ -> nested_unwrap value = nested_unwrap value];
  ()
[@@verocaml.axiom]

[%%verocaml.symbolic val sequence_value : 'a Vstd.Seq.t -> 'a]

let axiom_sequence_value (sequence : 'a Vstd.Seq.t) : unit =
  [%verocaml.ensures fun _ ->
    sequence_value sequence = sequence_value sequence];
  ()
[@@verocaml.axiom]

let use_option_axiom (value : 'a option) : unit =
  [%verocaml.requires option_is_some value];
  [%verocaml.ensures fun _ ->
    match value with
    | None -> false
    | Some payload -> payload = option_unwrap value];
  axiom_option_unwrap value
[@@verocaml.proof]

let use_nested_axiom
    (value : (('a box option) option, 'error box option) result) : unit =
  [%verocaml.ensures fun _ -> nested_unwrap value = nested_unwrap value];
  axiom_nested_unwrap value
[@@verocaml.proof]

let use_sequence_axiom (sequence : 'a Vstd.Seq.t) : unit =
  [%verocaml.ensures fun _ ->
    sequence_value sequence = sequence_value sequence];
  axiom_sequence_value sequence
[@@verocaml.proof]
