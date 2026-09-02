[@@@verocaml.verify]

type 'a box = Box of 'a

type 'a node = Empty | Node of 'a * 'a node

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

let rec node_length (node : 'a node) : Vstd.Int.t =
  [%verocaml.decreases node];
  match node with Empty -> 0 | Node (_, rest) -> 1 + node_length rest
[@@verocaml.spec]
[@@verocaml.revealed]

let rec node_length_nonnegative (node : 'a node [@finite]) : unit =
  [%verocaml.ensures fun _ -> 0 <= node_length node];
  [%verocaml.decreases node];
  match node with
  | Empty -> ()
  | Node (_, rest) -> node_length_nonnegative rest
[@@verocaml.proof]

[%%verocaml.symbolic val arbitrary_value : 'a]

let sequence_view (node : 'a node) : 'a Vstd.Seq.t =
  Vstd.Seq.init (node_length node) (fun _ -> arbitrary_value)
[@@verocaml.spec]

let sequence_view_reflexive (node : 'a node [@finite]) : unit =
  [%verocaml.ensures fun _ ->
    Vstd.Seq.get (sequence_view node) 0
    = Vstd.Seq.get (sequence_view node) 0];
  ()
[@@verocaml.proof]

let axiom_sequence_view_index
    (index : int) (node : 'a node) : unit =
  [%verocaml.requires 0 <= index && index < node_length node];
  [%verocaml.ensures fun _ ->
    Vstd.Seq.get (sequence_view node) index = arbitrary_value];
  ()
[@@verocaml.axiom]

let sequence_view_index (index : int) (node : 'a node [@finite]) : unit =
  [%verocaml.requires 0 <= index && index < node_length node];
  [%verocaml.ensures fun _ ->
    Vstd.Seq.get (sequence_view node) index = arbitrary_value];
  node_length_nonnegative node;
  axiom_sequence_view_index index node
[@@verocaml.proof]
