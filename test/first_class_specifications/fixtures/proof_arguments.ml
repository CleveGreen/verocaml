open Vstd

let add_offset (offset : Int.t) (index : Int.t) : Int.t = offset + index
[@@verocaml.spec]

let immediate_lambda_in_proof
    (size : Int.t)
    (offset : Int.t)
    (index : Int.t) : unit =
  [%verocaml.requires 0 <= size];
  [%verocaml.requires 0 <= index && index < size];
  Seq.axiom_init_get size (fun argument -> add_offset offset argument) index
[@@verocaml.proof]

let rec countdown (value : Int.t) : Int.t =
  [%verocaml.decreases value];
  if value <= 0 then 0 else countdown (value - 1)
[@@verocaml.spec]
[@@verocaml.revealed]

let constant_indexer (value : 'a) : Int.t -> 'a = fun _ -> value
[@@verocaml.spec]

let constant_sequence (size : Int.t) (value : 'a) : 'a Seq.t =
  Seq.init size (constant_indexer value)
[@@verocaml.spec]

let sequence_init_get (size : Int.t) (value : 'a) (index : Int.t) : unit =
  [%verocaml.requires 0 <= size];
  [%verocaml.requires 0 <= index && index < size];
  [%verocaml.ensures
    fun _ -> Seq.get (constant_sequence size value) index = value];
  Seq.axiom_init_get size (constant_indexer value) index
[@@verocaml.proof]
