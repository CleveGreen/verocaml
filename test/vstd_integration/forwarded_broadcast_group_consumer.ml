[@@@verocaml.verify]

[@@@verocaml.activate [Vstd_broadcast_forwarder.forwarded_seq_axioms]]

let constant_sequence (size : int) (value : 'a) : 'a Vstd.Seq.t =
  Vstd.Seq.init size (fun _ -> value)
[@@verocaml.spec]

let constant_sequence_index (size : int) (index : int) (value : 'a) : unit =
  [%verocaml.requires 0 <= index && index < size];
  [%verocaml.ensures fun _ ->
    Vstd.Seq.get (constant_sequence size value) index = value];
  ()
[@@verocaml.proof]
