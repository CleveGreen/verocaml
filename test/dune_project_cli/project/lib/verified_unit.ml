[@@@verocaml.verify]

let is_some option =
  match option with Some _ -> true | None -> false
[@@verocaml.spec]

let choose value =
  [%verocaml.ensures fun result -> value >= 0 || not (is_some result)];
  if value >= 0 then Some value else None

let sequence_length_is_valid sequence =
  [%verocaml.ensures fun _ ->
    Vstd.Seq.valid_length (Vstd.Seq.length sequence)];
  Vstd.Seq.axiom_length_domain sequence
[@@verocaml.proof]
