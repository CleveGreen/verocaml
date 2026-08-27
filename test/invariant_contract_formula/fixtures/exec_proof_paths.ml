let exec_if flag =
  [%verocaml.ensures fun result -> result = flag] ; if flag then true else false

let exec_match (value : int option) =
  [%verocaml.ensures fun result -> result = result]
  ;
  match value with None -> 0 | Some item -> item

let proof_if flag =
  [%verocaml.ensures fun _ -> flag || not flag]
  ;
  if flag then [%verocaml.assert flag] else [%verocaml.assert not flag]
[@@verocaml.proof]

let proof_match (value : int option) =
  [%verocaml.ensures fun _ -> true]
  ;
  match value with
  | None -> [%verocaml.assert true]
  | Some item -> [%verocaml.assert item = item]
[@@verocaml.proof]
