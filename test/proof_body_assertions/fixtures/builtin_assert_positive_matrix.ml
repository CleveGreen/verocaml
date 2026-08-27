let require_equal (left : int) (right : int) : unit =
  [%verocaml.requires left = right];
  ()

let proof_mixed (value : int) : unit =
  assert (value = value);
  [%verocaml.assert value <= value];
  assert (value >= value)
[@@verocaml.proof]

let proof_branch_tail (choose : bool) (value : int) : unit =
  if choose then assert (value = value) else assert (value <= value)
[@@verocaml.proof]

let exec_statement (value : int) : int =
  assert (value = value);
  value

let exec_branch_tail (choose : bool) (value : int) : unit =
  if choose then assert (value = value) else assert (value <= value)

let exec_proof_region (value : int) : int =
  [%verocaml.proof
    assert (value = value);
    [%verocaml.assert value <= value];
    ()];
  value

let exec_fact_export (value : int) : unit =
  let alias = value in
  assert (alias = value);
  require_equal alias value
