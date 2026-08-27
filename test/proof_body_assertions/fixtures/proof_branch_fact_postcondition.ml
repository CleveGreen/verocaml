let proof_if_branch_fact (take_left : bool) =
  [%verocaml.ensures fun _result -> take_left = take_left];
  if take_left then [%verocaml.assert take_left = true] else ()
[@@verocaml.proof]

let proof_match_branch_fact (selector : int) =
  [%verocaml.ensures fun _result -> selector = selector];
  match selector with
  | 0 -> [%verocaml.assert selector = 0]
  | _ -> ()
[@@verocaml.proof]

let proof_both_branch_facts (take_left : bool) =
  if take_left then [%verocaml.assert take_left = take_left]
  else [%verocaml.assert take_left = take_left];
  [%verocaml.assert take_left = take_left]
[@@verocaml.proof]

let spec_bool (value : bool) : bool = value
[@@verocaml.spec]

let require_true (value : bool) =
  [%verocaml.requires spec_bool value];
  ()

let exec_region_inner_branch (take_left : bool) =
  [%verocaml.proof
    if take_left then [%verocaml.assert spec_bool take_left] else ();
    ()];
  require_true take_left
