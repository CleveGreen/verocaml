let nonnegative (value : int) : bool = value >= 0 [@@verocaml.spec]

let proof_observe (value : int) : unit =
  [%verocaml.requires nonnegative value];
  [%verocaml.assert nonnegative value];
  [%verocaml.ensures fun result -> nonnegative value];
  ()
[@@verocaml.proof]

let rec recursive_observe (value : int) : unit =
  [%verocaml.requires nonnegative value];
  [%verocaml.ensures fun result -> nonnegative value];
  [%verocaml.decreases value];
  if value = 0 then () else recursive_observe (value - 1)
[@@verocaml.proof]

let tracked_successor (value : int [@tracked]) : (int [@tracked]) =
  [%verocaml.requires value >= 0];
  [%verocaml.ensures fun result -> result = value + 1];
  let[@tracked] next = (((value [@tracked]) + 1) [@tracked]) in
  (next [@tracked])
[@@verocaml.proof]

let run source =
  [%verocaml.requires nonnegative source];
  [%verocaml.ensures fun result -> result = source];
  let[@tracked] tracked = (source [@tracked]) in
  let[@ghost] ghost = (source [@ghost]) in
  [%verocaml.proof
    let _ = nonnegative source in
    let _ = nonnegative tracked in
    let _ = nonnegative ghost in
    proof_observe source;
    proof_observe tracked;
    proof_observe ghost;
    recursive_observe source;
    recursive_observe tracked;
    recursive_observe ghost;
    let[@tracked] next =
      (tracked_successor (tracked [@tracked]) [@tracked])
    in
    let _ = (next [@tracked]) in
    ()];
  source
