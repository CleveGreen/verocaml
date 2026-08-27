let tracked_id (value : int [@tracked]) : (int [@tracked]) =
  (value [@tracked])
[@@verocaml.proof]

let bad source =
  let[@tracked] tracked = (source [@tracked]) in
  let[@tracked] result = (tracked_id tracked [@tracked]) in
  (result [@tracked])
