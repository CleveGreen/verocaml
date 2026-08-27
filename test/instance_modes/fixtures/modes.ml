let observe (x : int) = x + 1 [@@verocaml.spec]
let proof_observe (x : int) = x + 1 [@@verocaml.proof]
let tracked_id (x : int [@tracked]) : (int [@tracked]) = (x [@tracked]) [@@verocaml.proof]
let exec_erased (x : int [@ghost]) (y : int [@tracked]) : (int [@ghost]) = (x [@ghost])
let exec_tracked_result x : (int [@tracked]) = (x [@tracked])
let run x =
  let[@ghost] g = (observe (x [@ghost]) [@ghost]) in
  let[@tracked] t = (x [@tracked]) in
  let[@ghost] _z = (exec_erased (g [@ghost]) (t [@tracked]) [@ghost]) in
  let[@tracked] _u = (exec_tracked_result x [@tracked]) in
  [%verocaml.proof let _ = observe (x [@ghost]) in let _ = proof_observe (t [@ghost]) in ()];
  x
