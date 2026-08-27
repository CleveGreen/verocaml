let tracked_id (x : int [@tracked]) : (int [@tracked]) = (x [@tracked])
[@@verocaml.proof]
let bad x = let[@ghost] g = (x [@ghost]) in (tracked_id (g [@tracked]) [@tracked])
