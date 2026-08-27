let identity (value : int [@tracked]) : (int [@tracked]) =
  value
[@@verocaml.proof]
