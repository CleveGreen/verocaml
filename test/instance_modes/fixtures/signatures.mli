val observe : int -> int
  [@@verocaml.spec]
val tracked_id : (int [@tracked]) -> (int [@tracked])
  [@@verocaml.proof]
val erased_exec : (int [@ghost]) -> (int [@ghost])
type packet = { run : int; ghost : int [@ghost] }
type choice = C of int * (int [@tracked])
