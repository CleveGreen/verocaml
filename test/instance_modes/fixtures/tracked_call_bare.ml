let consume (value : int [@tracked]) : unit = () [@@verocaml.proof]
let caller (value : int [@tracked]) : unit =
  consume value
[@@verocaml.proof]
