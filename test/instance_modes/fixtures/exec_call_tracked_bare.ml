let consume (value : int [@tracked]) : unit = () [@@verocaml.proof]
let caller source =
  let[@tracked] value = (source [@tracked]) in
  [%verocaml.proof consume value];
  source
