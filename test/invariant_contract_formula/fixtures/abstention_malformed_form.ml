let malformed_sequence (value : int) = (value; true) [@@verocaml.spec]
let malformed_result flag = if flag then 1 else 2 [@@verocaml.spec]
let malformed_pattern value =
  let Some item = value in
  item
[@@verocaml.spec]
