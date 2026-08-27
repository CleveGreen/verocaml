let callee (value : int) = value + 1
let exec_call (value : int) = callee value

let contract_callback (callback : int -> int) (value : int) =
  [%verocaml.requires call_requires (callback value)];
  [%verocaml.ensures fun result -> call_ensures (callback value) result];
  callback value
