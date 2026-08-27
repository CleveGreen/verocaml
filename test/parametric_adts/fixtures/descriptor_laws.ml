type 'a local_option = LNone | LSome of 'a
type 'a choice = First of 'a | Second of 'a
type 'a pair = { left : 'a; right : 'a }

let option_laws (value : int) (other : int) =
  [%verocaml.ensures fun result -> result];
  LNone <> LSome value
  && (LSome value = LSome value)
  && ((value <> other) || LSome value = LSome other)

let constructor_laws (value : int) =
  [%verocaml.ensures fun result -> result];
  First value <> Second value

let record_laws (left : int) (right : int) =
  [%verocaml.ensures fun result -> result];
  let record = { left; right } in
  record.left = left && record.right = right

let branch_bindings (value : int choice) =
  [%verocaml.ensures fun result -> result];
  match value with
  | First payload -> value = First payload
  | Second payload -> value = Second payload
