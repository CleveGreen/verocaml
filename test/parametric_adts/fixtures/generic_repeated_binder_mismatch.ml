let same_payload_pair (_pair : 'a option * 'a option) : bool = true
[@@verocaml.spec]

let rejected (value : int) : int option * bool option =
  [%verocaml.ensures fun result -> same_payload_pair result];
  (Some value, Some true)
