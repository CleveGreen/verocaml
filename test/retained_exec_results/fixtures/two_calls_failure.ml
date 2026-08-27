[@@@verocaml.verify]

let two_calls value =
  [%verocaml.ensures fun result -> result];
  let first = Provider.make_record value in
  let second = Provider.make_record value in
  first.Provider.unstated = second.Provider.unstated
