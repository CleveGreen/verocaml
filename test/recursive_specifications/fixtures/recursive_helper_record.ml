type record_value = { value : int }

let record_helper (value : int) : bool =
  let wrapped = { value } in
  wrapped.value = 0
[@@verocaml.spec]

let rec invalid_record (value : int) : int =
  [%verocaml.decreases value];
  if record_helper value then 0 else invalid_record (value - 1)
[@@verocaml.spec] [@@verocaml.opaque]
