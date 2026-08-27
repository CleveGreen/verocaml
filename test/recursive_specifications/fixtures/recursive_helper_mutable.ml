type chain = Empty | Link of { value : int; mutable next : chain }

let mutable_is_empty (value : chain) : bool =
  match value with Empty -> true | Link _ -> false
[@@verocaml.spec]

let rec invalid_mutable (value : chain) : bool =
  [%verocaml.decreases value];
  match value with
  | Empty -> mutable_is_empty value
  | Link link -> invalid_mutable link.next
[@@verocaml.spec] [@@verocaml.opaque]
