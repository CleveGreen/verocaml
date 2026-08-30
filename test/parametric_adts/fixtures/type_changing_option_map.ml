type 'a option_specification = 'a option
[@@verocaml.external_type_specification]

let map_option
    (callback : 'a -> 'b option)
    (value : 'a option) : 'b option =
  [%verocaml.requires
    match value with None -> true | Some payload -> call_requires (callback payload)];
  [%verocaml.ensures fun result ->
    match value with
    | None -> true
    | Some payload -> call_ensures (callback payload) result];
  match value with None -> None | Some payload -> callback payload
