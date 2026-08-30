type 'a option_specification = 'a option
[@@verocaml.external_type_specification]

let bool_option (option : bool option) : bool =
  match option with None -> false | Some _ -> true
[@@verocaml.spec]

let rejected (value : int) : int option =
  [%verocaml.ensures fun result -> bool_option result];
  Some value
