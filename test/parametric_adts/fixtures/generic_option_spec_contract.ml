let implies (premise : bool) (conclusion : bool) : bool =
  (not premise) || conclusion
[@@verocaml.spec]

let opt_is_some (option : 'a option) : bool =
  match option with None -> false | Some _ -> true
[@@verocaml.spec]

let opt_test (value : int) : int option =
  [%verocaml.ensures fun result ->
    implies (value < 5) (opt_is_some result)];
  if value < 5 then Some value else None

let result_is_ok (result : ('a, 'error) result) : bool =
  match result with Ok _ -> true | Error _ -> false
[@@verocaml.spec]

let result_test (value : int) : (int, bool) result =
  [%verocaml.ensures fun result ->
    implies (value < 5) (result_is_ok result)];
  if value < 5 then Ok value else Error false
