type 'a option_specification = 'a Option.t
[@@verocaml.external_type_specification]

type ('a, 'error) result_specification = ('a, 'error) Result.t
[@@verocaml.external_type_specification]

let opt_is_some (option : 'a Option.t) : bool =
  match option with None -> false | Some _ -> true
[@@verocaml.spec]

let result_is_ok (result : ('a, 'error) Result.t) : bool =
  match result with Ok _ -> true | Error _ -> false
[@@verocaml.spec]

let opt_test (value : int) : int Option.t =
  [%verocaml.ensures fun result ->
    (not (value < 5)) || opt_is_some result];
  if value < 5 then Some value else None

let result_test (value : int) : (int, bool) Result.t =
  [%verocaml.ensures fun result ->
    (not (value < 5)) || result_is_ok result];
  if value < 5 then Ok value else Error false

let pick fallback ?(value = fallback) () =
  [%verocaml.ensures fun result -> result = value];
  value

let omitted () =
  [%verocaml.ensures fun result -> result = 7];
  pick 7 ()

let forwarded (carrier : int Option.t) = pick 7 ?value:carrier ()
