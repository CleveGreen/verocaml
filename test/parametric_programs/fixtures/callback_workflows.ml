[@@@verocaml.verify]

type 'a option_specification = 'a option
[@@verocaml.external_type_specification]

type ('a, 'error) result_specification = ('a, 'error) result
[@@verocaml.external_type_specification]

type ('a, 'b) choice = First of 'a | Second of 'b
type ('a, 'error) outcome = Accepted of 'a | Rejected of 'error
type 'a envelope = { sequence : int; payload : 'a }

let apply (callback : 'a -> 'b) (value : 'a) : 'b =
  [%verocaml.requires call_requires (callback value)];
  [%verocaml.ensures fun (result : 'b) -> call_ensures (callback value) result];
  callback value

let apply_labelled (callback : context:'context -> item:'a -> 'b) context item :
    'b =
  [%verocaml.requires call_requires (callback ~context ~item)];
  [%verocaml.ensures
    fun (result : 'b) -> call_ensures (callback ~context ~item) result];
  callback ~item ~context

let transform_option (callback : 'a -> 'b) (value : 'a option) : 'b option =
  [%verocaml.requires
    match value with
    | None -> true
    | Some payload -> call_requires (callback payload)];
  [%verocaml.ensures
    fun (result : 'b option) ->
      match value with
      | None -> result = None
      | Some payload -> (
          match result with
          | None -> false
          | Some transformed -> call_ensures (callback payload) transformed)];
  match value with None -> None | Some payload -> Some (callback payload)

let transform_result (callback : 'a -> 'b) (value : ('a, 'error) result) :
    ('b, 'error) result =
  [%verocaml.requires
    match value with
    | Error _ -> true
    | Ok payload -> call_requires (callback payload)];
  [%verocaml.ensures
    fun (result : ('b, 'error) result) ->
      match (value, result) with
      | Error before, Error after -> before = after
      | Ok before, Ok after -> call_ensures (callback before) after
      | _, _ -> false];
  match value with
  | Error error -> Error error
  | Ok payload -> Ok (callback payload)

let recover_result (callback : 'error -> 'a) (value : ('a, 'error) result) : 'a
    =
  [%verocaml.requires
    match value with
    | Ok _ -> true
    | Error error -> call_requires (callback error)];
  [%verocaml.ensures
    fun (result : 'a) ->
      match value with
      | Ok payload -> result = payload
      | Error error -> call_ensures (callback error) result];
  match value with Ok payload -> payload | Error error -> callback error

let route_choice (on_first : 'a -> 'result) (on_second : 'b -> 'result)
    (value : ('a, 'b) choice) : 'result =
  [%verocaml.requires
    match value with
    | First payload -> call_requires (on_first payload)
    | Second payload -> call_requires (on_second payload)];
  [%verocaml.ensures
    fun (result : 'result) ->
      match value with
      | First payload -> call_ensures (on_first payload) result
      | Second payload -> call_ensures (on_second payload) result];
  match value with
  | First payload -> on_first payload
  | Second payload -> on_second payload

let transform_envelope (callback : 'a -> 'b) (envelope : 'a envelope) :
    'b envelope =
  [%verocaml.requires call_requires (callback envelope.payload)];
  [%verocaml.ensures
    fun (result : 'b envelope) ->
      result.sequence = envelope.sequence
      && call_ensures (callback envelope.payload) result.payload];
  { sequence = envelope.sequence; payload = callback envelope.payload }

let identity value =
  [%verocaml.requires true];
  [%verocaml.ensures fun result -> result = value];
  value

let logical_not value =
  [%verocaml.requires true];
  [%verocaml.ensures fun result -> result = not value];
  not value

let nonnegative value =
  [%verocaml.requires true];
  [%verocaml.ensures fun result -> result >= 0];
  if value >= 0 then value else 0

let add_context ~context ~item =
  [%verocaml.requires context >= 0];
  [%verocaml.requires item >= 0];
  [%verocaml.ensures fun result -> result >= context];
  if context > 4611686018427387903 - item then 4611686018427387903
  else context + item

let int_identity_client value =
  [%verocaml.ensures fun result -> result = value] ; apply identity value

let bool_identity_client value =
  [%verocaml.ensures fun result -> result = value] ; apply identity value

let bool_transform_client (value : bool) : bool option =
  [%verocaml.ensures fun (result : bool option) -> result = Some (not value)]
  ;
  transform_option logical_not (Some value)

let result_transform_client (value : int) : (int, unit) result =
  [%verocaml.ensures fun (result : (int, unit) result) ->
     match result with Error _ -> false | Ok transformed -> transformed >= 0]
  ;
  transform_result nonnegative (Ok value)

let labelled_client context item =
  [%verocaml.requires context >= 0];
  [%verocaml.requires item >= 0];
  [%verocaml.ensures fun result -> result >= context];
  apply_labelled add_context context item

let envelope_client (sequence : int) (payload : 'a) : 'a envelope =
  [%verocaml.ensures fun (result : 'a envelope) ->
     result.sequence = sequence && result.payload = payload]
  ;
  transform_envelope identity { sequence; payload }
