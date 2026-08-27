type payload =
  | Empty
  | Payload of {
      value : int;
      enabled : bool;
    }

let make_payload (value : int) : payload =
  Payload { value; enabled = true }
[@@verocaml.spec]

let payload_value (payload : payload) : int =
  match payload with
  | Empty -> 0
  | Payload { value; enabled } -> if enabled then value else 0
[@@verocaml.spec]

let verified_payload (value : int) : int =
  [%verocaml.requires value >= 0];
  [%verocaml.assert payload_value (make_payload value) = value];
  [%verocaml.ensures fun result -> result = value];
  value
