open Vstd

let increment (value : int) : Int.t = value + 1 [@@verocaml.spec]

let use (value : int) : int =
  [%verocaml.assert
    let pair = (increment, increment) in
    let function_value, _ = pair in
    function_value value = value + 1];
  value
