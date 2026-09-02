open Vstd

let invoke (callback : int -> int) (value : int) = callback value
let increment (value : int) : Int.t = value + 1 [@@verocaml.spec]
let bad (value : int) = invoke increment value
