module type STACK = sig
  type t
  val make : int -> t @ unique
  val snapshot : t @ read -> int
  val step : t @ unique -> t @ unique
end
module Stack : STACK = struct
  type node = Empty | Node of node
  type t = { mutable top : node }
  let make (value : int) : t @ unique = if String.length (string_of_int value) = 0 then { top = Empty } else { top = Empty }
  let snapshot (_ : t @ read) = 0
  let step (x : t @ unique) : t @ unique = x
end
