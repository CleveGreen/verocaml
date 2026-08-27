module type STACK = sig
  type t
  val make : int -> t @ unique
  val snapshot : t @ read -> int
  val step : t @ unique -> t @ unique
  val duplicate : t -> t * t
end
module Stack : STACK = struct
  type node = Empty | Node of node
  type t = { mutable top : node }
  let make (_ : int) : t @ unique = { top = Empty }
  let snapshot (_ : t @ read) = 0
  let step (x : t @ unique) : t @ unique = x
  let duplicate (x : t) = (x, x)
end
