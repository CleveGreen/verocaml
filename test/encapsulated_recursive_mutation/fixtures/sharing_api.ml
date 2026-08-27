module type STACK = sig
  type t
  val make : int -> t @ unique
  val snapshot : t @ read -> int
  val join : t @ unique -> t @ unique -> t @ unique
  val step : t @ unique -> t @ unique
end
module Stack : STACK = struct
  type node = Empty | Node of node
  type t = { mutable top : node }
  let make (_ : int) : t @ unique = { top = Empty }
  let snapshot (_ : t @ read) = 0
  let join (left : t @ unique) (_right : t @ unique) : t @ unique = left
  let step (x : t @ unique) : t @ unique = x
end
