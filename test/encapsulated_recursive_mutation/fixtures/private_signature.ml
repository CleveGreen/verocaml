module type STACK = sig
  type t = private { mutable next : t }
  val make : int -> t @ unique
  val snapshot : t @ read -> int
  val step : t @ unique -> t @ unique
end
module Stack : STACK = struct
  type t = { mutable next : t }
  let rec make (value : int) : t @ unique = make value
  let snapshot (_ : t @ read) = 0
  let step (x : t @ unique) : t @ unique = x
end
