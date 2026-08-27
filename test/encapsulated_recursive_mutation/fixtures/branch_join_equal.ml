module type STACK = sig
  type t
  val make : int -> t @ unique
  val snapshot : t @ read -> int
  val step : t @ unique -> t @ unique
end
module Stack : STACK = struct
  type node = Empty | Node of { value : int; mutable next : node }
  type t = { mutable top : node; mutable length : int }
  let make value : t @ unique =
    { top = Node { value; next = Empty }; length = 1 }
  let snapshot (stack : t @ read) = stack.length
  let step (stack : t @ unique) : t @ unique =
    (if stack.length = 0 then stack.length <- 1 else stack.length <- 2);
    stack.length <- 3;
    stack
end
