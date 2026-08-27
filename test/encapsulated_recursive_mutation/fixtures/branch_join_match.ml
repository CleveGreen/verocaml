module type STACK = sig
  type t
  val make : int -> t @ unique
  val snapshot : t @ read -> int
  val bad : t @ unique -> t @ unique
end
module Stack : STACK = struct
  type node = Empty | Node of { value : int; mutable next : node }
  type t = { mutable top : node; mutable length : int }
  let make value : t @ unique =
    { top = Node { value; next = Empty }; length = 1 }
  let snapshot (stack : t @ read) = stack.length
  let bad (stack : t @ unique) : t @ unique =
    (match stack.top with
    | Node record -> record.next <- Empty
    | Empty -> ());
    stack.length <- 1;
    stack
end
