module type STACK = sig
  type t
  val make : int -> t @ unique
  val snapshot : t @ read -> int
  val bad : t @ unique -> t @ unique
end
module Stack : STACK = struct
  type node = Empty | Node of { value : int; mutable next : node }
  type t = { mutable top : node }
  let make value : t @ unique = { top = Node { value; next = Empty } }
  let snapshot (_ : t @ read) = 0
  let bad (stack : t @ unique) : t @ unique =
    match stack.top with
    | Empty -> stack
    | Node { value = _; next } ->
        let copied = next in
        stack.top <- copied;
        stack
end
