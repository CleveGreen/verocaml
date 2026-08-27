type recursive_snapshot =
  | End
  | More of { length : int; head : int; rest : recursive_snapshot }
module type STACK = sig
  type t
  val model : t @ read -> recursive_snapshot @ immutable
  val singleton : int -> t @ unique
  val length : t @ read -> int
  val zero_head : t @ unique -> t @ unique
end
module Stack : STACK = struct
  type node = Empty | Node of { mutable value : int; mutable next : node }
  type t = { mutable top : node; mutable length : int }
  let model (stack : t @ read) : recursive_snapshot @ immutable =
    match stack.top with
    | Empty -> End
    | Node record -> More { length = stack.length; head = record.value; rest = End }
  [@@verocaml.spec]
  let singleton value : t @ unique =
    { top = Node { value; next = Empty }; length = 1 }
  let length (stack : t @ read) = stack.length
  let zero_head (stack : t @ unique) : t @ unique =
    match stack.top with
    | Empty -> stack
    | Node record -> record.value <- 0; stack
end
