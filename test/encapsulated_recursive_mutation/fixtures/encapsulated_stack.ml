module type STACK = sig
  type t
  type view
  val singleton : int -> t @ unique
  val snapshot : t @ read -> view @ immutable
  val view_length : view @ immutable -> int
  val zero_head : t @ unique -> t @ unique
  val cut : t @ unique -> t @ unique
  val drop : t @ unique -> t @ unique
end

module Stack : STACK = struct
  type node = Empty | Node of { mutable value : int; mutable next : node }
  type t = { mutable top : node; mutable length : int }
  type view = { length : int }

  let singleton value : t @ unique =
    { top = Node { value; next = Empty }; length = 1 }
  let snapshot (stack : t @ read) : view @ immutable =
    { length = stack.length }
  let view_length (view : view @ immutable) = view.length
  let zero_head (stack : t @ unique) : t @ unique =
    match stack.top with
    | Empty -> stack
    | Node record ->
        record.value <- 0;
        stack
  let cut (stack : t @ unique) : t @ unique =
    match stack.top with
    | Empty -> stack
    | Node record ->
        record.next <- Empty;
        stack.length <- 1;
        stack
  let drop (stack : t @ unique) : t @ unique =
    match stack.top with
    | Empty -> stack
    | Node { value = _; next } ->
        stack.top <- next;
        stack.length <- 0;
        stack
end

let client (stack : Stack.t @ unique) =
  let stack = Stack.zero_head stack in
  let stack = Stack.cut stack in
  let view = Stack.snapshot stack in
  Stack.view_length view
