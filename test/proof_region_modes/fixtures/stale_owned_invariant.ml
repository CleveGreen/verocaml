type snapshot = { length : int }

module type STACK = sig
  type t
  val singleton : int -> t @ unique
  val model : t @ read -> snapshot @ immutable
  val invariant : t @ read -> bool
  val snapshot : t @ read -> snapshot @ immutable
  val zero_head : t @ unique -> t @ unique
end

module Stack : STACK = struct
  type node = Empty | Node of { mutable value : int; mutable next : node }
  type t = { mutable top : node; mutable length : int }

  let singleton value : t @ unique =
    { top = Node { value; next = Empty }; length = 1 }

  let model (stack : t @ read) : snapshot @ immutable =
    { length = stack.length }
  [@@verocaml.spec]

  let invariant (stack : t @ read) =
    (model stack).length >= 0
  [@@verocaml.type_invariant]

  let snapshot (stack : t @ read) : snapshot @ immutable =
    { length = stack.length }

  let zero_head (stack : t @ unique) : t @ unique =
    match stack.top with
    | Empty -> stack
    | Node record ->
        record.value <- 0;
        stack
end

let run value : unit =
  let stack = Stack.singleton value in
  let stack = Stack.zero_head stack in
  [%verocaml.proof [%verocaml.use_type_invariant stack]];
  ()
