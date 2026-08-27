type snapshot = { length : int; head : int; is_empty : bool }
type node = Empty | Node of { mutable value : int; mutable next : node }
type hidden_t = { mutable top : node; mutable length : int }

module type STACK = sig
  type t = hidden_t
  val model : t @ read -> snapshot @ immutable
  val zero_head : t @ unique -> t @ unique
end

module Stack : STACK = struct
  type t = hidden_t

  let model (stack : t @ read) : snapshot @ immutable =
    match stack.top with
    | Empty ->
        { length = stack.length; head = 0; is_empty = true }
    | Node record ->
        { length = stack.length; head = record.value; is_empty = false }
  [@@verocaml.spec]

  let zero_head (stack : t @ unique) : t @ unique =
    match stack.top with
    | Empty -> stack
    | Node record ->
        record.value <- 0;
        stack
end

let substituted_client (stack : Stack.t @ read) = Stack.model stack
