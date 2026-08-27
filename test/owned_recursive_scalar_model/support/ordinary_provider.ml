type snapshot = { length : int; head : int; is_empty : bool }

module type STACK = sig
  type t
  val model : t @ read -> snapshot @ immutable
end

module Stack : STACK = struct
  type node =
    | Empty
    | Node of { value : int; next : node }

  type t = { top : node; length : int }

  let model (stack : t @ read) : snapshot @ immutable =
    match stack.top with
    | Empty ->
        { length = stack.length; head = 0; is_empty = true }
    | Node record ->
        { length = stack.length; head = record.value; is_empty = false }
end
