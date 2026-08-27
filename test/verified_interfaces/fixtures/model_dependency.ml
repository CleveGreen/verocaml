type snapshot = { length : int }

module type STACK = sig
  type t
  val singleton : int -> t @ unique
  val length : t @ read -> int
  val model : t @ read -> snapshot @ immutable
  val invariant : t @ read -> bool
end

module Stack : STACK = struct
  type t = { length : int }

  let singleton (_value : int) : t @ unique =
    { length = 1 }

  let length (stack : t @ read) = stack.length

  let model (stack : t @ read) : snapshot @ immutable =
    ({ length = stack.length } : snapshot)
  [@@verocaml.spec]

  let invariant (stack : t @ read) =
    (model stack).length >= 0
  [@@verocaml.type_invariant]
end

let observe (stack : Stack.t @ read) =
  Stack.length stack

let bounded (value : int) =
  [%verocaml.requires value >= 0];
  [%verocaml.ensures fun result -> result >= 0];
  value
