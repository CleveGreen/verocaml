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

let rec invalid_model_helper (stack : Stack.t @ read) (remaining : int) : int =
  [%verocaml.decreases remaining];
  if (Stack.model stack).length = 0 || remaining <= 0 then 0
  else invalid_model_helper stack (remaining - 1)
[@@verocaml.spec] [@@verocaml.opaque]
