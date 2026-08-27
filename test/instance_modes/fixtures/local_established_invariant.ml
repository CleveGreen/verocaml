type snapshot = { value : int }

module type BOX = sig
  type t
  val make : int -> t @ unique
  val model : t @ read -> snapshot @ immutable
  val invariant : t @ read -> bool
  val read : t @ read -> int
end

module Box : BOX = struct
  type t = Value of int

  let make value : t @ unique =
    [%verocaml.requires value >= 0];
    Value value

  let model (_box : t @ read) : snapshot @ immutable =
    ({ value = 0 } : snapshot)
  [@@verocaml.spec]

  let invariant (box : t @ read) : bool =
    (model box).value >= 0
  [@@verocaml.type_invariant]

  let read (box : t @ read) =
    let Value value = box in
    let local_exec = Value value in
    [%verocaml.proof
      [%verocaml.use_type_invariant local_exec]];
    let[@tracked] local = (Value value [@tracked]) in
    [%verocaml.proof
      [%verocaml.use_type_invariant (local [@tracked])]];
    value
end
