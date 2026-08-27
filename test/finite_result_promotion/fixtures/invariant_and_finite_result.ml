type node = Empty | Node of int * node
type snapshot = { value : int }

module type BOX = sig
  type t
  val make : int -> t @ unique
  val model : t @ read -> snapshot @ immutable
  val invariant : t @ read -> bool
  val read : t @ read -> int
end

module Box : BOX = struct
  type t = { top : node; value : int }

  let make value : t @ unique =
    [%verocaml.requires value >= 0];
    { top = Node (value, Empty); value }

  let model (box : t @ read) : snapshot @ immutable =
    ({ value = box.value } : snapshot)
  [@@verocaml.spec]

  let invariant (box : t @ read) = (model box).value >= 0
  [@@verocaml.type_invariant]

  let read (box : t @ read) = box.value
end

let finite_consume (_box : Box.t [@finite]) : unit = ()
[@@verocaml.proof]

let run value =
  [%verocaml.requires value >= 0];
  let box = Box.make value in
  [%verocaml.proof finite_consume box];
  box
