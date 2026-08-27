type snapshot = { value : int }
module type BOX = sig
  type t
  val make : int -> t @ unique
  val model : t @ read -> snapshot @ immutable
  val invariant : t @ read -> bool
  val read : (t [@finite]) @ read -> int
end
module Box : BOX = struct
  type node = Empty | Node of int * node
  type t = { value : int; chain : node }
  let make value : t @ unique =
    [%verocaml.requires value >= 0];
    { value; chain = Node (value, Empty) }
  let model (box : t @ read) : snapshot @ immutable = { value = box.value }
  [@@verocaml.spec]
  let invariant (box : t @ read) : bool = (model box).value >= 0
  [@@verocaml.type_invariant]
  let read (box : (t [@finite]) @ read) = box.value
end
let run value =
  [%verocaml.requires value >= 0];
  let box = Box.make value in
  let _ = Box.read box in
  ()
