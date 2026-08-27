type snapshot = { value : int }

module type CELL = sig
  type t
  val make : int -> t @ unique
  val model : t @ read -> snapshot @ immutable
  val invariant : t @ read -> bool
  val increment : t @ aliased -> unit
  val get : t @ read -> int
end

module Cell : CELL = struct
  type t = { mutable value : int }

  let make value : t @ unique =
    [%verocaml.requires 0 <= value];
    { value }

  let model (cell : t @ read) : snapshot @ immutable =
    ({ value = cell.value } : snapshot)
  [@@verocaml.spec]

  let invariant (cell : t @ read) : bool =
    (model cell).value >= 0
  [@@verocaml.type_invariant]

  let increment (cell : t @ aliased) : unit =
    [%verocaml.ensures fun _ -> (model cell).value = 0];
    cell.value <- 0

  let get (cell : t @ read) = cell.value
end

let branch_then_join flag : int =
  let cell = Cell.make 0 in
  if flag then Cell.increment cell;
  Cell.increment cell;
  Cell.get cell
