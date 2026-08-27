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
    cell.value <- 0

  let get (cell : t @ read) = cell.value
end

let rec reentrant_client count (cell : Cell.t @ aliased) : unit =
  if count <= 0 then ()
  else (
    Cell.increment cell;
    reentrant_client (count - 1) cell)
