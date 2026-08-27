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
    [%verocaml.requires
      (model cell).value < 4_611_686_018_427_387_903];
    [%verocaml.ensures fun _ ->
      (model cell).value = [%verocaml.old (model cell).value] + 1];
    let before = cell.value in
    cell.value <- -1;
    cell.value <- before + 1

  let get (cell : t @ read) = cell.value
end


type unique_box = { mutable unique_value : int }
type shared_box = { mutable shared_value : int }

let update_unique (cell : unique_box @ unique) : unique_box @ unique =
  cell.unique_value <- 9;
  cell

let update_shared (cell : shared_box @ aliased) : unit =
  cell.shared_value <- 7
