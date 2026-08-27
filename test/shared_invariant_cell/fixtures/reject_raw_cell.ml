type snapshot = { value : int }

module Cell = struct
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

let raw_client (cell : Cell.t @ aliased) =
  Cell.increment cell;
  Cell.get cell
