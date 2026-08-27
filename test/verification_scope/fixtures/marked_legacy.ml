[@@@verocaml.verify]

module F (X : sig
  val value : int
end) = struct
  let value = X.value
end

exception Legacy of int

let run value =
  try raise (Legacy value) with
  | Legacy result -> result
