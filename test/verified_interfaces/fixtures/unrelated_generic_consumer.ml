open Unrelated_generic_dependency

let structural_nonmodel_generic (value : int box) =
  match value with
  | Box integer -> integer
