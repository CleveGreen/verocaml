type resolved = Default_linear_z3
type error = Unsupported of Sst.verification_policy

let default = Default_linear_z3

let resolve = function
  | Sst.Default_linear_z3 -> Ok Default_linear_z3
  | (Sst.Default_linear_cvc5 | Sst.Nonlinear_z3) as policy ->
      Error (Unsupported policy)

let policy Default_linear_z3 = Sst.Default_linear_z3
let to_string Default_linear_z3 = "default-linear/default-z3"

let error_to_string (Unsupported policy) =
  let name =
    match policy with
    | Sst.Default_linear_z3 -> "default-linear/default-z3"
    | Sst.Default_linear_cvc5 -> "default-linear/default-cvc5"
    | Sst.Nonlinear_z3 -> "nonlinear/default-z3"
  in
  "unsupported verification policy " ^ name
