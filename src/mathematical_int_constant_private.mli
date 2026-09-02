type multiplication =
  | Constant_pair of Z.t * Z.t
  | Constant_left of Z.t
  | Constant_right of Z.t
  | Nonlinear

val parse_literal : string -> Z.t option

val classify_multiplication :
  coefficient:('expression -> Z.t option) ->
  left:'expression ->
  right:'expression ->
  multiplication
