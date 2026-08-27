type kind = Exec | Spec | Recursive_spec | Proof | Other

val eligible :
  kind:kind ->
  recursive:bool ->
  type_variables:int list ->
  signature:Types.type_expr list ->
  bool
