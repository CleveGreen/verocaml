type ('opaque, 'exact) observation =
  | Opaque of 'opaque
  | Exact of 'exact
  | Conditional of
      Vir.boolean_term
      * ('opaque, 'exact) observation
      * ('opaque, 'exact) observation

val fold :
  opaque:('opaque -> 'result) ->
  exact:('exact -> 'result) ->
  conditional:(Vir.boolean_term -> 'result -> 'result -> 'result) ->
  ('opaque, 'exact) observation ->
  'result

val tag :
  Vir.aggregate_type ->
  Vir.aggregate_term ->
  ((Vir.aggregate_term, Vir.integer_term) observation, string) result

val integer_selector :
  Vir.selector ->
  Vir.aggregate_term ->
  ((Vir.aggregate_term, Vir.integer_term) observation, string) result

val boolean_selector :
  Vir.selector ->
  Vir.aggregate_term ->
  ((Vir.aggregate_term, Vir.boolean_term) observation, string) result

val bit_vector_selector :
  Bv_width.t ->
  Vir.selector ->
  Vir.aggregate_term ->
  ((Vir.aggregate_term, Vir.bit_vector_term) observation, string) result

val aggregate_selector :
  Vir.selector ->
  Vir.aggregate_term ->
  ((Vir.aggregate_term, Vir.aggregate_term) observation, string) result

val equal :
  Vir.aggregate_term ->
  Vir.aggregate_term ->
  ( (Vir.aggregate_term * Vir.aggregate_term, Vir.boolean_term) observation,
    string )
  result
