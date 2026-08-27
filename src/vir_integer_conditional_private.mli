val create :
  condition:Vir.boolean_term ->
  consequent:Vir.integer_term ->
  alternative:Vir.integer_term ->
  Vir.integer_term

(** Issue an integer conditional for an authenticated logical formula. *)
val create_formula :
  condition:Vir.boolean_term ->
  consequent:Vir.integer_term ->
  alternative:Vir.integer_term ->
  Vir.integer_term

val authenticate : Vir.integer_term -> bool
