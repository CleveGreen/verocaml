(** Pure relevance classification over already-issued immutable aggregate facts. *)

val is_exact_aggregate_construction_equality : Vir.boolean_term -> bool
val goal_has_aggregate_equality : Vir.boolean_term -> bool

val facts_relevant_to_terms :
  Vir.boolean_term list -> Vir.boolean_term list -> bool
