type insertion = private {
  inserted : Broadcast_vc_private.inserted;
  theorem : Sst.function_definition option;
}

type obligation = private {
  materialized : Vir.obligation;
  base : Vir.obligation option;
  broadcasts : insertion list option;
}

type function_evidence = private {
  definition : Sst.function_definition option;
  execution : Vir.function_execution;
  obligations : obligation list;
}

type t

val functions : t -> function_evidence list
val matches_program : t -> Sst.program -> bool

(** Capture coordinator-owned evidence while the verification session's theorem
    registry is live. This alone confers no successful-verification authority;
    consumers must obtain it from the private driver's completion gate. *)
module For_pipeline : sig
  val capture : program:Sst.program -> Vir.program -> t
end
