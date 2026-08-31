type t

val empty : t
val status : Outcome.status -> t -> t
val require_frontend_code : string -> t -> t
val require_semantic : function_name:string -> Outcome.semantic_kind -> t -> t
val require_unit : string -> Outcome.unit_disposition -> t -> t
val require_named_fact : string -> Outcome.named_fact -> t -> t
val require_process_fact : Outcome.process_fact -> t -> t

val functions_at_most :
  maximum:int -> baseline:int -> rationale:string -> t -> (t, string) result

val obligations_at_most :
  maximum:int -> baseline:int -> rationale:string -> t -> (t, string) result

val check : t -> Outcome.t -> (unit, string) result
val describe : t -> string
