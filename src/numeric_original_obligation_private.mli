type t = private {
  consumer_artifact_full_key : string;
  dependency_artifact_full_keys : string list;
  function_origin_full_key : string;
  obligation : Vir.obligation;
  full_key : string;
  checked_digest : string;
}

val equal : t -> t -> bool
val matches_consumer : t -> Cmt_input.implementation -> bool

(** Coordinator-only capture before solver preparation or target specialization.
    These receipts identify obligations; they establish no proof result. *)
module For_pipeline : sig
  val capture :
    implementation:Cmt_input.implementation ->
    imported:Imported_callable.environment option ->
    program:Sst.program -> definition:Sst.function_definition ->
    Vir.function_execution -> t list
end
