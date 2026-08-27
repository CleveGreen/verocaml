type lowered

val lower :
  ?allow_public_parametric_signatures:bool ->
  ?external_specifications:External_target_specification_private.environment ->
  imported:Imported_callable.environment ->
  Cmt_input.implementation ->
  (lowered, Diagnostic.t) result

val program : lowered -> Sst.program
val registration : lowered -> Imported_callable.registration
val external_registration :
  lowered -> External_target_specification_private.registration option

module For_testing : sig
  val reset_lowering_entries : unit -> unit
  val lowering_entries : unit -> int
end
