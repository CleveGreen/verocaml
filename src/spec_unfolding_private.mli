type error
type prepared
type definition

type activation = Spec_unfolding.activation = {
  function_id : Sst.function_id;
  depth : int;
  span : Diagnostic.span;
}

val prepare : Sst.program -> (prepared, error) result
val prepare_validated :
  Sst_validation.validated_program -> (prepared, error) result
val validated_program : prepared -> Sst_validation.validated_program
val termination_plan : prepared -> Termination.plan
val termination_obligations : prepared -> Vir.obligation list
val definitions : prepared -> definition list
val proof_entry_activations :
  prepared -> Sst.function_id -> activation list

val definition_id : definition -> Sst.function_id
val definition_stable_id : definition -> string
val definition_parameters : definition -> Sst.parameter list
val definition_result_type : definition -> Sst.typ
val definition_body : definition -> Sst.expression
val definition_visibility : definition -> [ `Opaque | `Revealed ]
val definition_span : definition -> Diagnostic.span
val definition_types : definition -> Sst.type_definition list
val definition_parametric_adts : definition -> Parametric_adt.t list
val definition_program : definition -> Sst.program

module For_testing : sig
  val recursive_lowering_count : unit -> int
  val reset_recursive_lowering_count : unit -> unit
end

val error_to_string : error -> string
