type t
type semantic_authority = Checked_proof | Explicit_axiom
type trusted_dependency = private {
  compiler_identity : string;
  evidence_full_key : string;
}

(** Registry-owned issuance after it has correlated two actual admitted
    descriptors.  This module is deliberately lower than the registry/source
    layers so the sealed value can cross that boundary without a dependency
    cycle. *)
module For_registry : sig
  val issue :
    target:Build_target_profile_private.instance ->
    issuer_unit:string ->
    outer_callable_uid:string ->
    modular_callable_uid:string ->
    outer_callable_abi:string ->
    modular_callable_abi:string ->
    outer_law_full_key:string ->
    modular_law_full_key:string ->
    carrier_binding_full_key:string ->
    base_int_full_key:string ->
    semantic_width:int ->
    outer_authority:semantic_authority ->
    modular_authority:semantic_authority ->
    trusted_dependencies:(string * string) list ->
    t
end

val validate : target:Build_target_profile_private.instance -> t -> (unit, string) result
val issuer_unit : t -> string
val outer_callable_uid : t -> string
val modular_callable_uid : t -> string
val outer_callable_abi : t -> string
val modular_callable_abi : t -> string
val outer_law_full_key : t -> string
val modular_law_full_key : t -> string
val carrier_binding_full_key : t -> string
val base_int_full_key : t -> string
val semantic_width : t -> int
val outer_authority : t -> semantic_authority
val modular_authority : t -> semantic_authority
val trusted_dependencies : t -> trusted_dependency list
val full_key : t -> string
