type t

val of_string :
  profile:Build_target_profile_private.profile ->
  Bv_backend_capability_receipt_private.capability ->
  string ->
  (t, string) result

val of_z :
  profile:Build_target_profile_private.profile ->
  Bv_backend_capability_receipt_private.capability ->
  Z.t ->
  (t, string) result

val for_instance :
  Bv_backend_capability_receipt_private.capability ->
  Build_target_profile_private.instance ->
  t ->
  (t, string) result

val to_int : t -> int @@ portable
val to_z : t -> Z.t
val to_string : t -> string
val profile_full_key : t -> string
val target_full_key : t -> string option
val structural_identity_material : t -> string
val compare : t -> t -> int
val equal : t -> t -> bool
val authenticate_bound :
  Bv_backend_capability_receipt_private.capability -> t -> (unit, string) result
val encode_reference :
  Bv_backend_capability_receipt_private.capability ->
  t ->
  (string, string) result
val decode_reference :
  Bv_backend_capability_receipt_private.capability ->
  string ->
  (t, string) result
val decode_reference_for_worker : string -> (t, string) result @@ portable

module For_testing : sig
  val decode_reference_fields : string -> (string list, string) result
end
