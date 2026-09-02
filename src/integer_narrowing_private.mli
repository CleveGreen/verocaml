type conversion_identity

type application_reference =
  | Missing_reference
  | Callable_reference of {
      callable_path : string;
      callable_uid : string;
    }
  | Authenticated_reference of {
      callable_path : string;
      callable_uid : string;
      canonical_material : string;
      checked_digest : string;
    }

type rejection =
  | Missing_identity
  | Malformed_identity
  | Forged_identity
  | Ambiguous_identity
  | Invalid_semantic_boundary

val schema : string

val issue_authenticated :
  provider_origin:string ->
  callable_path:string ->
  callable_uid:string ->
  logical_sort:Logical_sort_private.t ->
  issuer_authority:string ->
  (conversion_identity, string) result

val validate_application :
  candidates:conversion_identity list ->
  reference:application_reference ->
  source:Parametric_type.t ->
  target:Parametric_type.t ->
  (conversion_identity, rejection) result

val rejection_name : rejection -> string

module For_testing : sig
  val reference : conversion_identity -> application_reference

  val malformed_reference :
    conversion_identity -> application_reference

  val callable_reference : conversion_identity -> application_reference
end
