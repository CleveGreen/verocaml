type profile = private {
  profile_claim : Target_profile_private.claim;
  build_declaration_full_key : string;
  full_key : string;
  checked_digest : string;
}

type instance = private {
  profile_full_key : string;
  profile_checked_digest : string;
  target_claim : Target_profile_private.instance_claim;
  full_key : string;
  checked_digest : string;
}

type capability

(** Returns the one capability embedded by this build.  It accepts no provider,
    source, host-runtime, or serialized profile facts. *)
val capability : unit -> capability

val authenticate_profile : capability -> (profile, string) result
val authenticate_instances : capability -> (instance list, string) result
val logical_bv_width_permitted : profile -> Z.t -> bool
val instance_logical_bv_width_permitted : instance -> Z.t -> bool

val decode_profile :
  capability -> string -> (profile, string) result

val decode_instance :
  capability -> string -> (instance, string) result

val decode_profile_for_worker :
  string -> (profile, string) result @@ portable

val decode_instance_for_worker :
  string -> (instance, string) result @@ portable

val instance_logical_bv_width_permitted_for_worker :
  instance -> int -> bool @@ portable

val compare_profile : profile -> profile -> int
val equal_profile : profile -> profile -> bool
val compare_instance : instance -> instance -> int
val equal_instance : instance -> instance -> bool
