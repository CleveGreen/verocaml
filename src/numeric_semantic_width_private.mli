type t = private {
  width : int;
  profile_full_key : string;
  target_full_key : string;
  evidence_full_key : string;
  full_key : string;
  checked_digest : string;
}

val issue :
  target:Build_target_profile_private.instance ->
  width:int ->
  evidence_full_key:string ->
  (t, string) result

val equal : t -> t -> bool
