type kind = Declaration | Group
type trust = Proved | Trusted

type identity = {
  provider_origin : string;
  interface_digest : string;
  compiler_uid : string;
  kind : kind;
  canonical_path : string;
  dependency_receipt : string;
  witness_receipt : string;
  artifact_family : string;
  provider_route : string;
}

type interface_member = {
  identity : identity;
  source_members : source_reference list;
}

and source_reference = {
  member_slot : int;
  member_provider_origin : string;
  member_interface_receipt : string;
  member_dependency_receipt : string;
  member_compiler_uid : string;
  member_kind : kind;
  member_canonical_path : string;
}

val make_identity :
  provider_origin:string ->
  interface_digest:string ->
  compiler_uid:string ->
  kind:kind ->
  canonical_path:string ->
  dependency_receipt:string ->
  witness_receipt:string ->
  artifact_family:string ->
  provider_route:string ->
  (identity, string) result

val compare_identity : identity -> identity -> int
val equal_identity : identity -> identity -> bool
val identity_key : identity -> string
val correlation : identity -> string
val kind_name : kind -> string

val duplicate_identity : identity list -> identity option
val canonical_set : identity list -> (identity list, identity) result
