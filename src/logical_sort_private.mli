type role = Mathematical_int

type t = private {
  schema : string;
  role : role;
  provider_origin : string;
  type_path : string;
  type_uid : string;
  manifest_path : string;
  manifest_uid : string;
  integer_literal_path : string;
  integer_literal_uid : string;
  issuer : string;
}

val schema : string
val role_name : role -> string

val create :
  provider_origin:string ->
  type_path:string ->
  type_uid:string ->
  manifest_path:string ->
  manifest_uid:string ->
  integer_literal_path:string ->
  integer_literal_uid:string ->
  issuer:string ->
  (t, string) result

val canonical_material : t -> string
val origin_material : t -> string
val digest : t -> string
val compare : t -> t -> int
val equal : t -> t -> bool
val decode_canonical : string -> (t, string) result
