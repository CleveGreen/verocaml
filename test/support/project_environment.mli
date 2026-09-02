type expected = {
  source_commit : string;
  context : string;
  package_root : string;
  binary_root : string;
  dune_path : string;
  tool_path : string;
  library_set_digest : string;
}

type t

val validate : expected:expected -> string -> (t, Failure.t) result
val source_commit : t -> string
val context : t -> string
val package_root : t -> string
val binary_root : t -> string
val dune_path : t -> string
val tool_path : t -> string
val ocaml_path : t -> string
val library_set_digest : t -> string
val library_names : t -> string list

val write :
  path:string ->
  source_commit:string ->
  context:string ->
  package_root:string ->
  binary_root:string ->
  dune_path:string ->
  tool_path:string ->
  libraries:(string * string) list ->
  string
