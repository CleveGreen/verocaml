type t
type error

val create :
  timeout_ms:int ->
  rlimit:int ->
  (t, error) result

val create_default :
  timeout_ms:int ->
  (t, error) result

val timeout_ms : t -> int
val rlimit : t -> int
val default_rlimit : int
val error_to_string : error -> string
