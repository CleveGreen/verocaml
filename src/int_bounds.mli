val expected_int_size : int
val minimum : Z.t
val maximum : Z.t

val check_target : ?int_size:int -> unit -> (unit, Diagnostic.t) result
