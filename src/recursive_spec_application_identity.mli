(** Private, process-local identities for reached aggregate recursive-Spec
    applications.  Values are deliberately opaque and are never serialized. *)
type t

val issue : unit -> t
val authenticate : t -> bool
val same : t -> t -> bool
val ordinal : t -> int

module For_testing : sig
  val forged : unit -> t
end
