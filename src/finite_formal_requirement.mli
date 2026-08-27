(** Private authentication of erasable finite-formal signature metadata.

    Descriptors state obligations only.  They cannot issue finite-value
    authority and never cross a verification session. *)

type environment
type descriptor

val prepare :
  structure:Typedtree.structure -> program:Sst.program -> unit

val seal :
  Cmt_input.implementation -> Sst.program -> (unit, string) result

val requires_authentication : Cmt_input.implementation -> bool

val validate : Sst.program -> (environment, string) result

val find :
  environment -> Sst.function_id -> ordinal:int -> descriptor option

val ordinal : descriptor -> int
val requirement_digest : descriptor -> string
val dump : environment -> string

val authenticate_constrained_signature :
  implementation:Parsetree.attributes ->
  interface:Parsetree.attributes ->
  (unit, string) result
