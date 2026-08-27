type contribution

val contribution : Z3_bridge.counters -> contribution
val commit : contribution -> unit
val account_facade_delta :
  before:Z3_bridge.counters -> after:Z3_bridge.counters -> unit

val solver_creation_count : unit -> int
val reset_count : unit -> int
val reset_solver_creation_count : unit -> unit
