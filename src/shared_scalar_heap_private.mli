type t

type hooks = {
  issued : unit -> unit;
  write_authenticated : unit -> unit;
  read_logged : unit -> unit;
  epoch_advanced : unit -> unit;
  torn_down : unit -> unit;
}

type read_view = Entry_view | Current_view | Epoch_view of int

type read_event = {
  read_field : Sst.field_id;
  read_path_id : int;
  read_epoch : int;
  read_location : Vir.aggregate_term;
  read_term : Vir.integer_term;
  read_entry_view : bool;
}

type write_event = {
  write_transition : Sst.shared_scalar_heap_transition;
  write_location : Vir.aggregate_term;
  write_value : Vir.integer_term;
}

val create :
  hooks:hooks ->
  session_token:unit ref ->
  program_snapshot:string ->
  session_active:(unit -> bool) ->
  Sst.shared_scalar_heap_transition ->
  (t, string) result

val read :
  t ->
  view:read_view ->
  field:Sst.field_id ->
  location:Vir.aggregate_term ->
  entry:Vir.integer_term ->
  (Vir.integer_term, string) result

val accepts_read :
  t -> field:Sst.field_id -> location:Vir.aggregate_term -> bool

val write :
  t ->
  transition:Sst.shared_scalar_heap_transition ->
  location:Vir.aggregate_term ->
  value:Vir.integer_term ->
  (t, string) result

val write_rebased :
  t ->
  base_epoch:int ->
  transition:Sst.shared_scalar_heap_transition ->
  location:Vir.aggregate_term ->
  value:Vir.integer_term ->
  (t, string) result

val current_epoch : t -> int
val fork : t -> (t, string) result
val read_events : t -> read_event list
val write_events : t -> write_event list
val destroy : t -> (unit, string) result
val destroy_if_live : t -> unit

module For_testing : sig
  val lifecycle_matrix : unit -> string list
end
