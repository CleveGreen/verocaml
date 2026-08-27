type t

type identity = {
  abstract_type : Sst.type_id;
  hidden_type : Sst.type_id;
  field : Sst.field_id;
  model : Sst.function_id;
  invariant : Sst.function_id;
}

type hooks = {
  entry_eligibility_issued : unit -> unit;
  constructor_eligibility_issued : unit -> unit;
  closed_initialized : unit -> unit;
  opened : unit -> unit;
  updated : unit -> unit;
  closed : unit -> unit;
  effect_instantiated : unit -> unit;
  terminal_read : unit -> unit;
  torn_down : unit -> unit;
}

val create :
  hooks:hooks ->
  session_token:unit ref ->
  program_snapshot:string ->
  session_active:(unit -> bool) ->
  owner:Sst.function_id ->
  t

val issue_entry :
  t ->
  identity:identity ->
  formal:Sst.binding ->
  ordinal:int ->
  location:Vir.aggregate_term ->
  (unit, string) result

val issue_constructor :
  t ->
  identity:identity ->
  constructor:Sst.function_id ->
  call_span:Diagnostic.span ->
  location:Vir.aggregate_term ->
  (unit, string) result

val open_cell :
  t ->
  identity:identity ->
  operation:Sst.function_id ->
  location:Vir.aggregate_term ->
  entry_epoch:int ->
  (unit, string) result

val note_update :
  t ->
  operation:Sst.function_id ->
  transition:Sst.shared_scalar_heap_transition ->
  effective_predecessor_epoch:int ->
  effective_successor_epoch:int ->
  (unit, string) result

val close_cell :
  t ->
  operation:Sst.function_id ->
  final_epoch:int ->
  (unit, string) result

val note_effect_instantiation : t -> (unit, string) result

val note_terminal_read :
  t ->
  identity:identity ->
  operation:Sst.function_id ->
  location:Vir.aggregate_term ->
  epoch:int ->
  (unit, string) result

val destroy : t -> (unit, string) result
val destroy_if_live : t -> unit

module For_testing : sig
  val lifecycle_matrix : unit -> string list
end
