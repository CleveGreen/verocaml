(** Installs VC-local logical ADT schemas into one Logic IR builder. *)

type t
type sort_registry

val create_sort_registry :
  builder:Logic_ir.builder ->
  descriptors:Parametric_adt.t list ->
  parametric_sort:(Parametric_type.binder -> Logic_ir.sort) ->
  span:Diagnostic.span ->
  sort_registry

val sort_for_sst : sort_registry -> Sst.typ -> Logic_ir.sort
val sort_for_aggregate : sort_registry -> Vir.aggregate_type -> Logic_ir.sort
val sst_type_of_aggregate :
  Parametric_adt.t list -> Vir.aggregate_type -> Sst.typ option
val parametric_sort : sort_registry -> Parametric_type.binder -> Logic_ir.sort

val aggregate_type :
  Logical_adt_schema_private.t -> Vir.aggregate_type

val declare :
  builder:Logic_ir.builder ->
  schemas:Logical_adt_schema_private.t list ->
  aggregate_sort:(Vir.aggregate_type -> Logic_ir.sort) ->
  parametric_sort:(Parametric_type.binder -> Logic_ir.sort) ->
  span:Diagnostic.span ->
  (t, string) result

val authenticates_application :
  descriptors:Parametric_adt.t list -> Sst.typ -> bool

val declare_for_types :
  registry:sort_registry ->
  descriptors:Parametric_adt.t list ->
  types:Sst.typ list ->
  additional_schemas:Logical_adt_schema_private.t list ->
  aggregate_types:Vir.aggregate_type list ->
  (t option, string) result

val sort : t -> Vir.aggregate_type -> Logic_ir.sort option
val constructor :
  t ->
  Vir.aggregate_type ->
  Sst.constructor_id ->
  Logic_ir.function_symbol option
val recognizer :
  t ->
  Vir.aggregate_type ->
  Sst.constructor_id ->
  Logic_ir.function_symbol option
val constructor_at :
  t -> Vir.aggregate_type -> int -> Logic_ir.function_symbol option
val recognizers :
  t ->
  Vir.aggregate_type ->
  (int * Logic_ir.function_symbol) list option
val selector : t -> Vir.selector -> Logic_ir.function_symbol option

type selector_resolution =
  | Declared_selector of Logic_ir.function_symbol
  | Invalid_declared_selector
  | Unavailable_selector_schema

val resolve_selector : t -> Vir.selector -> selector_resolution

type record_schema_validation =
  | Validated_record_schema
  | Unavailable_record_schema

val validate_record_fields :
  schemas:Logical_adt_schema_private.t list ->
  aggregate_type:Vir.aggregate_type ->
  record_type:Sst.type_id ->
  fields:(Sst.field_id * Vir.recursive_spec_argument) list ->
  (record_schema_validation, string) result

val schemas : Vir.obligation -> Logical_adt_schema_private.t list
val contains : Logical_adt_schema_private.t list -> Vir.aggregate_type -> bool

val attach_schemas :
  descriptors:Parametric_adt.t list ->
  Vir.obligation ->
  (Vir.obligation, string) result

val aggregate_type_of_sst :
  Parametric_adt.t list -> Sst.typ -> Vir.aggregate_type option

val vir_sort_of_sst : Parametric_adt.t list -> Sst.typ -> Vir.sort option

val recognizer_for_sst :
  t ->
  parametric_adts:Parametric_adt.t list ->
  owner:Sst.typ ->
  Sst.constructor_id ->
  Logic_ir.function_symbol option

val selector_for_constructor :
  t ->
  parametric_adts:Parametric_adt.t list ->
  owner:Sst.typ ->
  constructor:Sst.constructor_id ->
  index:int ->
  field_name:string ->
  field_type:Sst.typ ->
  Logic_ir.function_symbol option

type 'error routing

val routing :
  bindings:t option ->
  span:Diagnostic.span ->
  aggregate_sort:(Vir.aggregate_type -> Logic_ir.sort) ->
  error:(string -> 'error) ->
  fallback:
    (string ->
    Logic_ir.sort list ->
    Logic_ir.sort ->
    Diagnostic.span ->
    (Logic_ir.function_symbol, 'error) result) ->
  'error routing

val routed_selector :
  'error routing ->
  Vir.selector ->
  Logic_ir.sort ->
  Logic_ir.sort ->
  (Logic_ir.function_symbol, 'error) result

val routed_constructor :
  'error routing ->
  Vir.aggregate_type ->
  Sst.constructor_id ->
  Logic_ir.sort list ->
  (Logic_ir.function_symbol, 'error) result

val routed_record_constructor :
  'error routing ->
  Vir.aggregate_type ->
  Sst.type_id ->
  Logic_ir.sort list ->
  (Logic_ir.function_symbol, 'error) result

val routed_tag :
  'error routing ->
  Vir.aggregate_type ->
  Logic_ir.term ->
  (Logic_ir.term, 'error) result
