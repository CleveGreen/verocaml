val infer_type_arguments :
  binders:Parametric_type.binder list ->
  formals:Parametric_type.t list ->
  actuals:Parametric_type.t list ->
  formal_result:Parametric_type.t ->
  actual_result:Parametric_type.t ->
  (Parametric_type.t list, string) result

val infer_type_arguments_from_actuals :
  binders:Parametric_type.binder list ->
  formals:Parametric_type.t list ->
  actuals:Parametric_type.t list ->
  (Parametric_type.t list, string) result

val instantiate :
  binders:Parametric_type.binder list ->
  arguments:Parametric_type.t list ->
  Parametric_type.t ->
  (Parametric_type.t, string) result

type source_type_error =
  | Polymorphic_source_type
  | Higher_order_source_type
  | Unsupported_source_type

val source_type_variable_ids : Types.type_expr -> int list
val first_order_source_type : Types.type_expr -> bool

val contains_source_application :
  is_application:(Path.t -> bool) -> Types.type_expr -> bool

val variables_under_source_application :
  is_application:(Path.t -> bool) -> Types.type_expr -> int list

val lower_source_type :
  substitutions:(int * Types.type_expr) list ->
  binders:(int * Parametric_type.binder) list ->
  ?arrow:
    (Types.arg_label ->
    Parametric_type.t ->
    Parametric_type.t ->
    (Parametric_type.t, source_type_error) result) ->
  application:
    (Path.t ->
    Types.type_expr list ->
    (Parametric_type.t, source_type_error) result) ->
  Types.type_expr ->
  (Parametric_type.t, source_type_error) result

val formal_label : Typedtree.arg_label -> string option

val lower_typedtree_formals :
  lower:(Location.t -> Types.type_expr -> (Parametric_type.t, 'error) result) ->
  optional_carrier:
    (Location.t -> Parametric_type.t -> (Parametric_type.t, 'error) result) ->
  Typedtree.function_param list ->
  (Parametric_type.t list * string option list, 'error) result

val infer_labeled_type_arguments :
  binders:Parametric_type.binder list ->
  formal_types:Parametric_type.t list ->
  formal_labels:string option list ->
  actual_types:Parametric_type.t list ->
  actual_labels:string option list ->
  formal_result:Parametric_type.t ->
  actual_result:Parametric_type.t ->
  (Parametric_type.t list, string) result

val infer_labeled_type_arguments_from_actuals :
  binders:Parametric_type.binder list ->
  formal_types:Parametric_type.t list ->
  formal_labels:string option list ->
  actual_types:Parametric_type.t list ->
  actual_labels:string option list ->
  (Parametric_type.t list, string) result

type call_argument = {
  argument_label : string option;
  argument_type : Parametric_type.t;
}

type call_error = {
  argument_index : int option;
  message : string;
}

val validate_direct_call :
  binders:Parametric_type.binder list ->
  type_arguments:Parametric_type.t list ->
  formal_result:Parametric_type.t ->
  actual_result:Parametric_type.t ->
  formals:call_argument list ->
  actuals:call_argument list ->
  (unit, call_error) result

val validate_sst_optional :
  descriptors:Parametric_adt.t list ->
  Sst.expression ->
  (Sst.expression option, string) result

val validate_sst_direct_call :
  definition:Sst.function_definition ->
  type_arguments:Sst.typ list ->
  actual_result:Sst.typ ->
  call_span:Diagnostic.span ->
  arguments:(string option * Sst.expression) list ->
  (unit, Diagnostic.span * string) result
