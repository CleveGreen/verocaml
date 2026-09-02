val logical_context : Sst.verification_mode option -> ghost_depth:int -> bool

val source_arrow :
  lower:(Types.type_expr -> (Parametric_type.t, 'error) result) ->
  Types.type_expr ->
  (Parametric_type.t, 'error) result option

val captures_live_binding :
  ?allow:(Sst.binding -> bool) ->
  (Ident.t * Sst.binding) list ->
  Typedtree.expression ->
  bool

val function_bindings :
  (Ident.t * Sst.binding) list -> (Ident.t * Sst.binding) list

val residual_type :
  Sst.typ -> (Typedtree.arg_label * Typedtree.apply_arg) list -> Sst.typ option

val admissible_capture_type :
  aggregate:(Parametric_type.type_id -> bool) -> Sst.typ -> bool

type 'error lambda_services = {
  pattern_type : Typedtree.pattern -> (Sst.typ, 'error) result;
  lower_pattern :
    expected:Sst.typ ->
    (Ident.t * Sst.binding) list ->
    Typedtree.pattern ->
    (Sst.pattern * (Ident.t * Sst.binding) list, 'error) result;
  lower_expression :
    (Ident.t * Sst.binding) list ->
    Typedtree.expression ->
    (Sst.expression, 'error) result;
  adapt_result :
    Location.t ->
    expected:Sst.typ ->
    Sst.expression ->
    (Sst.expression, 'error) result;
  admit_capture : Sst.binding -> bool;
  parameter_label : Typedtree.arg_label -> string option;
  span : Location.t -> Diagnostic.span;
  higher_order_error : Location.t -> 'error;
}

val lower_lambda :
  'error lambda_services ->
  source_file:string ->
  bindings:(Ident.t * Sst.binding) list ->
  arrow:Sst.typ ->
  location:Location.t ->
  parameters:Typedtree.function_param list ->
  body:Typedtree.expression ->
  (Sst.expression, 'error) result

type 'error application_services = {
  normalize : Typedtree.expression -> (Sst.typ, 'error) result;
  lower : Typedtree.expression -> (Sst.expression, 'error) result;
  adapt_argument :
    Location.t ->
    expected:Sst.typ ->
    Sst.expression ->
    (Sst.expression, 'error) result;
  parameter_label : Typedtree.arg_label -> string option;
  span : Location.t -> Diagnostic.span;
  higher_order_error : Location.t -> 'error;
}

val apply_lowered :
  'error application_services ->
  application:Typedtree.expression ->
  result_type:Sst.typ ->
  function_:Sst.expression ->
  arguments:(Typedtree.arg_label * Typedtree.apply_arg) list ->
  (Sst.expression, 'error) result

val lower_application :
  'error application_services ->
  enabled:bool ->
  application:Typedtree.expression ->
  result_type:Sst.typ ->
  callee:Typedtree.expression ->
  arguments:(Typedtree.arg_label * Typedtree.apply_arg) list ->
  (Sst.expression, 'error) result option

val lower_symbolic_application :
  invalid:(string -> 'error) ->
  adapt_argument:
    (expected:Sst.typ ->
    Sst.expression ->
    (Sst.expression, 'error) result) ->
  span:Diagnostic.span ->
  declaration:Symbolic_application_private.declaration ->
  result_type:Sst.typ ->
  arguments:(string option * Sst.expression) list ->
  (Sst.expression, 'error) result

type 'error expression_type_services = {
  fallback : Typedtree.expression -> (Sst.typ, 'error) result;
  binding : Ident.t -> Sst.typ option;
  symbolic :
    Typedtree.expression ->
    (Typedtree.arg_label * Typedtree.apply_arg) list ->
    (Symbolic_application_private.declaration
    * (Typedtree.arg_label * Typedtree.apply_arg) list)
    option
    option;
  parameter_label : Typedtree.arg_label -> string option;
  polymorphic_error : Typedtree.expression -> 'error;
  higher_order_error : Typedtree.expression -> 'error;
}

val expression_type :
  'error expression_type_services ->
  Typedtree.expression ->
  (Sst.typ, 'error) result

type call = {
  function_id : Sst.function_id;
  type_binders : Parametric_type.binder list;
  formal_types : Sst.typ list;
  formal_labels : string option list;
  formal_result : Sst.typ;
  recursive : bool;
}

type semantic_signature = {
  formal_types : Sst.typ list;
  formal_labels : string option list;
  formal_result : Sst.typ;
}

val direct_call :
  invalid:(string -> 'error) ->
  adapt_argument:
    (expected:Sst.typ ->
    Sst.expression ->
    (Sst.expression, 'error) result) ->
  span:Diagnostic.span ->
  result_type:Sst.typ ->
  actuals:(string option * Sst.expression) list ->
  call ->
  (Sst.expression, 'error) result

val reference :
  invalid:(string -> 'error) ->
  span:Diagnostic.span ->
  result_type:Sst.typ ->
  function_id:Sst.function_id ->
  type_binders:Parametric_type.binder list ->
  formal_types:Sst.typ list ->
  formal_labels:string option list ->
  recursive:bool ->
  (Sst.expression, 'error) result

type preclassification_services = {
  logical : bool;
  normalize : Typedtree.expression -> Path.t -> Path.t;
  symbolic : Path.t -> bool;
  resolves_to : Path.t -> string -> string -> bool;
  known : Path.t -> string -> bool;
  local : Ident.t -> bool;
  callback : Ident.t -> bool;
  uid : Types.value_description -> string;
}

val symbolic_application_head :
  preclassification_services ->
  Typedtree.expression ->
  (Typedtree.arg_label * Typedtree.apply_arg) list ->
  (Path.t
  * Longident.t Location.loc
  * Types.value_description
  * (Typedtree.arg_label * Typedtree.apply_arg) list)
  option

val preclassify_application :
  preclassification_services ->
  Typedtree.expression ->
  Diagnostic.unsupported_construct option

val exact_callback_callee :
  find:(Ident.t -> 'a option) -> Typedtree.expression -> bool

val source_signature :
  ?semantic_signature:semantic_signature ->
  ?explicit_parameter_type:
    (Typedtree.pattern -> (Sst.typ option, 'error) result) ->
  ?explicit_result_type:
    (Typedtree.expression -> (Sst.typ option, 'error) result) ->
  lower:(Location.t -> Types.type_expr -> (Sst.typ, 'error) result) ->
  optional_carrier:
    (Location.t -> Sst.typ -> Sst.typ -> (Sst.typ, 'error) result) ->
  parameter_error:(Location.t -> string -> 'error) ->
  Typedtree.expression ->
  (Sst.typ list * string option list * Sst.typ, 'error) result option

val first_class_surface :
  signature:Types.type_expr list -> Typedtree.expression -> bool

val supported_first_class_surface :
  recursive:bool ->
  signature:Types.type_expr list ->
  Typedtree.expression ->
  bool

val classify_callee :
  normalize:(Typedtree.expression -> Path.t -> Path.t) ->
  candidates:(Path.t -> 'candidate list) ->
  kind:('candidate -> [ `Spec | `Recursive of bool | `Other ]) ->
  id:('candidate -> Sst.function_id) ->
  arity:('candidate -> int) ->
  first_class:('candidate -> bool) ->
  current:Sst.function_id option ->
  local:(Ident.t -> bool) ->
  argument_count:int ->
  result_type:Sst.typ ->
  Typedtree.expression ->
  bool * 'candidate option
