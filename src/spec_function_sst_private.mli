(** Portable descriptors encoded through reserved, authenticated SST calls. *)

type lambda = {
  lambda_site : string;
  lambda_arrow : Sst.typ;
  lambda_parameter : Sst.binding;
  lambda_body : Sst.expression;
  lambda_captures : Sst.binding list;
}

type application = {
  application_arrow : Sst.typ;
  application_function : Sst.expression;
  application_argument : Sst.expression;
  application_label : string option;
  application_result : Sst.typ;
}

val validate_lambda : lambda -> (unit, string) result
val capture_ids : lambda -> int list
val synthetic_name : site:string -> string
val is_synthetic_name : string -> bool
val is_bare_carrier : Sst.function_definition -> bool
val make_lambda : lambda -> (Sst.expression, string) result
val lambda : Sst.expression -> lambda option

val make_application :
  arrow:Sst.typ ->
  function_:Sst.expression ->
  argument:Sst.expression ->
  label:string option ->
  span:Sst.span ->
  (Sst.expression, string) result

val application : Sst.expression -> application option
val is_application_id : Sst.function_id -> bool
val application_has_lambda_head : Sst.expression -> bool
val is_reference : Sst.expression -> bool
val semantic_children : Sst.expression -> Sst.expression list option

val validate_instance_lambda :
  issue_pattern:(Sst.pattern -> (unit, 'error) result) ->
  issue_expression:(Sst.expression -> (unit, 'error) result) ->
  Sst.expression ->
  (unit, 'error) result option

val exact_owned_contents_construction :
  owned_tree_prerequisite:(Sst.typ -> Sst.owned_tree_prerequisite option) ->
  Sst.owned_tree_transition ->
  Sst.expression ->
  bool

val authenticate_rank1_recursion :
  descriptor:Parametric_adt.t ->
  Sst.function_definition ->
  Sst.expression ->
  (unit, string) result

type ('bound, 'error) validation_services = {
  recurse : 'bound -> Sst.expression -> ('bound, 'error) result;
  add_binding : 'bound -> Sst.binding -> ('bound, 'error) result;
  validate_type : Sst.typ -> (unit, 'error) result;
  find_definition : Sst.function_id -> Sst.function_definition option;
  invalid : Diagnostic.span -> string -> 'error;
}

val validate_special :
  ('bound, 'error) validation_services ->
  'bound ->
  Sst.expression ->
  ('bound, 'error) result option

type 'error logical_validation_services = {
  recurse : Sst.expression -> (unit, 'error) result;
  position : Sst.function_id -> int option;
  current_position : int option;
  invalid : Diagnostic.span -> string -> 'error;
}

val validate_logical_special :
  'error logical_validation_services ->
  Sst.expression ->
  (unit, 'error) result option
