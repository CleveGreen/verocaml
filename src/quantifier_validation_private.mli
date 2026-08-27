type 'error services = {
  bind :
    Typedtree.pattern ->
    ((Ident.t * Sst.binding), 'error) result;
  lower :
    Ident.t ->
    Sst.binding ->
    Typedtree.expression ->
    (Sst.expression, 'error) result;
  admit_type : Sst.typ -> bool;
  nested_quantifier : Typedtree.expression -> bool;
  owner : string;
  shadowed : Typedtree_logical_builtin_private.kind -> bool;
  span : Location.t -> Diagnostic.span;
  authentication_error : Location.t -> string -> 'error;
  trigger_error : Location.t -> string -> 'error;
  type_error : Location.t -> string -> 'error;
}

val lower :
  'error services ->
  kind:Typedtree_logical_builtin_private.kind ->
  source:Typedtree.expression ->
  quantifier:Typedtree.expression ->
  (Sst.expression, 'error) result

val validate_sst :
  ?allow_unclassified:bool ->
  ?expected_owner:string ->
  Logic_quantifier_private.kind ->
  Sst.quantifier ->
  (unit, string) result

val validate_sst_expression :
  ?allow_unclassified:bool ->
  ?expected_owner:string ->
  Sst.expression ->
  (unit, string) result

val validate_sst_for_function :
  Sst.function_id -> Sst.expression -> (unit, string) result

type ('mode, 'error) instance_mode_services = {
  issue_binder : identity:string -> Sst.binding -> unit;
  issue_ghost_expression : Sst.expression -> ('mode, 'error) result;
  exact_ghost :
    'mode -> Sst.expression -> string -> (unit, 'error) result;
}

val validate_instance_mode :
  ('mode, 'error) instance_mode_services ->
  function_index:int ->
  expression:Sst.expression ->
  mode:'mode ->
  Sst.quantifier ->
  (unit, 'error) result

type ('result, 'error) quantifier_value_services =
  ('result, 'error)
  Spec_function_type_private.quantifier_value_services

type 'term quantifier_body_services =
  'term
  Spec_function_type_private.quantifier_body_services
val quantifier_value :
  ('result, 'error) quantifier_value_services ->
  Sst.binding ->
  ('result, 'error) result

val quantifier_body :
  'term quantifier_body_services ->
  Logic_quantifier_private.kind ->
  Sst.binding ->
  'term ->
  'term

type 'argument explicit_trigger =
  | Spec_apply_trigger of {
      arrow : Sst.typ;
      arguments : 'argument list;
      result_type : Sst.typ;
      span : Diagnostic.span;
    }
  | Direct_trigger of {
      recursive : bool;
      callee : Sst.function_id;
      type_arguments : Sst.typ list;
      arguments : 'argument list;
      span : Diagnostic.span;
    }
  | Callback_requires_trigger of {
      application : Sst.callback_application;
      arguments : 'argument list;
    }
  | Callback_ensures_trigger of {
      application : Sst.callback_application;
      arguments : 'argument list;
      result : 'argument;
    }

type ('context, 'argument, 'term, 'state, 'error) trigger_services = {
  evaluate_argument :
    'context ->
    Diagnostic.span ->
    Sst.expression ->
    'state ->
    ('argument * 'state, 'error) result;
  recursive : 'context -> Sst.function_id -> bool;
  construct_trigger : 'argument explicit_trigger -> 'term;
  malformed_trigger : 'context -> Diagnostic.span -> string -> 'error;
}

val explicit_trigger :
  ('context, 'argument, 'term, 'state, 'error) trigger_services ->
  'context ->
  Sst.expression ->
  'state ->
  ('term * 'state, 'error) result

type ('term, 'state, 'error) quantifier_expression_services = {
  branches :
    Sst.expression ->
    'state ->
    (('term * 'state) list option, 'error) result;
  merge_branches : 'term -> 'term -> 'term;
  state_rank : 'state -> int;
  malformed_expression : Diagnostic.span -> string -> 'error;
}

val quantifier_expression :
  ('term, 'state, 'error) quantifier_expression_services ->
  Sst.expression ->
  'state ->
  ('term * 'state, 'error) result

type ('scope, 'error) scoped_sst_services = {
  admit_application : Sst.typ -> bool;
  validate_type_reference : Sst.binding -> (unit, 'error) result;
  add_binding : 'scope -> Sst.binding -> ('scope, 'error) result;
  validate_expression :
    'scope -> Sst.expression -> ('scope, 'error) result;
  malformed_sst : Diagnostic.span -> string -> 'error;
}

val validate_scoped_sst :
  ('scope, 'error) scoped_sst_services ->
  function_id:Sst.function_id ->
  outer:'scope ->
  Sst.expression ->
  Sst.quantifier ->
  ('scope, 'error) result

type 'error sst_content_services = {
  validate_content : Sst.expression -> (unit, 'error) result;
  invalid_sst : string -> (unit, 'error) result;
}

val validate_sst_contents :
  'error sst_content_services ->
  function_id:Sst.function_id ->
  Sst.expression ->
  Sst.quantifier ->
  (unit, 'error) result

val definition_has_quantifier : Sst.function_definition -> bool
val program_has_quantifier : Sst.program -> bool
