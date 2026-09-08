open Typedtree
type 'error services = {
  bind : Typedtree.pattern -> ((Ident.t * Sst.binding), 'error) result;
  lower : Ident.t -> Sst.binding -> Typedtree.expression ->
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
let ( let* ) result continuation =
  match result with Ok value -> continuation value | Error _ as error -> error
let trigger_attributes attributes =
  List.filter
    (fun attribute -> String.equal attribute.Parsetree.attr_name.txt "trigger")
    attributes
let trigger_payload_is_empty attribute =
  match attribute.Parsetree.attr_payload with PStr [] -> true | _ -> false
let collect_triggers services body =
  let found = ref [] in
  let misplaced = ref None in
  let default = Tast_iterator.default_iterator in
  let iterator =
    {
      default with
      expr =
        (fun self expression ->
          if expression != body && services.nested_quantifier expression then ()
          else (
            let attributes = trigger_attributes expression.exp_attributes in
            List.iter
              (fun attribute ->
                if trigger_payload_is_empty attribute then
                  found := expression :: !found
                else misplaced := Some attribute.Parsetree.attr_loc)
              attributes;
            default.expr self expression));
      pat =
        (fun self pattern ->
          match trigger_attributes pattern.pat_attributes with
          | attribute :: _ ->
              misplaced := Some attribute.Parsetree.attr_loc;
              default.pat self pattern
          | [] -> default.pat self pattern);
    }
  in
  iterator.expr iterator body;
  match !misplaced with
  | Some location ->
      Error (services.trigger_error location "trigger attribute is misplaced")
  | None -> Ok (List.rev !found)
let rec expression_uses_binding binding expression =
  let uses = expression_uses_binding binding in
  match expression.Sst.expression_desc with
  | Sst.Variable { binding = candidate; _ } ->
      Int.equal binding.Sst.id candidate.Sst.id
  | Sst.Forall quantifier | Sst.Exists quantifier ->
      if Int.equal binding.Sst.id quantifier.quantifier_binder.id then false
      else
        uses quantifier.quantifier_body
        || Option.fold ~none:false ~some:uses quantifier.quantifier_trigger
  | _ ->
      List.exists uses (Sst_callback_private.expression_children expression)
let logical_application ~allow_unclassified expression =
  match expression.Sst.expression_desc with
  | _ when Parametric_type.equal expression.Sst.typ Sst.Unit -> false
  | Sst.Direct_call
      {
        call_form;
        arguments = _ :: _;
        _;
      } -> (
      match call_form with
      | Sst.Specification_call | Sst.Proof_call -> true
      | Sst.Unclassified_call -> allow_unclassified
      | Sst.Exec_call -> false)
  | Sst.Callback_call { arguments = _ :: _; _ }
  | Sst.Callback_requires { arguments = _ :: _; _ }
  | Sst.Callback_ensures
      { application = { arguments = _ :: _; _ }; _ } ->
      true
  | Sst.Symbolic_application application ->
      not (Symbolic_application_private.is_nullary application)
  | Sst.Int_constant _ | Sst.Bool_constant _
  | Sst.Unit_constant
  | Sst.Variable _
  | Sst.Tuple_value _
  | Sst.Record_value _
  | Sst.Constructor_value _
  | Sst.Field_read _
  | Sst.Field_write _
  | Sst.Shared_scalar_field_write _
  | Sst.Owned_tree_nested_write _
  | Sst.Owned_tree_rebase _
  | Sst.Let_mutable _
  | Sst.Mutable_read _
  | Sst.Mutable_write _
  | Sst.Let _
  | Sst.Sequence _
  | Sst.If _
  | Sst.Match _
  | Sst.Lift_runtime_int _
  | Sst.Bv_literal _
  | Sst.Bv_int_to_bv_mod _
  | Sst.Bv_to_int_unsigned _
  | Sst.Bv_to_int_signed _
  | Sst.Bv_not _
  | Sst.Bv_binary _
  | Sst.Bv_compare _
  | Sst.Checked_arithmetic _
  | Sst.Compare _
  | Sst.Boolean_not _
  | Sst.Boolean_binary _
  | Sst.Forall _
  | Sst.Exists _
  | Sst.Direct_call _
  | Sst.Callback_call _
  | Sst.Callback_requires _
  | Sst.Callback_ensures _
  | Sst.Optional_absent
  | Sst.Optional_present _
  | Sst.Optional_forward _
  | Sst.Reveal _
  | Sst.Reveal_with_fuel _
  | Sst.Use_type_invariant _
  | Sst.Local_assert _
  | Sst.Proof_region _
  | Sst.Old _
  | Sst.Logical_constant_reference _ ->
      false
let validate_trigger services binder location trigger =
  if not (logical_application ~allow_unclassified:true trigger) then
    Error
      (services.trigger_error location
         "trigger must be a non-nullary logical application")
  else if not (expression_uses_binding binder trigger) then
    Error
      (services.trigger_error location
         "trigger does not cover the binder owned by this quantifier")
  else Ok ()
let validate_sst ?(allow_unclassified = false) ?expected_owner kind quantifier =
  let binder = quantifier.Sst.quantifier_binder in
  let metadata = quantifier.quantifier_metadata in
  let trigger_policy =
    match (kind, quantifier.quantifier_trigger) with
    | Logic_quantifier_private.Forall, Some trigger ->
        logical_application ~allow_unclassified trigger
        && expression_uses_binding binder trigger
    | Logic_quantifier_private.Exists, None -> true
    | Logic_quantifier_private.Forall, None
    | Logic_quantifier_private.Exists, Some _ ->
        false
  in
  let* () =
    Logic_quantifier_private.validate_shape ?expected_owner ~kind
      ~binder_index:binder.id ~binder_type:binder.typ metadata
  in
  if not (Parametric_type.equal quantifier.quantifier_body.typ Sst.Bool)
  then Error "quantifier body is not Boolean"
  else if not trigger_policy then
    Error "quantifier trigger policy or binder coverage is invalid"
  else Logic_quantifier_private.validate_identity metadata
let validate_sst_expression ?(allow_unclassified = false) ?expected_owner
    (expression : Sst.expression) =
  match expression.expression_desc with
  | Sst.Forall quantifier ->
      validate_sst ~allow_unclassified ?expected_owner
        Logic_quantifier_private.Forall quantifier
  | Sst.Exists quantifier ->
      validate_sst ~allow_unclassified ?expected_owner
        Logic_quantifier_private.Exists quantifier
  | Sst.Int_constant _ | Sst.Bool_constant _ | Sst.Unit_constant
  | Sst.Variable _ | Sst.Tuple_value _ | Sst.Record_value _
  | Sst.Constructor_value _ | Sst.Field_read _ | Sst.Field_write _
  | Sst.Shared_scalar_field_write _ | Sst.Owned_tree_nested_write _
  | Sst.Owned_tree_rebase _ | Sst.Let_mutable _ | Sst.Mutable_read _
  | Sst.Mutable_write _ | Sst.Let _ | Sst.Sequence _ | Sst.If _ | Sst.Match _
  | Sst.Lift_runtime_int _ | Sst.Checked_arithmetic _ | Sst.Compare _
  | Sst.Bv_literal _ | Sst.Bv_int_to_bv_mod _
  | Sst.Bv_to_int_unsigned _ | Sst.Bv_to_int_signed _ | Sst.Bv_not _
  | Sst.Bv_binary _ | Sst.Bv_compare _
  | Sst.Boolean_not _
  | Sst.Boolean_binary _ | Sst.Direct_call _ | Sst.Callback_call _
  | Sst.Callback_requires _ | Sst.Callback_ensures _ | Sst.Optional_absent
  | Sst.Symbolic_application _
  | Sst.Logical_constant_reference _
  | Sst.Optional_present _ | Sst.Optional_forward _ | Sst.Reveal _
  | Sst.Reveal_with_fuel _ | Sst.Use_type_invariant _ | Sst.Local_assert _
  | Sst.Proof_region _ | Sst.Old _ ->
      Error "quantifier validation received a non-quantifier expression"
let validate_sst_for_function function_id expression =
  validate_sst_expression
    ~expected_owner:("function:" ^ function_id.Sst.function_name)
    expression
type ('mode, 'error) instance_mode_services = {
  issue_binder : identity:string -> Sst.binding -> unit;
  issue_ghost_expression : Sst.expression -> ('mode, 'error) result;
  exact_ghost :
    'mode -> Sst.expression -> string -> (unit, 'error) result;
}
let validate_instance_mode services ~function_index ~expression ~mode
    quantifier =
  let binder = quantifier.Sst.quantifier_binder in
  services.issue_binder
    ~identity:
      (Printf.sprintf "quantifier-binding:%d:%d" function_index binder.id)
    binder;
  let* () =
    services.exact_ghost mode expression "logical quantifier term"
  in
  let validate_expression site nested =
    let* nested_mode = services.issue_ghost_expression nested in
    services.exact_ghost nested_mode nested site
  in
  let* () =
    validate_expression "logical quantifier body"
      quantifier.quantifier_body
  in
  Option.fold ~none:(Ok ())
    ~some:(validate_expression "logical quantifier trigger")
    quantifier.quantifier_trigger
type ('result, 'error) quantifier_value_services = ('result, 'error) Spec_function_type_private.quantifier_value_services and 'term quantifier_body_services = 'term Spec_function_type_private.quantifier_body_services
let quantifier_value services (binding : Sst.binding) =
  Spec_function_type_private.quantifier_value services ~typ:binding.typ
    ~span:binding.span
let quantifier_body services kind (binding : Sst.binding) body =
  Spec_function_type_private.quantifier_body services kind binding.typ body
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
let rec trigger_arguments services context span state lowered = function
  | [] -> Ok (List.rev lowered, state)
  | source :: rest ->
      let* argument, state =
        services.evaluate_argument context span source state
      in
      trigger_arguments services context span state (argument :: lowered) rest
let rec trigger_values malformed collected = function
  | [] -> Ok (List.rev collected)
  | Sst.Value_argument { value; _ } :: rest ->
      trigger_values malformed (value :: collected) rest
  | Sst.Callback_argument _ :: _ ->
      malformed
        "callback-valued explicit trigger arguments are not first-order"
let explicit_trigger services context (expression : Sst.expression) state =
  let arguments = trigger_arguments services context expression.span in
  let malformed message =
    Error (services.malformed_trigger context expression.span message)
  in
  match expression.expression_desc with
  | Sst.Direct_call _
    when Spec_function_sst_private.application_has_lambda_head expression ->
      malformed
        "closure literals are not authenticated specification-application \
         trigger heads"
  | Sst.Direct_call _
    when Spec_function_sst_private.application expression <> None ->
      let application =
        Option.get (Spec_function_sst_private.application expression)
      in
      let* lowered, state =
        arguments state []
          [
            application.application_function;
            application.application_argument;
          ]
      in
      Ok
        ( services.construct_trigger
            (Spec_apply_trigger
               {
                 arrow = application.application_arrow;
                 arguments = lowered;
                 result_type = application.application_result;
                 span = expression.span;
               }),
          state )
  | Sst.Direct_call
      {
        call_form = (Sst.Specification_call | Sst.Proof_call);
        callee;
        type_arguments;
        arguments = sources;
        _;
      } ->
      let* sources = trigger_values malformed [] sources in
      let* lowered, state = arguments state [] sources in
      let trigger =
        Direct_trigger
          {
            recursive = services.recursive context callee;
            callee;
            type_arguments;
            arguments = lowered;
            span = expression.span;
          }
      in
      Ok (services.construct_trigger trigger, state)
  | Sst.Callback_requires application ->
      let* lowered, state =
        arguments state [] (List.map snd application.arguments)
      in
      Ok
        ( services.construct_trigger
            (Callback_requires_trigger { application; arguments = lowered }),
          state )
  | Sst.Callback_ensures { application; result } -> (
      let* lowered, state =
        arguments state [] (List.map snd application.arguments @ [ result ])
      in
      match List.rev lowered with
      | result :: reversed_arguments ->
          let trigger =
            Callback_ensures_trigger
              {
                application;
                arguments = List.rev reversed_arguments;
                result;
              }
          in
          Ok (services.construct_trigger trigger, state)
      | [] -> malformed "callback ensures trigger lost its result")
  | Sst.Symbolic_application application ->
      let* lowered, state =
        arguments state [] (Symbolic_application_private.arguments application)
      in
      if lowered = [] then
        malformed "explicit universal trigger is nullary"
      else (
        let* application =
          match
            Symbolic_application_private.replace_arguments lowered application
          with
          | Ok application -> Ok application
          | Error message -> malformed message
        in
        Ok
          ( services.construct_trigger
              (Direct_trigger
                 {
                   recursive = false;
                   callee =
                     {
                       Sst.function_index =
                         Symbolic_application_private.declaration_index
                           (Symbolic_application_private.declaration application);
                       function_name =
                         Symbolic_application_private.declaration_name
                           (Symbolic_application_private.declaration application);
                     };
                   type_arguments =
                     Symbolic_application_private.type_arguments application;
                   arguments = lowered;
                   span = Symbolic_application_private.span application;
                 }),
            state ))
  | Sst.Int_constant _ | Sst.Bool_constant _ | Sst.Unit_constant
  | Sst.Variable _ | Sst.Tuple_value _ | Sst.Record_value _
  | Sst.Constructor_value _ | Sst.Field_read _ | Sst.Field_write _
  | Sst.Shared_scalar_field_write _ | Sst.Owned_tree_nested_write _
  | Sst.Owned_tree_rebase _ | Sst.Let_mutable _ | Sst.Mutable_read _
  | Sst.Mutable_write _ | Sst.Let _ | Sst.Sequence _ | Sst.If _ | Sst.Match _
  | Sst.Lift_runtime_int _ | Sst.Bv_literal _ | Sst.Bv_int_to_bv_mod _
  | Sst.Bv_to_int_unsigned _ | Sst.Bv_to_int_signed _ | Sst.Bv_not _
  | Sst.Bv_binary _ | Sst.Bv_compare _ | Sst.Checked_arithmetic _
  | Sst.Compare _
  | Sst.Boolean_not _
  | Sst.Boolean_binary _ | Sst.Forall _ | Sst.Exists _ | Sst.Direct_call _
  | Sst.Callback_call _ | Sst.Optional_absent | Sst.Optional_present _
  | Sst.Optional_forward _ | Sst.Reveal _ | Sst.Reveal_with_fuel _
  | Sst.Use_type_invariant _ | Sst.Local_assert _ | Sst.Proof_region _
  | Sst.Old _ | Sst.Logical_constant_reference _ ->
      malformed
        "explicit universal trigger is not an authenticated logical application"
type ('term, 'state, 'error) quantifier_expression_services = {
  branches :
    Sst.expression ->
    'state ->
    (('term * 'state) list option, 'error) result;
  merge_branches : 'term -> 'term -> 'term;
  state_rank : 'state -> int;
  malformed_expression : Diagnostic.span -> string -> 'error;
}
let quantifier_expression services (expression : Sst.expression) state =
  let* branches = services.branches expression state in
  match branches with
  | Some ((first_term, first_state) :: rest) ->
      let term =
        List.fold_left
          (fun combined (branch, _) ->
            services.merge_branches combined branch)
          first_term rest
      in
      let state =
        List.fold_left
          (fun newest (_, candidate) ->
            if services.state_rank candidate > services.state_rank newest then
              candidate
            else newest)
          first_state rest
      in
      Ok (term, state)
  | Some [] | None ->
      Error
        (services.malformed_expression expression.span
           "quantifier term must lower without logical obligations")
type ('scope, 'error) scoped_sst_services = {
  admit_application : Sst.typ -> bool;
  validate_type_reference : Sst.binding -> (unit, 'error) result;
  add_binding : 'scope -> Sst.binding -> ('scope, 'error) result;
  validate_expression :
    'scope -> Sst.expression -> ('scope, 'error) result;
  malformed_sst : Diagnostic.span -> string -> 'error;
}
let validate_scoped_sst services ~function_id ~outer
    (expression : Sst.expression) quantifier =
  let binder = quantifier.Sst.quantifier_binder in
  let admitted_binder =
    match binder.typ with
    | Sst.Int | Sst.Mathematical_int | Sst.Bool | Sst.Bit_vector _
    | Sst.Parameter _ -> true
    | Sst.Application _ as typ -> services.admit_application typ
    | Sst.Unit | Sst.Tuple _ | Sst.Aggregate _ -> false
  in
  let* () =
    match validate_sst_for_function function_id expression with
    | Ok () -> Ok ()
    | Error message ->
        Error (services.malformed_sst expression.span message)
  in
  let* () = services.validate_type_reference binder in
  let* () =
    if
      Parametric_type.equal expression.typ Sst.Bool
      && binder.uniqueness = Sst.Definitely_aliased
      && admitted_binder
    then Ok ()
    else
      Error
        (services.malformed_sst expression.span
           "quantifier binder is not a supported first-order logical value")
  in
  let* scoped = services.add_binding outer binder in
  let* _ = services.validate_expression scoped quantifier.quantifier_body in
  let* () =
    match quantifier.quantifier_trigger with
    | None -> Ok ()
    | Some trigger ->
        let* _ = services.validate_expression scoped trigger in
        Ok ()
  in
  Ok outer
type 'error sst_content_services = {
  validate_content : Sst.expression -> (unit, 'error) result;
  invalid_sst : string -> (unit, 'error) result;
}
let validate_sst_contents services ~function_id
    (expression : Sst.expression) quantifier =
  let* () =
    match validate_sst_for_function function_id expression with
    | Ok () -> Ok ()
    | Error message -> services.invalid_sst message
  in
  let* () = services.validate_content quantifier.Sst.quantifier_body in
  match quantifier.quantifier_trigger with
  | None -> Ok ()
  | Some trigger -> services.validate_content trigger
let rec expression_has_quantifier expression =
  match expression.Sst.expression_desc with
  | Sst.Forall _ | Sst.Exists _ -> true
  | _ ->
      List.exists expression_has_quantifier
        (Sst_callback_private.expression_children expression)
let definition_has_quantifier (definition : Sst.function_definition) =
  let staged staged = expression_has_quantifier staged.Sst.expression in
  let predicates (clauses : Sst.predicate_clause list) =
    List.exists
      (fun (clause : Sst.predicate_clause) -> staged clause.predicate)
      clauses
  in
  let optional = function
    | Sst.Value_parameter
        { optional_default = Some { optional_expression; _ }; _ } ->
        expression_has_quantifier optional_expression
    | Sst.Value_parameter { optional_default = None; _ }
    | Sst.Callback_parameter _ ->
        false
  in
  predicates definition.contracts.requires
  || predicates definition.contracts.decreases
  || predicates definition.contracts.assertions
  || List.exists
       (fun (clause : Sst.ensures_clause) -> staged clause.predicate)
       definition.contracts.ensures
  || List.exists optional definition.parameters
  ||
  match definition.body with
  | Sst.Checked_exec { body; _ }
  | Sst.Proof_body { body; _ }
  | Sst.Spec_definition body
  | Sst.Recursive_spec_definition { body; _ } ->
      staged body
  | Sst.External_specification _
  | Sst.Trusted_external_spec_target _
  | Sst.Trusted_external_body _
  | Sst.Symbolic_declaration _ ->
      false
let program_has_quantifier program =
  List.exists definition_has_quantifier program.Sst.functions
let parse_one_binder services quantifier =
  match quantifier.exp_desc with
  | Texp_function
      {
        params =
          [
            {
              fp_kind =
                Tparam_pat
                  ({
                     pat_desc = Tpat_var (_, _, _, _, _);
                     pat_attributes = [];
                     _;
                   } as pattern);
              fp_arg_label = Nolabel;
              _;
            };
          ];
        body = Tfunction_body body;
        _;
      } ->
      Ok (pattern, body)
  | Texp_function _ ->
      Error
        (services.authentication_error quantifier.exp_loc
           "quantifier must have exactly one unlabelled variable binder")
  | _ ->
      Error
        (services.authentication_error quantifier.exp_loc
           "authenticated carrier does not contain a function binder")
let validate_trigger_count services kind body triggers =
  match (kind, triggers) with
  | Typedtree_logical_builtin_private.Forall, [ trigger ] -> Ok (Some trigger)
  | Typedtree_logical_builtin_private.Exists, [] -> Ok None
  | Typedtree_logical_builtin_private.Forall, [] ->
      Error
        (services.trigger_error body.exp_loc
           "forall requires exactly one explicit trigger")
  | Typedtree_logical_builtin_private.Forall, _ :: _ :: _ ->
      Error
        (services.trigger_error body.exp_loc
           "forall has duplicate or grouped trigger attributes")
  | Typedtree_logical_builtin_private.Exists, _ :: _ ->
      Error
        (services.trigger_error body.exp_loc
           "exists does not accept a trigger in this tranche")
  | ( Typedtree_logical_builtin_private.Call_requires
    | Typedtree_logical_builtin_private.Call_ensures ),
    _ ->
      Error
        (services.authentication_error body.exp_loc
           "callback carrier was dispatched as a quantifier")
let quantifier_kind = function
  | Typedtree_logical_builtin_private.Forall -> Logic_quantifier_private.Forall
  | Typedtree_logical_builtin_private.Exists -> Logic_quantifier_private.Exists
  | Typedtree_logical_builtin_private.Call_requires
  | Typedtree_logical_builtin_private.Call_ensures ->
      invalid_arg "callback carrier has no quantifier kind"
let lower services ~kind ~source ~quantifier =
  let* () =
    if services.shadowed kind then
      Error
        (services.authentication_error source.exp_loc
           "quantifier builtin is shadowed in this lexical program")
    else Ok ()
  in
  let* pattern, body_source = parse_one_binder services quantifier in
  let* triggers = collect_triggers services body_source in
  let* trigger_source =
    validate_trigger_count services kind body_source triggers
  in
  let* ident, binder = services.bind pattern in
  let* () =
    if services.admit_type binder.Sst.typ then Ok ()
    else
      Error
        (services.type_error pattern.pat_loc
           "binder is not a supported scalar, abstract, or immutable generic ADT")
  in
  let* body = services.lower ident binder body_source in
  let* () =
    if Parametric_type.equal body.Sst.typ Sst.Bool then Ok ()
    else
      Error
        (services.authentication_error body_source.exp_loc
           "quantifier body must have type bool")
  in
  let* trigger =
    match trigger_source with
    | None -> Ok None
    | Some expression ->
        let* lowered = services.lower ident binder expression in
        let* () = validate_trigger services binder expression.exp_loc lowered in
        Ok (Some lowered)
  in
  let metadata =
    Logic_quantifier_private.create ~kind:(quantifier_kind kind)
      ~owner:services.owner ~binder_index:binder.Sst.id
      ~binder_type:binder.typ ~span:(services.span source.exp_loc)
  in
  let quantifier =
    {
      Sst.quantifier_metadata = metadata;
      quantifier_binder = binder;
      quantifier_body = body;
      quantifier_trigger = trigger;
    }
  in
  Ok
    {
      Sst.expression_desc =
        (match kind with
        | Typedtree_logical_builtin_private.Forall -> Sst.Forall quantifier
        | Typedtree_logical_builtin_private.Exists -> Sst.Exists quantifier
        | Typedtree_logical_builtin_private.Call_requires
        | Typedtree_logical_builtin_private.Call_ensures ->
            assert false);
      typ = Sst.Bool;
      span = services.span source.exp_loc;
    }
