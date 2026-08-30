type span = Diagnostic.span

type aggregate_type = {
  aggregate_type_index : int;
  aggregate_type_name : string;
  aggregate_type_arguments : Parametric_type.t list;
}

type sort =
  | Integer
  | Boolean
  | Aggregate of aggregate_type
  | Parametric of Parametric_type.binder

type symbol_role = Input | Local | Result

type symbol = {
  symbol_id : int;
  source_name : string;
  sort : sort;
  role : symbol_role;
  span : span;
}

type rank_fact =
  | Ground_rank_base of {
      constructor : Sst.constructor_id;
      rank : Z.t;
    }
  | Constructor_rank_nonnegative of {
      constructor : Sst.constructor_id;
    }
  | Positive_child_rank_smaller of {
      constructor : Sst.constructor_id;
      field : Sst.field_id;
      child_path : int list;
      child_type : aggregate_type;
    }

type rank_domain = {
  rank_id : string;
  rank_version : string;
  rank_digest : string;
  rank_component : aggregate_type list;
  rank_facts : rank_fact list;
}

type parametric_term = {
  parametric_sort : Parametric_type.binder;
  parametric_desc : parametric_term_desc;
}

and parametric_term_desc =
  | Parametric_symbol of symbol
  | Parametric_selector of selector * aggregate_term
  | Parametric_conditional of boolean_term * parametric_term * parametric_term
  | Parametric_symbolic_application of
      recursive_spec_argument Symbolic_application_private.t

and integer_term =
  | Integer_constant of Z.t
  | Integer_symbol of symbol
  | Integer_add of integer_term * integer_term
  | Integer_subtract of integer_term * integer_term
  | Integer_negate of integer_term
  | Integer_multiply_constant of Z.t * integer_term
  | Integer_absolute_value of integer_term
  | Integer_conditional of boolean_term * integer_term * integer_term
  | Integer_rank_project of rank_domain * aggregate_term
  | Aggregate_tag of aggregate_type * aggregate_term
  | Integer_selector of selector * aggregate_term
  | Integer_recursive_spec_application of {
      callee : Sst.function_id;
      type_arguments : Parametric_type.t list;
      arguments : recursive_spec_argument list;
      span : span;
    }
  | Integer_symbolic_application of
      recursive_spec_argument Symbolic_application_private.t

and aggregate_term = {
  aggregate_type : aggregate_type;
  aggregate_desc : aggregate_term_desc;
}

and aggregate_term_desc =
  | Aggregate_symbol of symbol
  | Aggregate_imported_model_application of {
      callee : Sst.function_id;
      callable_path : string;
      callable_uid : string;
      provider_unit : string;
      provider_interface : string;
      provider_source : string;
      provider_family : string;
      provider_import : string;
      summary_digest : string;
      closure_digest : string;
      call_snapshot : string;
      registration_snapshot : string;
      invocation_ordinal : int;
      application_identity : Imported_callable.aggregate_application_identity;
      arguments : recursive_spec_argument list;
      result_type : aggregate_type;
      span : span;
    }
  | Aggregate_selector of selector * aggregate_term
  | Aggregate_constructor of {
      constructor : Sst.constructor_id;
      arguments : recursive_spec_argument list;
    }
  | Aggregate_record of {
      record_type : Sst.type_id;
      fields : (Sst.field_id * recursive_spec_argument) list;
    }
  | Aggregate_conditional of boolean_term * aggregate_term * aggregate_term
  | Aggregate_recursive_spec_application of {
      callee : Sst.function_id;
      type_arguments : Parametric_type.t list;
      arguments : recursive_spec_argument list;
      result_type : aggregate_type;
      span : span;
      application_identity : Recursive_spec_application_identity.t;
    }
  | Aggregate_symbolic_application of
      recursive_spec_argument Symbolic_application_private.t

and selector = {
  selector_domain : aggregate_type;
  selector_range : sort;
  selector_namespace : string;
  selector_index : int;
  selector_name : string;
  selector_path : int list;
}

and recursive_spec_argument =
  | Recursive_integer_argument of integer_term
  | Recursive_boolean_argument of boolean_term
  | Recursive_aggregate_argument of aggregate_term
  | Recursive_parametric_argument of parametric_term

and callback_application = {
  callback : Sst.callback_binding;
  arguments : recursive_spec_argument list;
  call_span : span;
}

and boolean_quantifier = {
  boolean_quantifier_schema : Logic_quantifier_private.vector;
  boolean_quantifier_binders : symbol list;
  boolean_quantifier_body : boolean_term;
  boolean_quantifier_trigger : application_term option;
}

and application_term =
  | Integer_application of integer_term
  | Boolean_application of boolean_term
  | Aggregate_application of aggregate_term
  | Parametric_application of parametric_term

and boolean_term =
  | Logical_adt_schema of Logical_adt_schema_private.t list
  | Boolean_constant of bool
  | Boolean_symbol of symbol
  | Boolean_not of boolean_term
  | Boolean_and of boolean_term * boolean_term
  | Boolean_or of boolean_term * boolean_term
  | Forall_term of boolean_quantifier
  | Exists_term of boolean_quantifier
  | Integer_compare of comparison * integer_term * integer_term
  | Boolean_equal of boolean_term * boolean_term
  | Boolean_not_equal of boolean_term * boolean_term
  | Boolean_selector of selector * aggregate_term
  | Aggregate_equal of aggregate_term * aggregate_term
  | Parametric_equal of parametric_term * parametric_term
  | Boolean_invariant_application of {
      invariant_id : string;
      model : Sst.function_id;
      predicate : Sst.function_id;
      value : aggregate_term;
    }
  | Boolean_recursive_spec_application of {
      callee : Sst.function_id;
      type_arguments : Parametric_type.t list;
      arguments : recursive_spec_argument list;
      span : span;
    }
  | Boolean_specification_application of {
      callee : Sst.function_id;
      type_arguments : Parametric_type.t list;
      arguments : recursive_spec_argument list;
      span : span;
    }
  | Boolean_symbolic_application of
      recursive_spec_argument Symbolic_application_private.t
  | Callback_requires of callback_application
  | Callback_ensures of {
      application : callback_application;
      result : recursive_spec_argument;
    }

and comparison =
  | Equal
  | Not_equal
  | Less_than
  | Less_or_equal
  | Greater_than
  | Greater_or_equal

type spec_function_term = parametric_term

let symbolic_application_arguments = function
  | Integer_application (Integer_symbolic_application application)
  | Boolean_application (Boolean_symbolic_application application)
  | Aggregate_application
      { aggregate_desc = Aggregate_symbolic_application application; _ }
  | Parametric_application
      { parametric_desc = Parametric_symbolic_application application; _ } ->
      Some (Symbolic_application_private.arguments application)
  | Integer_application _
  | Boolean_application _
  | Aggregate_application _
  | Parametric_application _ ->
      None

let symbolic_application ~aggregate_type application =
  match Symbolic_application_private.result_type application with
  | Parametric_type.Int ->
      Ok (Integer_application (Integer_symbolic_application application))
  | Bool ->
      Ok (Boolean_application (Boolean_symbolic_application application))
  | Parameter binder ->
      Ok
        (Parametric_application
           {
             parametric_sort = binder;
             parametric_desc =
               Parametric_symbolic_application application;
           })
  | Application _ as typ -> (
      match aggregate_type typ with
      | Some aggregate_type ->
          Ok
            (Aggregate_application
               {
                 aggregate_type;
                 aggregate_desc =
                   Aggregate_symbolic_application application;
               })
      | None -> Error "symbolic application result sort is unavailable")
  | Unit | Tuple _ | Aggregate _ ->
      Error "symbolic application result type escaped validation"

let boolean_term_symbol_ids term =
  let add symbol ids =
    if List.mem symbol.symbol_id ids then ids else symbol.symbol_id :: ids
  in
  let rec argument ids = function
    | Recursive_integer_argument term -> integer ids term
    | Recursive_boolean_argument term -> boolean ids term
    | Recursive_aggregate_argument term -> aggregate ids term
    | Recursive_parametric_argument term -> parametric ids term
  and arguments ids values = List.fold_left argument ids values
  and parametric ids term =
    match term.parametric_desc with
    | Parametric_symbol symbol -> add symbol ids
    | Parametric_selector (_, source) -> aggregate ids source
    | Parametric_conditional (condition, consequent, alternative) ->
        parametric
          (parametric (boolean ids condition) consequent)
          alternative
    | Parametric_symbolic_application application ->
        arguments ids
          (Symbolic_application_private.arguments application)
  and integer ids = function
    | Integer_constant _ -> ids
    | Integer_symbol symbol -> add symbol ids
    | Integer_add (left, right) | Integer_subtract (left, right) ->
        integer (integer ids left) right
    | Integer_negate value | Integer_multiply_constant (_, value)
    | Integer_absolute_value value ->
        integer ids value
    | Integer_conditional (condition, consequent, alternative) ->
        integer (integer (boolean ids condition) consequent) alternative
    | Integer_rank_project (_, value) | Aggregate_tag (_, value)
    | Integer_selector (_, value) ->
        aggregate ids value
    | Integer_recursive_spec_application { arguments = values; _ } ->
        arguments ids values
    | Integer_symbolic_application application ->
        arguments ids
          (Symbolic_application_private.arguments application)
  and aggregate ids term =
    match term.aggregate_desc with
    | Aggregate_symbol symbol -> add symbol ids
    | Aggregate_selector (_, source) -> aggregate ids source
    | Aggregate_constructor { arguments = values; _ }
    | Aggregate_recursive_spec_application { arguments = values; _ }
    | Aggregate_imported_model_application { arguments = values; _ } ->
        arguments ids values
    | Aggregate_symbolic_application application ->
        arguments ids
          (Symbolic_application_private.arguments application)
    | Aggregate_record { fields; _ } ->
        List.fold_left
          (fun ids (_, value) -> argument ids value)
          ids fields
    | Aggregate_conditional (condition, consequent, alternative) ->
        aggregate (aggregate (boolean ids condition) consequent) alternative
  and callback ids application = arguments ids application.arguments
  and boolean ids = function
    | Logical_adt_schema _ | Boolean_constant _ -> ids
    | Boolean_symbol symbol -> add symbol ids
    | Boolean_not value -> boolean ids value
    | Boolean_and (left, right) | Boolean_or (left, right)
    | Boolean_equal (left, right) | Boolean_not_equal (left, right) ->
        boolean (boolean ids left) right
    | Forall_term quantifier | Exists_term quantifier ->
        let nested = boolean ids quantifier.boolean_quantifier_body in
        let nested =
          Option.fold ~none:nested ~some:(application nested)
            quantifier.boolean_quantifier_trigger
        in
        let bound =
          List.map
            (fun binder -> binder.symbol_id)
            quantifier.boolean_quantifier_binders
        in
        List.filter (fun id -> not (List.mem id bound)) nested
    | Integer_compare (_, left, right) -> integer (integer ids left) right
    | Boolean_selector (_, value)
    | Boolean_invariant_application { value; _ } ->
        aggregate ids value
    | Aggregate_equal (left, right) -> aggregate (aggregate ids left) right
    | Parametric_equal (left, right) -> parametric (parametric ids left) right
    | Boolean_recursive_spec_application { arguments = values; _ }
    | Boolean_specification_application { arguments = values; _ } ->
        arguments ids values
    | Boolean_symbolic_application application ->
        arguments ids
          (Symbolic_application_private.arguments application)
    | Callback_requires application -> callback ids application
    | Callback_ensures { application; result } ->
        argument (callback ids application) result
  and application ids = function
    | Integer_application term -> integer ids term
    | Boolean_application term -> boolean ids term
    | Aggregate_application term -> aggregate ids term
    | Parametric_application term -> parametric ids term
  in
  List.rev (boolean [] term)

let application_term_symbol_ids term =
  let wrapped =
    match term with
    | Integer_application term ->
        Integer_compare (Equal, term, term)
    | Boolean_application term -> term
    | Aggregate_application term -> Aggregate_equal (term, term)
    | Parametric_application term -> Parametric_equal (term, term)
  in
  boolean_term_symbol_ids wrapped

let application_head = function
  | Boolean_application
      (Boolean_recursive_spec_application { arguments; _ })
  | Boolean_application
      (Boolean_specification_application { arguments; _ }) ->
      arguments <> []
  | Boolean_application (Callback_requires { arguments; _ }) ->
      arguments <> []
  | Boolean_application
      (Callback_ensures { application = { arguments; _ }; _ }) ->
      arguments <> []
  | Integer_application (Integer_symbolic_application application)
  | Boolean_application (Boolean_symbolic_application application)
  | Aggregate_application
      { aggregate_desc = Aggregate_symbolic_application application; _ }
  | Parametric_application
      { parametric_desc = Parametric_symbolic_application application; _ } ->
      not (Symbolic_application_private.is_nullary application)
  | Integer_application _
  | Boolean_application _
  | Aggregate_application _
  | Parametric_application _ ->
      false

let make_boolean_quantifier ~sort_of_type ~schema ~binders ~body ~trigger =
  let metadata = Logic_quantifier_private.vector_binders schema in
  let kind = Logic_quantifier_private.vector_kind schema in
  let ids = List.map (fun binder -> binder.symbol_id) binders in
  let expected_sorts =
    List.fold_left
      (fun result metadata ->
        match result with
        | Error _ as error -> error
        | Ok sorts ->
            Result.map
              (fun sort -> sort :: sorts)
              (sort_of_type
                 (Logic_quantifier_private.binder_type metadata)))
      (Ok []) metadata
    |> Result.map List.rev
  in
  if Result.is_error (Logic_quantifier_private.validate_vector_identity schema)
  then Error "VIR user quantifier schema identity is invalid"
  else if binders = [] then Error "VIR user quantifier binder vector is empty"
  else if List.length binders <> List.length metadata then
    Error "VIR user quantifier schema and symbol vector arities differ"
  else if List.sort_uniq Int.compare ids <> List.sort Int.compare ids then
    Error "VIR user quantifier duplicates a binder symbol"
  else
    match expected_sorts with
    | Error message -> Error message
    | Ok expected_sorts
      when not
             (List.for_all2
                (fun symbol expected -> symbol.sort = expected)
                binders expected_sorts) ->
        Error "VIR user quantifier binder sort differs from its schema"
    | Ok _ ->
    let trigger_policy =
      match (kind, trigger) with
      | Logic_quantifier_private.Forall, Some trigger ->
          let used = application_term_symbol_ids trigger in
          application_head trigger
          && List.for_all (fun id -> List.mem id used) ids
      | Logic_quantifier_private.Exists, None -> true
      | Logic_quantifier_private.Forall, None
      | Logic_quantifier_private.Exists, Some _ ->
          false
    in
    if not trigger_policy then
      Error "VIR user quantifier trigger policy or vector coverage is invalid"
    else
      Ok
        {
          boolean_quantifier_schema = schema;
          boolean_quantifier_binders = binders;
          boolean_quantifier_body = body;
          boolean_quantifier_trigger = trigger;
        }

type checked_operation =
  | Add
  | Subtract
  | Negate
  | Multiply_constant of Z.t
  | Successor
  | Predecessor
  | Absolute_value

type violated_bound = Lower_bound | Upper_bound

type function_ref = {
  function_index : int;
  function_name : string;
}

type invariant_transition_kind =
  | Direct_root_transition
  | Nested_transition
  | Rebase_transition

type invariant_boundary =
  | Constructor_establishment
  | Transition_preservation of {
      transition_kind : invariant_transition_kind;
      root_binding_id : int;
      pre_version : int;
      successor_version : int;
    }
  | Call_argument of {
      callee : function_ref;
      argument_index : int;
    }
  | Call_result of { callee : function_ref }
  | Function_return
  | Shared_invariant_close of {
      entry_epoch : int;
      final_epoch : int;
    }
  | Terminal_observation of {
      operation : function_ref;
      snapshot : bool;
    }

type vc_kind =
  | Arithmetic_safety of {
      operation : checked_operation;
      mathematical_result : integer_term;
      violated_bound : violated_bound;
    }
  | Assertion of { assertion_ordinal : int }
  | Local_assertion of { local_assertion_ordinal : int }
  | Postcondition of {
      postcondition_ordinal : int;
      declaration_span : span;
    }
  | Call_precondition of {
      callee : function_ref;
      precondition_ordinal : int;
      declaration_span : span;
      call_span : span;
    }
  | Callback_precondition of {
      callback : Sst.callback_binding;
      call_span : span;
    }
  | Invariant_validity of {
      invariant_id : string;
      abstract_type : aggregate_type;
      model : function_ref;
      predicate : function_ref;
      operation : function_ref;
      boundary : invariant_boundary;
    }
  | Entry_measure_nonnegative of { declaration_span : span }
  | Recursive_call_measure_nonnegative of {
      callee : function_ref;
      declaration_span : span;
      call_span : span;
    }
  | Recursive_call_strict_descent of {
      callee : function_ref;
      declaration_span : span;
      call_span : span;
    }

type obligation = {
  obligation_index : int;
  function_ref : function_ref;
  kind : vc_kind;
  span : span;
  assumptions : boolean_term list;
  required_preceding_safety : boolean_term list;
  path_condition : boolean_term list;
  goal : boolean_term;
  projection_symbols : symbol list;
}

type result_value =
  | Unit_result
  | Integer_result of symbol
  | Boolean_result of symbol
  | Tuple_result of result_value list
  | Aggregate_result of symbol
  | Parametric_result of symbol

type trusted_summary_use =
  | Trusted_external_specification_use of {
      target : function_ref;
      wrapper : function_ref;
      target_span : span;
      wrapper_span : span;
      witness_span : span;
      call_span : span;
      requires_count : int;
      ensures_count : int;
    }
  | Trusted_external_target_specification_use of {
      consumer_artifact_digest : string;
      target_unit : string;
      target_interface_digest : string;
      import_crc : string;
      canonical_path : string;
      value_uid : string;
      callable_abi_digest : string;
      wrapper : function_ref;
      target_span : span;
      wrapper_span : span;
      witness_span : span;
      call_span : span;
      summary_digest : string;
      requires_count : int;
      ensures_count : int;
    }
  | Trusted_external_body_use of {
      function_ref : function_ref;
      mode : Sst.verification_mode;
      call_form : Sst.call_form;
      declaration_span : span;
      witness_span : span;
      call_span : span;
      requires_count : int;
      ensures_count : int;
    }

type reached_callback_call = {
  application : callback_application;
  result : result_value;
  ensures : boolean_term;
}

type trusted_external_body_declaration = {
  function_ref : function_ref;
  mode : Sst.verification_mode;
  declaration_span : span;
  witness_span : span;
  requires_count : int;
  ensures_count : int;
}

type exit = {
  assumptions : boolean_term list;
  path_condition : boolean_term list;
  result : result_value;
  projection_symbols : symbol list;
  trusted_summary_uses : trusted_summary_use list;
}

type shared_scalar_heap_read = {
  shared_read_field : Sst.field_id;
  shared_read_path_id : int;
  shared_read_epoch : int;
  shared_read_location : aggregate_term;
  shared_read_term : integer_term;
  shared_read_entry_view : bool;
}

type shared_scalar_heap_write = {
  shared_write_transition : Sst.shared_scalar_heap_transition;
  shared_write_location : aggregate_term;
  shared_write_value : integer_term;
}

type function_execution = {
  function_ref : function_ref;
  mode : Sst.verification_mode;
  body_provenance : Sst.body_provenance;
  policy : Sst.verification_policy;
  trusted_summary_uses : trusted_summary_use list;
  reached_callback_calls : reached_callback_call list;
  owned_tree_transitions : Sst.owned_tree_transition list;
  shared_scalar_heap_reads : shared_scalar_heap_read list;
  shared_scalar_heap_writes : shared_scalar_heap_write list;
  obligations : obligation list;
  exits : exit list;
}

type rank_term = {
  rank_term_domain : rank_domain;
  rank_term_value : aggregate_term;
}

type program = {
  policy : Sst.verification_policy;
  rank_domains : rank_domain list;
  trusted_external_body_declarations :
    trusted_external_body_declaration list;
  functions : function_execution list;
}

let aggregate_type_of_sst (type_id : Sst.type_id) =
  {
    aggregate_type_index = type_id.type_index;
    aggregate_type_name = type_id.type_name;
      aggregate_type_arguments = [];
  }

let aggregate_type_of_rank_identity arguments
    (identity : Typedtree_adapter.rank_type_identity) =
  let name =
    match arguments with
    | [] -> identity.rank_type_id.type_name
    | arguments ->
        identity.rank_type_id.type_name ^ "<"
        ^ String.concat "," (List.map Parametric_type.to_string arguments)
        ^ ">"
  in
  {
    aggregate_type_index = identity.rank_type_id.type_index;
    aggregate_type_name = name;
    aggregate_type_arguments = arguments;
  }

let rank_domains_of_validated validated =
  Sst_validation.rank_domains validated
  |> List.map (fun certificate ->
         let actual_arguments =
           Parametric_rank_domain_private.actual_arguments certificate
         in
         let component =
           Sst_validation.rank_component certificate
           |> List.map (aggregate_type_of_rank_identity actual_arguments)
         in
         let constructors =
           Sst_validation.rank_ground_witnesses certificate
           |> List.map
                (fun (witness : Typedtree_adapter.rank_ground_witness) ->
                  witness.rank_ground_constructor)
         in
         let facts =
           List.map
             (fun constructor ->
               Ground_rank_base { constructor; rank = Z.zero })
             constructors
           @
           (Sst_validation.rank_positive_children certificate
           |> List.map
                (fun (child : Typedtree_adapter.rank_positive_child) ->
                  Positive_child_rank_smaller
                    {
                      constructor = child.rank_constructor;
                      field = child.rank_field;
                      child_path = child.rank_child_path;
                      child_type =
                        Option.value
                          ~default:(aggregate_type_of_sst child.rank_child_type)
                          (List.find_opt
                             (fun aggregate ->
                               aggregate.aggregate_type_index
                                 = child.rank_child_type.type_index)
                             component);
                    }))
           @
           (let all_constructors =
              constructors
              @
              (Sst_validation.rank_positive_children certificate
              |> List.map
                   (fun (child : Typedtree_adapter.rank_positive_child) ->
                     child.rank_constructor))
              |> List.sort_uniq compare
            in
            List.map
              (fun constructor ->
                Constructor_rank_nonnegative { constructor })
              all_constructors)
         in
         {
           rank_id = Sst_validation.rank_domain_id certificate;
           rank_version = Sst_validation.rank_domain_version certificate;
           rank_digest = Sst_validation.rank_snapshot_digest certificate;
           rank_component = component;
           rank_facts = facts;
         })

let rank_domain_id domain = domain.rank_id
let rank_domain_version domain = domain.rank_version
let rank_domain_digest domain = domain.rank_digest
let rank_domain_component domain = domain.rank_component
let rank_domain_facts domain = domain.rank_facts
let rank_selector_is_positive_child domain selector =
  List.exists
    (function
      | Positive_child_rank_smaller
          { constructor; field; child_path; child_type } ->
          let owner =
            List.exists
            (fun aggregate ->
              aggregate = selector.selector_domain
              && aggregate.aggregate_type_index
                 = constructor.constructor_type.type_index
              )
            domain.rank_component
          in
          let range = selector.selector_range = Aggregate child_type in
          let index = selector.selector_index = field.field_index in
          let path = selector.selector_path = child_path in
          let name =
            String.equal selector.selector_name
              (Printf.sprintf "$arg%d" field.field_index)
          in
          if Sys.getenv_opt "VEROCAML_TEST_RANK_MATCH_TRACE" = Some "1" then
            [%log.debug "rank selector match"
              ~owner_matches:(Delator.Field.bool owner)
              ~range_matches:(Delator.Field.bool range)
              ~index_matches:(Delator.Field.bool index)
              ~path_matches:(Delator.Field.bool path)
              ~name_matches:(Delator.Field.bool name)
              ~selector_domain:
                (Delator.Field.string
                   selector.selector_domain.aggregate_type_name)
              ~selector_range:
                (Delator.Field.string
                   (match selector.selector_range with
                   | Aggregate aggregate -> aggregate.aggregate_type_name
                   | Integer -> "int"
                   | Boolean -> "bool"
                   | Parametric _ -> "parameter"))
              ~child_type:
                (Delator.Field.string child_type.aggregate_type_name)];
          owner && range && index && path && name
      | Ground_rank_base _ | Constructor_rank_nonnegative _ -> false)
    domain.rank_facts

let rank_project domain value =
  match value.aggregate_desc with
  | Aggregate_imported_model_application _ ->
      Error
        "retained aggregate model applications cannot authorize a rank projection"
  | Aggregate_symbolic_application _ ->
      Error
        "symbolic applications cannot authorize a rank projection"
  | Aggregate_symbol _ | Aggregate_selector _ | Aggregate_constructor _
  | Aggregate_record _ | Aggregate_conditional _
  | Aggregate_recursive_spec_application _ ->
    if List.mem value.aggregate_type domain.rank_component then
      Ok { rank_term_domain = domain; rank_term_value = value }
    else
    Error
      (Printf.sprintf "rank projection domain %s does not contain aggregate %s#%d"
         domain.rank_id value.aggregate_type.aggregate_type_name
         value.aggregate_type.aggregate_type_index)

let integer_range term =
  [
    Integer_compare
      (Less_or_equal, Integer_constant Int_bounds.minimum, term);
    Integer_compare
      (Less_or_equal, term, Integer_constant Int_bounds.maximum);
  ]

let symbol_to_string symbol =
  Printf.sprintf "%s$%d" symbol.source_name symbol.symbol_id

let comparison_to_string = function
  | Equal -> "="
  | Not_equal -> "distinct"
  | Less_than -> "<"
  | Less_or_equal -> "<="
  | Greater_than -> ">"
  | Greater_or_equal -> ">="

let aggregate_type_to_string type_ =
  Printf.sprintf "%s#%d" type_.aggregate_type_name type_.aggregate_type_index

let selector_to_string selector =
  let path =
    selector.selector_path
    |> List.map string_of_int |> String.concat "."
  in
  Printf.sprintf "%s.%s#%d%s" selector.selector_namespace
    selector.selector_name selector.selector_index
    (if String.equal path "" then "" else "." ^ path)

let rec aggregate_term_to_string term =
  match term.aggregate_desc with
  | Aggregate_symbol symbol -> symbol_to_string symbol
  | Aggregate_imported_model_application
      {
        callable_path;
        callable_uid;
        provider_unit;
        provider_interface;
        provider_source;
        provider_family;
        provider_import;
        summary_digest;
        closure_digest;
        call_snapshot;
        registration_snapshot;
        invocation_ordinal;
        arguments;
        result_type;
        _;
      } ->
      Printf.sprintf
        "imported-model-application %s uid=%s provider=%s interface=%s source=%s family=%s import=%s summary=%s closure=%s call=%s registration=%s ordinal=%d arguments=(%s) : %s"
        callable_path callable_uid provider_unit provider_interface provider_source
        provider_family provider_import summary_digest closure_digest
        call_snapshot registration_snapshot invocation_ordinal
        (String.concat ", "
           (List.map recursive_spec_argument_to_string arguments))
        (aggregate_type_to_string result_type)
  | Aggregate_selector (selector, aggregate) ->
      Printf.sprintf "(%s %s)" (selector_to_string selector)
        (aggregate_term_to_string aggregate)
  | Aggregate_constructor { constructor; arguments } ->
      Printf.sprintf "(ctor.%s#%d%s)" constructor.constructor_name
        constructor.constructor_index
        (match arguments with
        | [] -> ""
        | _ ->
            " "
            ^ String.concat " "
                (List.map recursive_spec_argument_to_string arguments))
  | Aggregate_record { record_type; fields } ->
      Printf.sprintf "(record.%s#%d%s)" record_type.type_name
        record_type.type_index
        (match fields with
        | [] -> ""
        | _ ->
            " "
            ^ String.concat " "
                (List.map
                   (fun (field, value) ->
                     Printf.sprintf "%s=%s" field.Sst.field_name
                       (recursive_spec_argument_to_string value))
                   fields))
  | Aggregate_conditional (condition, consequent, alternative) ->
      Printf.sprintf "(ite %s %s %s)" (boolean_term_to_string condition)
        (aggregate_term_to_string consequent)
        (aggregate_term_to_string alternative)
  | Aggregate_recursive_spec_application { callee; arguments; _ } ->
      Printf.sprintf "(spec.%d.%s%s)" callee.function_index
        callee.function_name
        (match arguments with
        | [] -> ""
        | _ ->
            " "
            ^ String.concat " "
                (List.map recursive_spec_argument_to_string arguments))
  | Aggregate_symbolic_application application ->
      Symbolic_application_private.to_string
        recursive_spec_argument_to_string application

and integer_term_to_string = function
  | Integer_constant value -> Z.to_string value
  | Integer_symbol symbol -> symbol_to_string symbol
  | Integer_add (left, right) ->
      Printf.sprintf "(+ %s %s)" (integer_term_to_string left)
        (integer_term_to_string right)
  | Integer_subtract (left, right) ->
      Printf.sprintf "(- %s %s)" (integer_term_to_string left)
        (integer_term_to_string right)
  | Integer_negate value ->
      Printf.sprintf "(- %s)" (integer_term_to_string value)
  | Integer_multiply_constant (constant, value) ->
      Printf.sprintf "(* %s %s)" (Z.to_string constant)
        (integer_term_to_string value)
  | Integer_absolute_value value ->
      Printf.sprintf "(abs %s)" (integer_term_to_string value)
  | Integer_conditional (condition, consequent, alternative) ->
      Printf.sprintf "(ite %s %s %s)" (boolean_term_to_string condition)
        (integer_term_to_string consequent)
        (integer_term_to_string alternative)
  | Integer_rank_project (domain, aggregate) ->
      Printf.sprintf "(rank[%s] %s)" domain.rank_id
        (aggregate_term_to_string aggregate)
  | Aggregate_tag (type_, aggregate) ->
      Printf.sprintf "(tag.%s %s)" (aggregate_type_to_string type_)
        (aggregate_term_to_string aggregate)
  | Integer_selector (selector, aggregate) ->
      Printf.sprintf "(%s %s)" (selector_to_string selector)
        (aggregate_term_to_string aggregate)
  | Integer_recursive_spec_application { callee; arguments; _ } ->
      Printf.sprintf "(spec.%d.%s%s)" callee.function_index
        callee.function_name
        (match arguments with
        | [] -> ""
        | _ ->
            " "
            ^ String.concat " "
                (List.map recursive_spec_argument_to_string arguments))
  | Integer_symbolic_application application ->
      Symbolic_application_private.to_string
        recursive_spec_argument_to_string application

and recursive_spec_argument_to_string = function
  | Recursive_integer_argument term -> integer_term_to_string term
  | Recursive_boolean_argument term -> boolean_term_to_string term
  | Recursive_aggregate_argument term -> aggregate_term_to_string term
  | Recursive_parametric_argument term -> parametric_term_to_string term

and parametric_term_to_string term =
  match term.parametric_desc with
  | Parametric_symbol symbol -> symbol_to_string symbol
  | Parametric_selector (selector, source) ->
      Printf.sprintf "(%s %s)" (selector_to_string selector)
        (aggregate_term_to_string source)
  | Parametric_conditional (condition, consequent, alternative) ->
      Printf.sprintf "(ite %s %s %s)" (boolean_term_to_string condition)
        (parametric_term_to_string consequent)
        (parametric_term_to_string alternative)
  | Parametric_symbolic_application application ->
      Symbolic_application_private.to_string
        recursive_spec_argument_to_string application

and boolean_term_to_string = function
  | Logical_adt_schema schemas ->
      let schema_to_string schema =
        let type_id =
          Logical_adt_schema_private.descriptor schema
          |> Parametric_adt.type_id
        in
        Printf.sprintf "%s<%s>" type_id.Parametric_type.type_name
          (String.concat ","
             (List.map Parametric_type.to_string
                (Logical_adt_schema_private.arguments schema)))
      in
      Printf.sprintf "(logical-adts %s)"
        (String.concat "," (List.map schema_to_string schemas))
  | Boolean_constant value -> string_of_bool value
  | Boolean_symbol symbol -> symbol_to_string symbol
  | Boolean_not value ->
      Printf.sprintf "(not %s)" (boolean_term_to_string value)
  | Boolean_and (left, right) ->
      Printf.sprintf "(and %s %s)" (boolean_term_to_string left)
        (boolean_term_to_string right)
  | Boolean_or (left, right) ->
      Printf.sprintf "(or %s %s)" (boolean_term_to_string left)
        (boolean_term_to_string right)
  | Forall_term quantifier | Exists_term quantifier ->
      let binders =
        quantifier.boolean_quantifier_binders
        |> List.map (fun binder ->
               Printf.sprintf "(%s:%s)" (symbol_to_string binder)
                 (match binder.sort with
                 | Integer -> "Int"
                 | Boolean -> "Bool"
                 | Aggregate aggregate -> aggregate_type_to_string aggregate
                 | Parametric binder ->
                     Parametric_type.binder_to_string binder))
        |> String.concat " "
      in
      Printf.sprintf "(%s (%s) (! %s%s :qid %s :skolemid %s))"
        (Logic_quantifier_private.kind_to_string
           (Logic_quantifier_private.vector_kind
              quantifier.boolean_quantifier_schema))
        binders
        (boolean_term_to_string quantifier.boolean_quantifier_body)
        (Option.fold ~none:""
           ~some:(fun trigger ->
             " :pattern ("
             ^ application_term_to_string trigger
             ^ ")")
           quantifier.boolean_quantifier_trigger)
        (Logic_quantifier_private.vector_qid quantifier.boolean_quantifier_schema)
        (Logic_quantifier_private.vector_skid quantifier.boolean_quantifier_schema)
  | Integer_compare (comparison, left, right) ->
      Printf.sprintf "(%s %s %s)" (comparison_to_string comparison)
        (integer_term_to_string left) (integer_term_to_string right)
  | Boolean_equal (left, right) ->
      Printf.sprintf "(= %s %s)" (boolean_term_to_string left)
        (boolean_term_to_string right)
  | Boolean_not_equal (left, right) ->
      Printf.sprintf "(distinct %s %s)" (boolean_term_to_string left)
        (boolean_term_to_string right)
  | Boolean_selector (selector, aggregate) ->
      Printf.sprintf "(%s %s)" (selector_to_string selector)
        (aggregate_term_to_string aggregate)
  | Aggregate_equal (left, right) ->
      Printf.sprintf "(= %s %s)" (aggregate_term_to_string left)
        (aggregate_term_to_string right)
  | Parametric_equal (left, right) ->
      Printf.sprintf "(=:%s %s %s)"
        (Parametric_type.binder_to_string left.parametric_sort)
        (parametric_term_to_string left) (parametric_term_to_string right)
  | Boolean_invariant_application
      { invariant_id; model; predicate; value } ->
      Printf.sprintf "(invariant[%s model=%s#%d predicate=%s#%d] %s)"
        invariant_id model.function_name model.function_index
        predicate.function_name predicate.function_index
        (aggregate_term_to_string value)
  | Boolean_recursive_spec_application { callee; arguments; _ } ->
      Printf.sprintf "(spec.%d.%s%s)" callee.function_index
        callee.function_name
        (match arguments with
        | [] -> ""
        | _ ->
            " "
            ^ String.concat " "
                (List.map recursive_spec_argument_to_string arguments))
  | Boolean_specification_application { callee; arguments; _ } ->
      Printf.sprintf "(trigger-spec.%d.%s%s)" callee.function_index
        callee.function_name
        (match arguments with
        | [] -> ""
        | _ ->
            " "
            ^ String.concat " "
                (List.map recursive_spec_argument_to_string arguments))
  | Boolean_symbolic_application application ->
      Symbolic_application_private.to_string
        recursive_spec_argument_to_string application
  | Callback_requires { callback; arguments; _ } ->
      Printf.sprintf "(callback-requires %s#%d%s)" callback.callback_name
        callback.callback_id
        (match arguments with
        | [] -> ""
        | _ -> " " ^ String.concat " "
                 (List.map recursive_spec_argument_to_string arguments))
  | Callback_ensures { application; result } ->
      Printf.sprintf "(callback-ensures %s#%d %s)"
        application.callback.callback_name application.callback.callback_id
        (recursive_spec_argument_to_string result)

and application_term_to_string = function
  | Integer_application term -> integer_term_to_string term
  | Boolean_application term -> boolean_term_to_string term
  | Aggregate_application term -> aggregate_term_to_string term
  | Parametric_application term -> parametric_term_to_string term

let specification_application_name callee type_arguments arguments =
  ignore arguments;
  let identity =
    String.concat "\000"
      ( string_of_int callee.Sst.function_index
      :: callee.function_name
      :: List.map Parametric_type.to_string type_arguments )
  in
  Printf.sprintf "trigger.spec.%d.%s" callee.function_index
    (Digest.to_hex (Digest.string identity))

let rec parametric_conditions term =
  match term.parametric_desc with
  | Parametric_symbol _ | Parametric_selector _
  | Parametric_symbolic_application _ ->
      []
  | Parametric_conditional (condition, consequent, alternative) ->
      condition :: parametric_conditions consequent @ parametric_conditions alternative

let rank_term_to_string term =
  Printf.sprintf "(rank[%s] %s)" term.rank_term_domain.rank_id
    (aggregate_term_to_string term.rank_term_value)

let rec aggregate_has_recursive_specification term =
  match term.aggregate_desc with
  | Aggregate_recursive_spec_application _ -> true
  | Aggregate_imported_model_application { arguments; _ } ->
      List.exists argument_has_recursive_specification arguments
  | Aggregate_symbolic_application application ->
      List.exists argument_has_recursive_specification
        (Symbolic_application_private.arguments application)
  | Aggregate_selector (_, source) ->
      aggregate_has_recursive_specification source
  | Aggregate_constructor { arguments; _ } ->
      List.exists argument_has_recursive_specification arguments
  | Aggregate_record { fields; _ } ->
      List.exists
        (fun (_, argument) -> argument_has_recursive_specification argument)
        fields
  | Aggregate_conditional (condition, consequent, alternative) ->
      boolean_has_recursive_specification condition
      || aggregate_has_recursive_specification consequent
      || aggregate_has_recursive_specification alternative
  | Aggregate_symbol _ -> false

and argument_has_recursive_specification = function
  | Recursive_integer_argument term -> integer_has_recursive_specification term
  | Recursive_boolean_argument term -> boolean_has_recursive_specification term
  | Recursive_aggregate_argument term -> aggregate_has_recursive_specification term
  | Recursive_parametric_argument term ->
      (match term.parametric_desc with
      | Parametric_symbol _ -> false
      | Parametric_selector (_, source) -> aggregate_has_recursive_specification source
      | Parametric_conditional (condition, consequent, alternative) ->
          boolean_has_recursive_specification condition
          || List.exists boolean_has_recursive_specification
               (parametric_conditions consequent @ parametric_conditions alternative)
      | Parametric_symbolic_application application ->
          List.exists argument_has_recursive_specification
            (Symbolic_application_private.arguments application))

and integer_has_recursive_specification = function
  | Integer_recursive_spec_application _ -> true
  | Integer_symbolic_application application ->
      List.exists argument_has_recursive_specification
        (Symbolic_application_private.arguments application)
  | Integer_add (left, right) | Integer_subtract (left, right) ->
      integer_has_recursive_specification left
      || integer_has_recursive_specification right
  | Integer_negate value | Integer_multiply_constant (_, value)
  | Integer_absolute_value value ->
      integer_has_recursive_specification value
  | Integer_conditional (condition, consequent, alternative) ->
      boolean_has_recursive_specification condition
      || integer_has_recursive_specification consequent
      || integer_has_recursive_specification alternative
  | Aggregate_tag (_, aggregate) | Integer_selector (_, aggregate)
  | Integer_rank_project (_, aggregate) ->
      aggregate_has_recursive_specification aggregate
  | Integer_constant _ | Integer_symbol _ ->
      false

and boolean_has_recursive_specification = function
  | Logical_adt_schema _ -> false
  | Forall_term quantifier | Exists_term quantifier ->
      boolean_has_recursive_specification quantifier.boolean_quantifier_body
  | Boolean_recursive_spec_application _ -> true
  | Boolean_specification_application { arguments; _ } ->
      List.exists argument_has_recursive_specification arguments
  | Boolean_symbolic_application application ->
      List.exists argument_has_recursive_specification
        (Symbolic_application_private.arguments application)
  | Callback_requires _ | Callback_ensures _ -> false
  | Boolean_not value -> boolean_has_recursive_specification value
  | Boolean_and (left, right) | Boolean_or (left, right)
  | Boolean_equal (left, right) | Boolean_not_equal (left, right) ->
      boolean_has_recursive_specification left
      || boolean_has_recursive_specification right
  | Integer_compare (_, left, right) ->
      integer_has_recursive_specification left
      || integer_has_recursive_specification right
  | Boolean_selector (_, aggregate) ->
      aggregate_has_recursive_specification aggregate
  | Aggregate_equal (left, right) ->
      aggregate_has_recursive_specification left
      || aggregate_has_recursive_specification right
  | Boolean_invariant_application { value; _ } ->
      aggregate_has_recursive_specification value
  | Parametric_equal (left, right) ->
      List.exists boolean_has_recursive_specification
        (parametric_conditions left @ parametric_conditions right)
  | Boolean_constant _ | Boolean_symbol _ -> false

let obligation_has_recursive_specification (obligation : obligation) =
  List.exists boolean_has_recursive_specification obligation.assumptions
  || List.exists boolean_has_recursive_specification
       obligation.required_preceding_safety
  || List.exists boolean_has_recursive_specification obligation.path_condition
  || boolean_has_recursive_specification obligation.goal

let ( let* ) result continuation =
  match result with Ok value -> continuation value | Error _ as error -> error

let obligation_aggregate_recursive_specifications (obligation : obligation) =
  let add application applications =
    if List.exists (fun candidate -> candidate = application) applications then
      applications
    else application :: applications
  in
  let rec aggregate applications term =
    match term.aggregate_desc with
    | Aggregate_recursive_spec_application
        { callee; arguments; result_type; span; application_identity; _ } ->
        if
          result_type <> term.aggregate_type
          || not
               (Recursive_spec_application_identity.authenticate
                  application_identity)
        then Error "aggregate recursive application has forged identity or sort"
        else
          arguments_fold
            (add (callee, span, application_identity) applications)
            arguments
    | Aggregate_selector (_, source) -> aggregate applications source
    | Aggregate_constructor { arguments; _ } ->
        arguments_fold applications arguments
    | Aggregate_record { fields; _ } ->
        arguments_fold applications (List.map snd fields)
    | Aggregate_conditional (condition, consequent, alternative) ->
        let* applications = boolean applications condition in
        let* applications = aggregate applications consequent in
        aggregate applications alternative
    | Aggregate_imported_model_application { arguments; _ } ->
        arguments_fold applications arguments
    | Aggregate_symbolic_application application ->
        arguments_fold applications
          (Symbolic_application_private.arguments application)
    | Aggregate_symbol _ -> Ok applications
  and argument applications = function
    | Recursive_integer_argument term -> integer applications term
    | Recursive_boolean_argument term -> boolean applications term
    | Recursive_aggregate_argument term -> aggregate applications term
    | Recursive_parametric_argument term -> parametric applications term
  and parametric applications term =
    match term.parametric_desc with
    | Parametric_symbol _ -> Ok applications
    | Parametric_selector (_, source) -> aggregate applications source
    | Parametric_conditional (condition, consequent, alternative) ->
        let* applications = boolean applications condition in
        let* applications = parametric applications consequent in
        parametric applications alternative
    | Parametric_symbolic_application application ->
        arguments_fold applications
          (Symbolic_application_private.arguments application)
  and arguments_fold applications arguments =
    List.fold_left
      (fun result argument_term ->
        let* applications = result in
        argument applications argument_term)
      (Ok applications) arguments
  and integer applications = function
    | Integer_recursive_spec_application { arguments; _ } ->
        arguments_fold applications arguments
    | Integer_symbolic_application application ->
        arguments_fold applications
          (Symbolic_application_private.arguments application)
    | Integer_add (left, right) | Integer_subtract (left, right) ->
        let* applications = integer applications left in
        integer applications right
    | Integer_negate value | Integer_multiply_constant (_, value)
    | Integer_absolute_value value ->
        integer applications value
    | Integer_conditional (condition, consequent, alternative) ->
        let* applications = boolean applications condition in
        let* applications = integer applications consequent in
        integer applications alternative
    | Aggregate_tag (_, value) | Integer_selector (_, value)
    | Integer_rank_project (_, value) ->
        aggregate applications value
    | Integer_constant _ | Integer_symbol _ -> Ok applications
  and boolean applications = function
    | Logical_adt_schema _ -> Ok applications
    | Forall_term quantifier | Exists_term quantifier ->
        boolean applications quantifier.boolean_quantifier_body
    | Boolean_recursive_spec_application { arguments; _ }
    | Boolean_specification_application { arguments; _ }
    | Callback_requires { arguments; _ } ->
        arguments_fold applications arguments
    | Boolean_symbolic_application application ->
        arguments_fold applications
          (Symbolic_application_private.arguments application)
    | Callback_ensures { application = { arguments; _ }; result } ->
        let* applications = arguments_fold applications arguments in
        argument applications result
    | Boolean_not value -> boolean applications value
    | Boolean_and (left, right) | Boolean_or (left, right)
    | Boolean_equal (left, right) | Boolean_not_equal (left, right) ->
        let* applications = boolean applications left in
        boolean applications right
    | Integer_compare (_, left, right) ->
        let* applications = integer applications left in
        integer applications right
    | Boolean_selector (_, value)
    | Boolean_invariant_application { value; _ } ->
        aggregate applications value
    | Aggregate_equal (left, right) ->
        let* applications = aggregate applications left in
        aggregate applications right
    | Parametric_equal (left, right) ->
        List.fold_left
          (fun result condition ->
            let* applications = result in
            boolean applications condition)
          (Ok applications) (parametric_conditions left @ parametric_conditions right)
    | Boolean_constant _ | Boolean_symbol _ -> Ok applications
  in
  let terms =
    obligation.assumptions @ obligation.required_preceding_safety
    @ obligation.path_condition @ [ obligation.goal ]
  in
  List.fold_left
    (fun result term ->
      let* applications = result in
      boolean applications term)
    (Ok []) terms
  |> Result.map List.rev

let obligation_aggregate_types (obligation : obligation) =
  let add type_ types =
    if List.exists (( = ) type_) types then types else type_ :: types
  in
  let add_sst type_id types =
    add
      {
        aggregate_type_index = type_id.Sst.type_index;
        aggregate_type_name = type_id.type_name;
      aggregate_type_arguments = [];
      }
      types
  in
  let sort types = function
    | Aggregate type_ -> add type_ types
    | Integer | Boolean | Parametric _ -> types
  in
  let field_owner types (field : Sst.field_id) =
    match field.field_owner with
    | Sst.Record_owner type_id -> add_sst type_id types
    | Sst.Constructor_owner constructor ->
        add_sst constructor.constructor_type types
  in
  let rec aggregate types term =
    let types = add term.aggregate_type types in
    match term.aggregate_desc with
    | Aggregate_symbol symbol -> sort types symbol.sort
    | Aggregate_imported_model_application
        { arguments; result_type; _ } ->
        arguments_fold (add result_type types) arguments
    | Aggregate_selector (selector, source) ->
        aggregate (sort (add selector.selector_domain types) selector.selector_range)
          source
    | Aggregate_constructor { constructor; arguments } ->
        arguments_fold
          (add_sst constructor.Sst.constructor_type types)
          arguments
    | Aggregate_record { record_type; fields } ->
        List.fold_left
          (fun types (field, argument_term) ->
            argument
              (field_owner (add_sst record_type types) field)
              argument_term)
          (add_sst record_type types) fields
    | Aggregate_conditional (condition, consequent, alternative) ->
        aggregate (aggregate (boolean types condition) consequent) alternative
    | Aggregate_recursive_spec_application
        { arguments; result_type; _ } ->
        arguments_fold (add result_type types) arguments
    | Aggregate_symbolic_application application ->
        arguments_fold types
          (Symbolic_application_private.arguments application)
  and argument types = function
    | Recursive_integer_argument term -> integer types term
    | Recursive_boolean_argument term -> boolean types term
    | Recursive_aggregate_argument term -> aggregate types term
    | Recursive_parametric_argument term ->
        (match term.parametric_desc with
        | Parametric_symbol _ -> types
        | Parametric_selector (_, source) -> aggregate types source
        | Parametric_conditional (condition, consequent, alternative) ->
            let types = boolean types condition in
            List.fold_left boolean types
              (parametric_conditions consequent @ parametric_conditions alternative)
        | Parametric_symbolic_application application ->
            arguments_fold types
              (Symbolic_application_private.arguments application))
  and arguments_fold types arguments =
    List.fold_left argument types arguments
  and integer types = function
    | Integer_recursive_spec_application { arguments; _ } ->
        arguments_fold types arguments
    | Integer_symbolic_application application ->
        arguments_fold types
          (Symbolic_application_private.arguments application)
    | Integer_add (left, right) | Integer_subtract (left, right) ->
        integer (integer types left) right
    | Integer_negate value | Integer_multiply_constant (_, value)
    | Integer_absolute_value value ->
        integer types value
    | Integer_conditional (condition, consequent, alternative) ->
        integer (integer (boolean types condition) consequent) alternative
    | Integer_rank_project (domain, value) ->
        aggregate
          (List.fold_left (fun types type_ -> add type_ types) types
             (rank_domain_component domain))
          value
    | Aggregate_tag (type_, value) -> aggregate (add type_ types) value
    | Integer_selector (selector, value) ->
        aggregate
          (sort (add selector.selector_domain types) selector.selector_range)
          value
    | Integer_constant _ -> types
    | Integer_symbol symbol -> sort types symbol.sort
  and boolean types = function
    | Logical_adt_schema _ -> types
    | Forall_term quantifier | Exists_term quantifier ->
        boolean
          (List.fold_left
             (fun types binder -> sort types binder.sort)
             types quantifier.boolean_quantifier_binders)
          quantifier.boolean_quantifier_body
    | Boolean_recursive_spec_application { arguments; _ }
    | Boolean_specification_application { arguments; _ }
    | Callback_requires { arguments; _ } ->
        arguments_fold types arguments
    | Boolean_symbolic_application application ->
        arguments_fold types
          (Symbolic_application_private.arguments application)
    | Callback_ensures { application = { arguments; _ }; result } ->
        argument (arguments_fold types arguments) result
    | Boolean_not value -> boolean types value
    | Boolean_and (left, right) | Boolean_or (left, right)
    | Boolean_equal (left, right) | Boolean_not_equal (left, right) ->
        boolean (boolean types left) right
    | Integer_compare (_, left, right) -> integer (integer types left) right
    | Boolean_selector (selector, value) ->
        aggregate
          (sort (add selector.selector_domain types) selector.selector_range)
          value
    | Aggregate_equal (left, right) ->
        aggregate (aggregate types left) right
    | Boolean_invariant_application { value; _ } -> aggregate types value
    | Parametric_equal (left, right) ->
        List.fold_left boolean types
          (parametric_conditions left @ parametric_conditions right)
    | Boolean_constant _ -> types
    | Boolean_symbol symbol -> sort types symbol.sort
  in
  obligation.assumptions @ obligation.required_preceding_safety
  @ obligation.path_condition @ [ obligation.goal ]
  |> List.fold_left boolean []
  |> List.rev

let rec aggregate_has_structural_rank term =
  match term.aggregate_desc with
  | Aggregate_imported_model_application _ -> false
  | Aggregate_symbolic_application application ->
      List.exists argument_has_structural_rank
        (Symbolic_application_private.arguments application)
  | Aggregate_selector (_, source) -> aggregate_has_structural_rank source
  | Aggregate_constructor { arguments; _ }
  | Aggregate_recursive_spec_application { arguments; _ } ->
      List.exists argument_has_structural_rank arguments
  | Aggregate_record { fields; _ } ->
      List.exists (fun (_, argument) -> argument_has_structural_rank argument) fields
  | Aggregate_conditional (condition, consequent, alternative) ->
      boolean_has_structural_rank condition
      || aggregate_has_structural_rank consequent
      || aggregate_has_structural_rank alternative
  | Aggregate_symbol _ -> false

and integer_has_structural_rank = function
  | Integer_rank_project _ -> true
  | Integer_add (left, right) | Integer_subtract (left, right) ->
      integer_has_structural_rank left || integer_has_structural_rank right
  | Integer_negate value | Integer_multiply_constant (_, value)
  | Integer_absolute_value value ->
      integer_has_structural_rank value
  | Integer_conditional (condition, consequent, alternative) ->
      boolean_has_structural_rank condition
      || integer_has_structural_rank consequent
      || integer_has_structural_rank alternative
  | Integer_recursive_spec_application { arguments; _ } ->
      List.exists argument_has_structural_rank arguments
  | Integer_symbolic_application application ->
      List.exists argument_has_structural_rank
        (Symbolic_application_private.arguments application)
  | Aggregate_tag (_, aggregate) | Integer_selector (_, aggregate) ->
      aggregate_has_structural_rank aggregate
  | Integer_constant _ | Integer_symbol _ -> false

and argument_has_structural_rank = function
  | Recursive_integer_argument term -> integer_has_structural_rank term
  | Recursive_boolean_argument term -> boolean_has_structural_rank term
  | Recursive_aggregate_argument term -> aggregate_has_structural_rank term
  | Recursive_parametric_argument term ->
      (match term.parametric_desc with
      | Parametric_symbol _ -> false
      | Parametric_selector (_, source) -> aggregate_has_structural_rank source
      | Parametric_conditional (condition, consequent, alternative) ->
          boolean_has_structural_rank condition
          || List.exists boolean_has_structural_rank
               (parametric_conditions consequent @ parametric_conditions alternative)
      | Parametric_symbolic_application application ->
          List.exists argument_has_structural_rank
            (Symbolic_application_private.arguments application))

and boolean_has_structural_rank = function
  | Logical_adt_schema _ -> false
  | Forall_term quantifier | Exists_term quantifier ->
      boolean_has_structural_rank quantifier.boolean_quantifier_body
  | Boolean_not value -> boolean_has_structural_rank value
  | Boolean_and (left, right) | Boolean_or (left, right)
  | Boolean_equal (left, right) | Boolean_not_equal (left, right) ->
      boolean_has_structural_rank left || boolean_has_structural_rank right
  | Integer_compare (_, left, right) ->
      integer_has_structural_rank left || integer_has_structural_rank right
  | Boolean_recursive_spec_application { arguments; _ }
  | Boolean_specification_application { arguments; _ }
  | Callback_requires { arguments; _ } ->
      List.exists argument_has_structural_rank arguments
  | Boolean_symbolic_application application ->
      List.exists argument_has_structural_rank
        (Symbolic_application_private.arguments application)
  | Callback_ensures { application = { arguments; _ }; result } ->
      List.exists argument_has_structural_rank arguments
      || argument_has_structural_rank result
  | Boolean_selector (_, aggregate) ->
      aggregate_has_structural_rank aggregate
  | Aggregate_equal (left, right) ->
      aggregate_has_structural_rank left || aggregate_has_structural_rank right
  | Boolean_invariant_application { value; _ } ->
      aggregate_has_structural_rank value
  | Parametric_equal (left, right) ->
      List.exists boolean_has_structural_rank
        (parametric_conditions left @ parametric_conditions right)
  | Boolean_constant _ | Boolean_symbol _ -> false

let obligation_has_structural_rank (obligation : obligation) =
  List.exists boolean_has_structural_rank obligation.assumptions
  || List.exists boolean_has_structural_rank
       obligation.required_preceding_safety
  || List.exists boolean_has_structural_rank obligation.path_condition
  || boolean_has_structural_rank obligation.goal

let rec aggregate_has_logical_construction term =
  match term.aggregate_desc with
  | Aggregate_constructor _ | Aggregate_record _
  | Aggregate_conditional _ ->
      true
  | Aggregate_selector (_, source) ->
      aggregate_has_logical_construction source
  | Aggregate_recursive_spec_application { arguments; _ } ->
      List.exists argument_has_logical_construction arguments
  | Aggregate_imported_model_application _ -> false
  | Aggregate_symbolic_application application ->
      List.exists argument_has_logical_construction
        (Symbolic_application_private.arguments application)
  | Aggregate_symbol _ -> false

and integer_has_logical_construction = function
  | Integer_add (left, right) | Integer_subtract (left, right) ->
      integer_has_logical_construction left
      || integer_has_logical_construction right
  | Integer_negate value | Integer_multiply_constant (_, value)
  | Integer_absolute_value value ->
      integer_has_logical_construction value
  | Integer_conditional (condition, consequent, alternative) ->
      boolean_has_logical_construction condition
      || integer_has_logical_construction consequent
      || integer_has_logical_construction alternative
  | Integer_recursive_spec_application { arguments; _ } ->
      List.exists argument_has_logical_construction arguments
  | Integer_symbolic_application application ->
      List.exists argument_has_logical_construction
        (Symbolic_application_private.arguments application)
  | Aggregate_tag (_, aggregate) | Integer_selector (_, aggregate)
  | Integer_rank_project (_, aggregate) ->
      aggregate_has_logical_construction aggregate
  | Integer_constant _ | Integer_symbol _ -> false

and argument_has_logical_construction = function
  | Recursive_integer_argument term ->
      integer_has_logical_construction term
  | Recursive_boolean_argument term ->
      boolean_has_logical_construction term
  | Recursive_aggregate_argument term ->
      aggregate_has_logical_construction term
  | Recursive_parametric_argument term ->
      (match term.parametric_desc with
      | Parametric_symbol _ -> false
      | Parametric_selector (_, source) -> aggregate_has_logical_construction source
      | Parametric_conditional (condition, consequent, alternative) ->
          boolean_has_logical_construction condition
          || List.exists boolean_has_logical_construction
               (parametric_conditions consequent @ parametric_conditions alternative)
      | Parametric_symbolic_application application ->
          List.exists argument_has_logical_construction
            (Symbolic_application_private.arguments application))

and boolean_has_logical_construction = function
  | Logical_adt_schema _ -> false
  | Forall_term quantifier | Exists_term quantifier ->
      boolean_has_logical_construction quantifier.boolean_quantifier_body
  | Boolean_not value -> boolean_has_logical_construction value
  | Boolean_and (left, right) | Boolean_or (left, right)
  | Boolean_equal (left, right) | Boolean_not_equal (left, right) ->
      boolean_has_logical_construction left
      || boolean_has_logical_construction right
  | Integer_compare (_, left, right) ->
      integer_has_logical_construction left
      || integer_has_logical_construction right
  | Boolean_recursive_spec_application { arguments; _ }
  | Boolean_specification_application { arguments; _ }
  | Callback_requires { arguments; _ } ->
      List.exists argument_has_logical_construction arguments
  | Boolean_symbolic_application application ->
      List.exists argument_has_logical_construction
        (Symbolic_application_private.arguments application)
  | Callback_ensures { application = { arguments; _ }; result } ->
      List.exists argument_has_logical_construction arguments
      || argument_has_logical_construction result
  | Boolean_selector (_, aggregate)
  | Boolean_invariant_application { value = aggregate; _ } ->
      aggregate_has_logical_construction aggregate
  | Aggregate_equal (left, right) ->
      aggregate_has_logical_construction left
      || aggregate_has_logical_construction right
  | Parametric_equal (left, right) ->
      List.exists boolean_has_logical_construction
        (parametric_conditions left @ parametric_conditions right)
  | Boolean_constant _ | Boolean_symbol _ -> false

let obligation_has_logical_aggregate_construction
    (obligation : obligation) =
  List.exists boolean_has_logical_construction obligation.assumptions
  || List.exists boolean_has_logical_construction
       obligation.required_preceding_safety
  || List.exists boolean_has_logical_construction obligation.path_condition
  || boolean_has_logical_construction obligation.goal

let rec aggregate_rank_domains term =
  match term.aggregate_desc with
  | Aggregate_imported_model_application _ -> []
  | Aggregate_symbolic_application application ->
      List.concat_map argument_rank_domains
        (Symbolic_application_private.arguments application)
  | Aggregate_selector (_, source) -> aggregate_rank_domains source
  | Aggregate_constructor { arguments; _ }
  | Aggregate_recursive_spec_application { arguments; _ } ->
      List.concat_map argument_rank_domains arguments
  | Aggregate_record { fields; _ } ->
      List.concat_map (fun (_, argument) -> argument_rank_domains argument) fields
  | Aggregate_conditional (condition, consequent, alternative) ->
      boolean_rank_domains condition @ aggregate_rank_domains consequent
      @ aggregate_rank_domains alternative
  | Aggregate_symbol _ -> []

and integer_rank_domains = function
  | Integer_rank_project (domain, aggregate) ->
      domain :: aggregate_rank_domains aggregate
  | Integer_add (left, right) | Integer_subtract (left, right) ->
      integer_rank_domains left @ integer_rank_domains right
  | Integer_negate value | Integer_multiply_constant (_, value)
  | Integer_absolute_value value ->
      integer_rank_domains value
  | Integer_conditional (condition, consequent, alternative) ->
      boolean_rank_domains condition @ integer_rank_domains consequent
      @ integer_rank_domains alternative
  | Integer_recursive_spec_application { arguments; _ } ->
      List.concat_map argument_rank_domains arguments
  | Integer_symbolic_application application ->
      List.concat_map argument_rank_domains
        (Symbolic_application_private.arguments application)
  | Aggregate_tag (_, aggregate) | Integer_selector (_, aggregate) ->
      aggregate_rank_domains aggregate
  | Integer_constant _ | Integer_symbol _ -> []

and argument_rank_domains = function
  | Recursive_integer_argument term -> integer_rank_domains term
  | Recursive_boolean_argument term -> boolean_rank_domains term
  | Recursive_aggregate_argument term -> aggregate_rank_domains term
  | Recursive_parametric_argument term ->
      (match term.parametric_desc with
      | Parametric_symbol _ -> []
      | Parametric_selector (_, source) -> aggregate_rank_domains source
      | Parametric_conditional (condition, consequent, alternative) ->
          boolean_rank_domains condition
          @ List.concat_map boolean_rank_domains
              (parametric_conditions consequent @ parametric_conditions alternative)
      | Parametric_symbolic_application application ->
          List.concat_map argument_rank_domains
            (Symbolic_application_private.arguments application))

and boolean_rank_domains = function
  | Logical_adt_schema _ -> []
  | Forall_term quantifier | Exists_term quantifier ->
      boolean_rank_domains quantifier.boolean_quantifier_body
  | Boolean_not value -> boolean_rank_domains value
  | Boolean_and (left, right) | Boolean_or (left, right)
  | Boolean_equal (left, right) | Boolean_not_equal (left, right) ->
      boolean_rank_domains left @ boolean_rank_domains right
  | Integer_compare (_, left, right) ->
      integer_rank_domains left @ integer_rank_domains right
  | Boolean_recursive_spec_application { arguments; _ }
  | Boolean_specification_application { arguments; _ }
  | Callback_requires { arguments; _ } ->
      List.concat_map argument_rank_domains arguments
  | Boolean_symbolic_application application ->
      List.concat_map argument_rank_domains
        (Symbolic_application_private.arguments application)
  | Callback_ensures { application = { arguments; _ }; result } ->
      List.concat_map argument_rank_domains arguments
      @ argument_rank_domains result
  | Boolean_selector (_, aggregate) -> aggregate_rank_domains aggregate
  | Aggregate_equal (left, right) ->
      aggregate_rank_domains left @ aggregate_rank_domains right
  | Boolean_invariant_application { value; _ } ->
      aggregate_rank_domains value
  | Parametric_equal (left, right) ->
      List.concat_map boolean_rank_domains
        (parametric_conditions left @ parametric_conditions right)
  | Boolean_constant _ | Boolean_symbol _ -> []

let obligation_rank_domains (obligation : obligation) =
  (List.concat_map boolean_rank_domains obligation.assumptions
  @ List.concat_map boolean_rank_domains obligation.required_preceding_safety
  @ List.concat_map boolean_rank_domains obligation.path_condition
  @ boolean_rank_domains obligation.goal)
  |> List.sort_uniq (fun left right ->
         String.compare left.rank_id right.rank_id)

let role_to_string = function
  | Input -> "input"
  | Local -> "local"
  | Result -> "result"

let operation_to_string = function
  | Add -> "add"
  | Subtract -> "subtract"
  | Negate -> "negate"
  | Multiply_constant value ->
      Printf.sprintf "multiply-constant(%s)" (Z.to_string value)
  | Successor -> "successor"
  | Predecessor -> "predecessor"
  | Absolute_value -> "absolute-value"

let bound_to_string = function
  | Lower_bound -> "lower"
  | Upper_bound -> "upper"

let span_to_string span =
  Printf.sprintf "%s:%d:%d-%d:%d" (Filename.basename span.Diagnostic.file)
    span.start_pos.line span.start_pos.column span.end_pos.line
    span.end_pos.column

let imported_model_application_snapshot ~callee ~arguments ~result_type ~span =
  String.concat "\000"
    [ callee.Sst.function_name; string_of_int callee.function_index;
      String.concat "\001"
        (List.map recursive_spec_argument_to_string arguments);
      aggregate_type_to_string result_type; span_to_string span ]

let line buffer indent format =
  Printf.ksprintf
    (fun text ->
      Buffer.add_string buffer (String.make indent ' ');
      Buffer.add_string buffer text;
      Buffer.add_char buffer '\n')
    format

let mode_to_string = function
  | Sst.Spec -> "spec"
  | Sst.Proof -> "proof"
  | Sst.Exec -> "exec"

let policy_to_string = function
  | Sst.Default_linear_z3 -> "default-linear/default-z3"
  | Sst.Default_linear_cvc5 -> "default-linear/default-cvc5"
  | Sst.Nonlinear_z3 -> "nonlinear/default-z3"

let provenance_to_string mode = function
  | Sst.Authenticated_typedtree { source_file; _ } ->
      (if mode = Sst.Proof then "proof-typedtree:" else "checked-typedtree:")
      ^ Filename.basename source_file
  | Sst.Raw_semantic_body _ ->
      if mode = Sst.Proof then "proof-raw-semantic"
      else "checked-raw-semantic"

let print_terms buffer indent heading terms =
  line buffer indent "%s" heading;
  match terms with
  | [] -> line buffer (indent + 2) "(none)"
  | terms ->
      List.iter
        (fun term ->
          line buffer (indent + 2) "%s" (boolean_term_to_string term))
        terms

let print_symbols buffer indent symbols =
  line buffer indent "project";
  match symbols with
  | [] -> line buffer (indent + 2) "(none)"
  | symbols ->
      List.iter
        (fun symbol ->
          line buffer (indent + 2) "%s %s %s @ %s"
            (symbol_to_string symbol)
            (match symbol.sort with
            | Integer -> "int"
            | Boolean -> "bool"
            | Parametric binder ->
                "parameter " ^ Parametric_type.binder_to_string binder
            | Aggregate type_ -> "aggregate " ^ aggregate_type_to_string type_)
            (role_to_string symbol.role)
            (span_to_string symbol.span))
        symbols

let print_result buffer indent =
  let rec loop indent = function
    | Unit_result -> line buffer indent "unit"
    | Integer_result symbol ->
        line buffer indent "int %s" (symbol_to_string symbol)
    | Boolean_result symbol ->
        line buffer indent "bool %s" (symbol_to_string symbol)
    | Tuple_result components ->
        line buffer indent "tuple";
        List.iter (loop (indent + 2)) components
    | Aggregate_result symbol ->
        line buffer indent "aggregate %s" (symbol_to_string symbol)
    | Parametric_result symbol ->
        let sort =
          match symbol.sort with
          | Parametric binder -> Parametric_type.binder_to_string binder
          | Integer | Boolean | Aggregate _ -> assert false
        in
        line buffer indent "parameter %s sort=%s" (symbol_to_string symbol) sort
  in
  loop indent

let sst_field_name (field : Sst.field_id) =
  let owner =
    match field.field_owner with
    | Sst.Record_owner type_id ->
        Printf.sprintf "%s#%d" type_id.type_name type_id.type_index
    | Sst.Constructor_owner constructor ->
        Printf.sprintf "%s#%d.%s#%d" constructor.constructor_type.type_name
          constructor.constructor_type.type_index constructor.constructor_name
          constructor.constructor_index
  in
  Printf.sprintf "%s.%s#%d" owner field.field_name field.field_index

let sst_path_name path =
  path
  |> List.map (function
       | Sst.Owned_tree_field field -> "field(" ^ sst_field_name field ^ ")"
       | Sst.Owned_tree_constructor constructor ->
           Printf.sprintf "constructor(%s#%d.%s#%d)"
             constructor.constructor_type.type_name
             constructor.constructor_type.type_index constructor.constructor_name
             constructor.constructor_index)
  |> String.concat "/"

let sst_uniqueness_modality = function
  | Sst.Preserve_uniqueness -> "preserve"
  | Sst.Force_unique -> "force-unique"
  | Sst.Force_aliased -> "force-aliased"

let sst_linearity_modality = function
  | Sst.Preserve_linearity -> "preserve"
  | Sst.Force_once -> "force-once"
  | Sst.Force_many -> "force-many"

let to_string program =
  let buffer = Buffer.create 8192 in
  line buffer 0 "policy %s" (policy_to_string program.policy);
  List.iter
    (fun domain ->
      line buffer 0 "rank-domain id=%s version=%s digest=%s immutable=true"
        domain.rank_id domain.rank_version domain.rank_digest;
      line buffer 2 "component %s"
        (String.concat ","
           (List.map aggregate_type_to_string domain.rank_component));
      List.iter
        (function
          | Ground_rank_base { constructor; rank } ->
              line buffer 2 "ground-base constructor=%s#%d.%s#%d rank=%s"
                constructor.constructor_type.type_name
                constructor.constructor_type.type_index
                constructor.constructor_name constructor.constructor_index
                (Z.to_string rank)
          | Constructor_rank_nonnegative { constructor } ->
              line buffer 2 "constructor-nonnegative constructor=%s#%d.%s#%d"
                constructor.constructor_type.type_name
                constructor.constructor_type.type_index
                constructor.constructor_name constructor.constructor_index
          | Positive_child_rank_smaller
              { constructor; field; child_path; child_type } ->
              line buffer 2
                "positive-child-smaller constructor=%s#%d.%s#%d field=%s#%d%s \
                 child=%s"
                constructor.constructor_type.type_name
                constructor.constructor_type.type_index
                constructor.constructor_name constructor.constructor_index
                field.field_name field.field_index
                (match child_path with
                | [] -> ""
                | path ->
                    "."
                    ^ String.concat "." (List.map string_of_int path))
                (aggregate_type_to_string child_type))
        domain.rank_facts)
    program.rank_domains;
  List.iter
    (fun (declaration : trusted_external_body_declaration) ->
      let mode =
        match declaration.mode with
        | Sst.Proof -> " mode=proof"
        | Sst.Exec | Sst.Spec -> ""
      in
      line buffer 0
        "trusted-external-body-declaration trust=axiomatic%s function=%s#%d \
         declaration-span=%s witness-span=%s requires=%d ensures=%d \
         body=unchecked"
        mode
        declaration.function_ref.function_name
        declaration.function_ref.function_index
        (span_to_string declaration.declaration_span)
        (span_to_string declaration.witness_span)
        declaration.requires_count declaration.ensures_count)
    program.trusted_external_body_declarations;
  List.iter
    (fun execution ->
      line buffer 0 "function %s#%d mode=%s body=%s policy=%s"
        execution.function_ref.function_name
        execution.function_ref.function_index
        (mode_to_string execution.mode)
        (provenance_to_string execution.mode execution.body_provenance)
        (policy_to_string execution.policy);
      List.iter
        (function
          | Trusted_external_specification_use use ->
              line buffer 2
                "trusted-external-specification trust=axiomatic target=%s#%d \
                 wrapper=%s#%d target-span=%s wrapper-span=%s witness-span=%s \
                 call=%s requires=%d ensures=%d result=unconstrained"
                use.target.function_name use.target.function_index
                use.wrapper.function_name use.wrapper.function_index
                (span_to_string use.target_span)
                (span_to_string use.wrapper_span)
                (span_to_string use.witness_span)
                (span_to_string use.call_span)
                use.requires_count use.ensures_count
          | Trusted_external_target_specification_use use ->
              line buffer 2
                "trusted-external-specification \
                 trust=imported-unverified-target consumer-artifact=%s \
                 target-unit=%s target-interface-digest=%s import-crc=%s \
                 target-path=%s target-uid=%s callable-abi=%s wrapper=%s#%d \
                 target-span=%s wrapper-span=%s witness-span=%s call=%s \
                 summary=%s requires=%d ensures=%d target-body=unverified \
                 result=constrained-only-by-ensures"
                use.consumer_artifact_digest use.target_unit
                use.target_interface_digest use.import_crc use.canonical_path
                use.value_uid use.callable_abi_digest use.wrapper.function_name
                use.wrapper.function_index
                (span_to_string use.target_span)
                (span_to_string use.wrapper_span)
                (span_to_string use.witness_span)
                (span_to_string use.call_span)
                use.summary_digest use.requires_count use.ensures_count
          | Trusted_external_body_use use ->
              let mode, call_form =
                match (use.mode, use.call_form) with
                | Sst.Proof, Sst.Proof_call ->
                    (" mode=proof", " call-form=proof")
                | _ -> ("", "")
              in
              line buffer 2
                "trusted-external-body trust=axiomatic%s%s function=%s#%d \
                 declaration-span=%s witness-span=%s call=%s requires=%d \
                 ensures=%d body=unchecked result=constrained-only-by-ensures"
                mode call_form
                use.function_ref.function_name use.function_ref.function_index
                (span_to_string use.declaration_span)
                (span_to_string use.witness_span)
                (span_to_string use.call_span)
                use.requires_count use.ensures_count)
        execution.trusted_summary_uses;
      List.iter
        (fun (transition : Sst.owned_tree_transition) ->
          line buffer 2
            "owned-tree-transition policy=functional-no-heap root=%s#%d \
             pre-version=%d successor-version=%d target=%s mutability=mutable \
             uniqueness=%s linearity=%s rhs=%s invalidated=%s"
            transition.root.name transition.root.id transition.pre_version
            transition.successor_version
            (sst_field_name transition.target_field)
            (sst_uniqueness_modality
               transition.target_modalities.uniqueness_modality)
            (sst_linearity_modality
               transition.target_modalities.linearity_modality)
            (match transition.rhs_provenance with
            | Sst.Ground_owned_tree_value -> "ground"
            | Sst.Guarded_descendant_move _ -> "guarded-descendant-move")
            (String.concat ","
               (List.map string_of_int transition.invalidated_cursor_ids));
          Option.iter
            (fun cursor ->
              line buffer 4 "cursor=%s#%d version=%d guarded-path=%s"
                cursor.Sst.cursor_binding.name cursor.cursor_binding.id
                cursor.root_version (sst_path_name cursor.guarded_path))
            transition.cursor;
          (match transition.rhs_provenance with
          | Sst.Guarded_descendant_move cursor ->
              line buffer 4 "source-path=%s"
                (sst_path_name cursor.guarded_path)
          | Sst.Ground_owned_tree_value -> ());
          List.iter
            (function
              | Sst.Reconstruct_record
                  { record_type; changed_field; preserved_fields } ->
                  line buffer 4
                    "successor-reconstruct record=%s#%d changed=%s \
                     preserved-siblings=%s"
                    record_type.type_name record_type.type_index
                    (sst_field_name changed_field)
                    (String.concat ","
                       (List.map sst_field_name preserved_fields))
              | Sst.Reconstruct_constructor constructor ->
                  line buffer 4 "successor-reconstruct constructor=%s#%d.%s#%d"
                    constructor.constructor_type.type_name
                    constructor.constructor_type.type_index
                    constructor.constructor_name constructor.constructor_index)
            transition.reconstruction)
        execution.owned_tree_transitions;
      List.iter
        (fun write ->
          let transition = write.shared_write_transition in
          line buffer 2
            "shared-heap-write policy=bounded-shared-scalar-heap-v1 path=%d \
             field=%s predecessor-epoch=%d successor-epoch=%d location=%s \
             value=%s"
            transition.shared_path_id
            (sst_field_name transition.shared_target_field)
            transition.shared_predecessor_epoch
            transition.shared_successor_epoch
            (aggregate_term_to_string write.shared_write_location)
            (integer_term_to_string write.shared_write_value))
        execution.shared_scalar_heap_writes;
      List.iter
        (fun read ->
          line buffer 2
            "shared-heap-read policy=bounded-shared-scalar-heap-v1 path=%d \
             field=%s epoch=%d view=%s location=%s term=%s"
            read.shared_read_path_id (sst_field_name read.shared_read_field)
            read.shared_read_epoch
            (if read.shared_read_entry_view then "entry" else "current")
            (aggregate_term_to_string read.shared_read_location)
            (integer_term_to_string read.shared_read_term))
        execution.shared_scalar_heap_reads;
      List.iter
        (fun obligation ->
          match obligation.kind with
          | Arithmetic_safety
              { operation; mathematical_result; violated_bound } ->
              line buffer 2 "vc %d arithmetic-%s operation=%s @ %s"
                obligation.obligation_index
                (bound_to_string violated_bound)
                (operation_to_string operation)
                (span_to_string obligation.span);
              line buffer 4 "mathematical-result %s"
                (integer_term_to_string mathematical_result);
              print_terms buffer 4 "assumptions" obligation.assumptions;
              print_terms buffer 4 "requires-preceding-safety"
                obligation.required_preceding_safety;
              print_terms buffer 4 "path" obligation.path_condition;
              line buffer 4 "goal %s"
                (boolean_term_to_string obligation.goal);
              print_symbols buffer 4 obligation.projection_symbols
          | Assertion { assertion_ordinal } ->
              line buffer 2 "vc %d assertion ordinal=%d @ %s"
                obligation.obligation_index assertion_ordinal
                (span_to_string obligation.span);
              print_terms buffer 4 "assumptions" obligation.assumptions;
              print_terms buffer 4 "requires-preceding-safety"
                obligation.required_preceding_safety;
              print_terms buffer 4 "path" obligation.path_condition;
              line buffer 4 "goal %s"
                (boolean_term_to_string obligation.goal);
              print_symbols buffer 4 obligation.projection_symbols
          | Local_assertion { local_assertion_ordinal } ->
              line buffer 2 "vc %d local-assertion ordinal=%d @ %s"
                obligation.obligation_index local_assertion_ordinal
                (span_to_string obligation.span);
              print_terms buffer 4 "assumptions" obligation.assumptions;
              print_terms buffer 4 "requires-preceding-safety"
                obligation.required_preceding_safety;
              print_terms buffer 4 "path" obligation.path_condition;
              line buffer 4 "goal %s"
                (boolean_term_to_string obligation.goal);
              print_symbols buffer 4 obligation.projection_symbols
          | Postcondition { postcondition_ordinal; declaration_span } ->
              line buffer 2 "vc %d postcondition ordinal=%d declaration=%s @ %s"
                obligation.obligation_index postcondition_ordinal
                (span_to_string declaration_span)
                (span_to_string obligation.span);
              print_terms buffer 4 "assumptions" obligation.assumptions;
              print_terms buffer 4 "requires-preceding-safety"
                obligation.required_preceding_safety;
              print_terms buffer 4 "path" obligation.path_condition;
              line buffer 4 "goal %s"
                (boolean_term_to_string obligation.goal);
              print_symbols buffer 4 obligation.projection_symbols
          | Call_precondition
              {
                callee;
                precondition_ordinal;
                declaration_span;
                call_span;
              } ->
              line buffer 2
                "vc %d call-precondition callee=%s#%d ordinal=%d \
                 declaration=%s call=%s @ %s"
                obligation.obligation_index callee.function_name
                callee.function_index precondition_ordinal
                (span_to_string declaration_span)
                (span_to_string call_span)
                (span_to_string obligation.span);
              print_terms buffer 4 "assumptions" obligation.assumptions;
              print_terms buffer 4 "requires-preceding-safety"
                obligation.required_preceding_safety;
              print_terms buffer 4 "path" obligation.path_condition;
              line buffer 4 "goal %s"
                (boolean_term_to_string obligation.goal);
              print_symbols buffer 4 obligation.projection_symbols
          | Callback_precondition { callback; call_span } ->
              line buffer 2 "vc %d callback-precondition callback=%s#%d call=%s @ %s"
                obligation.obligation_index callback.callback_name callback.callback_id
                (span_to_string call_span) (span_to_string obligation.span);
              print_terms buffer 4 "assumptions" obligation.assumptions;
              print_terms buffer 4 "requires-preceding-safety"
                obligation.required_preceding_safety;
              print_terms buffer 4 "path" obligation.path_condition;
              line buffer 4 "goal %s" (boolean_term_to_string obligation.goal);
              print_symbols buffer 4 obligation.projection_symbols
          | Invariant_validity
              { invariant_id; abstract_type; model; predicate; operation; boundary }
            ->
              let boundary =
                match boundary with
                | Constructor_establishment -> "constructor-establishment"
                | Transition_preservation
                    {
                      transition_kind;
                      root_binding_id;
                      pre_version;
                      successor_version;
                    } ->
                    Printf.sprintf "transition-preservation:%s:root=%d:v%d->v%d"
                      (match transition_kind with
                      | Direct_root_transition -> "root"
                      | Nested_transition -> "nested"
                      | Rebase_transition -> "rebase")
                      root_binding_id pre_version successor_version
                | Call_argument { callee; argument_index } ->
                    Printf.sprintf "call-argument:%s#%d:arg=%d"
                      callee.function_name callee.function_index argument_index
                | Call_result { callee } ->
                    Printf.sprintf "call-result:%s#%d" callee.function_name
                      callee.function_index
                | Function_return -> "function-return"
                | Shared_invariant_close { entry_epoch; final_epoch } ->
                    Printf.sprintf "shared-invariant-close:epoch=%d->%d"
                      entry_epoch final_epoch
                | Terminal_observation { operation; snapshot } ->
                    Printf.sprintf "terminal-%s:%s#%d"
                      (if snapshot then "snapshot" else "read")
                      operation.function_name operation.function_index
              in
              line buffer 2
                "vc %d invariant-validity boundary=%s \
                 invariant=%s type=%s model=%s#%d predicate=%s#%d \
                 operation=%s#%d @ %s"
                obligation.obligation_index boundary invariant_id
                (aggregate_type_to_string abstract_type)
                model.function_name model.function_index
                predicate.function_name predicate.function_index
                operation.function_name operation.function_index
                (span_to_string obligation.span);
              print_terms buffer 4 "assumptions" obligation.assumptions;
              print_terms buffer 4 "requires-preceding-safety"
                obligation.required_preceding_safety;
              print_terms buffer 4 "path" obligation.path_condition;
              line buffer 4 "goal %s"
                (boolean_term_to_string obligation.goal);
              print_symbols buffer 4 obligation.projection_symbols
          | Entry_measure_nonnegative { declaration_span } ->
              line buffer 2
                "vc %d entry-measure-nonnegative declaration=%s @ %s"
                obligation.obligation_index
                (span_to_string declaration_span)
                (span_to_string obligation.span);
              print_terms buffer 4 "assumptions" obligation.assumptions;
              print_terms buffer 4 "requires-preceding-safety"
                obligation.required_preceding_safety;
              print_terms buffer 4 "path" obligation.path_condition;
              line buffer 4 "goal %s"
                (boolean_term_to_string obligation.goal);
              print_symbols buffer 4 obligation.projection_symbols
          | Recursive_call_measure_nonnegative
              { callee; declaration_span; call_span } ->
              line buffer 2
                "vc %d recursive-call-measure-nonnegative callee=%s#%d \
                 declaration=%s call=%s @ %s"
                obligation.obligation_index callee.function_name
                callee.function_index
                (span_to_string declaration_span)
                (span_to_string call_span)
                (span_to_string obligation.span);
              print_terms buffer 4 "assumptions" obligation.assumptions;
              print_terms buffer 4 "requires-preceding-safety"
                obligation.required_preceding_safety;
              print_terms buffer 4 "path" obligation.path_condition;
              line buffer 4 "goal %s"
                (boolean_term_to_string obligation.goal);
              print_symbols buffer 4 obligation.projection_symbols
          | Recursive_call_strict_descent
              { callee; declaration_span; call_span } ->
              line buffer 2
                "vc %d recursive-call-strict-descent callee=%s#%d \
                 declaration=%s call=%s @ %s"
                obligation.obligation_index callee.function_name
                callee.function_index
                (span_to_string declaration_span)
                (span_to_string call_span)
                (span_to_string obligation.span);
              print_terms buffer 4 "assumptions" obligation.assumptions;
              print_terms buffer 4 "requires-preceding-safety"
                obligation.required_preceding_safety;
              print_terms buffer 4 "path" obligation.path_condition;
              line buffer 4 "goal %s"
                (boolean_term_to_string obligation.goal);
              print_symbols buffer 4 obligation.projection_symbols)
        execution.obligations;
      List.iteri
        (fun index exit ->
          line buffer 2 "exit %d" index;
          print_terms buffer 4 "assumptions" exit.assumptions;
          print_terms buffer 4 "path" exit.path_condition;
          line buffer 4 "result";
          print_result buffer 6 exit.result;
          print_symbols buffer 4 exit.projection_symbols)
        execution.exits)
    program.functions;
  Buffer.contents buffer
