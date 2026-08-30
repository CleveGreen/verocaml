let ( let* ) result continuation =
  match result with Ok value -> continuation value | Error _ as error -> error
type direct_candidate = {
  ident : Ident.t;
  function_id : Sst.function_id;
  value_binding : Typedtree.value_binding;
  type_substitutions : (int * Types.type_expr) list;
  type_binders : Parametric_type.binder list;
}

type retained_proof_region = {
  proof_manifest_text : string;
  proof_shadow_parameters : Typedtree.function_param list;
  proof_region_application : Typedtree.expression;
}
let select_candidate = function
  | [ candidate ] -> Some candidate
  | [] | _ :: _ :: _ -> None
let has_callback_formal candidate =
  match candidate.value_binding.Typedtree.vb_expr.exp_desc with
  | Typedtree.Texp_function { params; _ } ->
      List.exists
        (fun parameter ->
          match parameter.Typedtree.fp_kind with
          | Typedtree.Tparam_pat pattern
          | Typedtree.Tparam_optional_default (pattern, _, _) ->
              Typedtree_callback_private.callback_arrow_type pattern.pat_type)
        params
  | _ -> false
type 'error direct_lowering_services = {
  normalized_type :
    direct_candidate ->
    Location.t ->
    Types.type_expr ->
    (Parametric_type.t, 'error) result;
  optional_carrier :
    Location.t -> Parametric_type.t -> (Parametric_type.t, 'error) result;
  lower_expression :
    Typedtree.expression -> (Sst.expression, 'error) result;
  callback_actual :
    Callback_shape_private.t ->
    string option ->
    Typedtree.expression ->
    (Sst.call_argument, 'error) result;
  callback_candidate_contract : (unit, 'error) result;
  parameter_label : Typedtree.arg_label -> string option;
  policy_error : Location.t -> string -> 'error;
  polymorphic_error : Location.t -> 'error;
  higher_order_error : Location.t -> 'error;
  span : Location.t -> Sst.span;
  current : Ident.t option;
}
let shape_for_type services candidate location typ =
  let env = candidate.value_binding.Typedtree.vb_expr.exp_env in
  match
    Typedtree_callback_private.shape ~env
      ~lower:(services.normalized_type candidate)
      ~location
      (Ctype.full_expand ~may_forget_scope:false env typ)
  with
  | Ok shape -> Ok shape
  | Error (Typedtree_callback_private.Lowering_error error) -> Error error
  | Error (Typedtree_callback_private.Invalid_shape message) ->
      Error (services.policy_error location message)
let rec lower_mixed services candidate call_location lowered formal_types
    formal_labels actual_types parameters arguments =
  match parameters, arguments with
  | [], [] ->
      Ok
        ( List.rev lowered,
          List.rev formal_types,
          List.rev formal_labels,
          List.rev actual_types )
  | (parameter : Typedtree.function_param) :: parameters,
    (label, Typedtree.Arg ((source : Typedtree.expression), _)) :: arguments ->
      let expected_label = services.parameter_label parameter.fp_arg_label in
      let actual_label = services.parameter_label label in
      if expected_label <> actual_label then
        Error
          (services.policy_error source.exp_loc
             "callback call labels do not match compiler formal order")
      else (
        match parameter.fp_kind with
        | Typedtree.Tparam_optional_default (pattern, _, _) ->
            if Typedtree_callback_private.callback_arrow_type pattern.pat_type
            then
              Error
                (services.policy_error parameter.fp_loc
                   "optional callback endpoints are unsupported")
            else
              let* value = services.lower_expression source in
              let value =
                match value.Sst.expression_desc with
                | Sst.Optional_absent | Sst.Optional_present _
                | Sst.Optional_forward _ ->
                    value
                | _ ->
                    {
                      value with
                      expression_desc = Sst.Optional_forward value;
                    }
              in
              let* payload =
                services.normalized_type candidate pattern.pat_loc
                  pattern.pat_type
              in
              let* carrier =
                services.optional_carrier pattern.pat_loc payload
              in
              lower_mixed services candidate call_location
                (Typedtree_callback_private.Lowered_call_argument
                   (Sst.Value_argument { label = actual_label; value })
                :: lowered)
                (carrier :: formal_types)
                (expected_label :: formal_labels)
                (value.typ :: actual_types) parameters arguments
        | Typedtree.Tparam_pat pattern ->
            if
              Typedtree_callback_private.callback_arrow_type pattern.pat_type
            then
              let* shape =
                shape_for_type services candidate pattern.pat_loc
                  pattern.pat_type
              in
              lower_mixed services candidate call_location
                (Typedtree_callback_private.Pending_callback_argument
                   { shape; label = actual_label; source }
                :: lowered)
                formal_types formal_labels actual_types parameters arguments
            else
              let* value = services.lower_expression source in
              let* formal_type =
                services.normalized_type candidate pattern.pat_loc
                  pattern.pat_type
              in
              lower_mixed services candidate call_location
                (Typedtree_callback_private.Lowered_call_argument
                   (Sst.Value_argument { label = actual_label; value })
                :: lowered)
                (formal_type :: formal_types)
                (expected_label :: formal_labels)
                (value.typ :: actual_types) parameters arguments)
  | _ :: _, (_, Typedtree.Omitted _) :: _ ->
      let error = if has_callback_formal candidate then services.policy_error
        else fun location _ -> services.higher_order_error location in
      Error (error call_location "higher-order call is partial")
  | [], _ :: _ | _ :: _, [] ->
      let error = if has_callback_formal candidate then services.policy_error
        else fun location _ -> services.higher_order_error location in
      Error (error call_location "higher-order call arity differs from its formals")
let terminal_body expression =
  match expression.Typedtree.exp_desc with
  | Typedtree.Texp_function
      { body = Typedtree.Tfunction_body body; _ } ->
      Some body
  | _ -> None

let infer_type_arguments services candidate ~formal_types ~formal_labels
    ~actual_types ~formal_result ~result_type location =
  if candidate.type_binders = [] then Ok []
  else
    match
      Parametric_lowering_private.infer_labeled_type_arguments
        ~binders:candidate.type_binders ~formal_types ~formal_labels
        ~actual_types ~actual_labels:formal_labels ~formal_result
        ~actual_result:result_type
    with
    | Ok arguments -> Ok arguments
    | Error _ -> Error (services.polymorphic_error location)

let resolve_mixed services substitutions lowered =
  let rec resolve resolved = function
    | [] -> Ok (List.rev resolved)
    | Typedtree_callback_private.Lowered_call_argument argument :: rest ->
        resolve (argument :: resolved) rest
    | Typedtree_callback_private.Pending_callback_argument
        { shape; label; source }
      :: rest ->
        let shape = Callback_shape_private.instantiate substitutions shape in
        let* argument = services.callback_actual shape label source in
        resolve (argument :: resolved) rest
  in
  resolve [] lowered

let lower_direct_candidate services
    ~application:(application : Typedtree.expression) ~result_type
    ~source_arguments candidate =
  match candidate.value_binding.vb_expr.exp_desc with
  | Typedtree.Texp_function
      {
        params;
        body = Typedtree.Tfunction_body _;
        _;
      }
    when List.length params = List.length source_arguments ->
      let callback_contract =
        if has_callback_formal candidate then services.callback_candidate_contract
        else Ok ()
      in
      let* () =
        if Result.is_error callback_contract
           && not (Callback_shape_private.has_immediate_callback_actual
                     ~is_callback:Typedtree_callback_private.callback_arrow_type
                     params source_arguments)
        then callback_contract
        else Ok ()
      in
      let* lowered, formal_types, formal_labels, actual_types =
        lower_mixed services candidate application.exp_loc [] [] [] [] params
          source_arguments
      in
      let* formal_result =
        match terminal_body candidate.value_binding.vb_expr with
        | Some body ->
            services.normalized_type candidate body.exp_loc body.exp_type
        | None -> Error (services.higher_order_error application.exp_loc)
      in
      let* type_arguments =
        infer_type_arguments services candidate ~formal_types ~formal_labels
          ~actual_types ~formal_result ~result_type application.exp_loc
      in
      let recursive =
        Option.fold ~none:false
          ~some:(fun ident -> Ident.same candidate.ident ident)
          services.current
      in
      let* () =
        if
          recursive
          && candidate.type_binders <> []
          && not
               (List.equal Parametric_type.equal type_arguments
                  (List.map
                     (fun binder -> Parametric_type.Parameter binder)
                     candidate.type_binders))
        then Error (services.polymorphic_error application.exp_loc)
        else Ok ()
      in
      let substitutions =
        if
          List.length candidate.type_binders = List.length type_arguments
        then List.combine candidate.type_binders type_arguments
        else []
      in
      let* arguments = resolve_mixed services substitutions lowered in
      let* () =
        callback_contract
      in
      Ok
        {
          Sst.expression_desc =
            Sst.Direct_call
              {
                call_form = Sst.Unclassified_call;
                callee = candidate.function_id;
                type_arguments;
                arguments;
                recursive;
              };
          typ = result_type;
          span = services.span application.exp_loc;
        }
  | Typedtree.Texp_function _ | Typedtree.Texp_ident _
  | Typedtree.Texp_constant _ | Typedtree.Texp_let _
  | Typedtree.Texp_letmutable _ | Typedtree.Texp_apply _
  | Typedtree.Texp_match _ | Typedtree.Texp_try _
  | Typedtree.Texp_tuple _ | Typedtree.Texp_unboxed_tuple _
  | Typedtree.Texp_construct _ | Typedtree.Texp_variant _
  | Typedtree.Texp_record _ | Typedtree.Texp_record_unboxed_product _
  | Typedtree.Texp_atomic_loc _ | Typedtree.Texp_field _
  | Typedtree.Texp_unboxed_field _ | Typedtree.Texp_setfield _
  | Typedtree.Texp_array _ | Typedtree.Texp_idx _
  | Typedtree.Texp_list_comprehension _
  | Typedtree.Texp_array_comprehension _ | Typedtree.Texp_ifthenelse _
  | Typedtree.Texp_sequence _ | Typedtree.Texp_while _
  | Typedtree.Texp_for _ | Typedtree.Texp_send _ | Typedtree.Texp_new _
  | Typedtree.Texp_instvar _ | Typedtree.Texp_mutvar _
  | Typedtree.Texp_setinstvar _ | Typedtree.Texp_setmutvar _
  | Typedtree.Texp_override _ | Typedtree.Texp_letmodule _
  | Typedtree.Texp_letexception _ | Typedtree.Texp_assert _
  | Typedtree.Texp_lazy _ | Typedtree.Texp_object _
  | Typedtree.Texp_pack _ | Typedtree.Texp_letop _
  | Typedtree.Texp_unreachable | Typedtree.Texp_extension_constructor _
  | Typedtree.Texp_open _ | Typedtree.Texp_probe _
  | Typedtree.Texp_probe_is_enabled _ | Typedtree.Texp_exclave _
  | Typedtree.Texp_src_pos | Typedtree.Texp_overwrite _
  | Typedtree.Texp_hole _ | Typedtree.Texp_quotation _
  | Typedtree.Texp_antiquotation _ | Typedtree.Texp_eval _ ->
      Error (services.higher_order_error application.exp_loc)

type direct_clause = Callback_contract_private.execution_clause = {
  ordinal : int;
  span : Sst.span;
  binder : Sst.pattern option;
  payload : Sst.expression;
}

type ('obligation, 'path) evaluation = {
  obligations : 'obligation list;
  paths : 'path list;
}

type
  ( 'context,
    'state,
    'summary,
    'evaluated,
    'value,
    'function_ref,
    'assumption,
    'vc_kind,
    'symbol_role,
    'obligation,
    'error )
  direct_runtime = {
  evaluate :
    'context ->
    Sst.expression ->
    'state ->
    (('obligation, 'evaluated) evaluation, 'error) result;
  value : 'evaluated -> 'value;
  state : 'evaluated -> 'state;
  evaluated : 'value -> 'state -> 'evaluated;
  function_ref : 'context -> 'function_ref;
  function_name : 'context -> string;
  callback_environment :
    'context -> (Sst.callback_binding * Sst.callback_binding) list;
  definition : 'summary -> Sst.function_definition;
  requires : 'summary -> direct_clause list;
  ensures : 'summary -> direct_clause list;
  contract_context :
    'context ->
    Sst.function_definition ->
    (int * 'value) list ->
    (int * 'value) list option ->
    (Sst.callback_binding * Sst.callback_binding) list ->
    'context;
  environment : 'state -> (int * 'value) list;
  with_environment : 'state -> (int * 'value) list -> 'state;
  with_assumptions : 'state -> 'assumption list -> 'state;
  bind_pattern :
    string ->
    (int * 'value) list ->
    Sst.pattern ->
    'value ->
    (((int * 'value) list * 'assumption list), 'error) result;
  fresh_value :
    'state ->
    source_name:string ->
    role:'symbol_role ->
    span:Sst.span ->
    project:bool ->
    Sst.typ ->
    (('value * 'state), 'error) result;
  result_role : 'symbol_role;
  expect_boolean :
    string -> Sst.span -> 'value -> ('assumption, 'error) result;
  precondition_kind :
    Sst.function_definition -> direct_clause -> Sst.span -> 'vc_kind;
  emit_goal :
    'function_ref ->
    'vc_kind ->
    Sst.span ->
    'assumption ->
    'state ->
    'obligation * 'state;
  malformed : string -> Sst.span -> string -> 'error;
}

let malformed_direct runtime context span message =
  Error (runtime.malformed (runtime.function_name context) span message)

let direct_edges runtime context (expression : Sst.expression) summary
    arguments =
  let definition = runtime.definition summary in
  if List.length definition.Sst.parameters <> List.length arguments then
    malformed_direct runtime context expression.Sst.span
      "callback-bearing direct-call arity mismatch"
  else
    List.fold_left2
      (fun result formal argument ->
        let* values, callbacks = result in
        match formal, argument with
        | Sst.Value_parameter parameter, Sst.Value_argument { value; _ } ->
            if Option.is_some parameter.optional_default then
              malformed_direct runtime context expression.span
                "callback-bearing direct call has an optional value formal"
            else Ok ((parameter, value) :: values, callbacks)
        | ( Sst.Callback_parameter formal,
            Sst.Callback_argument { callback; _ } ) ->
            Ok (values, (formal.binding, callback) :: callbacks)
        | Sst.Value_parameter _, Sst.Callback_argument _
        | Sst.Callback_parameter _, Sst.Value_argument _ ->
            malformed_direct runtime context expression.span
              "callback/value direct-call edge differs from its formal")
      (Ok ([], runtime.callback_environment context))
      definition.parameters arguments
    |> Result.map (fun (values, callbacks) -> (List.rev values, callbacks))

let evaluate_values runtime context values state =
  let rec loop obligations paths = function
    | [] -> Ok { obligations; paths }
    | (parameter, argument) :: rest ->
        let* evaluated =
          let rec contexts obligations outputs = function
            | [] -> Ok { obligations; paths = List.rev outputs }
            | (state, values) :: pending ->
                let* argument = runtime.evaluate context argument state in
                contexts (obligations @ argument.obligations)
                  (List.rev_append
                     (List.map
                        (fun evaluated ->
                          ( runtime.state evaluated,
                            (parameter, runtime.value evaluated) :: values ))
                        argument.paths)
                     outputs)
                  pending
          in
          contexts [] [] paths
        in
        loop (obligations @ evaluated.obligations) evaluated.paths rest
  in
  loop [] [ (state, []) ] values

let bind_direct_environment runtime context (expression : Sst.expression)
    arguments values state =
  let function_name = runtime.function_name context in
  let* environment, ranges =
    List.fold_left
      (fun result (parameter, actual) ->
        let* environment, ranges = result in
        let* environment, nested =
          runtime.bind_pattern function_name environment
            parameter.Sst.pattern actual
        in
        Ok (environment, ranges @ nested))
      (Ok ([], [])) values
  in
  let* environment =
    List.fold_left
      (fun result -> function
        | Sst.Value_argument _ -> result
        | Sst.Callback_argument { callback; _ } ->
            List.fold_left
              (fun result
                   (capture : Callback_certificate_private.capture) ->
                let* environment = result in
                match
                  List.assoc_opt capture.binding_id
                    (runtime.environment state)
                with
                | Some value ->
                    Ok ((capture.binding_id, value) :: environment)
                | None ->
                    malformed_direct runtime context expression.Sst.span
                      "callback capture is absent at its direct call edge")
              result
              (Callback_certificate_private.captures
                 callback.callback_certificate))
      (Ok environment) arguments
  in
  Ok (environment, ranges)

let rec prove_requires runtime context (expression : Sst.expression) summary
    spec_context environment obligations states = function
  | [] -> Ok { obligations; paths = states }
  | clause :: rest ->
      let function_ref = runtime.function_ref context in
      let function_name = runtime.function_name context in
      let* evaluated =
        let prove state =
          let caller_environment = runtime.environment state in
          let* predicate =
            runtime.evaluate spec_context clause.payload
              (runtime.with_environment state environment)
          in
          let rec collect obligations states = function
            | [] -> Ok { obligations; paths = List.rev states }
            | evaluated :: remaining ->
                let* goal =
                  runtime.expect_boolean function_name clause.span
                    (runtime.value evaluated)
                in
                let obligation, state =
                  runtime.emit_goal function_ref
                    (runtime.precondition_kind
                       (runtime.definition summary) clause expression.Sst.span)
                    expression.span goal (runtime.state evaluated)
                in
                collect (obligation :: obligations)
                  (runtime.with_environment state caller_environment :: states)
                  remaining
          in
          collect predicate.obligations [] predicate.paths
        in
        let rec contexts obligations paths = function
          | [] -> Ok { obligations; paths = List.rev paths }
          | state :: pending ->
              let* proved = prove state in
              contexts (obligations @ proved.obligations)
                (List.rev_append proved.paths paths) pending
        in
        contexts [] [] states
      in
      prove_requires runtime context expression summary spec_context
        environment (obligations @ evaluated.obligations)
        evaluated.paths rest

let assume_posts runtime context summary spec_context environment result state =
  let function_name = runtime.function_name context in
  let rec loop obligations states = function
    | [] ->
        Ok
          {
            obligations;
            paths =
              List.map (fun state -> runtime.evaluated result state) states;
          }
    | clause :: rest ->
        let* evaluated =
          let assume state =
            let caller_environment = runtime.environment state in
            let* clause_environment, ranges =
              match clause.binder with
              | None -> Ok (environment, [])
              | Some binder ->
                  runtime.bind_pattern function_name environment binder result
            in
            let* predicate =
              runtime.evaluate spec_context clause.payload
                (runtime.with_assumptions
                   (runtime.with_environment state clause_environment)
                   ranges)
            in
            let rec collect states = function
              | [] -> Ok (List.rev states)
              | evaluated :: remaining ->
                  let* assumption =
                    runtime.expect_boolean function_name clause.span
                      (runtime.value evaluated)
                  in
                  collect
                    (runtime.with_environment
                       (runtime.with_assumptions
                          (runtime.state evaluated)
                          [ assumption ])
                       caller_environment
                    :: states)
                    remaining
            in
            let* paths = collect [] predicate.paths in
            Ok { obligations = predicate.obligations; paths }
          in
          let rec contexts obligations paths = function
            | [] -> Ok { obligations; paths = List.rev paths }
            | state :: pending ->
                let* assumed = assume state in
                contexts (obligations @ assumed.obligations)
                  (List.rev_append assumed.paths paths) pending
          in
          contexts [] [] states
        in
        loop (obligations @ evaluated.obligations) evaluated.paths rest
  in
  loop [] [ state ] (runtime.ensures summary)

let instantiate_direct runtime context (expression : Sst.expression) summary
    callback_environment arguments (caller_state, reversed_values) =
  let values = List.rev reversed_values in
  let* environment, ranges =
    bind_direct_environment runtime context expression arguments values
      caller_state
  in
  let caller_state = runtime.with_assumptions caller_state ranges in
  let definition = runtime.definition summary in
  let requires_context =
    runtime.contract_context context definition environment None
      callback_environment
  in
  let* proved =
    prove_requires runtime context expression summary requires_context
      environment [] [ caller_state ] (runtime.requires summary)
  in
  let assume state =
    let* result, state =
      runtime.fresh_value state
        ~source_name:(definition.function_id.function_name ^ ".result")
        ~role:runtime.result_role ~span:expression.Sst.span ~project:true
        expression.typ
    in
    let post_context =
      runtime.contract_context context definition environment
        (Some environment) callback_environment
    in
    assume_posts runtime context summary post_context environment result state
  in
  let rec contexts obligations paths = function
    | [] -> Ok { obligations; paths = List.rev paths }
    | state :: pending ->
        let* assumed = assume state in
        contexts (obligations @ assumed.obligations)
          (List.rev_append assumed.paths paths) pending
  in
  let* assumed = contexts [] [] proved.paths in
  Ok
    {
      obligations = proved.obligations @ assumed.obligations;
      paths = assumed.paths;
    }

let evaluate_direct runtime context (expression : Sst.expression) summary
    call_form arguments state =
  let definition = runtime.definition summary in
  let* () =
    if
      call_form = Sst.Exec_call
      && not definition.recursive
      &&
      match definition.body with
      | Sst.Checked_exec _ -> true
      | Sst.Spec_definition _ | Sst.Recursive_spec_definition _
      | Sst.Proof_body _ | Sst.External_specification _
      | Sst.Trusted_external_spec_target _
      | Sst.Trusted_external_body _
      | Sst.Symbolic_declaration _ ->
          false
    then Ok ()
    else
      malformed_direct runtime context expression.span
        "callback-bearing direct call is not a nonrecursive checked Exec"
  in
  let* values, callback_environment =
    direct_edges runtime context expression summary arguments
  in
  let* evaluated = evaluate_values runtime context values state in
  let rec contexts obligations paths = function
    | [] -> Ok { obligations; paths = List.rev paths }
    | input :: pending ->
        let* instantiated =
          instantiate_direct runtime context expression summary
            callback_environment arguments input
        in
        contexts (obligations @ instantiated.obligations)
          (List.rev_append instantiated.paths paths) pending
  in
  let* instantiated = contexts [] [] evaluated.paths in
  Ok
    {
      obligations = evaluated.obligations @ instantiated.obligations;
      paths = instantiated.paths;
    }
