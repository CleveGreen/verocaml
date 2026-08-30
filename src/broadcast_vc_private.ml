type inserted = {
  broadcast_id : string;
  theorem_function_id : Sst.function_id;
  type_vector : Parametric_type.t list;
  qid : string;
  skid : string;
  insertion_ordinal : int;
  trusted : bool;
  declaration_span : Diagnostic.span;
  witness_span : Diagnostic.span option;
  requires_count : int;
  ensures_count : int;
  selecting_paths : string list list;
}

type report = {
  active_declarations : int;
  trusted_broadcast_declarations : int;
  trusted_broadcast_uses : int;
  inserted : inserted list;
}

type report_entry = {
  program : Sst.program Weak.t;
  obligation : Vir.obligation Weak.t;
  base_obligation : Vir.obligation;
  report : report;
}

let reports : report_entry list ref = ref []
let instance_cap = 16

let ( let* ) result continuation =
  match result with Ok value -> continuation value | Error _ as error -> error

let same_function_id left right =
  if
    Spec_function_sst_private.is_application_id left
    && Spec_function_sst_private.is_application_id right
  then true
  else
    left.Sst.function_index = right.Sst.function_index
    && String.equal left.function_name right.function_name

let make_binders ~schema ~formals ~sort_of_type =
  if
    List.length formals
    <> List.length (Logic_quantifier_private.vector_binders schema)
  then Error "broadcast formal/schema arity mismatch"
  else
    List.map2
      (fun (binding_id, source_name, typ, span) metadata ->
        let* sort = sort_of_type typ in
        Ok
          ( binding_id,
            Vir.
              {
                symbol_id = Logic_quantifier_private.binder_index metadata;
                source_name;
                sort;
                role = Local;
                span;
              } ))
      formals (Logic_quantifier_private.vector_binders schema)
    |> List.fold_left
         (fun result binding ->
           let* bindings = result in
           let* binding = binding in
           Ok (binding :: bindings))
         (Ok [])
    |> Result.map List.rev

let conjunction = function
  | [] -> Vir.Boolean_constant true
  | first :: rest ->
      List.fold_left
        (fun combined term -> Vir.Boolean_and (combined, term))
        first rest

let make_theorem_quantifier ~sort_of_type ~schema ~binders ~requires ~ensures
    ~trigger =
  let body =
    Vir.Boolean_or
      (Vir.Boolean_not (conjunction requires), conjunction ensures)
  in
  let body =
    List.fold_right
      (fun symbol body ->
        match symbol.Vir.sort with
        | Vir.Integer ->
            Vir.Boolean_or
              ( Vir.Boolean_not
                  (conjunction
                     (Vir.integer_range (Vir.Integer_symbol symbol))),
                body )
        | Boolean | Aggregate _ | Parametric _ -> body)
      binders body
  in
  Vir.make_boolean_quantifier ~sort_of_type ~schema ~binders ~body
    ~trigger:(Some trigger)

type ('state, 'value, 'error) lowering = {
  error : Diagnostic.span -> string -> 'error;
  sort_of_type : Parametric_type.t -> (Vir.sort, string) result;
  value_of_symbol :
    Parametric_type.t -> Vir.symbol -> ('value, 'error) result;
  initial_state : (int * 'value) list -> 'state;
  evaluate :
    (Parametric_type.binder * Parametric_type.t) list ->
    'state ->
    Sst.expression ->
    (Vir.boolean_term * 'state, 'error) result;
  evaluate_ensure :
    (Parametric_type.binder * Parametric_type.t) list ->
    'state ->
    Sst.ensures_clause ->
    (Vir.boolean_term * 'state, 'error) result;
  evaluate_trigger :
    (Parametric_type.binder * Parametric_type.t) list ->
    'state ->
    Sst.expression ->
    (Vir.application_term * 'state, 'error) result;
}

let make_lowering ~error ~aggregate_of_type ~integer_value ~boolean_value
    ~parametric_value ~spec_function_value ~aggregate_value ~initial_state ~evaluate
    ~evaluate_ensure ~evaluate_trigger =
  let sort_of_type = function
    | Parametric_type.Int -> Ok Vir.Integer
    | Bool -> Ok Vir.Boolean
    | Parameter binder -> Ok (Vir.Parametric binder)
    | Application _ as typ when Parametric_type.is_spec_function typ ->
        Ok (Vir.Parametric (Spec_function_logic_private.binder typ))
    | Application _ as typ -> (
        match aggregate_of_type typ with
        | Some aggregate -> Ok (Vir.Aggregate aggregate)
        | None -> Error "broadcast binder application has no logical ADT")
    | Unit | Tuple _ | Aggregate _ ->
        Error "broadcast binder has an unsupported first-order sort"
  in
  let value_of_symbol typ (symbol : Vir.symbol) =
    if Parametric_type.is_spec_function typ then
      Spec_function_logic_private.of_symbol ~arrow:typ symbol
      |> Result.map (spec_function_value typ)
      |> Result.map_error (error symbol.span)
    else
      match symbol.sort with
    | Vir.Integer -> Ok (integer_value (Vir.Integer_symbol symbol))
    | Boolean -> Ok (boolean_value (Vir.Boolean_symbol symbol))
    | Parametric _ ->
        Parametric_logic_private.of_symbol symbol
        |> Result.map parametric_value
        |> Result.map_error (error symbol.span)
    | Aggregate aggregate_type ->
        Ok
          (aggregate_value
             {
               Vir.aggregate_type;
               aggregate_desc = Vir.Aggregate_symbol symbol;
             })
  in
  {
    error;
    sort_of_type;
    value_of_symbol;
    initial_state;
    evaluate;
    evaluate_ensure;
    evaluate_trigger;
  }

let make_evaluator_lowering ~error ~aggregate_of_type ~integer_value
    ~boolean_value ~parametric_value ~spec_function_value ~aggregate_value
    ~map_expression
    ~environment ~reset ~context ~evaluate_formula ~prepare_ensure ~restore
    ~evaluate_trigger_formula =
  make_lowering ~error ~aggregate_of_type ~integer_value ~boolean_value
    ~parametric_value ~spec_function_value ~aggregate_value ~initial_state:reset
    ~evaluate:(fun substitution state expression ->
      evaluate_formula (context (environment state))
        (map_expression substitution expression)
        state)
    ~evaluate_ensure:(fun substitution state clause ->
      let outer = environment state in
      let* state = prepare_ensure state clause.Sst.binder in
      let* term, state =
        evaluate_formula (context outer)
          (map_expression substitution clause.predicate.expression)
          state
      in
      Ok (term, restore outer state))
    ~evaluate_trigger:(fun substitution state expression ->
      evaluate_trigger_formula (context (environment state))
        (map_expression substitution expression)
        state)

let build_quantifier lowering theorem actual_types =
  let definition = Broadcast_declaration_private.definition theorem in
  let declaration_span =
    Broadcast_declaration_private.declaration_span theorem
  in
  let from_string result =
    Result.map_error (lowering.error declaration_span) result
  in
  let* schema =
    Broadcast_declaration_private.schema theorem ~actual_types |> from_string
  in
  let substitution = List.combine definition.Sst.type_binders actual_types in
  let formals =
    Broadcast_declaration_private.formals theorem
    |> List.map (fun (formal : Sst.binding) ->
           ( formal.id,
             formal.name,
             Parametric_type.substitute substitution formal.typ,
             formal.span ))
  in
  let* symbol_bindings =
    make_binders ~schema ~formals ~sort_of_type:lowering.sort_of_type
    |> from_string
  in
  let* bindings =
    List.fold_left
      (fun result
           ( (binding_id, _, typ, _),
             (symbol_binding_id, (symbol : Vir.symbol)) ) ->
        let* bindings = result in
        if binding_id <> symbol_binding_id then
          Error
            (lowering.error symbol.span
               "broadcast formal/symbol binding identity mismatch")
        else
          let* value = lowering.value_of_symbol typ symbol in
          Ok ((binding_id, symbol, value) :: bindings))
      (Ok []) (List.combine formals symbol_bindings)
    |> Result.map List.rev
  in
  let initial_state =
    lowering.initial_state
      (List.map (fun (binding_id, _, value) -> (binding_id, value)) bindings)
  in
  let* requires, state =
    List.fold_left
      (fun result (clause : Sst.predicate_clause) ->
        let* terms, state = result in
        let* term, state =
          lowering.evaluate substitution state clause.predicate.expression
        in
        Ok (term :: terms, state))
      (Ok ([], initial_state)) definition.contracts.requires
    |> Result.map (fun (terms, state) -> (List.rev terms, state))
  in
  let* ensures, _state =
    List.fold_left
      (fun result clause ->
        let* terms, state = result in
        let* term, state =
          lowering.evaluate_ensure substitution state clause
        in
        Ok (term :: terms, state))
      (Ok ([], state)) definition.contracts.ensures
    |> Result.map (fun (terms, state) -> (List.rev terms, state))
  in
  let* trigger, _ =
    lowering.evaluate_trigger substitution initial_state
      (Broadcast_declaration_private.trigger theorem)
  in
  make_theorem_quantifier ~sort_of_type:lowering.sort_of_type ~schema
    ~binders:(List.map (fun (_, symbol, _) -> symbol) bindings)
    ~requires ~ensures ~trigger
  |> from_string

let add_occurrence callee type_arguments occurrences =
  (callee, type_arguments) :: occurrences

let rec argument_occurrences occurrences = function
  | Vir.Recursive_integer_argument term -> integer_occurrences occurrences term
  | Recursive_boolean_argument term -> boolean_occurrences occurrences term
  | Recursive_aggregate_argument term -> aggregate_occurrences occurrences term
  | Recursive_parametric_argument term ->
      parametric_occurrences occurrences term

and arguments_occurrences occurrences arguments =
  List.fold_left argument_occurrences occurrences arguments

and symbolic_occurrences occurrences application =
  let declaration =
    Symbolic_application_private.declaration application
  in
  let callee, type_arguments =
    match Spec_function_logic_private.is_application application with
    | Some arrow ->
        ( {
            Sst.function_index = -110;
            function_name =
              "$verocaml.spec-apply:"
              ^ Parametric_type.structural_identity_digest arrow;
          },
          [ arrow ] )
    | None ->
        ( {
            Sst.function_index =
              Symbolic_application_private.declaration_index declaration;
            function_name =
              Symbolic_application_private.declaration_name declaration;
          },
          Symbolic_application_private.type_arguments application )
  in
  arguments_occurrences
    (add_occurrence callee type_arguments occurrences)
    (Symbolic_application_private.arguments application)

and integer_occurrences occurrences = function
  | Vir.Integer_constant _ | Integer_symbol _ -> occurrences
  | Integer_add (left, right) | Integer_subtract (left, right) ->
      integer_occurrences (integer_occurrences occurrences left) right
  | Integer_negate term
  | Integer_multiply_constant (_, term)
  | Integer_absolute_value term ->
      integer_occurrences occurrences term
  | Integer_conditional (condition, consequent, alternative) ->
      integer_occurrences
        (integer_occurrences
           (boolean_occurrences occurrences condition)
           consequent)
        alternative
  | Integer_rank_project (_, aggregate)
  | Aggregate_tag (_, aggregate)
  | Integer_selector (_, aggregate) ->
      aggregate_occurrences occurrences aggregate
  | Integer_recursive_spec_application { callee; type_arguments; arguments; _ }
    ->
      arguments_occurrences
        (add_occurrence callee type_arguments occurrences)
        arguments
  | Integer_symbolic_application application ->
      symbolic_occurrences occurrences application

and aggregate_occurrences occurrences aggregate =
  match aggregate.Vir.aggregate_desc with
  | Vir.Aggregate_symbol _ -> occurrences
  | Aggregate_imported_model_application { arguments; _ }
  | Aggregate_constructor { arguments; _ } ->
      arguments_occurrences occurrences arguments
  | Aggregate_selector (_, source) -> aggregate_occurrences occurrences source
  | Aggregate_record { fields; _ } ->
      List.fold_left
        (fun occurrences (_, argument) ->
          argument_occurrences occurrences argument)
        occurrences fields
  | Aggregate_conditional (condition, consequent, alternative) ->
      aggregate_occurrences
        (aggregate_occurrences
           (boolean_occurrences occurrences condition)
           consequent)
        alternative
  | Aggregate_recursive_spec_application
      { callee; type_arguments; arguments; _ } ->
      arguments_occurrences
        (add_occurrence callee type_arguments occurrences)
        arguments
  | Aggregate_symbolic_application application ->
      symbolic_occurrences occurrences application

and parametric_occurrences occurrences term =
  match term.Vir.parametric_desc with
  | Vir.Parametric_symbol _ -> occurrences
  | Parametric_selector (_, aggregate) ->
      aggregate_occurrences occurrences aggregate
  | Parametric_conditional (condition, consequent, alternative) ->
      parametric_occurrences
        (parametric_occurrences
           (boolean_occurrences occurrences condition)
           consequent)
        alternative
  | Parametric_symbolic_application application ->
      symbolic_occurrences occurrences application

and boolean_occurrences occurrences = function
  | Vir.Logical_adt_schema _ | Boolean_constant _ | Boolean_symbol _ ->
      occurrences
  | Boolean_not term -> boolean_occurrences occurrences term
  | Boolean_and (left, right)
  | Boolean_or (left, right)
  | Boolean_equal (left, right)
  | Boolean_not_equal (left, right) ->
      boolean_occurrences (boolean_occurrences occurrences left) right
  | Forall_term quantifier | Exists_term quantifier ->
      Option.fold
        ~none:
          (boolean_occurrences occurrences quantifier.boolean_quantifier_body)
        ~some:(fun trigger ->
          application_occurrences
            (boolean_occurrences occurrences quantifier.boolean_quantifier_body)
            trigger)
        quantifier.boolean_quantifier_trigger
  | Integer_compare (_, left, right) ->
      integer_occurrences (integer_occurrences occurrences left) right
  | Boolean_selector (_, aggregate)
  | Boolean_invariant_application { value = aggregate; _ } ->
      aggregate_occurrences occurrences aggregate
  | Aggregate_equal (left, right) ->
      aggregate_occurrences (aggregate_occurrences occurrences left) right
  | Parametric_equal (left, right) ->
      parametric_occurrences (parametric_occurrences occurrences left) right
  | Boolean_recursive_spec_application { callee; type_arguments; arguments; _ }
  | Boolean_specification_application { callee; type_arguments; arguments; _ }
    ->
      arguments_occurrences
        (add_occurrence callee type_arguments occurrences)
        arguments
  | Boolean_symbolic_application application ->
      symbolic_occurrences occurrences application
  | Callback_requires application ->
      arguments_occurrences occurrences application.arguments
  | Callback_ensures { application; result } ->
      argument_occurrences
        (arguments_occurrences occurrences application.arguments)
        result

and application_occurrences occurrences = function
  | Vir.Integer_application term -> integer_occurrences occurrences term
  | Boolean_application term -> boolean_occurrences occurrences term
  | Aggregate_application term -> aggregate_occurrences occurrences term
  | Parametric_application term -> parametric_occurrences occurrences term

let obligation_occurrences obligation =
  List.fold_left boolean_occurrences []
    ((obligation.Vir.goal :: obligation.assumptions)
    @ obligation.required_preceding_safety @ obligation.path_condition)
  |> List.rev

let vector_key vector =
  String.concat "," (List.map Parametric_type.to_string vector)

let candidates ~program selections occurrences =
  [%log.trace "match broadcast candidates"
    ~selections:(Delator.Field.int (List.length selections))
    ~occurrences:(Delator.Field.int (List.length occurrences))];
  List.fold_left
    (fun result selection ->
      let* instances = result in
      let declaration = selection.Broadcast_scope_private.declaration in
      match
        Broadcast_declaration_private.find ~program
          ~id:declaration.declaration_id
      with
      | None ->
          Error
            ("active broadcast declaration is absent: "
           ^ declaration.declaration_id)
      | Some theorem ->
          let matching =
            List.filter
              (fun (callee, _) ->
                same_function_id callee
                  (Broadcast_declaration_private.trigger_head theorem))
              occurrences
          in
          List.fold_left
            (fun result (_, type_arguments) ->
              let* instances = result in
              let* inferred =
                Broadcast_declaration_private.infer_type_vector theorem
                  type_arguments
              in
              match inferred with
              | None -> Ok instances
              | Some vector ->
                  let key =
                    declaration.declaration_id ^ "|" ^ vector_key vector
                  in
                  if
                    List.exists
                      (fun (existing, _, _, _) -> String.equal existing key)
                      instances
                  then Ok instances
                  else Ok ((key, selection, theorem, vector) :: instances))
            (Ok instances) matching)
    (Ok []) selections
  |> Result.map
       (List.sort (fun (left, _, _, _) (right, _, _, _) ->
            String.compare left right))

let remember program obligation base_obligation report =
  let live =
    List.filter
      (fun entry -> Option.is_some (Weak.get entry.obligation 0))
      !reports
  in
  let weak = Weak.create 1 in
  Weak.set weak 0 (Some obligation);
  let program_weak = Weak.create 1 in
  Weak.set program_weak 0 (Some program);
  reports :=
    { program = program_weak; obligation = weak; base_obligation; report }
    :: live

let materialize ~program ~function_id ~obligation ~build =
  if not (Broadcast_scope_private.registered program) then Ok obligation
  else
  let base_obligation = obligation in
  let* selections =
    Broadcast_scope_private.active ~program ~function_id
      ~span:obligation.Vir.span
  in
  let occurrences = obligation_occurrences obligation in
  let* candidates = candidates ~program selections occurrences in
  [%log.debug "materialize broadcast obligation"
    ~function_name:(Delator.Field.string function_id.Sst.function_name)
    ~obligation_index:(Delator.Field.int obligation.Vir.obligation_index)
    ~selections:(Delator.Field.int (List.length selections))
    ~occurrences:(Delator.Field.int (List.length occurrences))
    ~candidates:(Delator.Field.int (List.length candidates))];
  if List.length candidates > instance_cap then
    Error
      (Printf.sprintf "broadcast instance cap exceeded: %d > %d"
         (List.length candidates) instance_cap)
  else
    let* assumptions, inserted =
      List.fold_left
        (fun result
             ( _,
               (selection : Broadcast_scope_private.selection),
               theorem,
               vector ) ->
          let* assumptions, inserted = result in
          let* quantifier = build theorem vector in
          let ordinal = List.length inserted in
          let schema = quantifier.Vir.boolean_quantifier_schema in
          let trusted =
            Broadcast_declaration_private.kind theorem
            = Broadcast_declaration_private.Trusted_axiom
          in
          let metadata =
            let definition =
              Broadcast_declaration_private.definition theorem
            in
            {
              broadcast_id = Broadcast_declaration_private.id theorem;
              theorem_function_id = definition.Sst.function_id;
              type_vector = vector;
              qid = Logic_quantifier_private.vector_qid schema;
              skid = Logic_quantifier_private.vector_skid schema;
              insertion_ordinal = ordinal;
              trusted;
              declaration_span =
                Broadcast_declaration_private.declaration_span theorem;
              witness_span =
                Broadcast_declaration_private.witness_span theorem;
              requires_count =
                List.length definition.Sst.contracts.requires;
              ensures_count = List.length definition.Sst.contracts.ensures;
              selecting_paths = selection.selecting_paths;
            }
          in
          Ok (Vir.Forall_term quantifier :: assumptions, metadata :: inserted))
        (Ok (List.rev obligation.assumptions, []))
        candidates
    in
    let obligation =
      { obligation with Vir.assumptions = List.rev assumptions }
    in
    let scope_counters = Broadcast_scope_private.counters selections in
    let inserted = List.rev inserted in
    remember program obligation base_obligation
      {
        active_declarations = scope_counters.active_declarations;
        trusted_broadcast_declarations = scope_counters.trusted_declarations;
        trusted_broadcast_uses =
          List.fold_left
            (fun count inserted ->
              if inserted.trusted then count + 1 else count)
            0 inserted;
        inserted;
      };
    Ok obligation

let report obligation =
  let live, found =
    List.fold_left
      (fun (live, found) entry ->
        match Weak.get entry.obligation 0 with
        | None -> (live, found)
        | Some candidate ->
            ( entry :: live,
              if candidate == obligation then Some entry.report else found ))
      ([], None) !reports
  in
  reports := List.rev live;
  found

let pre_materialization_obligation obligation =
  List.find_map
    (fun entry ->
      match Weak.get entry.obligation 0 with
      | Some candidate when candidate == obligation ->
          Some entry.base_obligation
      | Some _ | None -> None)
    !reports

let base_obligation obligation =
  Option.value ~default:obligation
    (pre_materialization_obligation obligation)

let transfer_report ~from ~to_ =
  List.iter
    (fun entry ->
      match Weak.get entry.obligation 0 with
      | Some candidate when candidate == from ->
          Weak.set entry.obligation 0 (Some to_)
      | Some _ | None -> ())
    !reports

let materialize_and_attach ~program ~function_id ~descriptors ~obligation
    ~build ~map_error =
  let attached =
    let* materialized =
      materialize ~program ~function_id ~obligation ~build
    in
    let* attached =
      Logical_adt_encoding_private.attach_schemas ~descriptors materialized
    in
    transfer_report ~from:materialized ~to_:attached;
    Ok attached
  in
  Result.map_error map_error attached

let clear_program program =
  reports :=
    List.filter
      (fun entry ->
        Option.fold ~none:false
          ~some:(fun candidate -> candidate != program)
          (Weak.get entry.program 0)
        && Option.is_some (Weak.get entry.obligation 0))
      !reports
