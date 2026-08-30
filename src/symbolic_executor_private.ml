module Finite_induction = Finite_induction_private
open Call_contract_execution_private
type unsupported =
  | Ghost_call
  | Or_pattern
  | Missing_summary of Sst.function_id
  | Recursion_awaits_totality of Sst.function_id
  | Missing_decreases
  | Duplicate_decreases
  | Inapplicable_decreases
  | Non_integer_decreases
  | Malformed_sst of string
module Owned_scalar = Sst_validation_private.Public
type error = {
  function_name : string;
  span : Diagnostic.span;
  unsupported : unsupported;
}
type value = Logical_spec_evaluation_private.value =
  | Unit_value
  | Integer_value of Vir.integer_term
  | Boolean_value of Vir.boolean_term
  | Tuple_value of value list
  | Aggregate_value of Vir.aggregate_term
  | Parametric_value of Vir.parametric_term
  | Function_value of Logical_spec_evaluation_private.function_value
type authority_observation_event =
  | Recursive_spec_lowering
  | Recursive_proof_rank_lowering
type authority_observation = {
  recursive_spec_lowerings : int;
  recursive_proof_rank_lowerings : int;
}
let authority_observation_events = ref None
let suppress_recursive_proof_summaries = ref false
let suppress_recursive_proof_calls = ref false
let suppress_exec_summary_for_callable = ref None
let suppress_exec_ensure_assumption_for_callable = ref None
let suppress_proof_call_for_testing = ref None
let suppress_proof_summary_for_testing = ref None
let suppress_local_assertion_successor_for_testing = ref false
let suppress_local_assertion_successor_sites_for_testing = ref []
let proof_activation_snapshot_attack_for_testing = ref None
let suppress_invariant_completion_for_callable_for_testing = ref None
let suppress_recursive_spec_result_consumption_for_testing = ref false
let recursive_spec_application_identity_forgery_for_testing = ref false
let suppress_aggregate_recursive_source_traversal_for_testing = ref false
let suppress_frozen_spine_child_witness_for_testing = ref false
let source_route_attack_observation_enabled_for_testing = ref false
let aggregate_recursive_rank_route_observations = ref 0
let aggregate_recursive_argument_route_observations = ref 0
let observed_local_assertion_exports = ref 0
let proof_activation_prefinalizer_observer_for_testing = ref None
let observed_proof_call_spent_visits = ref 0
let observed_proof_call_ledger_summaries = ref []
let observed_exec_region_proof_summary_exports = ref []
let aggregate_contains_recursive_specification =
  Recursive_spec_term_private.aggregate_contains_recursive_specification
let argument_contains_recursive_specification =
  Recursive_spec_term_private.argument_contains_recursive_specification
type finite_induction_binding = {
  induction_hypothesis : Finite_induction.hypothesis;
  induction_actual_receipts :
    (Vir.aggregate_term * Finite_value_registry.receipt) list;
}
type pending_ground_constructor_match = {
  pending_scrutinee_expression : Sst.expression;
  pending_case : Sst.case;
  pending_value : Vir.aggregate_term;
  pending_path_condition : Vir.boolean_term list;
}
type frozen_terminal_observation = {
  frozen_terminal_permit : Verification_session.frozen_observation_permit;
  frozen_terminal_descriptor : Sst.frozen_spine_prerequisite;
  frozen_terminal_root : Vir.aggregate_term;
  frozen_terminal_model : Vir.aggregate_term;
  frozen_terminal_value : Vir.integer_term;
  frozen_terminal_call_path : Diagnostic.span list;
  frozen_terminal_path_condition : Vir.boolean_term list;
  frozen_terminal_epoch : int;
}
let record_authority_observation event =
  match !authority_observation_events with
  | None -> ()
  | Some events -> authority_observation_events := Some (event :: events)
let authority_observation () =
  List.fold_left
    (fun observation -> function
      | Recursive_spec_lowering ->
          {
            observation with
            recursive_spec_lowerings = observation.recursive_spec_lowerings + 1;
          }
      | Recursive_proof_rank_lowering ->
          {
            observation with
            recursive_proof_rank_lowerings =
              observation.recursive_proof_rank_lowerings + 1;
          })
    { recursive_spec_lowerings = 0; recursive_proof_rank_lowerings = 0 }
    (Option.value ~default:[] !authority_observation_events)
type state = {
  parametric_adts : Parametric_adt.t list;
  environment : (int * value) list;
  owned_root_versions : (int * Vir.aggregate_term * int) list;
  owned_contents_origins :
    (Vir.aggregate_term * Verification_session.owned_contents_origin) list;
  shared_scalar_heap : Shared_scalar_heap_private.t option;
  assumptions : Vir.boolean_term list;
  immutable_aggregate_facts : Vir.boolean_term list;
  local_invariant_assumptions : Vir.boolean_term list;
  closed_invariant_facts : Vir.boolean_term list;
  consumed_receipt_facts : Verification_session.consumed_fact list;
  finite_receipts : Finite_value_registry.receipt list;
  proof_call_visits : Finite_value_registry.proof_call_visit list;
  spent_proof_call_visits : Finite_value_registry.proof_call_visit list;
  proof_call_summaries : Finite_value_registry.proof_call_summary list;
  exported_proof_summary_facts : Vir.boolean_term list;
  exported_local_assertion_facts : Vir.boolean_term list;
  pending_finite_induction : finite_induction_binding option;
  required_preceding_safety : Vir.boolean_term list;
  path_condition : Vir.boolean_term list;
  projection_symbols : Vir.symbol list;
  trusted_summary_uses : Vir.trusted_summary_use list;
  next_symbol : int;
  next_obligation : int;
  entry_measure : (Termination.decrease_domain * Vir.integer_term) option;
  proof_activation_authority :
    Verification_session.proof_activation_authority option;
  proof_activations : Spec_unfolding.activation list;
  reached_proof_activations : Spec_unfolding.activation list;
  ground_constructor_matches :
    Verification_session.ground_constructor_match list;
  pending_ground_constructor_matches : pending_ground_constructor_match list;
  frozen_terminal_observations : frozen_terminal_observation list;
}
type evaluated = { value : value; state : state }
type emitted_obligation = {
  provisional_obligation : Vir.obligation;
  expected_proof_activation_authority :
    Verification_session.proof_activation_authority option;
  proof_activation_snapshot :
    Verification_session.proof_activation_snapshot option;
}
type 'a path_evaluation =
  (emitted_obligation, 'a) Call_contract_execution_private.evaluation
type provisional_invariant_cell_close = {
  authority : Shared_invariant_cell_private.t;
  operation : Sst.function_id;
  final_epoch : int;
}
type lowered_execution = {
  execution : Vir.function_execution;
  proof_activation_batch : Verification_session.proof_activation_batch option;
  provisional_invariant_cell_close : provisional_invariant_cell_close option;
}
type contract_clause = Call_contract_execution_private.clause = {
  ordinal : int;
  span : Diagnostic.span;
  binder : Sst.pattern option;
  payload : Sst.expression;
}
type summary = Call_contract_execution_private.call_summary = {
  definition : Sst.function_definition;
  requires : contract_clause list;
  ensures : contract_clause list;
  assertions : contract_clause list;
  executable_body : Sst.expression;
}
type finite_result_call_site = {
  finite_result_callee : Sst.function_id;
  finite_result_call_span : Diagnostic.span;
}
module Invariant_formula_capture = Logical_spec_capability_private.Make_model_capture (Type_invariant)
module Invariant_formula_policy = Logical_spec_authentication_private.Make_invariant_policy (Type_invariant)
type owned_contents_scope = {
  owned_contents_candidate : Verification_session.owned_contents_candidate;
  owned_contents_path : Sst.field_id list;
  owned_contents_node : Vir.aggregate_term;
}
type evaluation_context = {
  validated : Sst_validation.validated_program;
  imports : Imported_callable.registration option;
  function_ref : Vir.function_ref;
  execution_definition : Sst.function_definition;
  current_definition : Sst.function_definition;
  current_callable : Sst.function_id;
  canonical_callable : string option;
  current_mode : Sst.verification_mode;
  definitions : (int * Sst_validation.callable_descriptor) list;
  summaries : (int * summary) list;
  termination : Termination.plan;
  type_definitions : Sst.type_definition list;
  entry_environment : (int * value) list;
  logical : bool;
  old_environment : (int * value) list option;
  spec_call_stack : int list;
  spec_expansion_limit : int;
  rank_domains : Vir.rank_domain list;
  invariants : Type_invariant.environment;
  verification_session : Verification_session.t option;
  proof_entry_activations : Spec_unfolding.activation list;
  finite_result_eligible : bool;
  finite_result_candidate : bool;
  finite_result_demand_sites : finite_result_call_site list;
  frozen_observation_call_path : Diagnostic.span list;
  frozen_observation_scope : frozen_observation_scope option;
  owned_root_scalar_plan :
    Verification_session.owned_root_scalar_observation_plan option;
  owned_contents_stack : owned_contents_scope option;
  ground_retry_candidate_scope : bool;
  shared_heap_view : Shared_scalar_heap_private.read_view;
  shared_old_heap_view : Shared_scalar_heap_private.read_view option;
  shared_effect_base_epoch : int;
  invariant_cell_authority : Shared_invariant_cell_private.t option;
  shared_heap_owner : Shared_scalar_heap_private.t list ref;
  shared_entry_transitions : Sst.shared_scalar_heap_transition list;
  shared_entry_reads : Vir.shared_scalar_heap_read list ref;
  reached_callback_calls : Vir.reached_callback_call list ref;
  callback_environment : (Sst.callback_binding * Sst.callback_binding) list; formula_registry : Logical_spec_admission_private.registry_entry list;
}
and frozen_observation_scope = {
  frozen_scope_permit : Verification_session.frozen_observation_permit;
  frozen_scope_definition : Sst.function_definition;
  frozen_scope_descriptor : Sst.frozen_spine_prerequisite;
  frozen_scope_root : Vir.aggregate_term;
  frozen_scope_call_path : Diagnostic.span list;
  frozen_scope_path_condition : Vir.boolean_term list;
  frozen_scope_epoch : int;
}
let program_has_owned_abstraction context =
  (Sst_validation.program context.validated).Sst.types
  |> List.exists (fun (definition : Sst.type_definition) ->
      match definition.representation with
      | Sst.Abstract_with_evidence
          ( Sst.Authenticated_same_cmt_abstraction evidence
          | Sst.Proposed_same_cmt_abstraction evidence )
        when Option.is_some evidence.owned_tree_prerequisite ->
          true
      | Sst.Revealed
      | Sst.Abstract_with_evidence (Sst.Incomplete_abstraction_evidence _)
      | Sst.Abstract_with_evidence
          ( Sst.Proposed_same_cmt_abstraction _
          | Sst.Authenticated_same_cmt_abstraction _ ) ->
          false)
let immutable_pattern_reconstruction_enabled context =
  Option.is_none context.owned_root_scalar_plan
  && Option.is_none context.owned_contents_stack
  && Option.is_none context.frozen_observation_scope
  && Option.is_none
       (Type_invariant.find_for_operation context.invariants
          context.current_definition.function_id)
  && context.shared_entry_transitions = []
let owned_recursive_construction_enabled context =
  program_has_owned_abstraction context
  && List.exists
       (fun (definition : Sst.function_definition) ->
         match definition.body with
         | Sst.Recursive_spec_definition _ -> true
         | Sst.Checked_exec _ | Sst.Spec_definition _ | Sst.Proof_body _
         | Sst.External_specification _ | Sst.Trusted_external_spec_target _
         | Sst.Trusted_external_body _ | Sst.Symbolic_declaration _ ->
             false)
       (Sst_validation.program context.validated).functions
let vir_aggregate_type = Logical_spec_evaluation_private.vir_aggregate_type
let vir_aggregate_type_of_sst =
  Logical_spec_evaluation_private.vir_aggregate_type_of_sst
module Logical_adt = Logical_adt_evaluation_private
let owner_type = function
  | Sst.Record_owner type_id -> type_id
  | Sst.Constructor_owner constructor -> constructor.constructor_type
let field_selector = Logical_spec_evaluation_private.field_selector
let argument_selector = Logical_spec_evaluation_private.argument_selector
let ( let* ) result f =
  match result with Ok value -> f value | Error _ as e -> e
let error function_name span unsupported =
  Error { function_name; span; unsupported }
let function_ref (definition : Sst.function_definition) : Vir.function_ref =
  {
    function_index = definition.function_id.function_index;
    function_name = definition.function_id.function_name;
  }
let suppressed_proof_edge suppression caller callee =
  match !suppression with
  | Some (expected_caller, expected_callee) ->
      String.equal caller.Sst.function_name expected_caller
      && String.equal callee.Sst.function_name expected_callee
  | None -> false
let vir_function_ref (id : Sst.function_id) : Vir.function_ref =
  { function_index = id.function_index; function_name = id.function_name }
let append left right = left @ right
let with_assumptions state assumptions =
  { state with assumptions = append state.assumptions assumptions }
let with_immutable_aggregate_facts state facts =
  {
    state with
    immutable_aggregate_facts = append state.immutable_aggregate_facts facts;
  }
let with_local_invariant_assumption state assumption =
  {
    state with
    local_invariant_assumptions =
      append state.local_invariant_assumptions [ assumption ];
  }
let with_closed_invariant_fact state fact =
  {
    state with
    closed_invariant_facts = append state.closed_invariant_facts [ fact ];
  }
let with_consumed_receipt_fact state fact =
  {
    state with
    consumed_receipt_facts = append state.consumed_receipt_facts [ fact ];
  }
let with_finite_receipt state receipt =
  if
    List.exists
      (fun existing -> Finite_value_registry.same_receipt existing receipt)
      state.finite_receipts
  then state
  else { state with finite_receipts = receipt :: state.finite_receipts }
let common_consumed_receipt_facts states =
  match states with
  | [] -> []
  | state :: rest ->
      List.filter
        (fun fact ->
          List.for_all
            (fun state ->
              List.exists
                (Verification_session.same_consumed_fact fact)
                state.consumed_receipt_facts)
            rest)
        state.consumed_receipt_facts
let intersect_consumed_receipt_facts paths =
  let common =
    List.map (fun evaluated -> evaluated.state) paths
    |> common_consumed_receipt_facts
  in
  List.map
    (fun evaluated ->
      let dropped =
        List.filter
          (fun fact ->
            not
              (List.exists
                 (Verification_session.same_consumed_fact fact)
                 common))
          evaluated.state.consumed_receipt_facts
      in
      let closed_invariant_facts =
        List.filter
          (fun closed ->
            not
              (List.exists
                 (fun fact ->
                   Verification_session.consumed_matches_closed_fact fact closed)
                 dropped))
          evaluated.state.closed_invariant_facts
      in
      {
        evaluated with
        state =
          {
            evaluated.state with
            consumed_receipt_facts = common;
            closed_invariant_facts;
          };
      })
    paths
let intersect_finite_receipts paths =
  (* Finite receipts remain exact per symbolic predecessor.  A later call
     previews every predecessor atomically; no joined receipt is invented. *)
  paths
let intersect_proof_call_authority paths =
  (* Visits and summaries are usable postcondition authority, so only facts
     available on every predecessor survive a join.  Spent visits are a
     separate one-use ledger and remain attached to each concrete predecessor:
     joining must not make an already-used child fresh again. *)
  let common selector same =
    match paths with
    | [] -> []
    | first :: rest ->
        List.filter
          (fun fact ->
            List.for_all
              (fun evaluated ->
                List.exists (same fact) (selector evaluated.state))
              rest)
          (selector first.state)
  in
  let visits =
    common
      (fun state -> state.proof_call_visits)
      Finite_value_registry.same_proof_call_visit
  in
  let summaries =
    common
      (fun state -> state.proof_call_summaries)
      Finite_value_registry.same_proof_call_summary
  in
  List.map
    (fun evaluated ->
      {
        evaluated with
        state =
          {
            evaluated.state with
            proof_call_visits = visits;
            proof_call_summaries = summaries;
          };
      })
    paths
let intersect_local_assertion_facts ~current_mode paths =
  match current_mode with
  | Sst.Proof -> paths
  | Sst.Exec | Sst.Spec ->
      let same_fact left right =
        String.equal
          (Vir.boolean_term_to_string left)
          (Vir.boolean_term_to_string right)
      in
      let common =
        match paths with
        | [] -> []
        | first :: rest ->
            List.filter
              (fun fact ->
                List.for_all
                  (fun evaluated ->
                    List.exists (same_fact fact)
                      evaluated.state.exported_local_assertion_facts)
                  rest)
              first.state.exported_local_assertion_facts
      in
      let rec remove_one fact prefix = function
        | [] -> List.rev prefix
        | candidate :: rest when same_fact candidate fact ->
            List.rev_append prefix rest
        | candidate :: rest -> remove_one fact (candidate :: prefix) rest
      in
      List.map
        (fun evaluated ->
          let dropped =
            List.filter
              (fun fact -> not (List.exists (same_fact fact) common))
              evaluated.state.exported_local_assertion_facts
          in
          {
            evaluated with
            state =
              {
                evaluated.state with
                assumptions =
                  List.fold_left
                    (fun assumptions fact -> remove_one fact [] assumptions)
                    evaluated.state.assumptions dropped;
                exported_local_assertion_facts = common;
              };
          })
        paths
let forget_consumed_fact_for_value state = function
  | Aggregate_value aggregate ->
      {
        state with
        consumed_receipt_facts =
          List.filter
            (fun consumed ->
              match Verification_session.consumed_closed_fact consumed with
              | Vir.Boolean_invariant_application { value; _ } ->
                  value <> aggregate
              | _ -> true)
            state.consumed_receipt_facts;
        closed_invariant_facts =
          List.filter
            (function
              | Vir.Boolean_invariant_application { value; _ } ->
                  value <> aggregate
              | _ -> true)
            state.closed_invariant_facts;
      }
  | Unit_value | Integer_value _ | Boolean_value _ | Tuple_value _
  | Parametric_value _ | Function_value _ ->
      state
let forget_finite_receipt_for_value state = function
  | Aggregate_value aggregate ->
      {
        state with
        finite_receipts =
          List.filter
            (fun receipt ->
              not
                (Finite_value_registry.receipt_matches_value receipt aggregate))
            state.finite_receipts;
      }
  | Unit_value | Integer_value _ | Boolean_value _ | Tuple_value _
  | Parametric_value _ | Function_value _ ->
      state
let effective_assumptions state =
  append state.assumptions state.local_invariant_assumptions
let with_path state term =
  { state with path_condition = append state.path_condition [ term ] }
let vir_comparison = Logical_spec_evaluation_private.vir_comparison
let fresh_symbol state ~source_name ~sort ~role ~span ~project =
  let symbol : Vir.symbol =
    { symbol_id = state.next_symbol; source_name; sort; role; span }
  in
  let state =
    {
      state with
      next_symbol = state.next_symbol + 1;
      projection_symbols =
        (if project then append state.projection_symbols [ symbol ]
         else state.projection_symbols);
    }
  in
  (symbol, state)
let equality = Logical_spec_evaluation_private.equality
let rec fresh_value state ~source_name ~role ~span ~project = function
  | Sst.Unit -> Ok (Unit_value, state)
  | Sst.Int ->
      let symbol, state =
        fresh_symbol state ~source_name ~sort:Integer ~role ~span ~project
      in
      let term = Vir.Integer_symbol symbol in
      let state = with_assumptions state (Vir.integer_range term) in
      Ok (Integer_value term, state)
  | Sst.Bool ->
      let symbol, state =
        fresh_symbol state ~source_name ~sort:Boolean ~role ~span ~project
      in
      Ok (Boolean_value (Vir.Boolean_symbol symbol), state)
  | Sst.Tuple components ->
      let rec loop index state values = function
        | [] -> Ok (Tuple_value (List.rev values), state)
        | (label, typ) :: rest ->
            let component_name =
              match label with
              | Some label -> source_name ^ "." ^ label
              | None -> Printf.sprintf "%s.%d" source_name index
            in
            let* value, state =
              fresh_value state ~source_name:component_name ~role ~span ~project
                typ
            in
            loop (index + 1) state (value :: values) rest
      in
      loop 0 state [] components
  | Sst.Parameter binder -> (
      let symbol, state =
        fresh_symbol state ~source_name ~sort:(Vir.Parametric binder) ~role
          ~span ~project:false
      in
      match Parametric_logic_private.of_symbol symbol with
      | Ok term -> Ok (Parametric_value term, state)
      | Error message -> error source_name span (Malformed_sst message))
  | Sst.Application _ as typ when Parametric_type.is_spec_function typ ->
      let sort = Vir.Parametric (Spec_function_logic_private.binder typ) in
      let symbol, state =
        fresh_symbol state ~source_name ~sort ~role ~span ~project:false
      in
      (match Spec_function_logic_private.of_symbol ~arrow:typ symbol with
      | Ok function_term ->
          Ok
            ( Function_value
                {
                  function_term;
                  function_arrow = typ;
                  function_closure = Abstract_function;
                },
              state )
      | Error message -> error source_name span (Malformed_sst message))
  | Sst.Application _ as typ -> (
      match vir_aggregate_type_of_sst state.parametric_adts typ with
      | None ->
          error source_name span
            (Malformed_sst "generic ADT value has no authenticated descriptor")
      | Some aggregate_type ->
          let symbol, state =
            fresh_symbol state ~source_name ~sort:(Vir.Aggregate aggregate_type)
              ~role ~span ~project
          in
          Ok
            ( Aggregate_value
                {
                  Vir.aggregate_type;
                  aggregate_desc = Vir.Aggregate_symbol symbol;
                },
              state ))
  | Sst.Aggregate type_id ->
      let aggregate_type = vir_aggregate_type type_id in
      let symbol, state =
        fresh_symbol state ~source_name ~sort:(Vir.Aggregate aggregate_type)
          ~role ~span ~project
      in
      Ok
        ( Aggregate_value
            { Vir.aggregate_type; aggregate_desc = Vir.Aggregate_symbol symbol },
          state )
let fresh_quantifier_value state (binding : Sst.binding) =
  let make sort value =
    let symbol, state =
      fresh_symbol state ~source_name:binding.name ~sort ~role:Vir.Local
        ~span:binding.span ~project:false
    in
    (symbol, value symbol, state)
  in
  Quantifier_validation_private.quantifier_value
    {
      lower =
        (function
        | Logic_quantifier_private.Integer_binder ->
            Some
              (make Vir.Integer (fun symbol ->
                   Integer_value (Vir.Integer_symbol symbol)))
        | Logic_quantifier_private.Boolean_binder ->
            Some
              (make Vir.Boolean (fun symbol ->
                   Boolean_value (Vir.Boolean_symbol symbol)))
        | Logic_quantifier_private.Parameter_binder binder ->
            Some
              (make (Vir.Parametric binder) (fun symbol ->
                   Parametric_value
                     (Result.get_ok
                        (Parametric_logic_private.of_symbol symbol))))
        | Logic_quantifier_private.Application_binder typ ->
            if Parametric_type.is_spec_function typ then
              Some
                (make
                   (Vir.Parametric (Spec_function_logic_private.binder typ))
                   (fun symbol ->
                     Function_value
                       {
                         function_term =
                           Result.get_ok
                             (Spec_function_logic_private.of_symbol ~arrow:typ
                                symbol);
                         function_arrow = typ;
                         function_closure = Abstract_function;
                       }))
            else
              Option.map
                (fun aggregate_type ->
                  make (Vir.Aggregate aggregate_type) (fun symbol ->
                      Aggregate_value
                        {
                          Vir.aggregate_type;
                          aggregate_desc = Vir.Aggregate_symbol symbol;
                        }))
                (vir_aggregate_type_of_sst state.parametric_adts typ));
      value_error =
        (fun span message ->
          { function_name = binding.name; span; unsupported = Malformed_sst message });
    }
    binding
let selected_value state aggregate make_selector path typ =
  let value =
    Logical_spec_evaluation_private.selected_parametric_value_without_state
      ~aggregate_type:(vir_aggregate_type_of_sst state.parametric_adts)
      aggregate
      (fun path sort ->
        {
          (make_selector path sort) with
          Vir.selector_domain = aggregate.Vir.aggregate_type;
        })
      path typ
  in
  let rec ranges = function
    | Integer_value term -> Vir.integer_range term
    | Tuple_value values -> List.concat_map ranges values
    | Unit_value | Boolean_value _ | Aggregate_value _ | Parametric_value _ | Function_value _ ->
        []
  in
  Ok (value, with_assumptions state (ranges value))
let select_field state aggregate field typ =
  selected_value state aggregate (field_selector field) [] typ
type owned_tree_breadcrumb =
  | Owned_field_parent of {
      aggregate : Vir.aggregate_term;
      field : Sst.field_id;
      fields : Sst.field_definition list;
    }
  | Owned_constructor_parent of {
      aggregate : Vir.aggregate_term;
      constructor : Sst.constructor_id;
    }
let owned_fields context owner =
  Logical_adt_evaluation_private.fields_of_owner context.type_definitions owner
let fresh_owned_aggregate state root span type_id suffix =
  let aggregate_type = vir_aggregate_type type_id in
  let symbol, state =
    fresh_symbol state ~source_name:(root.Sst.name ^ suffix)
      ~sort:(Vir.Aggregate aggregate_type) ~role:Vir.Local ~span ~project:true
  in
  ({ Vir.aggregate_type; aggregate_desc = Vir.Aggregate_symbol symbol }, state)
let rebuild_owned_fields function_name context span root state old_aggregate
    owner changed_field changed_value =
  let fields = owned_fields context owner in
  if fields = [] then
    error function_name span
      (Malformed_sst "owned-tree reconstruction owner has no fields")
  else
    let type_id = owner_type owner in
    let fresh_aggregate, state =
      fresh_owned_aggregate state root span type_id ".owned-state"
    in
    let rec loop state equations = function
      | [] ->
          Ok
            ( Aggregate_value fresh_aggregate,
              with_assumptions state (List.rev equations) )
      | definition :: rest ->
          let* selected_new, state =
            select_field state fresh_aggregate definition.Sst.field_id
              definition.field_type
          in
          let* rhs, state =
            if definition.field_id = changed_field then Ok (changed_value, state)
            else
              select_field state old_aggregate definition.field_id
                definition.field_type
          in
          let* equation =
            match equality selected_new rhs with
            | Some equation -> Ok equation
            | None ->
                error function_name span
                  (Malformed_sst
                     "owned-tree reconstruction field has the wrong type")
          in
          loop state (equation :: equations) rest
    in
    loop state [] fields
let descend_owned_path function_name context span state root_aggregate path =
  let rec loop state value breadcrumbs = function
    | [] -> Ok (value, breadcrumbs, state)
    | Sst.Owned_tree_field field :: rest -> (
        match value with
        | Aggregate_value aggregate ->
            let fields = owned_fields context field.field_owner in
            let* definition =
              match
                List.find_opt
                  (fun (definition : Sst.field_definition) ->
                    definition.field_id = field)
                  fields
              with
              | Some definition -> Ok definition
              | None ->
                  error function_name span
                    (Malformed_sst "owned-tree path field is not registered")
            in
            let* selected, state =
              select_field state aggregate field definition.field_type
            in
            loop state selected
              (Owned_field_parent { aggregate; field; fields } :: breadcrumbs)
              rest
        | _ ->
            error function_name span
              (Malformed_sst "owned-tree field path crossed a scalar"))
    | Sst.Owned_tree_constructor constructor :: rest -> (
        match value with
        | Aggregate_value aggregate ->
            let payload_type = Sst.Aggregate constructor.constructor_type in
            let* selected, state =
              selected_value state aggregate
                (argument_selector constructor 0)
                [] payload_type
            in
            loop state selected
              (Owned_constructor_parent { aggregate; constructor }
              :: breadcrumbs)
              rest
        | _ ->
            error function_name span
              (Malformed_sst "owned-tree constructor path crossed a scalar"))
  in
  loop state (Aggregate_value root_aggregate) [] path
let rebuild_owned_ancestors function_name context span root state value
    breadcrumbs =
  let rec loop state value = function
    | [] -> Ok (value, state)
    | Owned_field_parent { aggregate; field; fields = _ } :: rest ->
        let* value, state =
          rebuild_owned_fields function_name context span root state aggregate
            field.field_owner field value
        in
        loop state value rest
    | Owned_constructor_parent { aggregate = _; constructor } :: rest -> (
        match value with
        | Aggregate_value payload ->
            let fresh, state =
              fresh_owned_aggregate state root span constructor.constructor_type
                ".owned-constructor"
            in
            let tag_equation =
              Vir.Integer_compare
                ( Vir.Equal,
                  Vir.Aggregate_tag
                    (vir_aggregate_type constructor.constructor_type, fresh),
                  Vir.Integer_constant (Z.of_int constructor.constructor_index)
                )
            in
            let payload_value =
              Aggregate_value
                {
                  Vir.aggregate_type =
                    vir_aggregate_type constructor.constructor_type;
                  aggregate_desc =
                    Vir.Aggregate_selector
                      ( argument_selector constructor 0 []
                          (Vir.Aggregate
                             (vir_aggregate_type constructor.constructor_type)),
                        fresh );
                }
            in
            let* payload_equation =
              match equality payload_value (Aggregate_value payload) with
              | Some equation -> Ok equation
              | None ->
                  error function_name span
                    (Malformed_sst
                       "owned-tree constructor payload has the wrong type")
            in
            loop
              (with_assumptions state [ tag_equation; payload_equation ])
              (Aggregate_value fresh) rest
        | _ ->
            error function_name span
              (Malformed_sst "owned-tree constructor payload is not aggregate"))
  in
  loop state value breadcrumbs
let selected_value_without_state =
  Logical_spec_evaluation_private.selected_value_without_state
let selected_parametric_value_without_state =
  Logical_spec_evaluation_private.selected_parametric_value_without_state
let selector_domain aggregate selector =
  { selector with Vir.selector_domain = aggregate.Vir.aggregate_type }
let ranges_of_value = Logical_spec_evaluation_private.ranges_of_value
let bind_scalar state (binding : Sst.binding) value =
  let sort =
    match binding.typ with
    | Sst.Int -> Some Vir.Integer
    | Sst.Bool -> Some Vir.Boolean
    | Sst.Parameter binder -> Some (Vir.Parametric binder)
    | Sst.Unit | Sst.Tuple _ | Sst.Aggregate _ | Sst.Application _ -> None
  in
  match sort with
  | None ->
      let owned_root_versions =
        List.filter
          (fun (binding_id, _, _) -> binding_id <> binding.id)
          state.owned_root_versions
      in
      let owned_root_versions =
        match value with
        | Aggregate_value aggregate ->
            let version =
              List.find_map
                (fun (_, candidate, version) ->
                  if candidate = aggregate then Some version else None)
                state.owned_root_versions
              |> Option.value ~default:0
            in
            (binding.id, aggregate, version) :: owned_root_versions
        | Unit_value | Integer_value _ | Boolean_value _ | Tuple_value _
        | Parametric_value _ | Function_value _ ->
            owned_root_versions
      in
      Ok
        {
          state with
          environment = (binding.id, value) :: state.environment;
          owned_root_versions;
        }
  | Some sort ->
      let symbol, state =
        fresh_symbol state ~source_name:binding.name ~sort ~role:Vir.Local
          ~span:binding.span
          ~project:(match sort with Vir.Parametric _ -> false | _ -> true)
      in
      let alias =
        match sort with
        | Vir.Integer -> Integer_value (Vir.Integer_symbol symbol)
        | Vir.Boolean -> Boolean_value (Vir.Boolean_symbol symbol)
        | Vir.Parametric _ ->
            Parametric_value
              (Result.get_ok (Parametric_logic_private.of_symbol symbol))
        | Vir.Aggregate _ -> assert false
      in
      let* equation =
        match equality alias value with
        | Some equation -> Ok equation
        | None ->
            Error
              {
                function_name = "";
                span = binding.span;
                unsupported = Malformed_sst "binding type/value mismatch";
              }
      in
      let assumptions =
        match alias with
        | Integer_value term -> equation :: Vir.integer_range term
        | Boolean_value _ | Parametric_value _ | Function_value _ -> [ equation ]
        | Unit_value | Tuple_value _ | Aggregate_value _ -> assert false
      in
      Ok
        {
          state with
          environment = (binding.id, alias) :: state.environment;
          assumptions = append state.assumptions assumptions;
        }
let rec bind_pattern function_name state (pattern : Sst.pattern) value =
  match pattern.pattern_desc with
  | Sst.Wildcard -> Ok state
  | Sst.Bind binding -> (
      match bind_scalar state binding value with
      | Ok state -> Ok state
      | Error error -> Error { error with function_name })
  | Sst.Owned_tree_cursor_pattern _ ->
      error function_name pattern.span
        (Malformed_sst "owned-tree cursor used as an irrefutable binding")
  | Sst.Unit_pattern -> (
      match value with
      | Unit_value -> Ok state
      | _ ->
          error function_name pattern.span
            (Malformed_sst "unit pattern type/value mismatch"))
  | Sst.Tuple_pattern patterns -> (
      match value with
      | Tuple_value values when List.length patterns = List.length values ->
          List.fold_left2
            (fun result (_, pattern) value ->
              let* state = result in
              bind_pattern function_name state pattern value)
            (Ok state) patterns values
      | _ ->
          error function_name pattern.span
            (Malformed_sst "tuple pattern type/value mismatch"))
  | Sst.Record_pattern fields -> (
      match value with
      | Aggregate_value aggregate ->
          List.fold_left
            (fun result (field, (pattern : Sst.pattern)) ->
              let* state = result in
              let* selected, state =
                select_field state aggregate field pattern.Sst.typ
              in
              bind_pattern function_name state pattern selected)
            (Ok state) fields
      | _ ->
          error function_name pattern.span
            (Malformed_sst "record pattern type/value mismatch"))
  | Sst.Constructor_pattern _ ->
      error function_name pattern.span
        (Malformed_sst "refutable constructor used as a binding pattern")
  | Sst.Int_pattern _ | Sst.Bool_pattern _ ->
      error function_name pattern.span
        (Malformed_sst "refutable binding pattern reached VIR lowering")
  | Sst.Or_pattern _ -> error function_name pattern.span Or_pattern
let rec bind_pattern_direct ?(parametric_adts = []) function_name environment
    (pattern : Sst.pattern) value =
  match pattern.pattern_desc with
  | Sst.Wildcard -> Ok (environment, [])
  | Sst.Bind binding -> Ok ((binding.id, value) :: environment, [])
  | Sst.Owned_tree_cursor_pattern _ ->
      error function_name pattern.span
        (Malformed_sst "owned-tree cursor used as an irrefutable binding")
  | Sst.Unit_pattern -> (
      match value with
      | Unit_value -> Ok (environment, [])
      | _ ->
          error function_name pattern.span
            (Malformed_sst "unit pattern type/value mismatch"))
  | Sst.Tuple_pattern patterns -> (
      match value with
      | Tuple_value values when List.length patterns = List.length values ->
          List.fold_left2
            (fun result (_, pattern) value ->
              let* environment, ranges = result in
              let* environment, nested_ranges =
                bind_pattern_direct ~parametric_adts function_name environment
                  pattern value
              in
              Ok (environment, ranges @ nested_ranges))
            (Ok (environment, []))
            patterns values
      | _ ->
          error function_name pattern.span
            (Malformed_sst "tuple pattern type/value mismatch"))
  | Sst.Record_pattern fields -> (
      match value with
      | Aggregate_value aggregate ->
          List.fold_left
            (fun result (field, (pattern : Sst.pattern)) ->
              let* environment, ranges = result in
              let selected =
                if parametric_adts = [] then
                  selected_value_without_state aggregate (field_selector field)
                    [] pattern.Sst.typ
                else
                  selected_parametric_value_without_state
                    ~aggregate_type:(vir_aggregate_type_of_sst parametric_adts)
                    aggregate (field_selector field) [] pattern.Sst.typ
              in
              let* environment, nested_ranges =
                bind_pattern_direct ~parametric_adts function_name environment
                  pattern selected
              in
              Ok (environment, ranges @ ranges_of_value selected @ nested_ranges))
            (Ok (environment, []))
            fields
      | _ ->
          error function_name pattern.span
            (Malformed_sst "record pattern type/value mismatch"))
  | Sst.Constructor_pattern _ ->
      error function_name pattern.span
        (Malformed_sst "refutable constructor used as an irrefutable binding")
  | Sst.Int_pattern _ | Sst.Bool_pattern _ ->
      error function_name pattern.span
        (Malformed_sst "refutable pattern used as an irrefutable binding")
  | Sst.Or_pattern _ -> error function_name pattern.span Or_pattern
let option_constructor instance constructor =
  {
    Sst.constructor_type =
      Parametric_adt.type_id instance.Parametric_adt.option_descriptor;
    constructor_index = constructor.Parametric_adt.constructor_index;
    constructor_name = constructor.constructor_name;
  }
let optional_argument function_name span = function
  | Integer_value term -> Ok (Vir.Recursive_integer_argument term)
  | Boolean_value term -> Ok (Vir.Recursive_boolean_argument term)
  | Aggregate_value term -> Ok (Vir.Recursive_aggregate_argument term)
  | Parametric_value term -> Ok (Vir.Recursive_parametric_argument term)
  | Function_value _ ->
      error function_name span
        (Malformed_sst "optional payload cannot store a function value")
  | Unit_value ->
      Ok (Vir.Recursive_boolean_argument (Vir.Boolean_constant true))
  | Tuple_value _ ->
      error function_name span
        (Malformed_sst
           "optional payload escaped the scalar/aggregate VIR boundary")
let trigger_argument function_name span = function
  | Function_value function_ ->
      Ok (Vir.Recursive_parametric_argument function_.function_term)
  | value -> optional_argument function_name span value
let symbolic_argument function_name span = function
  | Function_value function_ ->
      Ok (Vir.Recursive_parametric_argument function_.function_term)
  | value -> optional_argument function_name span value
let construct_optional function_name span descriptors typ payload =
  match
    ( Parametric_adt.option_instance descriptors typ,
      vir_aggregate_type_of_sst descriptors typ )
  with
  | Some instance, Some aggregate_type ->
      let* constructor, arguments =
        match payload with
        | None -> Ok (instance.option_absent, [])
        | Some payload ->
            let* argument = optional_argument function_name span payload in
            Ok (instance.option_present, [ argument ])
      in
      Ok
        (Aggregate_value
           {
             Vir.aggregate_type;
             aggregate_desc =
               Vir.Aggregate_constructor
                 {
                   constructor = option_constructor instance constructor;
                   arguments;
                 };
           })
  | (None | Some _), (None | Some _) ->
      error function_name span
        (Malformed_sst "optional carrier lacks the pinned compiler descriptor")
let resolve_optional_formal ~evaluate_default ~function_name ~span ~environment
    ~state ~obligations ~carrier_type optional_default aggregate =
  let* instance =
    match Parametric_adt.option_instance state.parametric_adts carrier_type with
    | Some instance -> Ok instance
    | None ->
        error function_name span
          (Malformed_sst "optional formal lacks the pinned compiler descriptor")
  in
  let* expected_type =
    match vir_aggregate_type_of_sst state.parametric_adts carrier_type with
    | Some expected_type when expected_type = aggregate.Vir.aggregate_type ->
        Ok expected_type
    | Some _ | None ->
        error function_name span
          (Malformed_sst
             "optional formal received the wrong descriptor instance")
  in
  let present = option_constructor instance instance.option_present in
  let presence =
    Vir.Integer_compare
      ( Vir.Equal,
        Vir.Aggregate_tag (expected_type, aggregate),
        Vir.Integer_constant (Z.of_int present.constructor_index) )
  in
  let payload =
    selected_parametric_value_without_state
      ~aggregate_type:(vir_aggregate_type_of_sst state.parametric_adts)
      aggregate
      (fun path sort ->
        {
          (argument_selector present 0 path sort) with
          Vir.selector_domain = aggregate.aggregate_type;
        })
      [] instance.option_payload_type
  in
  let* evaluated_default = evaluate_default { state with environment } in
  let* default, state =
    match evaluated_default.paths with
    | [ evaluated ] -> Ok (evaluated.value, evaluated.state)
    | [] | _ :: _ :: _ ->
        error function_name span
          (Malformed_sst "optional default did not have one exact path")
  in
  let* payload, state =
    match presence with
    | Vir.Boolean_constant true -> Ok (payload, state)
    | Vir.Boolean_constant false -> Ok (default, state)
    | presence ->
        let* selected, state =
          fresh_value state ~source_name:"optional.selected" ~role:Vir.Local
            ~span ~project:false instance.option_payload_type
        in
        let* present_equality =
          match equality selected payload with
          | Some equality -> Ok equality
          | None ->
              error function_name span
                (Malformed_sst "optional present payload type mismatch")
        in
        let* default_equality =
          match equality selected default with
          | Some equality -> Ok equality
          | None ->
              error function_name span
                (Malformed_sst "optional default type mismatch")
        in
        let assumptions =
          [
            Vir.Boolean_or (Vir.Boolean_not presence, present_equality);
            Vir.Boolean_or (presence, default_equality);
          ]
        in
        Ok (selected, with_assumptions state assumptions)
  in
  let obligations = append obligations evaluated_default.obligations in
  let* environment, ranges =
    bind_pattern_direct function_name environment
      optional_default.Sst.optional_pattern payload
  in
  Ok (environment, ranges, state, obligations)
let rec fresh_parameter function_name state index (pattern : Sst.pattern) =
  match pattern.pattern_desc with
  | Sst.Bind binding ->
      let* value, state =
        fresh_value state ~source_name:binding.name ~role:Vir.Input
          ~span:binding.span ~project:true binding.typ
      in
      let state =
        match value with
        | Aggregate_value aggregate ->
            {
              state with
              environment = (binding.id, value) :: state.environment;
              owned_root_versions =
                (binding.id, aggregate, 0) :: state.owned_root_versions;
            }
        | Unit_value | Integer_value _ | Boolean_value _ | Tuple_value _
        | Parametric_value _ | Function_value _ ->
            {
              state with
              environment = (binding.id, value) :: state.environment;
            }
      in
      Ok (value, state)
  | Sst.Owned_tree_cursor_pattern _ ->
      error function_name pattern.span
        (Malformed_sst "owned-tree cursor used as a function parameter")
  | Sst.Wildcard ->
      fresh_value state
        ~source_name:(Printf.sprintf "_parameter%d" index)
        ~role:Vir.Input ~span:pattern.span ~project:false pattern.typ
  | Sst.Unit_pattern -> Ok (Unit_value, state)
  | Sst.Tuple_pattern patterns ->
      let rec loop component state values = function
        | [] -> Ok (Tuple_value (List.rev values), state)
        | (_, pattern) :: rest ->
            let* value, state =
              fresh_parameter function_name state
                ((index * 1000) + component + 1)
                pattern
            in
            loop (component + 1) state (value :: values) rest
      in
      loop 0 state [] patterns
  | Sst.Record_pattern _ ->
      let* value, state =
        fresh_value state
          ~source_name:(Printf.sprintf "_parameter%d" index)
          ~role:Vir.Input ~span:pattern.span ~project:true pattern.typ
      in
      let* state = bind_pattern function_name state pattern value in
      Ok (value, state)
  | Sst.Constructor_pattern _ ->
      error function_name pattern.span
        (Malformed_sst
           "refutable constructor function parameter reached VIR lowering")
  | Sst.Int_pattern _ | Sst.Bool_pattern _ ->
      error function_name pattern.span
        (Malformed_sst "refutable function parameter reached VIR lowering")
  | Sst.Or_pattern _ -> error function_name pattern.span Or_pattern
let expect_integer function_name span = function
  | Integer_value term -> Ok term
  | _ -> error function_name span (Malformed_sst "expected integer value")
let expect_boolean function_name span = function
  | Boolean_value term -> Ok term
  | _ -> error function_name span (Malformed_sst "expected Boolean value")
let ranked_measure function_name span rank_domains domain value =
  match (domain, value) with
  | Termination.Integer_height, Integer_value term -> Ok term
  | Termination.Structural_rank certificate, Aggregate_value aggregate -> (
      let rank_id = Termination.structural_rank_id certificate in
      match
        List.find_opt
          (fun domain -> String.equal (Vir.rank_domain_id domain) rank_id)
          rank_domains
      with
      | Some domain ->
          if
            List.exists
              (( = ) aggregate.Vir.aggregate_type)
              (Vir.rank_domain_component domain)
          then Ok (Vir.Integer_rank_project (domain, aggregate))
          else
            error function_name span
              (Malformed_sst
                 "structural measure type is outside its sealed rank domain")
      | None ->
          error function_name span
            (Malformed_sst "sealed structural rank domain is unavailable"))
  | Termination.Frozen_spine_direct_edge frozen, Aggregate_value aggregate ->
      Ok
        (match aggregate.Vir.aggregate_desc with
        | Vir.Aggregate_selector (selector, _)
          when selector.selector_index
               = frozen.Sst.frozen_next_child_field.field_index
               && selector.selector_domain.aggregate_type_index
                  = frozen.frozen_link.type_index
               && String.equal selector.selector_domain.aggregate_type_name
                    frozen.frozen_link.type_name ->
            Vir.Integer_constant Z.zero
        | _ -> Vir.Integer_constant Z.one)
  | Termination.Parametric_direct_edge _, Aggregate_value aggregate ->
      Ok
        (match aggregate.Vir.aggregate_desc with
        | Vir.Aggregate_selector _ -> Vir.Integer_constant Z.zero
        | _ -> Vir.Integer_constant Z.one)
  | ( ( Termination.Integer_height | Termination.Structural_rank _
      | Termination.Parametric_direct_edge _
      | Termination.Frozen_spine_direct_edge _ ),
      _ ) ->
      error function_name span
        (Malformed_sst "recursive measure value has the wrong sealed domain")
let same_decrease_domain left right =
  match (left, right) with
  | Termination.Integer_height, Termination.Integer_height -> true
  | Termination.Structural_rank left, Termination.Structural_rank right ->
      String.equal
        (Termination.structural_rank_id left)
        (Termination.structural_rank_id right)
  | ( Termination.Parametric_direct_edge left,
      Termination.Parametric_direct_edge right ) ->
      String.equal
        (Parametric_adt.to_string left)
        (Parametric_adt.to_string right)
  | ( Termination.Frozen_spine_direct_edge left,
      Termination.Frozen_spine_direct_edge right ) ->
      left.Sst.frozen_root = right.Sst.frozen_root
      && left.frozen_link = right.frozen_link
      && left.frozen_next_child_field = right.frozen_next_child_field
  | ( Termination.Integer_height,
      ( Termination.Structural_rank _ | Termination.Parametric_direct_edge _
      | Termination.Frozen_spine_direct_edge _ ) )
  | ( Termination.Structural_rank _,
      ( Termination.Integer_height | Termination.Parametric_direct_edge _
      | Termination.Frozen_spine_direct_edge _ ) )
  | ( Termination.Parametric_direct_edge _,
      ( Termination.Integer_height | Termination.Structural_rank _
      | Termination.Frozen_spine_direct_edge _ ) )
  | ( Termination.Frozen_spine_direct_edge _,
      ( Termination.Integer_height | Termination.Structural_rank _
      | Termination.Parametric_direct_edge _ ) ) ->
      false
let evaluate_contexts = Call_contract_execution_private.evaluate_contexts
let arithmetic_operation = function
  | Sst.Add -> Vir.Add
  | Sst.Subtract -> Vir.Subtract
  | Sst.Negate -> Vir.Negate
  | Sst.Multiply_constant value -> Vir.Multiply_constant value
  | Sst.Successor -> Vir.Successor
  | Sst.Predecessor -> Vir.Predecessor
  | Sst.Absolute_value -> Vir.Absolute_value
let arithmetic_term function_name span operation arguments =
  let* arguments =
    let rec loop values = function
      | [] -> Ok (List.rev values)
      | value :: rest ->
          let* value = expect_integer function_name span value in
          loop (value :: values) rest
    in
    loop [] arguments
  in
  match (operation, arguments) with
  | Sst.Add, [ left; right ] -> Ok (Vir.Integer_add (left, right))
  | Sst.Subtract, [ left; right ] -> Ok (Vir.Integer_subtract (left, right))
  | Sst.Negate, [ value ] -> Ok (Vir.Integer_negate value)
  | Sst.Multiply_constant constant, [ value ] ->
      Ok (Vir.Integer_multiply_constant (constant, value))
  | Sst.Successor, [ value ] ->
      Ok (Vir.Integer_add (value, Vir.Integer_constant Z.one))
  | Sst.Predecessor, [ value ] ->
      Ok (Vir.Integer_subtract (value, Vir.Integer_constant Z.one))
  | Sst.Absolute_value, [ value ] -> Ok (Vir.Integer_absolute_value value)
  | _ ->
      error function_name span
        (Malformed_sst "checked arithmetic has an invalid arity")
let emit_checked function_ref span operation mathematical_result state =
  let lower, upper =
    match Vir.integer_range mathematical_result with
    | [ lower; upper ] -> (lower, upper)
    | _ -> assert false
  in
  let make obligation_index violated_bound goal =
    {
      Vir.obligation_index;
      function_ref;
      kind =
        Arithmetic_safety
          {
            operation = arithmetic_operation operation;
            mathematical_result;
            violated_bound;
          };
      span;
      assumptions = effective_assumptions state;
      required_preceding_safety = state.required_preceding_safety;
      path_condition = state.path_condition;
      goal;
      projection_symbols = state.projection_symbols;
    }
  in
  let snapshot obligation =
    {
      provisional_obligation = obligation;
      expected_proof_activation_authority = state.proof_activation_authority;
      proof_activation_snapshot =
        Option.map
          (fun authority ->
            Verification_session.snapshot_proof_activation authority
              ~activations:state.proof_activations
              ~ground_matches:state.ground_constructor_matches obligation)
          state.proof_activation_authority;
    }
  in
  let obligations =
    [
      snapshot (make state.next_obligation Vir.Lower_bound lower);
      snapshot (make (state.next_obligation + 1) Vir.Upper_bound upper);
    ]
  in
  (* Neither bound is present in either obligation's assumptions.  Both become
     certified facts only after both ordered obligations have been emitted, so
     a later operation may rely on earlier safety without circularly proving
     the operation that introduced the mathematical result. *)
  let state =
    {
      state with
      assumptions = append state.assumptions [ lower; upper ];
      required_preceding_safety =
        append state.required_preceding_safety [ lower; upper ];
      next_obligation = state.next_obligation + 2;
    }
  in
  {
    obligations;
    paths = [ { value = Integer_value mathematical_result; state } ];
  }
let solver_assumptions kind goal state =
  let include_facts =
    match Recursive_spec_retry_demand_private.of_goal goal with
    | Ok (Some demand) -> (
        match kind with
        | Vir.Local_assertion _ ->
            let callee = Recursive_spec_retry_demand_private.callee demand in
            List.exists
              (fun (activation : Spec_unfolding.activation) ->
                activation.function_id.function_index = callee.function_index
                && String.equal activation.function_id.function_name
                     callee.function_name
                && activation.depth >= 1 && activation.depth <= 64)
              state.proof_activations
        | Vir.Arithmetic_safety _ | Vir.Assertion _ | Vir.Postcondition _
        | Vir.Call_precondition _ | Vir.Callback_precondition _
        | Vir.Invariant_validity _ | Vir.Entry_measure_nonnegative _
        | Vir.Recursive_call_measure_nonnegative _
        | Vir.Recursive_call_strict_descent _ ->
            true)
    | Ok None -> (
        Immutable_aggregate_fact_relevance_private.goal_has_aggregate_equality
          goal
        ||
        match kind with
        | Vir.Local_assertion _ ->
            Immutable_aggregate_fact_relevance_private.facts_relevant_to_terms
              state.immutable_aggregate_facts
              (goal :: state.path_condition)
        | _ -> false)
    | Error _ -> false
  in
  if include_facts then
    append (effective_assumptions state) state.immutable_aggregate_facts
  else effective_assumptions state
let emit_goal function_ref kind span goal state =
  let obligation =
    {
      Vir.obligation_index = state.next_obligation;
      function_ref;
      kind;
      span;
      assumptions = solver_assumptions kind goal state;
      required_preceding_safety = state.required_preceding_safety;
      path_condition = state.path_condition;
      goal;
      projection_symbols = state.projection_symbols;
    }
  in
  let emitted =
    {
      provisional_obligation = obligation;
      expected_proof_activation_authority = state.proof_activation_authority;
      proof_activation_snapshot =
        Option.map
          (fun authority ->
            Verification_session.snapshot_proof_activation authority
              ~activations:state.proof_activations
              ~ground_matches:state.ground_constructor_matches obligation)
          state.proof_activation_authority;
    }
  in
  let state =
    {
      state with
      assumptions = append state.assumptions [ goal ];
      next_obligation = state.next_obligation + 1;
    }
  in
  (emitted, state)
let closed_invariant_application handle = function
  | Aggregate_value value
    when value.Vir.aggregate_type
         = vir_aggregate_type (Type_invariant.abstract_type handle) ->
      Ok
        (Vir.Boolean_invariant_application
           {
             invariant_id = Type_invariant.invariant_id handle;
             model = Type_invariant.model_callable handle;
             predicate = Type_invariant.predicate_callable handle;
             value;
           })
  | Aggregate_value _ | Unit_value | Integer_value _ | Boolean_value _
  | Tuple_value _ | Parametric_value _ | Function_value _ ->
      Error "invariant boundary value does not have the exact abstract type"
let emit_closed_invariant_goal ?(verified_assumptions = []) context handle
    boundary operation span value state =
  match closed_invariant_application handle value with
  | Error message ->
      error context.function_ref.function_name span (Malformed_sst message)
  | Ok goal ->
      let kind =
        Vir.Invariant_validity
          {
            invariant_id = Type_invariant.invariant_id handle;
            abstract_type =
              Type_invariant.abstract_type handle |> vir_aggregate_type;
            model = Type_invariant.model_callable handle |> vir_function_ref;
            predicate =
              Type_invariant.predicate_callable handle |> vir_function_ref;
            operation;
            boundary;
          }
      in
      let obligation =
        {
          Vir.obligation_index = state.next_obligation;
          function_ref = context.function_ref;
          kind;
          span;
          assumptions =
            append
              (append
                 (effective_assumptions state)
                 state.closed_invariant_facts)
              verified_assumptions;
          required_preceding_safety = state.required_preceding_safety;
          path_condition = state.path_condition;
          goal;
          projection_symbols = state.projection_symbols;
        }
      in
      let state =
        {
          (with_closed_invariant_fact state goal) with
          next_obligation = state.next_obligation + 1;
        }
      in
      let emitted =
        {
          provisional_obligation = obligation;
          expected_proof_activation_authority = state.proof_activation_authority;
          proof_activation_snapshot =
            Option.map
              (fun authority ->
                Verification_session.snapshot_proof_activation authority
                  ~activations:state.proof_activations
                  ~ground_matches:state.ground_constructor_matches obligation)
              state.proof_activation_authority;
        }
      in
      Ok (emitted, state)
let invariant_for_typ environment = function
  | Sst.Aggregate type_id -> Type_invariant.find_for_type environment type_id
  | Sst.Unit | Sst.Int | Sst.Bool | Sst.Tuple _ | Sst.Parameter _
  | Sst.Application _ ->
      None
let boolean_and terms =
  List.fold_left
    (fun left right -> Vir.Boolean_and (left, right))
    (Vir.Boolean_constant true) terms
let same_type_id (left : Sst.type_id) (right : Sst.type_id) =
  left.type_index = right.type_index
  && String.equal left.type_name right.type_name
let same_function_id (left : Sst.function_id) (right : Sst.function_id) =
  left.function_index = right.function_index
  && String.equal left.function_name right.function_name
let same_direct_recursion plan left right =
  if not (same_function_id left right) then false
  else
    match Termination.find_pending_summary plan left with
    | None -> false
    | Some pending -> (
        match Termination.scc_members (Termination.pending_group pending) with
        | [ callable ] ->
            same_function_id (Termination.callable_id callable) left
        | [] | _ :: _ :: _ -> false)
let frozen_spine_for_transition validated function_id
    (transition : Sst.shared_scalar_heap_transition) =
  match
    Sst_validation.abstraction_evidence validated transition.shared_record_type
  with
  | Some evidence -> (
      match Sst.frozen_spine_prerequisite evidence with
      | Some frozen
        when same_function_id function_id frozen.frozen_mutator
             && frozen.frozen_root = transition.shared_record_type
             && frozen.frozen_payload_field = transition.shared_target_field ->
          Some frozen
      | Some _ | None -> None)
  | None -> None
let frozen_spine_for_model validated function_id =
  (Sst_validation.program validated).Sst.types
  |> List.find_map (fun (definition : Sst.type_definition) ->
      match definition.representation with
      | Sst.Abstract_with_evidence
          (Sst.Authenticated_same_cmt_abstraction evidence) -> (
          match Sst.frozen_spine_prerequisite evidence with
          | Some frozen when same_function_id function_id frozen.frozen_model ->
              Some frozen
          | Some _ | None -> None)
      | Sst.Revealed
      | Sst.Abstract_with_evidence
          ( Sst.Incomplete_abstraction_evidence _
          | Sst.Proposed_same_cmt_abstraction _ ) ->
          None)
let frozen_spine_for_constructor validated function_id =
  (Sst_validation.program validated).Sst.types
  |> List.find_map (fun (definition : Sst.type_definition) ->
      match definition.representation with
      | Sst.Abstract_with_evidence
          (Sst.Authenticated_same_cmt_abstraction evidence) -> (
          match Sst.frozen_spine_prerequisite evidence with
          | Some frozen
            when same_function_id function_id frozen.frozen_constructor ->
              Some frozen
          | Some _ | None -> None)
      | Sst.Revealed
      | Sst.Abstract_with_evidence
          ( Sst.Incomplete_abstraction_evidence _
          | Sst.Proposed_same_cmt_abstraction _ ) ->
          None)
let frozen_spine_terminal validated function_id =
  (Sst_validation.program validated).Sst.types
  |> List.exists (fun (definition : Sst.type_definition) ->
      match definition.representation with
      | Sst.Abstract_with_evidence
          (Sst.Authenticated_same_cmt_abstraction evidence) -> (
          match Sst.frozen_spine_prerequisite evidence with
          | Some frozen -> same_function_id function_id frozen.frozen_terminal
          | None -> false)
      | Sst.Revealed
      | Sst.Abstract_with_evidence
          ( Sst.Incomplete_abstraction_evidence _
          | Sst.Proposed_same_cmt_abstraction _ ) ->
          false)
let record_frozen_observation context constructor_scope frozen model value
    child_value assumptions state =
  let assumptions =
    assumptions @ Vir.integer_range value
    @ Option.fold ~none:[] ~some:Vir.integer_range child_value
  in
  let state = with_assumptions state assumptions in
  let state =
    match context.frozen_observation_scope with
    | Some scope when not constructor_scope ->
        {
          state with
          frozen_terminal_observations =
            {
              frozen_terminal_permit = scope.frozen_scope_permit;
              frozen_terminal_descriptor = frozen;
              frozen_terminal_root = scope.frozen_scope_root;
              frozen_terminal_model = model;
              frozen_terminal_value = value;
              frozen_terminal_call_path = scope.frozen_scope_call_path;
              frozen_terminal_path_condition =
                scope.frozen_scope_path_condition;
              frozen_terminal_epoch = scope.frozen_scope_epoch;
            }
            :: state.frozen_terminal_observations;
        }
    | Some _ | None -> state
  in
  (state, model)
let same_constructor_id (left : Sst.constructor_id) (right : Sst.constructor_id)
    =
  same_type_id left.constructor_type right.constructor_type
  && left.constructor_index = right.constructor_index
  && String.equal left.constructor_name right.constructor_name
let same_rank_domain left right =
  String.equal (Vir.rank_domain_id left) (Vir.rank_domain_id right)
  && String.equal (Vir.rank_domain_version left) (Vir.rank_domain_version right)
  && String.equal (Vir.rank_domain_digest left) (Vir.rank_domain_digest right)
  && Vir.rank_domain_component left = Vir.rank_domain_component right
let rank_domain_contains_type domain type_id =
  List.exists
    (fun aggregate -> aggregate = vir_aggregate_type type_id)
    (Vir.rank_domain_component domain)
let merge_rank_domain left right =
  match (left, right) with
  | None, domain | domain, None -> Ok domain
  | Some left, Some right when same_rank_domain left right -> Ok (Some left)
  | Some _, Some _ -> Error ()
let query_rank_domain ~parametric_adts ~rank_domains ~type_definitions typ =
  let rec classify visited typ =
    match typ with
    | Sst.Application _ -> (
        match vir_aggregate_type_of_sst parametric_adts typ with
        | None -> Ok None
        | Some aggregate ->
            Ok
              (List.find_opt
                 (fun domain ->
                   List.mem aggregate (Vir.rank_domain_component domain))
                 rank_domains))
    | Sst.Unit | Sst.Bool | Sst.Int | Sst.Parameter _ -> Ok None
    | Sst.Tuple components ->
        List.fold_left
          (fun result (_, typ) ->
            match result with
            | Error () -> Error ()
            | Ok selected -> (
                match classify visited typ with
                | Error () -> Error ()
                | Ok next -> merge_rank_domain selected next))
          (Ok None) components
    | Sst.Aggregate type_id -> (
        match
          List.find_opt
            (fun domain -> rank_domain_contains_type domain type_id)
            rank_domains
        with
        | Some domain -> Ok (Some domain)
        | None when List.mem type_id visited -> Ok None
        | None -> (
            match
              List.find_opt
                (fun (definition : Sst.type_definition) ->
                  same_type_id definition.type_id type_id)
                type_definitions
            with
            | None -> Ok None
            | Some
                {
                  representation =
                    Sst.Abstract_with_evidence
                      (Sst.Authenticated_same_cmt_abstraction _);
                  type_kind;
                  _;
                }
            | Some { representation = Sst.Revealed; type_kind; _ } ->
                let fields =
                  match type_kind with
                  | Sst.Record_definition fields -> fields
                  | Sst.Variant_definition constructors ->
                      List.concat_map
                        (fun (constructor : Sst.constructor_definition) ->
                          constructor.constructor_fields)
                        constructors
                in
                if
                  List.exists
                    (fun (field : Sst.field_definition) ->
                      field.field_mutability <> Sst.Immutable_field
                      || field.field_modalities.uniqueness_modality
                         = Sst.Force_aliased)
                    fields
                then Ok None
                else
                  List.fold_left
                    (fun result (field : Sst.field_definition) ->
                      match result with
                      | Error () -> Error ()
                      | Ok selected -> (
                          match
                            classify (type_id :: visited) field.field_type
                          with
                          | Error () -> Error ()
                          | Ok next -> merge_rank_domain selected next))
                    (Ok None) fields
            | Some
                {
                  representation =
                    Sst.Abstract_with_evidence
                      ( Sst.Incomplete_abstraction_evidence _
                      | Sst.Proposed_same_cmt_abstraction _ );
                  _;
                } ->
                Ok None))
  in
  classify [] typ
let rank_domain_for_type context span typ =
  match
    query_rank_domain
      ~parametric_adts:
        (Sst_validation.program context.validated).Sst.parametric_adts
      ~rank_domains:context.rank_domains
      ~type_definitions:context.type_definitions typ
  with
  | Ok domain -> Ok domain
  | Error () ->
      error context.function_ref.function_name span
        (Malformed_sst "finite value shape combines distinct rank domains")
let static_rank_domain_for_type parametric_adts rank_domains type_definitions
    typ =
  match
    query_rank_domain ~parametric_adts ~rank_domains ~type_definitions typ
  with
  | Ok domain -> domain
  | Error () -> None
let rank_free_type context typ =
  static_rank_domain_for_type (Sst_validation.program context.validated).Sst.parametric_adts context.rank_domains context.type_definitions typ = None
let finite_result_rank context span typ =
  match typ with
  | Sst.Application _ | Sst.Aggregate _ -> rank_domain_for_type context span typ
  | Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _ | Sst.Parameter _ -> Ok None
let authenticated_logical_application validated typ =
  Logical_adt_encoding_private.authenticates_application
    ~descriptors:(Sst_validation.program validated).Sst.parametric_adts typ
let exact_agg_type context typ =
  match
    vir_aggregate_type_of_sst
      (Sst_validation.program context.validated).Sst.parametric_adts typ
  with
  | Some aggregate_type -> aggregate_type
  | None -> assert false
let admits_rank_free_recursive_result validated typ =
  Option.is_some (Sst_validation.find_logical_type validated typ)
  || authenticated_logical_application validated typ
let rank_free_recursive_result context span typ observed_result state =
  if admits_rank_free_recursive_result context.validated typ then
    Ok (Aggregate_value observed_result, state)
  else
    error context.function_ref.function_name span
      (Malformed_sst
         "recursive aggregate result has no exact local rank domain")
let finite_rank_snapshot_of_certificate domain certificate =
  let component_snapshot =
    Sst_validation.rank_component certificate
    |> List.map (fun (identity : Typedtree_adapter.rank_type_identity) ->
        Printf.sprintf "%s#%d|%s|%s" identity.rank_type_id.type_name
          identity.rank_type_id.type_index identity.rank_path identity.rank_uid)
  in
  let child_snapshot =
    Sst_validation.rank_positive_children certificate
    |> List.map (fun (child : Typedtree_adapter.rank_positive_child) ->
        Printf.sprintf "child:%s#%d|%s|%d|%s|%s|%s#%d|%s"
          child.rank_constructor.constructor_name
          child.rank_constructor.constructor_index child.rank_constructor_uid
          child.rank_field.field_index child.rank_field_uid
          (String.concat "." (List.map string_of_int child.rank_child_path))
          child.rank_child_type.type_name child.rank_child_type.type_index
          (String.concat ">" child.rank_expansion_trace))
  in
  let ground_snapshot =
    Sst_validation.rank_ground_witnesses certificate
    |> List.map (fun (witness : Typedtree_adapter.rank_ground_witness) ->
        Printf.sprintf "ground:%s#%d|%s"
          witness.rank_ground_constructor.constructor_name
          witness.rank_ground_constructor.constructor_index
          witness.rank_ground_constructor_uid)
  in
  let digest = Vir.rank_domain_digest domain in
  {
    Finite_value_registry.domain_id = Vir.rank_domain_id domain;
    domain_version = Vir.rank_domain_version domain;
    domain_digest = digest;
    component_snapshot;
    profile_actual_snapshot =
      ("authenticated-complete-profile/actual-digest:" ^ digest)
      :: child_snapshot
      @ ground_snapshot;
  }
let finite_rank_snapshot context span domain =
  let* certificate =
    match
      Sst_validation.rank_domains context.validated
      |> List.find_opt (fun certificate ->
          String.equal
            (Sst_validation.rank_domain_id certificate)
            (Vir.rank_domain_id domain)
          && String.equal
               (Sst_validation.rank_domain_version certificate)
               (Vir.rank_domain_version domain)
          && String.equal
               (Sst_validation.rank_snapshot_digest certificate)
               (Vir.rank_domain_digest domain))
    with
    | Some certificate -> Ok certificate
    | None ->
        error context.function_ref.function_name span
          (Malformed_sst "finite rank snapshot is absent or stale")
  in
  Ok (finite_rank_snapshot_of_certificate domain certificate)
let expression_instance_mode context expression =
  try
    Ok
      (match expression.Sst.expression_desc with
      | Sst.Variable { binding; _ } | Sst.Mutable_read binding ->
          Sst_validation.binding_instance_mode context.validated
            context.current_callable binding
      | _ ->
          Sst_validation.expression_instance_mode context.validated
            context.current_callable expression)
  with Invalid_argument _ ->
    if context.logical then Ok Sst.Ghost_instance
    else
      error context.function_ref.function_name expression.Sst.span
        (Malformed_sst "finite construction lacks authenticated instance mode")
let finite_registry context span =
  match (context.verification_session, context.canonical_callable) with
  | Some session, Some callable -> (
      match Verification_session.finite_registry session with
      | Ok registry -> Ok (registry, callable)
      | Error message ->
          error context.function_ref.function_name span (Malformed_sst message))
  | None, None | None, Some _ | Some _, None ->
      error context.function_ref.function_name span
        (Malformed_sst
           "finite authority requires the private verification session")
let authenticate_exact_source_mode registry ~facts ~callable ~value
    ~preferred_mode ~typ ~rank =
  let modes =
    preferred_mode
    :: List.filter (( <> ) preferred_mode)
         [ Sst.Exec_instance; Sst.Tracked_instance; Sst.Ghost_instance ]
  in
  let rec authenticate = function
    | [] ->
        Error
          "exact finite receipt is unavailable for this \
           session/callable/value/version/path/mode/type/domain/profile"
    | mode :: rest -> (
        match
          Finite_value_registry.authenticate registry ~facts ~callable ~value
            ~mode ~typ ~rank
        with
        | Ok receipt -> Ok (receipt, mode)
        | Error _ -> authenticate rest)
  in
  authenticate modes
let authorize_finite_formal_call context ~callee_definition ~arguments
    ~call_span paths =
  let function_name = context.function_ref.function_name in
  let* callee_descriptor =
    match
      Sst_validation.find_callable context.validated
        callee_definition.Sst.function_id
    with
    | Some descriptor -> Ok descriptor
    | None ->
        error function_name call_span
          (Malformed_sst "finite call target is absent from validation")
  in
  let requirements =
    callee_definition.parameters
    |> List.mapi (fun ordinal _ ->
        Option.map
          (fun requirement -> (ordinal, requirement))
          (Sst_validation.finite_formal_requirement context.validated
             callee_descriptor ordinal))
    |> List.filter_map Fun.id
  in
  match requirements with
  | [] -> Ok paths
  | _ ->
      let* session =
        match context.verification_session with
        | Some session -> Ok session
        | None ->
            error function_name call_span
              (Malformed_sst
                 "finite formal call requires the private verification session")
      in
      let* caller =
        match context.canonical_callable with
        | Some caller -> Ok caller
        | None ->
            error function_name call_span
              (Malformed_sst
                 "finite formal call lacks canonical caller identity")
      in
      let* callee =
        match
          Verification_session.canonical_callable_key session callee_definition
        with
        | Ok callee -> Ok callee
        | Error message -> error function_name call_span (Malformed_sst message)
      in
      let* registry, _ = finite_registry context call_span in
      let argument_expressions = List.map snd arguments in
      let prepare_path (state, reversed_actuals) =
        let actuals = List.rev reversed_actuals in
        if List.length actuals <> List.length argument_expressions then
          error function_name call_span
            (Malformed_sst "finite formal call argument count mismatch")
        else
          let* rows =
            let rec collect rows = function
              | [] -> Ok (List.rev rows)
              | (ordinal, requirement) :: rest ->
                  let* actual, (actual_expression : Sst.expression) =
                    match
                      ( List.nth_opt actuals ordinal,
                        List.nth_opt argument_expressions ordinal )
                    with
                    | Some actual, Some expression -> Ok (actual, expression)
                    | _ ->
                        error function_name call_span
                          (Malformed_sst
                             "finite formal call ordinal is out of range")
                  in
                  let* aggregate =
                    match actual with
                    | Aggregate_value aggregate -> Ok aggregate
                    | Unit_value | Integer_value _ | Boolean_value _
                    | Tuple_value _ | Parametric_value _ | Function_value _ ->
                        error function_name actual_expression.span
                          (Malformed_sst
                             "finite formal actual is not an exact aggregate")
                  in
                  let* domain =
                    let* domain =
                      rank_domain_for_type context actual_expression.span
                        actual_expression.typ
                    in
                    match domain with
                    | Some domain -> Ok domain
                    | None ->
                        error function_name actual_expression.span
                          (Malformed_sst
                             "finite formal actual has no exact rank domain")
                  in
                  let* rank =
                    finite_rank_snapshot context actual_expression.span domain
                  in
                  let* preferred_mode =
                    expression_instance_mode context actual_expression
                  in
                  let mode =
                    match
                      authenticate_exact_source_mode registry
                        ~facts:state.finite_receipts ~callable:caller
                        ~value:aggregate ~preferred_mode
                        ~typ:actual_expression.typ ~rank
                    with
                    | Ok (_, mode) -> mode
                    | Error _ -> preferred_mode
                  in
                  let* slot =
                    match
                      Finite_value_registry.find_formal registry ~callee
                        ~ordinal:
                          (Sst_validation.finite_formal_ordinal requirement)
                    with
                    | Some slot -> Ok slot
                    | None ->
                        error function_name call_span
                          (Malformed_sst
                             "finite formal call slot is absent or stale")
                  in
                  collect
                    ((slot, aggregate, mode, actual_expression.typ, rank)
                    :: rows)
                    rest
            in
            collect [] requirements
          in
          Ok (state.path_condition, state.finite_receipts, rows)
      in
      let* facts_and_actuals =
        let rec collect prepared = function
          | [] -> Ok (List.rev prepared)
          | path :: rest ->
              let* prepared_path = prepare_path path in
              collect (prepared_path :: prepared) rest
        in
        collect [] paths
      in
      let* batches =
        match
          Finite_value_registry.authorize_call_paths registry ~caller ~callee
            ~call_span ~facts_and_actuals
        with
        | Ok batches -> Ok batches
        | Error message -> error function_name call_span (Malformed_sst message)
      in
      let* () =
        let rec consume = function
          | [] -> Ok ()
          | batch :: rest -> (
              match Finite_value_registry.consume_call registry batch with
              | Ok () -> consume rest
              | Error message ->
                  error function_name call_span (Malformed_sst message))
        in
        consume batches
      in
      Ok paths
module Direct_recursion_induction = struct
  type prepared = {
    context : Finite_induction.context option;
    actual_receipts : (Vir.aggregate_term * Finite_value_registry.receipt) list;
  }
  let exact_finite_actual_receipts context ~callee_definition ~arguments
      ~actuals ~call_span state =
    let function_name = context.function_ref.function_name in
    let* descriptor =
      match
        Sst_validation.find_callable context.validated
          callee_definition.Sst.function_id
      with
      | Some descriptor -> Ok descriptor
      | None ->
          error function_name call_span
            (Malformed_sst "finite induction target is absent from validation")
    in
    let requirements =
      callee_definition.parameters
      |> List.mapi (fun ordinal _ ->
          Option.map
            (fun requirement -> (ordinal, requirement))
            (Sst_validation.finite_formal_requirement context.validated
               descriptor ordinal))
      |> List.filter_map Fun.id
    in
    let argument_expressions : Sst.expression list = List.map snd arguments in
    let* registry, callable = finite_registry context call_span in
    let rec collect receipts = function
      | [] -> Ok (List.rev receipts)
      | (ordinal, _) :: rest ->
          let* actual, (actual_expression : Sst.expression) =
            match
              ( List.nth_opt actuals ordinal,
                List.nth_opt argument_expressions ordinal )
            with
            | Some (Aggregate_value actual), Some expression ->
                Ok (actual, expression)
            | Some _, Some expression ->
                error function_name expression.span
                  (Malformed_sst
                     "finite induction designated actual is not an aggregate")
            | _ ->
                error function_name call_span
                  (Malformed_sst
                     "finite induction actual ordinal is out of range")
          in
          let* domain =
            let* candidate =
              rank_domain_for_type context actual_expression.span
                actual_expression.typ
            in
            match candidate with
            | Some domain -> Ok domain
            | None ->
                error function_name actual_expression.span
                  (Malformed_sst
                     "finite induction actual has no exact rank domain")
          in
          let* rank =
            finite_rank_snapshot context actual_expression.span domain
          in
          let* preferred_mode =
            expression_instance_mode context actual_expression
          in
          let* receipt, _ =
            match
              authenticate_exact_source_mode registry
                ~facts:state.finite_receipts ~callable ~value:actual
                ~preferred_mode ~typ:actual_expression.typ ~rank
            with
            | Ok authenticated -> Ok authenticated
            | Error message ->
                error function_name actual_expression.span
                  (Malformed_sst message)
          in
          collect ((actual, receipt) :: receipts) rest
    in
    collect [] requirements
  let prepare context ~recursive ~call_form ~callee ~callee_definition
      ~arguments ~actuals ~call_span state =
    let function_name = context.function_ref.function_name in
    let* domain =
      if (not recursive) || call_form <> Sst.Exec_call then Ok None
      else
        finite_result_rank context call_span callee_definition.Sst.result_type
    in
    let* induction =
      match domain with
      | None -> Ok None
      | Some _ -> (
          match
            Termination.find_edge_intent context.termination
              ~caller:context.current_callable ~callee ~span:call_span
          with
          | Some edge -> (
              match Termination.edge_domain edge with
              | Termination.Parametric_direct_edge _
              | Termination.Frozen_spine_direct_edge _ ->
                  Ok None
              | Termination.Integer_height | Termination.Structural_rank _ -> (
                  match
                    Finite_induction.authenticate context.termination
                      context.current_callable
                  with
                  | Ok induction -> Ok induction
                  | Error message ->
                      error function_name call_span (Malformed_sst message)))
          | None ->
              error function_name call_span
                (Malformed_sst
                   "direct finite induction has no authenticated termination \
                    edge"))
    in
    let* actual_receipts =
      match induction with
      | None -> Ok []
      | Some _ ->
          exact_finite_actual_receipts context ~callee_definition ~arguments
            ~actuals ~call_span state
    in
    Ok { context = induction; actual_receipts }
  let bind_hypothesis context prepared ~callee ~call_span ~actual_snapshot
      ~nonnegative_obligation ~strict_descent_obligation state =
    match prepared.context with
    | None -> Ok state
    | Some induction -> (
        let finite_actuals =
          List.map
            (fun (actual, _) ->
              Digest.string (Marshal.to_string actual [ Marshal.No_sharing ])
              |> Digest.to_hex)
            prepared.actual_receipts
        in
        match
          Finite_induction.hypothesis induction ~caller:context.current_callable
            ~callee ~call_span ~actual_snapshot ~finite_actuals
            ~nonnegative_obligation ~strict_descent_obligation
        with
        | Ok hypothesis ->
            Ok
              {
                state with
                pending_finite_induction =
                  Some
                    {
                      induction_hypothesis = hypothesis;
                      induction_actual_receipts = prepared.actual_receipts;
                    };
              }
        | Error message ->
            error context.function_ref.function_name call_span
              (Malformed_sst message))
  let issue_result context (expression : Sst.expression) result state =
    match state.pending_finite_induction with
    | None -> Ok state
    | Some binding -> (
        let state = { state with pending_finite_induction = None } in
        match
          (context.verification_session, context.canonical_callable, result)
        with
        | Some session, Some callable, Aggregate_value aggregate -> (
            let* domain =
              finite_result_rank context expression.span expression.typ
            in
            let* domain =
              match domain with
              | Some domain -> Ok domain
              | None ->
                  error context.function_ref.function_name expression.span
                    (Malformed_sst
                       "finite induction result has no exact rank domain")
            in
            let* rank = finite_rank_snapshot context expression.span domain in
            let* mode = expression_instance_mode context expression in
            let* registry =
              match Verification_session.finite_registry session with
              | Ok registry -> Ok registry
              | Error message ->
                  error context.function_ref.function_name expression.span
                    (Malformed_sst message)
            in
            match
              Finite_value_registry.issue_induction_result registry
                ~hypothesis_snapshot:
                  (Finite_induction.hypothesis_snapshot
                     binding.induction_hypothesis)
                ~actual_receipts:binding.induction_actual_receipts
                ~call_path_digest:
                  (Finite_value_registry.result_path_digest state.path_condition)
                ~callable ~result:aggregate ~mode ~typ:expression.typ ~rank
            with
            | Ok receipt -> Ok (with_finite_receipt state receipt)
            | Error message ->
                error context.function_ref.function_name expression.span
                  (Malformed_sst message))
        | ( (Some _ | None),
            (Some _ | None),
            (Unit_value | Integer_value _ | Boolean_value _ | Tuple_value _) )
          ->
            Ok state
        | _ ->
            error context.function_ref.function_name expression.span
              (Malformed_sst
                 "finite induction result lacks private session identity"))
end
module Finite_result_integration = struct
  let consume_published context ~demanded ~recursive
      ~(callee_definition : Sst.function_definition) ~call_span result state =
    if (not demanded) || recursive then Ok state
    else
      match (context.verification_session, result) with
      | Some session, Aggregate_value aggregate -> (
          let* domain =
            finite_result_rank context call_span callee_definition.result_type
          in
          match domain with
          | None -> Ok state
          | Some domain -> (
              let* descriptor =
                match
                  Sst_validation.find_callable context.validated
                    callee_definition.function_id
                with
                | Some descriptor -> Ok descriptor
                | None ->
                    error context.function_ref.function_name call_span
                      (Malformed_sst
                         "finite-result callee is absent from validated \
                          authority")
              in
              match
                Sst_validation.result_instance_mode context.validated descriptor
              with
              | Sst.Ghost_instance -> Ok state
              | Sst.Exec_instance | Sst.Tracked_instance -> (
                  let* rank = finite_rank_snapshot context call_span domain in
                  let* snapshot =
                    match
                      Verification_session.finite_result_snapshot session
                        ~validated:context.validated callee_definition ~rank
                    with
                    | Ok snapshot -> Ok snapshot
                    | Error message ->
                        error context.function_ref.function_name call_span
                          (Malformed_sst message)
                  in
                  let* caller_callable =
                    match context.canonical_callable with
                    | Some callable -> Ok callable
                    | None ->
                        error context.function_ref.function_name call_span
                          (Malformed_sst
                             "finite call result lacks canonical caller \
                              identity")
                  in
                  match
                    Verification_session.consume_finite_result session snapshot
                      ~caller:context.current_callable ~caller_callable
                      ~call_span ~path_condition:state.path_condition
                      ~result:aggregate
                  with
                  | Ok None -> Ok state
                  | Ok (Some receipt) -> Ok (with_finite_receipt state receipt)
                  | Error message ->
                      error context.function_ref.function_name call_span
                        (Malformed_sst message))))
      | None, _ | Some _, _ -> Ok state
  let record_materialized_exit context ~eligible ~validated ~callable
      ~(definition : Sst.function_definition) ~session ~source_value
      ~result_value state =
    if not eligible then Ok state
    else
      match (source_value, result_value) with
      | Aggregate_value source_value, Aggregate_value result_value -> (
          let function_name = context.function_ref.function_name in
          let* domain =
            finite_result_rank context definition.span definition.result_type
          in
          let* domain =
            match domain with
            | Some domain -> Ok domain
            | None ->
                error function_name definition.span
                  (Malformed_sst
                     "finite result witness lacks an exact rank domain")
          in
          let* rank = finite_rank_snapshot context definition.span domain in
          let* registry, callable_key =
            finite_registry context definition.span
          in
          let mode = Sst_validation.result_instance_mode validated callable in
          let* snapshot =
            match
              Verification_session.finite_result_snapshot session ~validated
                definition ~rank
            with
            | Ok snapshot -> Ok snapshot
            | Error message ->
                error function_name definition.span (Malformed_sst message)
          in
          let* path_digest =
            match
              Verification_session.record_finite_result_exit session snapshot
                ~result:result_value ~path_condition:state.path_condition
            with
            | Ok path_digest -> Ok path_digest
            | Error message ->
                error function_name definition.span (Malformed_sst message)
          in
          let* source =
            match
              List.find_opt
                (fun receipt ->
                  Finite_value_registry.receipt_matches_value receipt
                    source_value)
                state.finite_receipts
            with
            | None -> Ok None
            | Some _ -> (
                match
                  Finite_value_registry.authenticate registry
                    ~facts:state.finite_receipts ~callable:callable_key
                    ~value:source_value ~mode ~typ:definition.result_type ~rank
                with
                | Ok receipt -> Ok (Some receipt)
                | Error message ->
                    error function_name definition.span (Malformed_sst message))
          in
          match source with
          | None when not context.finite_result_candidate -> Ok state
          | Some _ | None -> (
              match
                Verification_session.promote_finite_result session snapshot
                  ~source ~result:result_value ~path_digest
              with
              | Ok receipt -> Ok (with_finite_receipt state receipt)
              | Error message ->
                  error function_name definition.span (Malformed_sst message)))
      | ( ( Unit_value | Integer_value _ | Boolean_value _ | Tuple_value _
          | Aggregate_value _ | Parametric_value _ | Function_value _ ),
          ( Unit_value | Integer_value _ | Boolean_value _ | Tuple_value _
          | Aggregate_value _ | Parametric_value _ | Function_value _ ) ) ->
          Ok state
end
let current_shared_epoch state =
  match state.shared_scalar_heap with
  | Some heap -> Shared_scalar_heap_private.current_epoch heap
  | None -> 0
let authorize_frozen_formal_call context
    ~(callee_definition : Sst.function_definition) ~call_form ~arguments
    ~call_span state actuals =
  let function_name = context.function_ref.function_name in
  if List.length arguments <> List.length actuals then
    error function_name call_span
      (Malformed_sst "frozen-spine call argument count mismatch")
  else
    let actuals =
      List.map2
        (fun (_, expression) value ->
          ( expression,
            match value with
            | Aggregate_value root -> Some root
            | Unit_value | Integer_value _ | Boolean_value _ | Tuple_value _
            | Parametric_value _ | Function_value _ ->
                None ))
        arguments actuals
    in
    match context.verification_session with
    | Some session -> (
        match
          Verification_session.authorize_frozen_formal_call session
            ~definition:context.current_definition ~callee:callee_definition
            ~call_form
            ~call_path:(context.frozen_observation_call_path @ [ call_span ])
            ~actuals ~path_condition:state.path_condition
            ~epoch:(current_shared_epoch state)
        with
        | Ok () -> Ok ()
        | Error message -> error function_name call_span (Malformed_sst message)
        )
    | None ->
        let frozen =
          match
            Sst_validation.find_callable context.validated
              callee_definition.function_id
          with
          | None -> false
          | Some callable ->
              callee_definition.parameters
              |> List.mapi (fun ordinal _ ->
                  Sst_validation.frozen_formal_requirement context.validated
                    callable ordinal)
              |> List.exists Option.is_some
        in
        if frozen then
          error function_name call_span
            (Malformed_sst
               "frozen-spine call requires the private verification session")
        else Ok ()
module Immutable_fact_integration = struct
  let rec ranked_children typ value =
    match (typ, value) with
    | Sst.Tuple components, Tuple_value values
      when List.length components = List.length values ->
        List.map2
          (fun (_, typ) value -> ranked_children typ value)
          components values
        |> List.concat
    | (Sst.Aggregate _ | Sst.Application _), Aggregate_value aggregate ->
        [ (typ, aggregate) ]
    | (Sst.Unit | Sst.Bool | Sst.Int | Sst.Parameter _), _
    | Sst.Tuple _, _
    | Sst.Aggregate _, _
    | Sst.Application _, _ ->
        []
  let issue_concrete_finite_construction context (expression : Sst.expression)
      state aggregate children immutable shape =
    let* domain =
      rank_domain_for_type context expression.Sst.span expression.typ
    in
    match domain with
    | None -> Ok state
    | Some _ when not immutable ->
        error context.function_ref.function_name expression.span
          (Malformed_sst
             "mutable or shared construction cannot issue a finite receipt")
    | Some domain -> (
        let* registry, callable = finite_registry context expression.span in
        let* mode = expression_instance_mode context expression in
        let* rank = finite_rank_snapshot context expression.span domain in
        let* required =
          List.fold_left
            (fun result ((child_expression : Sst.expression), value) ->
              let* required = result in
              let* child_domain =
                rank_domain_for_type context child_expression.Sst.span
                  child_expression.typ
              in
              match child_domain with
              | Some child_domain when same_rank_domain domain child_domain ->
                  let* child_mode =
                    expression_instance_mode context child_expression
                  in
                  Ok
                    (List.rev_append
                       (ranked_children child_expression.typ value
                       |> List.map (fun (typ, child) ->
                           (typ, child, child_mode)))
                       required)
              | None | Some _ -> Ok required)
            (Ok []) children
        in
        let* required_children =
          List.fold_left
            (fun result (typ, child, child_mode) ->
              let* children = result in
              let receipt =
                match
                  authenticate_exact_source_mode registry
                    ~facts:state.finite_receipts ~callable ~value:child
                    ~preferred_mode:child_mode ~typ ~rank
                with
                | Ok (receipt, _) -> Some receipt
                | Error _ -> None
              in
              Ok ((typ, child, receipt) :: children))
            (Ok []) required
        in
        let missing_child =
          List.exists
            (fun (_, _, receipt) -> Option.is_none receipt)
            required_children
        in
        let provenance =
          match mode with
          | Sst.Ghost_instance ->
              Finite_value_registry.Closed_logical_construction shape
          | Sst.Exec_instance ->
              Finite_value_registry.Local_exec_construction shape
          | Sst.Tracked_instance ->
              Finite_value_registry.Local_tracked_construction shape
        in
        match
          Finite_value_registry.issue_parent registry ~callable ~value:aggregate
            ~mode ~typ:expression.typ ~rank ~provenance ~required_children
        with
        | Ok receipt -> Ok (with_finite_receipt state receipt)
        | Error _ when missing_child && not context.finite_result_candidate ->
            Ok state
        | Error message ->
            error context.function_ref.function_name expression.span
              (Malformed_sst message))
  let issue_finite_construction context expression state aggregate children
      immutable shape =
    issue_concrete_finite_construction context expression state aggregate
      children immutable shape
  let derive_finite_aggregate context span state ~parent ~child ~mode ~typ
      ~provenance =
    let* domain = rank_domain_for_type context span typ in
    match domain with
    | None -> Ok state
    | Some domain -> (
        let* registry, _ = finite_registry context span in
        let* rank = finite_rank_snapshot context span domain in
        if
          not
            (List.exists
               (fun receipt ->
                 Finite_value_registry.receipt_matches_value receipt parent)
               state.finite_receipts)
        then Ok state
        else
          let* selector =
            match child.Vir.aggregate_desc with
            | Vir.Aggregate_selector (selector, actual_parent)
              when actual_parent = parent ->
                Ok selector
            | Vir.Aggregate_symbol _ | Vir.Aggregate_selector _ ->
                error context.function_ref.function_name span
                  (Malformed_sst
                     "finite child path is not an exact direct selector")
            | Vir.Aggregate_constructor _ | Vir.Aggregate_record _
            | Vir.Aggregate_conditional _
            | Vir.Aggregate_symbolic_application _
            | Vir.Aggregate_imported_model_application _
            | Vir.Aggregate_recursive_spec_application _ ->
                error context.function_ref.function_name span
                  (Malformed_sst
                     "finite child path is not a materialized selector")
          in
          match
            Finite_value_registry.derive_child registry
              ~facts:state.finite_receipts ~parent ~child ~selector ~span ~mode
              ~typ ~rank ~provenance
          with
          | Ok receipt -> Ok (with_finite_receipt state receipt)
          | Error message ->
              error context.function_ref.function_name span
                (Malformed_sst message))
  let rec derive_finite_selected context span state ~parent ~mode ~typ value
      ~provenance =
    match (typ, value) with
    | (Sst.Aggregate _ | Sst.Application _), Aggregate_value child ->
        derive_finite_aggregate context span state ~parent ~child ~mode ~typ
          ~provenance
    | Sst.Tuple components, Tuple_value values
      when List.length components = List.length values ->
        List.fold_left2
          (fun result (_, typ) value ->
            let* state = result in
            derive_finite_selected context span state ~parent ~mode ~typ value
              ~provenance)
          (Ok state) components values
    | (Sst.Unit | Sst.Bool | Sst.Int | Sst.Parameter _), _
    | Sst.Tuple _, _
    | Sst.Aggregate _, _
    | Sst.Application _, _ ->
        Ok state
  let rec derive_finite_pattern context span state ~parent ~mode
      (pattern : Sst.pattern) value =
    derive_concrete_finite_pattern context span state ~parent ~mode pattern
      value
  and derive_concrete_finite_pattern context span state ~parent ~mode
      (pattern : Sst.pattern) value =
    match (pattern.pattern_desc, value) with
    | Sst.Record_pattern fields, Aggregate_value aggregate ->
        List.fold_left
          (fun result (field, (nested : Sst.pattern)) ->
            let* state = result in
            let selected =
              selected_parametric_value_without_state
                ~aggregate_type:
                  (vir_aggregate_type_of_sst
                     (Sst_validation.program context.validated).parametric_adts)
                aggregate
                (fun path sort ->
                  selector_domain aggregate (field_selector field path sort))
                [] nested.Sst.typ
            in
            let* state =
              derive_finite_selected context span state ~parent:aggregate ~mode
                ~typ:nested.typ selected
                ~provenance:Finite_value_registry.Successful_pattern
            in
            derive_finite_pattern context span state ~parent:aggregate ~mode
              nested selected)
          (Ok state) fields
    | Sst.Constructor_pattern (constructor, patterns), Aggregate_value aggregate
      ->
        let rec loop index state = function
          | [] -> Ok state
          | (nested : Sst.pattern) :: rest ->
              let selected =
                selected_parametric_value_without_state
                  ~aggregate_type:
                    (vir_aggregate_type_of_sst
                       (Sst_validation.program context.validated)
                         .parametric_adts)
                  aggregate
                  (fun path sort ->
                    selector_domain aggregate
                      (argument_selector constructor index path sort))
                  [] nested.Sst.typ
              in
              let* state =
                derive_finite_selected context span state ~parent:aggregate
                  ~mode ~typ:nested.typ selected
                  ~provenance:Finite_value_registry.Successful_pattern
              in
              let* state =
                derive_finite_pattern context span state ~parent:aggregate ~mode
                  nested selected
              in
              loop (index + 1) state rest
        in
        loop 0 state patterns
    | Sst.Tuple_pattern patterns, Tuple_value values
      when List.length patterns = List.length values ->
        List.fold_left2
          (fun result (_, nested) value ->
            let* state = result in
            derive_finite_pattern context span state ~parent ~mode nested value)
          (Ok state) patterns values
    | ( ( Sst.Wildcard | Sst.Bind _ | Sst.Owned_tree_cursor_pattern _
        | Sst.Int_pattern _ | Sst.Bool_pattern _ | Sst.Unit_pattern ),
        _ ) ->
        Ok state
    | Sst.Or_pattern _, _ ->
        error context.function_ref.function_name span Or_pattern
    | ( (Sst.Record_pattern _ | Sst.Constructor_pattern _ | Sst.Tuple_pattern _),
        _ ) ->
        error context.function_ref.function_name span
          (Malformed_sst "finite successful-pattern value mismatch")
  let rec derive_finite_ephemeral_pattern context span state
      (expression : Sst.expression) (pattern : Sst.pattern) value =
    match
      (expression.expression_desc, pattern.pattern_desc, value)
    with
    | Sst.Tuple_value expressions, Sst.Tuple_pattern patterns,
      Tuple_value values
      when List.length expressions = List.length patterns
           && List.length patterns = List.length values ->
        List.fold_left2
          (fun result ((_, expression), (_, pattern)) value ->
            let* state = result in
            derive_finite_ephemeral_pattern context span state expression
              pattern value)
          (Ok state) (List.combine expressions patterns) values
    | _, _, Aggregate_value parent ->
        let* mode = expression_instance_mode context expression in
        derive_finite_pattern context span state ~parent ~mode pattern value
    | Sst.Tuple_value _, Sst.Tuple_pattern _, Tuple_value _ ->
        error context.function_ref.function_name span
          (Malformed_sst "finite ephemeral tuple pattern arity mismatch")
    | _, _,
      ( Unit_value | Integer_value _ | Boolean_value _ | Tuple_value _
      | Parametric_value _ | Function_value _ ) ->
        Ok state
end
let path_condition_has_direct_contradiction conditions =
  List.exists
    (function
      | Vir.Boolean_not term -> List.mem term conditions
      | term -> List.mem (Vir.Boolean_not term) conditions)
    conditions
let retain_ground_constructor_match_candidate context scrutinee_expression case
    state scrutinee_value =
  match
    ( case.Sst.case_pattern.pattern_desc,
      scrutinee_value,
      context.verification_session,
      state.proof_activation_authority )
  with
  | ( Sst.Constructor_pattern (_, []),
      Aggregate_value
        ({ Vir.aggregate_desc = Vir.Aggregate_symbol _; _ } as aggregate),
      Some _,
      Some _ )
    when context.ground_retry_candidate_scope
         && context.spec_call_stack = []
         && (not (path_condition_has_direct_contradiction state.path_condition))
         && List.exists
              (fun receipt ->
                Finite_value_registry.receipt_matches_value receipt aggregate)
              state.finite_receipts ->
      {
        state with
        pending_ground_constructor_matches =
          {
            pending_scrutinee_expression = scrutinee_expression;
            pending_case = case;
            pending_value = aggregate;
            pending_path_condition = state.path_condition;
          }
          :: state.pending_ground_constructor_matches;
      }
  | _ -> state
let demand_mentions_value demand value =
  List.exists
    (function
      | Vir.Recursive_aggregate_argument candidate -> candidate = value
      | Vir.Recursive_integer_argument _ | Vir.Recursive_boolean_argument _
      | Vir.Recursive_parametric_argument _ ->
          false)
    (Recursive_spec_retry_demand_private.arguments demand)
let issue_retry_ground_constructor_match context goal state =
  match Recursive_spec_retry_demand_private.of_goal goal with
  | Error (`Malformed (span, message)) ->
      error context.function_ref.function_name span (Malformed_sst message)
  | Ok None -> Ok (state, state.ground_constructor_matches)
  | Ok (Some demand) -> (
      let rec select skipped = function
        | [] -> None
        | candidate :: rest ->
            if demand_mentions_value demand candidate.pending_value then
              Some (candidate, List.rev_append skipped rest)
            else select (candidate :: skipped) rest
      in
      match select [] state.pending_ground_constructor_matches with
      | None -> Ok (state, state.ground_constructor_matches)
      | Some
          ( {
              pending_scrutinee_expression = scrutinee_expression;
              pending_case = case;
              pending_value = aggregate;
              pending_path_condition = path_condition;
            },
            pending_ground_constructor_matches ) -> (
          match
            (context.verification_session, state.proof_activation_authority)
          with
          | Some session, Some authority -> (
              let* domain =
                rank_domain_for_type context case.case_span
                  case.case_pattern.typ
              in
              match domain with
              | None -> Ok (state, state.ground_constructor_matches)
              | Some domain -> (
                  let* registry, callable =
                    finite_registry context case.case_span
                  in
                  let* rank =
                    finite_rank_snapshot context case.case_span domain
                  in
                  let* preferred_mode =
                    expression_instance_mode context scrutinee_expression
                  in
                  match
                    authenticate_exact_source_mode registry
                      ~facts:state.finite_receipts ~callable ~value:aggregate
                      ~preferred_mode ~typ:case.case_pattern.typ ~rank
                  with
                  | Error _ -> Ok (state, state.ground_constructor_matches)
                  | Ok (receipt, mode) -> (
                      match
                        Verification_session.issue_ground_constructor_match
                          session ~authority ~validated:context.validated
                          ~definition:context.current_definition ~case
                          ~value:aggregate ~mode ~rank ~receipt ~path_condition
                      with
                      | Ok ground_match ->
                          let continuation_ground_matches =
                            state.ground_constructor_matches
                          in
                          Ok
                            ( {
                                state with
                                ground_constructor_matches =
                                  ground_match :: continuation_ground_matches;
                                pending_ground_constructor_matches;
                              },
                              continuation_ground_matches )
                      | Error message ->
                          error context.function_ref.function_name
                            case.case_span (Malformed_sst message))))
          | None, _ | Some _, None ->
              Ok (state, state.ground_constructor_matches)))
let constrain_immediate_record_fields function_name span type_definitions state
    type_id aggregate =
  match
    List.find_opt
      (fun (definition : Sst.type_definition) ->
        same_type_id definition.type_id type_id)
      type_definitions
  with
  | Some { type_kind = Sst.Record_definition fields; _ } ->
      List.fold_left
        (fun state (field : Sst.field_definition) ->
          let* state = state in
          let* _, state =
            select_field state aggregate field.field_id field.field_type
          in
          Ok state)
        (Ok state) fields
  | Some { type_kind = Sst.Variant_definition _; _ } ->
      error function_name span
        (Malformed_sst "unique returned parameter is not a record")
  | None ->
      error function_name span
        (Malformed_sst "unique returned record type is not registered")
let variant_tag_domain function_name span type_definitions parametric_adts
    pattern_type (constructor : Sst.constructor_id)
    (aggregate : Vir.aggregate_term) =
  let aggregate_type = constructor.constructor_type in
  let expected_type =
    match pattern_type with
    | Sst.Application (type_constructor, _) ->
        Parametric_adt.find parametric_adts type_constructor
        |> Option.map Parametric_adt.type_id
        |> Option.value ~default:aggregate_type
    | Sst.Aggregate type_id -> type_id
    | Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _ | Sst.Parameter _ ->
        aggregate_type
  in
  if aggregate.aggregate_type.aggregate_type_index <> expected_type.type_index
  then
    error function_name span
      (Malformed_sst "constructor pattern has the wrong aggregate type")
  else
    match
      List.find_opt
        (fun (definition : Sst.type_definition) ->
          same_type_id definition.type_id aggregate_type)
        type_definitions
    with
    | None ->
        error function_name span
          (Malformed_sst "constructor pattern refers to an unknown variant type")
    | Some { type_kind = Sst.Record_definition _; _ } ->
        error function_name span
          (Malformed_sst "constructor pattern refers to a record type")
    | Some { type_kind = Sst.Variant_definition constructors; _ } -> (
        if
          not
            (List.exists
               (fun (definition : Sst.constructor_definition) ->
                 same_constructor_id definition.constructor_id constructor)
               constructors)
        then
          error function_name span
            (Malformed_sst
               "constructor pattern does not belong to its declared variant")
        else
          let tag = Vir.Aggregate_tag (aggregate.aggregate_type, aggregate) in
          let alternatives =
            List.mapi
              (fun ordinal (definition : Sst.constructor_definition) ->
                let declared = definition.constructor_id in
                if
                  (not (same_type_id declared.constructor_type aggregate_type))
                  || declared.constructor_index <> ordinal
                then
                  error function_name definition.span
                    (Malformed_sst
                       "variant registry contains a constructor with an \
                        invalid owner or ordinal")
                else
                  Ok
                    (Vir.Integer_compare
                       (Vir.Equal, tag, Vir.Integer_constant (Z.of_int ordinal))))
              constructors
          in
          let rec collect reversed = function
            | [] -> Ok (List.rev reversed)
            | Ok alternative :: rest -> collect (alternative :: reversed) rest
            | (Error _ as error) :: _ -> error
          in
          let* alternatives = collect [] alternatives in
          match alternatives with
          | [] ->
              error function_name span
                (Malformed_sst "variant type has no constructors")
          | first :: rest ->
              Ok
                (List.fold_left
                   (fun domain alternative ->
                     Vir.Boolean_or (domain, alternative))
                   first rest))
let rec pattern_condition_and_bindings function_name type_definitions
    parametric_adts (pattern : Sst.pattern) value =
  match (pattern.pattern_desc, value) with
  | Sst.Wildcard, _ -> Ok (Vir.Boolean_constant true, [], [])
  | Sst.Bind binding, value ->
      Ok (Vir.Boolean_constant true, [ (binding.id, value) ], [])
  | Sst.Owned_tree_cursor_pattern _, _ -> Ok (Vir.Boolean_constant true, [], [])
  | Sst.Unit_pattern, Unit_value -> Ok (Vir.Boolean_constant true, [], [])
  | Sst.Int_pattern expected, Integer_value actual ->
      Ok
        ( Vir.Integer_compare (Vir.Equal, actual, Vir.Integer_constant expected),
          [],
          [] )
  | Sst.Bool_pattern expected, Boolean_value actual ->
      Ok (Vir.Boolean_equal (actual, Vir.Boolean_constant expected), [], [])
  | Sst.Tuple_pattern patterns, Tuple_value values
    when List.length patterns = List.length values ->
      let rec loop conditions bindings ranges patterns values =
        match (patterns, values) with
        | [], [] ->
            Ok
              ( boolean_and (List.rev conditions),
                List.rev bindings,
                List.rev ranges |> List.concat )
        | (_, pattern) :: patterns, value :: values ->
            let* condition, nested, nested_ranges =
              pattern_condition_and_bindings function_name type_definitions
                parametric_adts pattern value
            in
            loop (condition :: conditions)
              (List.rev_append nested bindings)
              (nested_ranges :: ranges) patterns values
        | _ -> assert false
      in
      loop [] [] [] patterns values
  | Sst.Record_pattern fields, Aggregate_value aggregate ->
      let rec loop conditions bindings ranges = function
        | [] ->
            Ok
              ( boolean_and (List.rev conditions),
                List.rev bindings,
                List.rev ranges |> List.concat )
        | (field, (pattern : Sst.pattern)) :: rest ->
            let selected =
              selected_parametric_value_without_state
                ~aggregate_type:(vir_aggregate_type_of_sst parametric_adts)
                aggregate
                (fun path sort ->
                  selector_domain aggregate (field_selector field path sort))
                [] pattern.Sst.typ
            in
            let* condition, nested, nested_ranges =
              pattern_condition_and_bindings function_name type_definitions
                parametric_adts pattern selected
            in
            loop (condition :: conditions)
              (List.rev_append nested bindings)
              (ranges_of_value selected :: nested_ranges :: ranges)
              rest
      in
      loop [] [] [] fields
  | Sst.Constructor_pattern (constructor, arguments), Aggregate_value aggregate
    ->
      let* tag_domain =
        variant_tag_domain function_name pattern.span type_definitions
          parametric_adts pattern.typ constructor aggregate
      in
      let tag =
        Vir.Integer_compare
          ( Vir.Equal,
            Vir.Aggregate_tag (aggregate.aggregate_type, aggregate),
            Vir.Integer_constant (Z.of_int constructor.constructor_index) )
      in
      let rec loop index conditions bindings ranges = function
        | [] ->
            Ok
              ( boolean_and (tag :: List.rev conditions),
                List.rev bindings,
                List.rev ranges |> List.concat )
        | (pattern : Sst.pattern) :: rest ->
            let selected =
              selected_parametric_value_without_state
                ~aggregate_type:(vir_aggregate_type_of_sst parametric_adts)
                aggregate
                (fun path sort ->
                  selector_domain aggregate
                    (argument_selector constructor index path sort))
                [] pattern.Sst.typ
            in
            let* condition, nested, nested_ranges =
              pattern_condition_and_bindings function_name type_definitions
                parametric_adts pattern selected
            in
            loop (index + 1) (condition :: conditions)
              (List.rev_append nested bindings)
              (ranges_of_value selected :: nested_ranges :: ranges)
              rest
      in
      let* condition, bindings, ranges = loop 0 [] [] [] arguments in
      Ok (condition, bindings, tag_domain :: ranges)
  | Sst.Or_pattern _, _ -> error function_name pattern.span Or_pattern
  | _ ->
      error function_name pattern.span
        (Malformed_sst "pattern type/value mismatch")
let add_pattern_bindings state bindings =
  { state with environment = bindings @ state.environment }
let restore_environment environment state = { state with environment }
let restore_pattern_environment outer_environment state =
  let environment =
    List.map
      (fun (binding_id, prior) ->
        let current =
          Option.value ~default:prior
            (List.assoc_opt binding_id state.environment)
        in
        (binding_id, current))
      outer_environment
  in
  { state with environment }
let remove_binding binding_id environment =
  List.filter (fun (candidate, _) -> candidate <> binding_id) environment
let replace_binding binding value environment =
  (binding.Sst.id, value) :: remove_binding binding.id environment
let replace_owned_root state binding value version =
  let owned_root_versions =
    List.filter
      (fun (binding_id, _, _) -> binding_id <> binding.Sst.id)
      state.owned_root_versions
  in
  let owned_root_versions =
    match value with
    | Aggregate_value aggregate ->
        (binding.id, aggregate, version) :: owned_root_versions
    | Unit_value | Integer_value _ | Boolean_value _ | Tuple_value _
    | Parametric_value _ | Function_value _ ->
        owned_root_versions
  in
  {
    state with
    environment = replace_binding binding value state.environment;
    owned_root_versions;
  }
let owned_root_version state binding root =
  let exact =
    List.find_map
      (fun (binding_id, candidate, version) ->
        if binding_id = binding.Sst.id && candidate = root then Some version
        else None)
      state.owned_root_versions
  in
  (match exact with
    | Some _ -> exact
    | None ->
        List.find_map
          (fun (_, candidate, version) ->
            if candidate = root then Some version else None)
          state.owned_root_versions)
  |> Option.value ~default:0
let replace_owned_contents_origin state root origin =
  {
    state with
    owned_contents_origins =
      (root, origin)
      :: List.filter
           (fun (candidate, _) -> candidate <> root)
           state.owned_contents_origins;
  }
let find_owned_contents_origin state root =
  List.assoc_opt root state.owned_contents_origins
let find_summary summaries (id : Sst.function_id) =
  match List.assoc_opt id.function_index summaries with
  | Some summary
    when String.equal summary.definition.function_id.function_name
           id.function_name ->
      Some summary
  | _ -> None
let owned_tree_transitions =
  Immutable_aggregate_reconstruction_private.owned_tree_transitions
let transition_obligation_snapshot =
  Type_invariant.transition_obligation_snapshot
let owned_step_name = function
  | Sst.Owned_tree_field field ->
      Printf.sprintf "f%d:%s" field.field_index field.field_name
  | Sst.Owned_tree_constructor constructor ->
      Printf.sprintf "c%d:%s" constructor.constructor_index
        constructor.constructor_name
let owned_terminal_name = function
  | Owned_scalar.Owned_terminal_int -> "int"
  | Owned_scalar.Owned_terminal_bool -> "bool"
  | Owned_scalar.Owned_terminal_unit -> "unit"
  | Owned_scalar.Owned_terminal_tag constructor ->
      Printf.sprintf "tag%d:%s" constructor.constructor_index
        constructor.constructor_name
let owned_scalar_selector_from_names root step_names terminal range =
  let terminal_name = owned_terminal_name terminal in
  let identity = String.concat "/" (step_names @ [ terminal_name ]) in
  {
    Vir.selector_domain = root.Vir.aggregate_type;
    selector_range = range;
    selector_namespace = "verocaml_owned_root_scalar_v1";
    selector_index = Hashtbl.hash identity land max_int;
    selector_name = identity;
    selector_path = [];
  }
let owned_scalar_selector root steps terminal range =
  owned_scalar_selector_from_names root
    (List.map owned_step_name steps)
    terminal range
let owned_selector_root_and_path aggregate =
  let rec loop reversed (aggregate : Vir.aggregate_term) =
    match aggregate.aggregate_desc with
    | Vir.Aggregate_selector (selector, source)
      when selector.selector_path = [] && selector.selector_name <> "$arg0" ->
        loop
          (Printf.sprintf "f%d:%s" selector.selector_index
             selector.selector_name
          :: reversed)
          source
    | Vir.Aggregate_symbol _ -> Some (aggregate, reversed)
    | Vir.Aggregate_selector _ | Vir.Aggregate_constructor _
    | Vir.Aggregate_record _ | Vir.Aggregate_conditional _
    | Vir.Aggregate_symbolic_application _
    | Vir.Aggregate_imported_model_application _
    | Vir.Aggregate_recursive_spec_application _ ->
        None
  in
  loop [] aggregate
let owned_runtime_tag aggregate constructor =
  Option.map
    (fun (root, path) ->
      Vir.Boolean_selector
        ( owned_scalar_selector_from_names root path
            (Owned_scalar.Owned_terminal_tag constructor) Vir.Boolean,
          root ))
    (owned_selector_root_and_path aggregate)
let observe_formula_model_field context state (field : Sst.field_id) value =
  match value with
    | Aggregate_value aggregate ->
        Logical_adt_evaluation_private.constructors_of_field context.type_definitions field.field_owner field |> List.filter_map (fun (constructor : Sst.constructor_id) -> Option.map (fun observed -> Vir.Boolean_equal (observed, Vir.Integer_compare (Vir.Equal, Vir.Aggregate_tag (aggregate.aggregate_type, aggregate), Vir.Integer_constant (Z.of_int constructor.constructor_index)))) (owned_runtime_tag aggregate constructor)) |> with_assumptions state
    | Unit_value | Integer_value _ | Boolean_value _ | Tuple_value _ | Parametric_value _ | Function_value _ -> state
let rec pattern_has_owned_cursor (pattern : Sst.pattern) =
  match pattern.pattern_desc with
  | Sst.Owned_tree_cursor_pattern _ -> true
  | Sst.Tuple_pattern components ->
      List.exists
        (fun (_, nested) -> pattern_has_owned_cursor nested)
        components
  | Sst.Record_pattern fields ->
      List.exists (fun (_, nested) -> pattern_has_owned_cursor nested) fields
  | Sst.Constructor_pattern (_, arguments) ->
      List.exists pattern_has_owned_cursor arguments
  | Sst.Or_pattern (left, right) ->
      pattern_has_owned_cursor left || pattern_has_owned_cursor right
  | Sst.Wildcard | Sst.Bind _ | Sst.Int_pattern _ | Sst.Bool_pattern _
  | Sst.Unit_pattern ->
      false
let owned_aggregate_normal_form_rank replacement =
  match replacement.Vir.aggregate_desc with
  | Vir.Aggregate_constructor _ | Vir.Aggregate_record _ -> 0
  | Vir.Aggregate_symbol _ -> 1
  | Vir.Aggregate_selector _ -> 2
  | Vir.Aggregate_conditional _ | Vir.Aggregate_imported_model_application _
  | Vir.Aggregate_symbolic_application _
  | Vir.Aggregate_recursive_spec_application _ ->
      3
let find_owned_contents_aggregate_equation assumptions candidate =
  let replacements =
    List.filter_map
      (function
        | Vir.Aggregate_equal (left, right) when left = candidate -> Some right
        | Vir.Aggregate_equal (left, right)
          when right = candidate
               && Option.is_some (owned_selector_root_and_path candidate) ->
            Some left
        | _ -> None)
      assumptions
    |> List.filter (fun replacement -> replacement <> candidate)
  in
  match
    List.sort
      (fun left right ->
        Int.compare
          (owned_aggregate_normal_form_rank left)
          (owned_aggregate_normal_form_rank right))
      replacements
  with
  | replacement :: _ -> Some replacement
  | [] -> None
let normalize_owned_contents_aggregate assumptions fuel term =
  let rec normalize seen fuel term =
    if fuel = 0 then term
    else
      match find_owned_contents_aggregate_equation assumptions term with
      | Some replacement ->
          if List.mem replacement (term :: seen) then
            List.fold_left
              (fun best candidate ->
                if
                  owned_aggregate_normal_form_rank candidate
                  < owned_aggregate_normal_form_rank best
                then candidate
                else best)
              term (replacement :: seen)
          else normalize (term :: seen) (fuel - 1) replacement
      | None -> (
          match term.Vir.aggregate_desc with
          | Vir.Aggregate_selector (selector, parent) -> (
              let parent = normalize (term :: seen) (fuel - 1) parent in
              let aggregate_argument = function
                | Vir.Recursive_aggregate_argument aggregate -> Some aggregate
                | Vir.Recursive_integer_argument _
                | Vir.Recursive_boolean_argument _
                | Vir.Recursive_parametric_argument _ ->
                    None
              in
              let projected =
                match parent.Vir.aggregate_desc with
                | Vir.Aggregate_constructor { arguments; _ } ->
                    Option.bind
                      (List.nth_opt arguments selector.Vir.selector_index)
                      aggregate_argument
                | Vir.Aggregate_record { fields; _ } ->
                    fields
                    |> List.find_map (fun (field, argument) ->
                        if field.Sst.field_index = selector.Vir.selector_index
                        then aggregate_argument argument
                        else None)
                | Vir.Aggregate_symbol _ | Vir.Aggregate_selector _
                | Vir.Aggregate_conditional _
                | Vir.Aggregate_symbolic_application _
                | Vir.Aggregate_imported_model_application _
                | Vir.Aggregate_recursive_spec_application _ ->
                    None
              in
              match projected with
              | Some projected -> normalize (term :: seen) (fuel - 1) projected
              | None ->
                  let selected =
                    {
                      term with
                      Vir.aggregate_desc =
                        Vir.Aggregate_selector (selector, parent);
                    }
                  in
                  if selected = term then term
                  else normalize (term :: seen) (fuel - 1) selected)
          | Vir.Aggregate_symbol _ | Vir.Aggregate_constructor _
          | Vir.Aggregate_record _ | Vir.Aggregate_conditional _
          | Vir.Aggregate_symbolic_application _
          | Vir.Aggregate_imported_model_application _
          | Vir.Aggregate_recursive_spec_application _ ->
              term)
  in
  normalize [] fuel term
let owned_contents_project_field assumptions field aggregate =
  let rec collect found aggregate =
    let aggregate =
      normalize_owned_contents_aggregate assumptions 128 aggregate
    in
    match aggregate.Vir.aggregate_desc with
    | Vir.Aggregate_record { fields; _ } ->
        List.fold_left
          (fun found (candidate, argument) ->
            if candidate = field then
              match argument with
              | Vir.Recursive_aggregate_argument value -> value :: found
              | Vir.Recursive_integer_argument _
              | Vir.Recursive_boolean_argument _
              | Vir.Recursive_parametric_argument _ ->
                  found
            else found)
          found fields
    | Vir.Aggregate_constructor { arguments; _ } ->
        List.fold_left
          (fun found -> function
            | Vir.Recursive_aggregate_argument nested -> collect found nested
            | Vir.Recursive_integer_argument _
            | Vir.Recursive_boolean_argument _
            | Vir.Recursive_parametric_argument _ ->
                found)
          found arguments
    | Vir.Aggregate_symbol _ | Vir.Aggregate_selector _
    | Vir.Aggregate_conditional _ | Vir.Aggregate_imported_model_application _
    | Vir.Aggregate_symbolic_application _
    | Vir.Aggregate_recursive_spec_application _ ->
        found
  in
  match collect [] aggregate with
  | [ child ] -> Some (normalize_owned_contents_aggregate assumptions 128 child)
  | _ :: _ :: _ -> None
  | [] -> (
      match field.Sst.field_owner with
      | Sst.Constructor_owner constructor ->
          let aggregate_type = aggregate.Vir.aggregate_type in
          let payload =
            normalize_owned_contents_aggregate assumptions 128
              {
                Vir.aggregate_type;
                aggregate_desc =
                  Vir.Aggregate_selector
                    ( argument_selector constructor 0 []
                        (Vir.Aggregate aggregate_type),
                      aggregate );
              }
          in
          Some
            (normalize_owned_contents_aggregate assumptions 128
               {
                 Vir.aggregate_type;
                 aggregate_desc =
                   Vir.Aggregate_selector
                     ( field_selector field [] (Vir.Aggregate aggregate_type),
                       payload );
               })
      | Sst.Record_owner _ -> None)
let owned_contents_constructor assumptions grammar aggregate =
  let aggregate =
    normalize_owned_contents_aggregate assumptions 128 aggregate
  in
  match aggregate.Vir.aggregate_desc with
  | Vir.Aggregate_constructor { constructor; _ } -> Some constructor
  | Vir.Aggregate_symbol _ | Vir.Aggregate_selector _ | Vir.Aggregate_record _
  | Vir.Aggregate_conditional _ | Vir.Aggregate_imported_model_application _
  | Vir.Aggregate_symbolic_application _
  | Vir.Aggregate_recursive_spec_application _ ->
      let ordinal =
        List.find_map
          (function
            | Vir.Integer_compare
                ( Vir.Equal,
                  Vir.Aggregate_tag (_, candidate),
                  Vir.Integer_constant ordinal )
              when normalize_owned_contents_aggregate assumptions 128 candidate
                   = aggregate ->
                Some (Z.to_int ordinal)
            | _ -> None)
          assumptions
      in
      Option.bind ordinal (fun ordinal ->
          Sst_validation_private.Owned_recursive_contents_private.cases grammar
          |> List.find_map (fun case ->
              let constructor =
                Sst_validation_private.Owned_recursive_contents_private
                .case_carrier_constructor case
              in
              if constructor.constructor_index = ordinal then Some constructor
              else None))
let owned_contents_topology assumptions grammar root =
  let rec walk path nodes edges value =
    let value = normalize_owned_contents_aggregate assumptions 128 value in
    if
      List.exists
        (fun (node : Verification_session.owned_contents_topology_node) ->
          node.contents_node_value = value)
        nodes
    then Error "owned-contents concrete topology is shared or cyclic"
    else
      match owned_contents_constructor assumptions grammar value with
      | Some constructor -> (
          match
            Sst_validation_private.Owned_recursive_contents_private.cases
              grammar
            |> List.find_opt (fun case ->
                Sst_validation_private.Owned_recursive_contents_private
                .case_carrier_constructor case
                = constructor)
          with
          | None ->
              Error
                "owned-contents concrete topology has an unmapped carrier \
                 constructor"
          | Some case ->
              let node : Verification_session.owned_contents_topology_node =
                {
                  contents_node_path = path;
                  contents_node_value = value;
                  contents_node_constructor = constructor;
                }
              in
              let rec descend nodes edges = function
                | [] -> Ok (nodes, edges)
                | field :: rest -> (
                    match
                      owned_contents_project_field assumptions field value
                    with
                    | None ->
                        Error
                          "owned-contents concrete topology has a missing, \
                           ambiguous, or non-aggregate child"
                    | Some child ->
                        let edge :
                            Verification_session.owned_contents_topology_edge =
                          {
                            contents_edge_parent_path = path;
                            contents_edge_field = field;
                            contents_edge_parent = value;
                            contents_edge_child = child;
                          }
                        in
                        let* nodes, edges =
                          walk (path @ [ field ]) nodes (edge :: edges) child
                        in
                        descend nodes edges rest)
              in
              descend (node :: nodes) edges
                (Sst_validation_private.Owned_recursive_contents_private
                 .case_recursive_edges case))
      | None ->
          Error "owned-contents root has no exact closed acyclic construction"
  in
  let root = normalize_owned_contents_aggregate assumptions 128 root in
  let* nodes, edges = walk [] [] [] root in
  Verification_session.make_owned_contents_topology ~root
    ~nodes:(List.rev nodes) ~edges:(List.rev edges)
let owned_contents_carrier_from_root context assumptions root fields =
  List.fold_left
    (fun aggregate field ->
      Option.bind aggregate (fun aggregate ->
          let nested_type =
            owned_fields context field.Sst.field_owner
            |> List.find_map (fun (definition : Sst.field_definition) ->
                if definition.field_id = field then
                  match definition.field_type with
                  | Sst.Aggregate type_id -> Some type_id
                  | Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _
                  | Sst.Parameter _ | Sst.Application _ ->
                      None
                else None)
          in
          Option.map
            (fun nested_type ->
              let nested_type = vir_aggregate_type nested_type in
              normalize_owned_contents_aggregate assumptions 128
                {
                  Vir.aggregate_type = nested_type;
                  aggregate_desc =
                    Vir.Aggregate_selector
                      ( field_selector field [] (Vir.Aggregate nested_type),
                        aggregate );
                })
            nested_type))
    (Some root) fields
let rec exact_owned_pattern_match assumptions (pattern : Sst.pattern) value =
  match (pattern.pattern_desc, value) with
  | (Sst.Wildcard | Sst.Bind _ | Sst.Owned_tree_cursor_pattern _), _ ->
      Some true
  | Sst.Unit_pattern, Unit_value -> Some true
  | Sst.Int_pattern expected, Integer_value (Vir.Integer_constant reached) ->
      Some (Z.equal expected reached)
  | Sst.Bool_pattern expected, Boolean_value (Vir.Boolean_constant reached) ->
      Some (Bool.equal expected reached)
  | Sst.Tuple_pattern patterns, Tuple_value values
    when List.length patterns = List.length values ->
      exact_owned_pattern_matches assumptions (List.map snd patterns) values
  | Sst.Record_pattern fields, Aggregate_value aggregate ->
      fields
      |> List.map (fun (field, (pattern : Sst.pattern)) ->
          ( pattern,
            selected_value_without_state aggregate (field_selector field) []
              pattern.Sst.typ ))
      |> List.split
      |> fun (patterns, values) ->
      exact_owned_pattern_matches assumptions patterns values
  | Sst.Constructor_pattern (expected, arguments), Aggregate_value aggregate
    -> (
      let aggregate =
        normalize_owned_contents_aggregate assumptions 128 aggregate
      in
      match aggregate.Vir.aggregate_desc with
      | Vir.Aggregate_constructor { constructor; _ }
        when constructor <> expected ->
          Some false
      | Vir.Aggregate_constructor { constructor; _ } when constructor = expected
        ->
          let values =
            List.mapi
              (fun index (pattern : Sst.pattern) ->
                selected_value_without_state aggregate
                  (argument_selector expected index)
                  [] pattern.Sst.typ)
              arguments
          in
          exact_owned_pattern_matches assumptions arguments values
      | Vir.Aggregate_constructor _ -> Some false
      | Vir.Aggregate_symbol _ | Vir.Aggregate_selector _
      | Vir.Aggregate_record _ | Vir.Aggregate_conditional _
      | Vir.Aggregate_symbolic_application _
      | Vir.Aggregate_imported_model_application _
      | Vir.Aggregate_recursive_spec_application _ ->
          None)
  | Sst.Or_pattern (left, right), value -> (
      match
        ( exact_owned_pattern_match assumptions left value,
          exact_owned_pattern_match assumptions right value )
      with
      | Some left, Some right -> Some (left || right)
      | Some true, None | None, Some true -> Some true
      | Some false, None | None, Some false | None, None -> None)
  | ( ( Sst.Unit_pattern | Sst.Int_pattern _ | Sst.Bool_pattern _
      | Sst.Tuple_pattern _ | Sst.Record_pattern _ | Sst.Constructor_pattern _
        ),
      ( Unit_value | Integer_value _ | Boolean_value _ | Tuple_value _
      | Aggregate_value _ | Parametric_value _ | Function_value _ ) ) ->
      None
and exact_owned_pattern_matches assumptions patterns values =
  match (patterns, values) with
  | [], [] -> Some true
  | pattern :: patterns, value :: values -> (
      match exact_owned_pattern_match assumptions pattern value with
      | Some false -> Some false
      | Some true -> exact_owned_pattern_matches assumptions patterns values
      | None -> None)
  | [], _ :: _ | _ :: _, [] -> Some false
let owned_aggregate_at_path context assumptions root steps =
  Logical_adt.owned_aggregate_at_path context.type_definitions assumptions root
    steps
let split_last = function
  | [] -> None
  | steps ->
      let reversed = List.rev steps in
      Some (List.rev (List.tl reversed), List.hd reversed)
let resolve_owned_scalar context state root path =
  let steps = Owned_scalar.owned_scalar_path_steps path in
  match Owned_scalar.owned_scalar_path_terminal path with
  | Owned_scalar.Owned_terminal_unit -> Some Unit_value
  | Owned_scalar.Owned_terminal_tag constructor ->
      Option.bind (owned_aggregate_at_path context state.assumptions root steps)
        (fun (aggregate : Vir.aggregate_term) ->
          let expected =
            Vir.Integer_compare
              ( Vir.Equal,
                Vir.Aggregate_tag (aggregate.aggregate_type, aggregate),
                Vir.Integer_constant (Z.of_int constructor.constructor_index) )
          in
          if List.exists (( = ) expected) state.assumptions then
            Some (Boolean_value (Vir.Boolean_constant true))
          else
            let other_constructor =
              List.exists
                (function
                  | Vir.Integer_compare
                      ( Vir.Equal,
                        Vir.Aggregate_tag (_, candidate),
                        Vir.Integer_constant ordinal )
                    when candidate = aggregate
                         && not
                              (Z.equal ordinal
                                 (Z.of_int constructor.constructor_index)) ->
                      true
                  | _ -> false)
                state.assumptions
            in
            if other_constructor then
              Some (Boolean_value (Vir.Boolean_constant false))
            else None)
  | (Owned_scalar.Owned_terminal_int | Owned_scalar.Owned_terminal_bool) as
    terminal -> (
      match split_last steps with
      | Some (prefix, Sst.Owned_tree_field field) ->
          Option.map
            (fun aggregate ->
              match terminal with
              | Owned_scalar.Owned_terminal_int ->
                  let selected =
                    Vir.Integer_selector
                      (field_selector field [] Vir.Integer, aggregate)
                  in
                  Integer_value
                    (Logical_adt.normalize_owned_integer state.assumptions 32
                       selected)
              | Owned_scalar.Owned_terminal_bool ->
                  let selected =
                    Vir.Boolean_selector
                      (field_selector field [] Vir.Boolean, aggregate)
                  in
                  Boolean_value
                    (Logical_adt.normalize_owned_boolean state.assumptions 32
                       selected)
              | Owned_scalar.Owned_terminal_unit
              | Owned_scalar.Owned_terminal_tag _ ->
                  assert false)
            (owned_aggregate_at_path context state.assumptions root prefix)
      | None | Some (_, Sst.Owned_tree_constructor _) -> None)
let rec owned_materialized_root aggregate = function
  | [] -> (
      match aggregate.Vir.aggregate_desc with
      | Vir.Aggregate_symbol _ -> Some aggregate
      | Vir.Aggregate_selector _ | Vir.Aggregate_constructor _
      | Vir.Aggregate_record _ | Vir.Aggregate_conditional _
      | Vir.Aggregate_symbolic_application _
      | Vir.Aggregate_imported_model_application _
      | Vir.Aggregate_recursive_spec_application _ ->
          None)
  | step :: rest -> (
      match aggregate.Vir.aggregate_desc with
      | Vir.Aggregate_selector (selector, source) ->
          let exact =
            match step with
            | Sst.Owned_tree_field field ->
                selector
                = field_selector field []
                    (Vir.Aggregate aggregate.aggregate_type)
            | Sst.Owned_tree_constructor constructor ->
                selector
                = argument_selector constructor 0 []
                    (Vir.Aggregate aggregate.aggregate_type)
          in
          if exact then owned_materialized_root source rest else None
      | Vir.Aggregate_symbol _ | Vir.Aggregate_constructor _
      | Vir.Aggregate_record _ | Vir.Aggregate_conditional _
      | Vir.Aggregate_symbolic_application _
      | Vir.Aggregate_imported_model_application _
      | Vir.Aggregate_recursive_spec_application _ ->
          None)
let materialized_owned_integer root steps terminal resolved =
  match (split_last steps, resolved) with
  | ( Some (prefix, Sst.Owned_tree_field field),
      Vir.Integer_selector (selector, aggregate) )
    when selector = field_selector field [] Vir.Integer ->
      Option.bind
        (owned_materialized_root aggregate (List.rev prefix))
        (fun predecessor ->
          if predecessor = root then None
          else
            Some
              (Vir.Integer_selector
                 ( owned_scalar_selector predecessor steps terminal Vir.Integer,
                   predecessor )))
  | None, _
  | Some (_, Sst.Owned_tree_constructor _), _
  | Some (_, Sst.Owned_tree_field _), _ ->
      None
let materialized_owned_boolean root steps terminal resolved =
  match (split_last steps, resolved) with
  | ( Some (prefix, Sst.Owned_tree_field field),
      Vir.Boolean_selector (selector, aggregate) )
    when selector = field_selector field [] Vir.Boolean ->
      Option.bind
        (owned_materialized_root aggregate (List.rev prefix))
        (fun predecessor ->
          if predecessor = root then None
          else
            Some
              (Vir.Boolean_selector
                 ( owned_scalar_selector predecessor steps terminal Vir.Boolean,
                   predecessor )))
  | None, _
  | Some (_, Sst.Owned_tree_constructor _), _
  | Some (_, Sst.Owned_tree_field _), _ ->
      None
let observe_owned_scalar context state path =
  match (context.owned_root_scalar_plan, context.verification_session) with
  | Some plan, Some session -> (
      let root = Verification_session.owned_root_scalar_plan_root plan in
      let steps = Owned_scalar.owned_scalar_path_steps path in
      let terminal = Owned_scalar.owned_scalar_path_terminal path in
      Verification_session.note_owned_root_scalar_observation session;
      match terminal with
      | Owned_scalar.Owned_terminal_unit -> (Unit_value, state)
      | Owned_scalar.Owned_terminal_int ->
          let observed =
            Vir.Integer_selector
              (owned_scalar_selector root steps terminal Vir.Integer, root)
          in
          let state =
            match resolve_owned_scalar context state root path with
            | Some (Integer_value raw_resolved) ->
                let resolved, bridged =
                  if Logical_adt.owned_scalar_integer_is_flat raw_resolved then
                    (raw_resolved, false)
                  else
                    match
                      materialized_owned_integer root steps terminal
                        raw_resolved
                    with
                    | Some resolved -> (resolved, true)
                    | None -> (raw_resolved, false)
                in
                if
                  resolved <> observed
                  && Logical_adt.owned_scalar_integer_is_flat resolved
                then (
                  if bridged then (
                    Verification_session.note_owned_root_scalar_bridge_path
                      session;
                    Verification_session.note_owned_root_scalar_bridge_equation
                      session);
                  Verification_session.note_owned_root_scalar_equation session;
                  with_assumptions state
                    [ Vir.Integer_compare (Vir.Equal, observed, resolved) ])
                else state
            | Some _ | None -> state
          in
          (Integer_value observed, state)
      | Owned_scalar.Owned_terminal_bool | Owned_scalar.Owned_terminal_tag _ ->
          let observed =
            Vir.Boolean_selector
              (owned_scalar_selector root steps terminal Vir.Boolean, root)
          in
          let state =
            match resolve_owned_scalar context state root path with
            | Some (Boolean_value raw_resolved) ->
                let resolved, bridged =
                  if Logical_adt.owned_scalar_boolean_is_flat raw_resolved then
                    (raw_resolved, false)
                  else
                    match
                      materialized_owned_boolean root steps terminal
                        raw_resolved
                    with
                    | Some resolved -> (resolved, true)
                    | None -> (raw_resolved, false)
                in
                if
                  resolved <> observed
                  && Logical_adt.owned_scalar_boolean_is_flat resolved
                then (
                  if bridged then (
                    Verification_session.note_owned_root_scalar_bridge_path
                      session;
                    Verification_session.note_owned_root_scalar_bridge_equation
                      session);
                  Verification_session.note_owned_root_scalar_equation session;
                  with_assumptions state
                    [ Vir.Boolean_equal (observed, resolved) ])
                else state
            | Some _ | None -> state
          in
          (Boolean_value observed, state))
  | Some _, None -> assert false
  | None, (None | Some _) -> assert false
let owned_projection context expression =
  match context.owned_root_scalar_plan with
  | None -> None
  | Some plan -> (
      let template =
        Verification_session.owned_root_scalar_plan_template plan
      in
      let read =
        Owned_scalar.owned_root_scalar_reads template
        |> List.find_opt (fun read ->
            Owned_scalar.owned_scalar_read_expression read == expression)
      in
      match read with
      | Some read -> Some (`Read (Owned_scalar.owned_scalar_read_path read))
      | None ->
          Owned_scalar.owned_root_scalar_projections template
          |> List.find_opt (fun projection ->
              Owned_scalar.owned_scalar_projection_expression projection
              == expression)
          |> Option.map (fun _ -> `Intermediate))
let owned_match context expression =
  match context.owned_root_scalar_plan with
  | None -> None
  | Some plan ->
      Verification_session.owned_root_scalar_plan_template plan
      |> Owned_scalar.owned_root_scalar_matches
      |> List.find_opt (fun matched ->
          Owned_scalar.owned_scalar_match_expression matched == expression)
let nested_owned_reconstruction_evidence context state ~root ~changed_path
    ~target_field ~written_expression =
  let assumptions = state.assumptions @ state.path_condition in
  let constructors =
    Logical_adt_evaluation_private.constructors_of_field
      context.type_definitions target_field.Sst.field_owner target_field
  in
  Logical_adt_evaluation_private.reconstruction_evidence assumptions
    ~constructors ~written_expression ~observe:(fun constructor ->
      Vir.Boolean_selector
        ( owned_scalar_selector root changed_path
            (Owned_scalar.Owned_terminal_tag constructor) Vir.Boolean,
          root ))
let logical_spec_call_target context callee =
  match List.assoc_opt callee.Sst.function_index context.definitions with
  | Some descriptor
    when same_function_id (Sst_validation.callable_id descriptor) callee -> (
      let definition = Sst_validation.callable_definition descriptor in
      Logical_spec_evaluation_private.classify_definition definition
        ~excluded:(fun definition ->
          Option.is_some
               (Type_invariant.find_for_operation context.invariants
                  definition.function_id)
          || Option.is_some
               (Sst_validation.find_model context.validated
                  definition.function_id)))
  | Some _ | None -> Logical_spec_evaluation_private.Unsupported
let logical_evaluation_callbacks context ~aggregate_type ~option_instance ~evaluate_recursive ~error =
  Logical_spec_evaluation_private.
    {
      classify = logical_spec_call_target context;
      aggregate_type;
      option_instance;
      environment = (fun (state : state) -> state.environment);
      with_environment = (fun (state : state) environment -> { state with environment });
      assume = with_assumptions;
      observe_field_read = (fun _ state _ _ _ -> state);
      enter_definition =
        (fun context definition ->
          {
            context with
            current_definition = definition;
            current_callable = definition.function_id;
            entry_environment = [];
            old_environment = None;
            spec_call_stack = definition.function_id.function_index :: context.spec_call_stack;
          });
      evaluate_recursive;
      error;
    }
let formula_registry_inputs ~imports ~validated ~invariants definitions =
  let models, invariant_roots = Invariant_formula_capture.materialize validated (Invariant_formula_policy.materialization_decisions ~imports ~validated
        ~invariants ~frozen:(frozen_spine_for_model validated)
        ~owned:(Sst_validation_private.Owned_recursive_contents_private.model_for_program
          (Sst_validation.program validated))) in
  let classify = Invariant_formula_policy.classify ~imports ~validated ~invariants ~models definitions in
  (invariant_roots, classify)
let prepare_formula_registry ~imports ~session ~validated ~invariants ~type_definitions definitions =
  let invariant_roots, classify = formula_registry_inputs ~imports ~validated ~invariants definitions in
  let before = Verification_session.counters session in
  let entries = Logical_spec_admission_private.prepare ~validated ~type_definitions ~classify ~excluded_contract:(Logical_spec_authentication_private.excluded_contract_root imports) ~authority_snapshot:(Verification_session.render_counters session) ~invariant_roots in
  Logical_spec_admission_private.For_testing.observe_program_candidates ~validated ~type_definitions ~classify;
  Logical_spec_capability_private.For_testing.trace_registry ~unchanged:(before = Verification_session.counters session) ~entries:(List.length entries);
  entries
let authenticated_aggregate_type function_name span descriptors typ =
  match Symbolic_parametric_private.aggregate_type descriptors typ with
  | Ok aggregate_type -> Ok aggregate_type
  | Error message -> error function_name span (Malformed_sst message)
let record_construction_equality function_name span context ~parametric:_ ~record_type ~aggregate ~fields =
  match
    Immutable_aggregate_reconstruction_private.record_construction_equality context.type_definitions
      ~record_type ~aggregate ~fields
  with
  | Ok equality -> Ok equality
  | Error message -> error function_name span (Malformed_sst message)
let constructor_construction_equality function_name span context ~parametric:_ ~constructor ~aggregate
    ~arguments =
  match
    Immutable_aggregate_reconstruction_private.constructor_construction_equality context.type_definitions
      ~constructor ~aggregate ~arguments
  with
  | Ok equality -> Ok equality
  | Error message -> error function_name span (Malformed_sst message)
let authenticated_construction context (expression : Sst.expression) =
  let descriptors = (Sst_validation.program context.validated).parametric_adts in
  let* aggregate_type =
    authenticated_aggregate_type context.current_callable.function_name expression.span descriptors
      expression.typ
  in
  Ok (descriptors, aggregate_type, Symbolic_parametric_private.is_application expression.typ)
let construction_selector descriptors parametric aggregate make_selector path typ =
  let rec needs_parametric_selector = function
    | Sst.Parameter _ | Sst.Application _ -> true
    | Sst.Tuple components ->
        List.exists (fun (_, component) -> needs_parametric_selector component) components
    | Sst.Unit | Sst.Int | Sst.Bool | Sst.Aggregate _ -> false
  in
  if parametric || needs_parametric_selector typ then
    selected_parametric_value_without_state
      ~aggregate_type:(Logical_spec_evaluation_private.vir_aggregate_type_of_sst descriptors)
      aggregate
      (fun path sort -> selector_domain aggregate (make_selector path sort))
      path typ
  else selected_value_without_state aggregate make_selector path typ
let requires_owned_construction_equality context descriptors parametric typ
    construction_equality =
  Option.is_none construction_equality
  && (parametric
     || Immutable_aggregate_reconstruction_private.transparent_schema_embedding
          descriptors context.type_definitions typ
     || owned_recursive_construction_enabled context)
let logical_type_callbacks_for_call validated (definition, type_arguments) =
  let descriptors = (Sst_validation.program validated).parametric_adts in
  let instantiate typ =
    match
      Parametric_lowering_private.instantiate ~binders:definition.Sst.type_binders ~arguments:type_arguments
        typ
    with
    | Ok typ -> typ
    | Error _ -> typ
  in
  Symbolic_parametric_private.type_callbacks descriptors instantiate
let validate_parametric_recursive_measure function_name span measure =
  match Symbolic_parametric_private.recursive_measure measure with
  | Ok () -> Ok ()
  | Error message -> error function_name span (Malformed_sst message)
let unsupported_retained_generic state (definition : Sst.function_definition) (expression : Sst.expression) =
  fresh_value state
    ~source_name:(definition.Sst.function_id.function_name ^ ".parametric-result")
    ~role:Vir.Result ~span:expression.Sst.span ~project:true expression.typ
let call_contract_view imports expression descriptor =
  Imported_callable.ephemeral_contract_view imports expression (Sst_validation.callable_definition descriptor)
let value_parameters (definition : Sst.function_definition) =
  List.map Sst.require_value_parameter definition.parameters
let value_arguments arguments = List.map Sst.require_value_argument arguments
let value_argument_expressions arguments = List.map snd (value_arguments arguments)
let instantiate_call_summary function_name (expression : Sst.expression)
    summary type_arguments =
  match instantiate_summary summary type_arguments with
  | Ok _ as result -> result
  | Error message ->
      error function_name expression.span (Malformed_sst message)
let call_summary function_name (expression : Sst.expression) summary
    type_arguments call_arguments =
  let* summary =
    instantiate_call_summary function_name expression summary type_arguments
  in
  Ok
    ( summary,
      value_arguments call_arguments,
      value_parameters summary.definition )
let unique_parameter_actual definition arguments parameter_index =
  ( Option.bind (List.nth_opt definition.Sst.parameters parameter_index) Sst.value_parameter,
    Option.bind (List.nth_opt arguments parameter_index) Sst.value_argument )
let plain_args function_name span arguments =
  match Sst.value_arguments arguments with
  | Some arguments -> Ok arguments
  | None -> error function_name span (Malformed_sst "specification calls do not admit callback arguments")
let comparison_equality_for state typ left right =
  Symbolic_parametric_private.equality state.parametric_adts typ left right
let consume_frozen_terminal_equality state typ left right equality =
  let current_epoch =
    match state.shared_scalar_heap with
    | Some heap -> Shared_scalar_heap_private.current_epoch heap
    | None -> 0
  in
  let observations =
    match (typ, left, right) with
    | Sst.Aggregate type_id, Aggregate_value left, Aggregate_value right ->
        state.frozen_terminal_observations
        |> List.filter_map (fun observation ->
               let frozen = observation.frozen_terminal_descriptor in
               if
                 frozen.frozen_result_type = type_id
                 && observation.frozen_terminal_root.aggregate_type
                    = vir_aggregate_type frozen.frozen_root
                 && observation.frozen_terminal_call_path <> []
                 && observation.frozen_terminal_path_condition
                    = state.path_condition
                 && observation.frozen_terminal_epoch = current_epoch
               then
                 if observation.frozen_terminal_model = left then
                   Some (observation, right)
                 else if observation.frozen_terminal_model = right then
                   Some (observation, left)
                 else None
               else None)
    | _ -> []
  in
  match observations with
  | [ (observation, compared) ] ->
      let frozen = observation.frozen_terminal_descriptor in
      let result_type = vir_aggregate_type frozen.frozen_result_type in
      let compared_is_more =
        Vir.Integer_compare
          ( Vir.Equal,
            Vir.Aggregate_tag (result_type, compared),
            Vir.Integer_constant
              (Z.of_int frozen.frozen_more_constructor.constructor_index) )
      in
      let compared_head =
        Vir.Integer_selector
          ( argument_selector frozen.frozen_more_constructor 0 [] Vir.Integer,
            compared )
      in
      let terminal_correspondence =
        Vir.Boolean_or
          ( Vir.Boolean_not compared_is_more,
            Vir.Integer_compare
              (Vir.Equal, compared_head, observation.frozen_terminal_value) )
      in
      ( Vir.Boolean_and (equality, terminal_correspondence),
        {
          state with
          frozen_terminal_observations =
            List.filter
              (fun candidate -> candidate != observation)
              state.frozen_terminal_observations;
        } )
  | [] | _ :: _ :: _ -> (equality, state)
let consume_frozen_terminal_comparison state comparison typ left right term =
  if comparison = Sst.Equal then
    consume_frozen_terminal_equality state typ left right term
  else (term, state)
let bind_call_actual evaluate context summary type_arguments caller_environment function_name span result
    (parameter : Sst.parameter) actual =
  let parameter = Sst.require_value_parameter parameter in
  let* environment, ranges, state, obligations = result in
  match parameter.optional_default with
  | None ->
      let* environment, nested_ranges =
        bind_pattern_direct function_name environment parameter.pattern actual
      in
      Ok (environment, ranges @ nested_ranges, state, obligations)
  | Some optional_default ->
      let* carrier_type =
        match
          Parametric_lowering_private.instantiate ~binders:summary.definition.type_binders
            ~arguments:type_arguments parameter.pattern.typ
        with
        | Ok typ -> Ok typ
        | Error message -> error function_name span (Malformed_sst message)
      in
      let* aggregate =
        match actual with
        | Aggregate_value aggregate -> Ok aggregate
        | _ -> error function_name span (Malformed_sst "optional formal received a non-carrier value")
      in
      let* environment, nested_ranges, state, obligations =
        resolve_optional_formal
          ~evaluate_default:(fun state -> evaluate context optional_default.optional_expression state)
          ~function_name ~span ~environment ~state ~obligations ~carrier_type optional_default aggregate
      in
      Ok (environment, ranges @ nested_ranges, { state with environment = caller_environment }, obligations)
let bind_call_actuals evaluate context summary type_arguments caller_state function_name span actuals =
  let caller_environment = caller_state.environment in
  List.fold_left2
    (bind_call_actual evaluate context summary type_arguments caller_environment function_name span)
    (Ok ([], [], caller_state, []))
    summary.definition.parameters actuals
let evaluate_optional evaluate context (expression : Sst.expression) state =
  let function_name = context.current_callable.function_name in
  match expression.expression_desc with
  | Sst.Optional_absent ->
      let* value =
        construct_optional function_name expression.span state.parametric_adts expression.typ None
      in
      Ok { obligations = []; paths = [ { value; state } ] }
  | Sst.Optional_present payload ->
      let* evaluated = evaluate context payload state in
      let rec construct paths = function
        | [] -> Ok { obligations = evaluated.obligations; paths = List.rev paths }
        | path :: rest ->
            let* value =
              construct_optional function_name expression.span path.state.parametric_adts expression.typ
                (Some path.value)
            in
            construct ({ path with value } :: paths) rest
      in
      construct [] evaluated.paths
  | Sst.Optional_forward carrier ->
      let* evaluated = evaluate context carrier state in
      let expected = vir_aggregate_type_of_sst state.parametric_adts expression.typ in
      let rec check paths = function
        | [] -> Ok { obligations = evaluated.obligations; paths = List.rev paths }
        | ({ value = Aggregate_value aggregate; _ } as path) :: rest
          when Some aggregate.aggregate_type = expected ->
            check (path :: paths) rest
        | _ :: _ ->
            error function_name expression.span
              (Malformed_sst "optional forwarding received a non-option aggregate")
      in
      check [] evaluated.paths
  | _ ->
      error function_name expression.span (Malformed_sst "non-optional expression reached optional dispatch")
let callback_runtime evaluate context =
  {
    Call_contract_execution_private.evaluate;
    value = (fun evaluated -> evaluated.value);
    state = (fun evaluated -> evaluated.state);
    evaluated = (fun value state -> { value; state });
    function_ref = (fun context -> context.function_ref);
    logical = (fun context -> context.logical);
    callback_environment = (fun context -> context.callback_environment);
    find_summary = (fun context id -> find_summary context.summaries id);
    definition = (fun summary -> summary.definition);
    requires_clauses = (fun summary -> summary.requires);
    ensures_clauses = (fun summary -> summary.ensures);
    contract_context =
      (fun context definition entry_environment old_environment ->
        {
          context with
          current_definition = definition;
          current_callable = definition.function_id;
          current_mode = definition.mode;
          entry_environment;
          logical = true;
          old_environment;
        });
    environment = (fun state -> state.environment);
    with_environment = (fun state environment -> { state with environment });
    with_assumptions;
    bind_pattern = bind_pattern_direct;
    fresh_value;
    expect_boolean;
    emit_goal;
    malformed =
      (fun function_name span message -> { function_name; span; unsupported = Malformed_sst message });
    record = (fun reached -> context.reached_callback_calls := reached :: !(context.reached_callback_calls));
  }
let callback_direct_runtime evaluate =
  {
    Callback_call_private.evaluate =
      (fun context expression state ->
        evaluate context expression state
        |> Result.map (fun evaluated ->
            { Callback_call_private.obligations = evaluated.obligations; paths = evaluated.paths }));
    value = (fun evaluated -> evaluated.value);
    state = (fun evaluated -> evaluated.state);
    evaluated = (fun value state -> { value; state });
    function_ref = (fun context -> context.function_ref);
    function_name = (fun context -> context.function_ref.Vir.function_name);
    callback_environment = (fun context -> context.callback_environment);
    definition = (fun summary -> summary.definition);
    requires = (fun summary -> summary.requires);
    ensures = (fun summary -> summary.ensures);
    contract_context =
      (fun context definition entry_environment old_environment callback_environment ->
        {
          context with
          current_definition = definition;
          current_callable = definition.function_id;
          current_mode = definition.mode;
          entry_environment;
          logical = true;
          old_environment;
          callback_environment;
        });
    environment = (fun state -> state.environment);
    with_environment = (fun state environment -> { state with environment });
    with_assumptions;
    bind_pattern = bind_pattern_direct;
    fresh_value;
    result_role = Vir.Result;
    expect_boolean;
    precondition_kind = Call_contract_execution_private.call_precondition;
    emit_goal;
    malformed =
      (fun function_name span message -> { function_name; span; unsupported = Malformed_sst message });
  }
let callback_direct evaluate context
    (expression : Sst.expression) summary call_form arguments state =
  Callback_call_private.evaluate_direct
    (callback_direct_runtime evaluate)
    context expression summary call_form arguments state
  |> Result.map
       (fun
         (result :
           (emitted_obligation, evaluated) Callback_call_private.evaluation)
       ->
         ({ obligations = result.obligations; paths = result.paths }
           : evaluated path_evaluation))
let callback_expression evaluate context expression state =
  Call_contract_execution_private.evaluate_callback
    (callback_runtime evaluate context)
    context expression state
let permit_for_body permit (body : Sst.expression) =
  match (permit, body.expression_desc) with
  | Some _, (Sst.Record_value _ | Sst.Constructor_value _) -> None
  | (None | Some _), _ -> permit
let view function_name disposition definition type_arguments arguments
    (expression : Sst.expression) =
  let* () =
    match
      Parametric_lowering_private.validate_sst_direct_call ~definition
        ~type_arguments ~actual_result:expression.typ ~call_span:expression.span
        ~arguments
    with
    | Ok () -> Ok ()
    | Error (span, message) -> error function_name span (Malformed_sst message)
  in
  let* result_type =
    match
      Parametric_lowering_private.instantiate
        ~binders:definition.Sst.type_binders ~arguments:type_arguments
        definition.result_type
    with
    | Ok result_type -> Ok result_type
    | Error message ->
        error function_name expression.span (Malformed_sst message)
  in
  let* () =
    if Parametric_type.equal result_type expression.typ then Ok ()
    else
      error function_name expression.span
        (Malformed_sst "specification result differs from its exact application")
  in
  let substitute =
    Parametric_type.substitute
      (List.combine definition.type_binders type_arguments)
  in
  let parameters =
    value_parameters definition
    |> List.map (fun (parameter : Sst.value_parameter) ->
           {
             parameter with
             Sst.pattern = Sst.map_pattern_types substitute parameter.pattern;
           })
  in
  let disposition =
    match disposition with
    | `Expand (body, permit) ->
        let body =
          if type_arguments = [] then body
          else Sst.map_expression_types substitute body in
        `Expand (body, permit)
    | (`Opaque_imported | `Opaque_invariant _ | `Opaque_recursive) as disposition -> disposition
  in
  Ok (parameters, result_type, disposition)

let recursive_integer_application callee type_arguments arguments span =
  Vir.Integer_recursive_spec_application
    { callee; arguments; span; type_arguments }
let recursive_boolean_application callee type_arguments arguments span =
  Vir.Boolean_recursive_spec_application
    { callee; arguments; span; type_arguments }
let symbolic_value parametric_adts application =
  Spec_function_logic_private.value_of_application
    ~aggregate_type:(vir_aggregate_type_of_sst parametric_adts)
    ~error:(fun _ message -> message)
    ~span:(Symbolic_application_private.span application)
    (Symbolic_application_private.result_type application) application
  |> Result.get_ok
let symbolic_trigger parametric_adts application =
  Spec_function_logic_private.application_term
    ~error:(fun _ message -> message)
    ~span:(Symbolic_application_private.span application)
    (symbolic_value parametric_adts application)
  |> Result.get_ok
let evaluate_explicit_trigger evaluate context (expression : Sst.expression) state =
  Quantifier_validation_private.explicit_trigger
    {
      evaluate_argument =
        (fun context span source state ->
          let function_name = context.function_ref.Vir.function_name in
          let* evaluated = evaluate context source state in
          match (evaluated.obligations, evaluated.paths) with
          | [], [ path ] ->
              let* argument =
                trigger_argument function_name span path.value
              in
              Ok (argument, path.state)
          | [], [] | [], _ :: _ :: _ | _ :: _, _ ->
              error function_name span
                (Malformed_sst
                   "explicit trigger arguments must lower to one pure logical \
                    term"));
      recursive =
        (fun context callee ->
          match List.assoc_opt callee.Sst.function_index context.definitions with
          | Some descriptor -> (
              match
                (Sst_validation.callable_definition descriptor).Sst.body
              with
              | Sst.Recursive_spec_definition _ -> true
              | Sst.Spec_definition _ | Sst.Checked_exec _ | Sst.Proof_body _
              | Sst.External_specification _
              | Sst.Trusted_external_spec_target _
              | Sst.Trusted_external_body _
              | Sst.Symbolic_declaration _ ->
                  false)
              | None -> false);
      construct_trigger =
        (function
        | Quantifier_validation_private.Spec_apply_trigger
            { arrow; arguments; result_type; span } -> (
            match arguments with
            | Vir.Recursive_parametric_argument function_ :: argument :: [] ->
                let application =
                  Spec_function_logic_private.application ~arrow ~function_
                    ~argument ~result_type ~span
                  |> Result.get_ok
                in
                symbolic_trigger state.parametric_adts application
            | [] | [ _ ] | _ :: _ :: [] | _ :: _ :: _ :: _ -> assert false)
        | Quantifier_validation_private.Direct_trigger
            { recursive; callee; type_arguments; arguments; span } ->
            (match expression.expression_desc with
            | Sst.Symbolic_application application ->
                let application =
                  match
                    Symbolic_application_private.replace_arguments arguments
                      application
                  with
                  | Ok application -> application
                  | Error _ -> assert false
                in
                symbolic_trigger state.parametric_adts application
            | _ ->
                if recursive then
                  Vir.Boolean_application
                    (Vir.Boolean_recursive_spec_application
                       { callee; type_arguments; arguments; span })
                else
                  Vir.Boolean_application
                    (Vir.Boolean_specification_application
                       { callee; type_arguments; arguments; span }))
        | Quantifier_validation_private.Callback_requires_trigger
            { application; arguments } ->
            Vir.Boolean_application
              (Vir.Callback_requires
                 {
                   Vir.callback = application.callback;
                   arguments;
                   call_span = application.callback_span;
                 })
        | Quantifier_validation_private.Callback_ensures_trigger
            { application; arguments; result } ->
            Vir.Boolean_application
              (Vir.Callback_ensures
                 {
                   application =
                     {
                       Vir.callback = application.callback;
                       arguments;
                       call_span = application.callback_span;
                     };
                   result;
                 })
        );
      malformed_trigger =
        (fun context span message ->
          {
            function_name = context.function_ref.Vir.function_name;
            span;
            unsupported = Malformed_sst message;
          });
    }
    context expression state
let evaluate_logical_formula evaluate context function_name
    (source : Sst.expression) state =
  Quantifier_validation_private.quantifier_expression
    {
      branches =
        (fun source state ->
          let* evaluated = evaluate context source state in
          match evaluated.obligations with
          | _ :: _ -> Ok None
          | [] ->
              let base_length = List.length state.path_condition in
              let rec drop count values =
                if count = 0 then values
                else
                  match values with
                  | [] -> []
                  | _ :: rest -> drop (count - 1) rest
              in
              let branch path =
                let* term =
                  expect_boolean function_name source.span path.value
                in
                let conditions =
                  drop base_length path.state.path_condition
                in
                Ok
                  ( List.fold_left
                      (fun term condition -> Vir.Boolean_and (condition, term))
                      term conditions,
                    path.state )
              in
              let* branches =
                List.fold_left
                  (fun result path ->
                    let* branches = result in
                    let* branch = branch path in
                    Ok (branch :: branches))
                  (Ok []) evaluated.paths
              in
              Ok (Some (List.rev branches)));
      merge_branches = (fun left right -> Vir.Boolean_or (left, right));
      state_rank = (fun state -> state.next_symbol);
      malformed_expression =
        (fun span message ->
          { function_name; span; unsupported = Malformed_sst message });
    }
    source state
let evaluate_quantifier evaluate context (expression : Sst.expression) kind
    (quantifier : Sst.quantifier) state =
  let function_name = context.function_ref.Vir.function_name in
  if not context.logical then
    error function_name expression.span
      (Malformed_sst "logical quantifier reached runtime execution")
  else
    let outer = state in
    let* binder, value, state =
      fresh_quantifier_value state quantifier.quantifier_binder
    in
    let scoped =
      {
        state with
        environment =
          (quantifier.quantifier_binder.id, value) :: state.environment;
      }
    in
    let one = evaluate_logical_formula evaluate context function_name in
    let* body, body_state = one quantifier.quantifier_body scoped in
    let* trigger, inner =
      match quantifier.quantifier_trigger with
      | None -> Ok (None, body_state)
      | Some trigger ->
          let trigger_state =
            { scoped with next_symbol = body_state.next_symbol }
          in
          let* trigger, trigger_state =
            evaluate_explicit_trigger evaluate context trigger trigger_state
          in
          Ok (Some trigger, trigger_state)
    in
    let bounded_body =
      Quantifier_validation_private.quantifier_body
        {
          integer_range =
            (fun () -> Vir.integer_range (Vir.Integer_symbol binder));
          truth = Vir.Boolean_constant true;
          conjunction = (fun left right -> Vir.Boolean_and (left, right));
          disjunction = (fun left right -> Vir.Boolean_or (left, right));
          negation = (fun term -> Vir.Boolean_not term);
        }
        kind quantifier.quantifier_binder body
    in
    let* term =
      match
        Vir.make_boolean_quantifier
          ~sort_of_type:(function
            | Parametric_type.Int -> Ok Vir.Integer
            | Bool -> Ok Vir.Boolean
            | Parameter binder -> Ok (Vir.Parametric binder)
            | Application _ as typ
              when Parametric_type.is_spec_function typ ->
                Ok
                  (Vir.Parametric
                     (Spec_function_logic_private.binder typ))
            | Application _ as typ -> (
                match
                  vir_aggregate_type_of_sst state.parametric_adts typ
                with
                | Some aggregate -> Ok (Vir.Aggregate aggregate)
                | None -> Error "quantifier application sort is unavailable")
            | Unit | Tuple _ | Aggregate _ ->
                Error "unsupported quantifier binder sort")
          ~schema:
            (Logic_quantifier_private.singleton
               quantifier.quantifier_metadata)
          ~binders:[ binder ] ~body:bounded_body ~trigger
      with
      | Ok term -> Ok term
      | Error message ->
          error function_name expression.span (Malformed_sst message)
    in
    Ok
      {
        obligations = [];
        paths =
          [
            {
              value =
                Boolean_value
                  (match kind with
                  | Logic_quantifier_private.Forall -> Vir.Forall_term term
                  | Logic_quantifier_private.Exists -> Vir.Exists_term term);
              state = { outer with next_symbol = inner.next_symbol };
            };
          ];
      }
let logical_expression evaluate context (expression : Sst.expression) state =
  match expression.expression_desc with
  | Sst.Forall quantifier | Sst.Exists quantifier ->
      evaluate_quantifier evaluate context expression
        (Logic_quantifier_private.kind quantifier.quantifier_metadata)
        quantifier state
  | Sst.Callback_call _ | Sst.Callback_requires _ | Sst.Callback_ensures _ ->
      callback_expression evaluate context expression state
  | Sst.Int_constant _ | Sst.Bool_constant _ | Sst.Unit_constant
  | Sst.Variable _ | Sst.Tuple_value _ | Sst.Record_value _
  | Sst.Constructor_value _ | Sst.Field_read _ | Sst.Field_write _
  | Sst.Shared_scalar_field_write _ | Sst.Owned_tree_nested_write _
  | Sst.Owned_tree_rebase _ | Sst.Let_mutable _ | Sst.Mutable_read _
  | Sst.Mutable_write _ | Sst.Let _ | Sst.Sequence _ | Sst.If _ | Sst.Match _
  | Sst.Checked_arithmetic _ | Sst.Compare _ | Sst.Boolean_not _
  | Sst.Boolean_binary _ | Sst.Direct_call _ | Sst.Optional_absent
  | Sst.Optional_present _ | Sst.Optional_forward _ | Sst.Reveal _
  | Sst.Reveal_with_fuel _ | Sst.Use_type_invariant _ | Sst.Local_assert _
  | Sst.Proof_region _ | Sst.Old _ ->
      assert false
  | Sst.Symbolic_application _ ->
      assert false
let rec evaluate context expression state =
  let context =
    if
      (not context.logical)
      &&
      match
        try
          Some
            (Sst_validation.expression_instance_mode context.validated
               context.current_callable expression)
        with Invalid_argument _ -> None
      with
      | Some (Sst.Ghost_instance | Sst.Tracked_instance) -> true
      | Some Sst.Exec_instance | None -> false
    then { context with logical = true }
    else context
  in
  let function_ref = context.function_ref in
  let function_name = function_ref.Vir.function_name in
  match expression.Sst.expression_desc with
  | Sst.Int_constant value ->
      Ok
        {
          obligations = [];
          paths =
            [ { value = Integer_value (Vir.Integer_constant value); state } ];
        }
  | Sst.Bool_constant value ->
      Ok
        {
          obligations = [];
          paths =
            [ { value = Boolean_value (Vir.Boolean_constant value); state } ];
        }
  | Sst.Unit_constant ->
      Ok { obligations = []; paths = [ { value = Unit_value; state } ] }
  | Sst.Symbolic_application application ->
      if not context.logical then
        error function_name expression.span
          (Malformed_sst
             "symbolic declarations are unavailable to executable code")
      else
        let rec evaluate_arguments obligations contexts = function
          | [] ->
              let* paths =
                List.fold_left
                  (fun result (state, reversed) ->
                    let* paths = result in
                    let* application =
                      Symbolic_application_private.replace_arguments
                        (List.rev reversed) application
                      |> Result.map_error (fun message ->
                             {
                               function_name;
                               span = expression.span;
                               unsupported = Malformed_sst message;
                             })
                    in
                    Ok
                      ({
                         value =
                           symbolic_value state.parametric_adts application;
                         state;
                       }
                      :: paths))
                  (Ok []) contexts
              in
              Ok { obligations; paths = List.rev paths }
          | source :: rest ->
              let* evaluated =
                evaluate_contexts
                  (fun (state, arguments) ->
                    let* result = evaluate context source state in
                    let* paths =
                      List.fold_left
                        (fun paths evaluated ->
                          let* paths = paths in
                          let* argument =
                            symbolic_argument function_name source.span
                              evaluated.value
                          in
                          Ok
                            ((evaluated.state, argument :: arguments) :: paths))
                        (Ok []) result.paths
                    in
                    Ok
                      {
                        obligations = result.obligations;
                        paths = List.rev paths;
                      })
                  contexts
              in
              evaluate_arguments
                (append obligations evaluated.obligations)
                evaluated.paths rest
        in
        evaluate_arguments [] [ (state, []) ]
          (Symbolic_application_private.arguments application)
  | Sst.Optional_absent | Sst.Optional_present _ | Sst.Optional_forward _ ->
      evaluate_optional evaluate context expression state
  | Sst.Reveal target ->
      if context.logical then
        let activation =
          {
            Spec_unfolding.function_id = target;
            depth = 1;
            span = expression.span;
          }
        in
        Ok
          {
            obligations = [];
            paths =
              [
                {
                  value = Unit_value;
                  state =
                    {
                      state with
                      proof_activations =
                        append state.proof_activations [ activation ];
                      reached_proof_activations =
                        append state.reached_proof_activations [ activation ];
                    };
                };
              ];
          }
      else
        error function_name expression.span
          (Malformed_sst "reveal reached runtime evaluation")
  | Sst.Reveal_with_fuel { function_id = target; literal_depth } ->
      if context.logical then
        let* depth =
          match
            Spec_unfolding.literal_depth ~span:expression.span literal_depth
          with
          | Ok depth -> Ok depth
          | Error message ->
              error function_name expression.span (Malformed_sst message)
        in
        let activation =
          { Spec_unfolding.function_id = target; depth; span = expression.span }
        in
        Ok
          {
            obligations = [];
            paths =
              [
                {
                  value = Unit_value;
                  state =
                    {
                      state with
                      proof_activations =
                        append state.proof_activations [ activation ];
                      reached_proof_activations =
                        append state.reached_proof_activations [ activation ];
                    };
                };
              ];
          }
      else
        error function_name expression.span
          (Malformed_sst "reveal reached runtime evaluation")
  | Sst.Variable { binding; _ } -> (
      match List.assoc_opt binding.id state.environment with
      | Some value -> Ok { obligations = []; paths = [ { value; state } ] }
      | None ->
          error function_name expression.span
            (Malformed_sst ("unbound variable " ^ binding.name)))
  | Sst.Tuple_value components ->
      let initial = [ (state, []) ] in
      let rec loop obligations contexts = function
        | [] ->
            Ok
              {
                obligations;
                paths =
                  List.map
                    (fun (state, values) ->
                      { value = Tuple_value (List.rev values); state })
                    contexts;
              }
        | (_, component) :: rest ->
            let* evaluated =
              evaluate_contexts
                (fun (state, values) ->
                  let* result = evaluate context component state in
                  Ok
                    {
                      obligations = result.obligations;
                      paths =
                        List.map
                          (fun evaluated ->
                            (evaluated.state, evaluated.value :: values))
                          result.paths;
                    })
                contexts
            in
            loop (append obligations evaluated.obligations) evaluated.paths rest
      in
      loop [] initial components
  | Sst.Record_value { record_type; fields } ->
      let rec evaluate_fields obligations contexts = function
        | [] -> Ok { obligations; paths = contexts }
        | (field, field_expression) :: rest ->
            let* evaluated =
              evaluate_contexts
                (fun (state, values) ->
                  let* result = evaluate context field_expression state in
                  Ok
                    {
                      obligations = result.obligations;
                      paths =
                        List.map
                          (fun evaluated ->
                            ( evaluated.state,
                              (field, field_expression, evaluated.value)
                              :: values ))
                          result.paths;
                    })
                contexts
            in
            evaluate_fields
              (append obligations evaluated.obligations)
              evaluated.paths rest
      in
      let* fields = evaluate_fields [] [ (state, []) ] fields in
      let make (state, reversed_fields) =
        let* descriptors, aggregate_type, parametric =
          authenticated_construction context expression
        in
        let symbol, state =
          fresh_symbol state
            ~source_name:("_" ^ record_type.type_name)
            ~sort:(Vir.Aggregate aggregate_type) ~role:Vir.Local
            ~span:expression.span ~project:false
        in
        let* vir_fields =
          List.fold_left
            (fun result (field, _, value) ->
              let* fields = result in
              let* value =
                match value with
                | Integer_value term -> Ok (Vir.Recursive_integer_argument term)
                | Boolean_value term -> Ok (Vir.Recursive_boolean_argument term)
                | Aggregate_value term ->
                    Ok (Vir.Recursive_aggregate_argument term)
                | Unit_value ->
                    (* Unit has one inhabitant and no VIR sort.  It needs no
                       constructor argument or selector equation. *)
                    Ok
                      (Vir.Recursive_boolean_argument
                         (Vir.Boolean_constant true))
                | Parametric_value term ->
                    Ok (Vir.Recursive_parametric_argument term)
                | Tuple_value _ | Function_value _ ->
                    error function_name expression.span
                      (Malformed_sst
                         "record construction escaped the scalar/aggregate VIR \
                          boundary")
              in
              Ok ((field, value) :: fields))
            (Ok []) (List.rev reversed_fields)
        in
        let aggregate =
          { Vir.aggregate_type; aggregate_desc = Vir.Aggregate_symbol symbol }
        in
        let* construction_equality =
          record_construction_equality function_name expression.span context
            ~parametric ~record_type ~aggregate ~fields:(List.rev vir_fields)
        in
        let select = construction_selector descriptors parametric in
        let* equations =
          let rec loop equations = function
            | [] -> Ok (List.rev equations)
            | (field, field_expression, value) :: rest -> (
                let selected =
                  select aggregate (field_selector field) []
                    field_expression.Sst.typ
                in
                match equality selected value with
                | Some equation -> loop (equation :: equations) rest
                | None ->
                    error function_name expression.span
                      (Malformed_sst "record field type/value mismatch"))
          in
          loop [] (List.rev reversed_fields)
        in
        let owned_equality =
          if requires_owned_construction_equality context descriptors parametric
               expression.typ construction_equality
          then
            [
              Vir.Aggregate_equal
                ( aggregate,
                  {
                    Vir.aggregate_type;
                    aggregate_desc =
                      Vir.Aggregate_record
                        { record_type; fields = List.rev vir_fields };
                  } );
            ]
          else []
        in
        let state = with_assumptions state (owned_equality @ equations) in
        let state =
          with_immutable_aggregate_facts state
            (Option.to_list construction_equality)
        in
        let immutable =
          match
            List.find_opt
              (fun (definition : Sst.type_definition) ->
                same_type_id definition.type_id record_type)
              context.type_definitions
          with
          | Some { type_kind = Sst.Record_definition definitions; _ } ->
              List.for_all Finite_value_registry.Finite_domain.immutable_field
                definitions
              && List.length definitions = List.length reversed_fields
          | Some { type_kind = Sst.Variant_definition _; _ } | None -> false
        in
        let children =
          List.rev reversed_fields
          |> List.map (fun (_, child_expression, value) ->
              (child_expression, value))
        in
        let* state =
          Immutable_fact_integration.issue_finite_construction context
            expression state aggregate children immutable
            (Finite_value_registry.Record_construction record_type)
        in
        let state =
          match context.verification_session with
          | Some session -> (
              match
                Verification_session.issue_closed_owned_contents_origin session
                  ~validated:context.validated
                  ~caller:context.current_definition ~expression ~root:aggregate
                  ~path_condition:state.path_condition
              with
              | Ok origin ->
                  replace_owned_contents_origin state aggregate origin
              | Error _ -> state)
          | None -> state
        in
        Ok
          {
            obligations = [];
            paths = [ { value = Aggregate_value aggregate; state } ];
          }
      in
      let* constructed = evaluate_contexts make fields.paths in
      let instance_mode =
        try
          Some
            (Sst_validation.expression_instance_mode context.validated
               context.current_callable expression)
        with Invalid_argument _ -> None
      in
      let* established =
        match
          ( invariant_for_typ context.invariants (Sst.Aggregate record_type),
            instance_mode )
        with
        | Some handle, Some (Sst.Exec_instance | Sst.Tracked_instance) ->
            evaluate_contexts
              (fun evaluated ->
                prove_invariant_validity context handle
                  Vir.Constructor_establishment context.function_ref
                  expression.span evaluated.value evaluated.state)
              constructed.paths
        | None, _ | Some _, (Some Sst.Ghost_instance | None) ->
            Ok { obligations = []; paths = constructed.paths }
      in
      Ok
        {
          obligations =
            append fields.obligations
              (append constructed.obligations established.obligations);
          paths = established.paths;
        }
  | Sst.Constructor_value { constructor; arguments } ->
      let rec evaluate_arguments obligations contexts = function
        | [] -> Ok { obligations; paths = contexts }
        | argument :: rest ->
            let* evaluated =
              evaluate_contexts
                (fun (state, values) ->
                  let* result = evaluate context argument state in
                  Ok
                    {
                      obligations = result.obligations;
                      paths =
                        List.map
                          (fun evaluated ->
                            ( evaluated.state,
                              (argument, evaluated.value) :: values ))
                          result.paths;
                    })
                contexts
            in
            evaluate_arguments
              (append obligations evaluated.obligations)
              evaluated.paths rest
      in
      let* arguments = evaluate_arguments [] [ (state, []) ] arguments in
      let make (state, reversed_arguments) =
        let* descriptors, aggregate_type, parametric =
          authenticated_construction context expression
        in
        let symbol, state =
          fresh_symbol state
            ~source_name:("_" ^ constructor.constructor_name)
            ~sort:(Vir.Aggregate aggregate_type) ~role:Vir.Local
            ~span:expression.span ~project:false
        in
        let* vir_arguments =
          List.fold_left
            (fun result (_, value) ->
              let* arguments = result in
              let* value =
                match value with
                | Integer_value term -> Ok (Vir.Recursive_integer_argument term)
                | Boolean_value term -> Ok (Vir.Recursive_boolean_argument term)
                | Aggregate_value term ->
                    Ok (Vir.Recursive_aggregate_argument term)
                | Unit_value ->
                    Ok
                      (Vir.Recursive_boolean_argument
                         (Vir.Boolean_constant true))
                | Parametric_value term ->
                    Ok (Vir.Recursive_parametric_argument term)
                | Tuple_value _ | Function_value _ ->
                    error function_name expression.span
                      (Malformed_sst
                         "constructor escaped the scalar/aggregate VIR boundary")
              in
              Ok (value :: arguments))
            (Ok [])
            (List.rev reversed_arguments)
        in
        let aggregate =
          { Vir.aggregate_type; aggregate_desc = Vir.Aggregate_symbol symbol }
        in
        let* construction_equality =
          constructor_construction_equality function_name expression.span
            context ~parametric ~constructor ~aggregate
            ~arguments:(List.rev vir_arguments)
        in
        let select = construction_selector descriptors parametric in
        let arguments = List.rev reversed_arguments in
        let* selector_equations =
          let rec loop index equations = function
            | [] -> Ok (List.rev equations)
            | (argument, value) :: rest -> (
                let selected =
                  select aggregate (argument_selector constructor index) []
                    argument.Sst.typ
                in
                match equality selected value with
                | Some equation -> loop (index + 1) (equation :: equations) rest
                | None ->
                    error function_name expression.span
                      (Malformed_sst "constructor argument type/value mismatch")
                )
          in
          loop 0 [] arguments
        in
        let tag =
          Vir.Integer_compare
            ( Vir.Equal,
              Vir.Aggregate_tag (aggregate_type, aggregate),
              Vir.Integer_constant (Z.of_int constructor.constructor_index) )
        in
        let owned_equality =
          if requires_owned_construction_equality context descriptors parametric
               expression.typ construction_equality
          then
            [
              Vir.Aggregate_equal
                ( aggregate,
                  {
                    Vir.aggregate_type;
                    aggregate_desc =
                      Vir.Aggregate_constructor
                        { constructor; arguments = List.rev vir_arguments };
                  } );
            ]
          else []
        in
        let state =
          with_assumptions state (owned_equality @ (tag :: selector_equations))
        in
        let state =
          with_immutable_aggregate_facts state
            (Option.to_list construction_equality)
        in
        let immutable =
          match
            List.find_opt
              (fun (definition : Sst.type_definition) ->
                same_type_id definition.type_id constructor.constructor_type)
              context.type_definitions
          with
          | Some { type_kind = Sst.Variant_definition constructors; _ } -> (
              match
                List.find_opt
                  (fun (definition : Sst.constructor_definition) ->
                    same_constructor_id definition.constructor_id constructor)
                  constructors
              with
              | Some definition ->
                  List.for_all
                    Finite_value_registry.Finite_domain.immutable_field
                    definition.constructor_fields
                  && List.length definition.constructor_fields
                     = List.length reversed_arguments
              | None -> false)
          | Some { type_kind = Sst.Record_definition _; _ } | None -> false
        in
        let* state =
          Immutable_fact_integration.issue_finite_construction context
            expression state aggregate
            (List.rev reversed_arguments
            |> List.map (fun (child_expression, value) ->
                (child_expression, value)))
            immutable
            (Finite_value_registry.Constructor_construction constructor)
        in
        let state =
          match context.verification_session with
          | Some session -> (
              match
                Verification_session.issue_closed_owned_contents_origin session
                  ~validated:context.validated
                  ~caller:context.current_definition ~expression ~root:aggregate
                  ~path_condition:state.path_condition
              with
              | Ok origin ->
                  replace_owned_contents_origin state aggregate origin
              | Error _ -> state)
          | None -> state
        in
        Ok
          {
            obligations = [];
            paths = [ { value = Aggregate_value aggregate; state } ];
          }
      in
      let* constructed = evaluate_contexts make arguments.paths in
      let instance_mode =
        try
          Some
            (Sst_validation.expression_instance_mode context.validated
               context.current_callable expression)
        with Invalid_argument _ -> None
      in
      let* established =
        match
          ( invariant_for_typ context.invariants
              (Sst.Aggregate constructor.constructor_type),
            instance_mode )
        with
        | Some handle, Some (Sst.Exec_instance | Sst.Tracked_instance) ->
            evaluate_contexts
              (fun evaluated ->
                prove_invariant_validity context handle
                  Vir.Constructor_establishment context.function_ref
                  expression.span evaluated.value evaluated.state)
              constructed.paths
        | None, _ | Some _, (Some Sst.Ghost_instance | None) ->
            Ok { obligations = []; paths = constructed.paths }
      in
      Ok
        {
          obligations =
            append arguments.obligations
              (append constructed.obligations established.obligations);
          paths = established.paths;
        }
  | Sst.Field_read { record; field } -> (
      match owned_projection context expression with
      | Some (`Read path) ->
          let value, state = observe_owned_scalar context state path in
          Ok { obligations = []; paths = [ { value; state } ] }
      | Some `Intermediate ->
          let root =
            Option.get context.owned_root_scalar_plan
            |> Verification_session.owned_root_scalar_plan_root
          in
          Ok
            {
              obligations = [];
              paths = [ { value = Aggregate_value root; state } ];
            }
      | None ->
          let* record = evaluate context record state in
          let* projected =
            evaluate_contexts
              (fun evaluated ->
                match evaluated.value with
                | Aggregate_value aggregate ->
                    let* value, state =
                      select_field evaluated.state aggregate field
                        expression.typ
                    in
                    let* value, state =
                      match (state.shared_scalar_heap, value) with
                      | Some heap, Integer_value entry
                        when Shared_scalar_heap_private.accepts_read heap ~field
                               ~location:aggregate -> (
                          match
                            Shared_scalar_heap_private.read heap
                              ~view:context.shared_heap_view ~field
                              ~location:aggregate ~entry
                          with
                          | Ok term ->
                              Ok
                                ( Integer_value term,
                                  with_assumptions state
                                    (Vir.integer_range term) )
                          | Error message ->
                              error function_name expression.span
                                (Malformed_sst message))
                      | Some _, Integer_value _ -> Ok (value, state)
                      | Some _, _ ->
                          error function_name expression.span
                            (Malformed_sst
                               "shared heap read changed from integer field \
                                type")
                      | None, Integer_value entry -> (
                          match
                            List.find_opt
                              (fun (transition :
                                     Sst.shared_scalar_heap_transition) ->
                                transition.shared_target_field = field
                                && aggregate.aggregate_type
                                   = vir_aggregate_type
                                       transition.shared_record_type)
                              context.shared_entry_transitions
                          with
                          | None -> Ok (value, state)
                          | Some transition ->
                              Option.iter
                                Verification_session.note_shared_heap_read_log
                                context.verification_session;
                              context.shared_entry_reads :=
                                {
                                  Vir.shared_read_field = field;
                                  shared_read_path_id =
                                    transition.shared_path_id;
                                  shared_read_epoch = 0;
                                  shared_read_location = aggregate;
                                  shared_read_term = entry;
                                  shared_read_entry_view =
                                    context.shared_heap_view
                                    = Shared_scalar_heap_private.Entry_view;
                                }
                                :: !(context.shared_entry_reads);
                              Ok (value, state))
                      | None, _ -> Ok (value, state)
                    in
                    let immutable =
                      List.exists
                        (fun (definition : Sst.type_definition) ->
                          match definition.type_kind with
                          | Sst.Record_definition fields
                          | Sst.Variant_definition
                              [ { Sst.constructor_fields = fields; _ } ] ->
                              List.exists
                                (fun definition ->
                                  definition.Sst.field_id = field
                                  && Finite_value_registry.Finite_domain
                                     .immutable_field definition)
                                fields
                          | Sst.Variant_definition _ -> false)
                        context.type_definitions
                    in
                    let* state =
                      if not immutable then Ok state
                      else
                        let* mode =
                          expression_instance_mode context expression
                        in
                        Immutable_fact_integration.derive_finite_selected
                          context expression.span state ~parent:aggregate ~mode
                          ~typ:expression.typ value
                          ~provenance:
                            Finite_value_registry.Immutable_record_field
                    in
                    Ok { obligations = []; paths = [ { value; state } ] }
                | _ ->
                    error function_name expression.span
                      (Malformed_sst "field read receiver is not an aggregate"))
              record.paths
          in
          Ok
            {
              obligations = append record.obligations projected.obligations;
              paths = projected.paths;
            })
  | Sst.Field_write { provenance; field; value; transition } ->
      let erased_update =
        match
          Sst_validation.expression_instance_mode context.validated
            context.current_callable expression
        with
        | Sst.Ghost_instance | Sst.Tracked_instance -> true
        | Sst.Exec_instance -> false
      in
      if context.logical && not erased_update then
        error function_name expression.span
          (Malformed_sst "mutation is not allowed in a logical expression")
      else if
        (not erased_update)
        && provenance.binding_pattern_uniqueness <> Sst.Definitely_unique
        || (not provenance.field_is_local)
        || (not provenance.field_is_public)
        || not provenance.field_is_mutable
      then
        error function_name expression.span
          (Malformed_sst "field write lacks definite unique provenance")
      else
        let root = provenance.root in
        let* fields =
          match root.typ with
          | Sst.Aggregate type_id -> (
              match
                List.find_opt
                  (fun (definition : Sst.type_definition) ->
                    same_type_id definition.type_id type_id)
                  context.type_definitions
              with
              | Some { type_kind = Sst.Record_definition fields; _ } ->
                  if
                    List.exists
                      (fun (definition : Sst.field_definition) ->
                        definition.field_id = field)
                      fields
                  then Ok fields
                  else
                    error function_name expression.span
                      (Malformed_sst
                         "field write does not belong to its root record")
              | Some { type_kind = Sst.Variant_definition _; _ } ->
                  error function_name expression.span
                    (Malformed_sst "direct field write root is not a record")
              | None ->
                  error function_name expression.span
                    (Malformed_sst "field-write root type is not registered"))
          | _ ->
              error function_name expression.span
                (Malformed_sst "field-write root has non-aggregate type")
        in
        let* evaluated_value = evaluate context value state in
        let advance evaluated =
          let* old_aggregate =
            match List.assoc_opt root.id evaluated.state.environment with
            | Some (Aggregate_value aggregate) -> Ok aggregate
            | Some _ | None ->
                error function_name expression.span
                  (Malformed_sst
                     "field-write root changed incompatibly while evaluating \
                      its value")
          in
          let aggregate_type = old_aggregate.Vir.aggregate_type in
          let symbol, state =
            fresh_symbol evaluated.state ~source_name:(root.name ^ ".state")
              ~sort:(Vir.Aggregate aggregate_type) ~role:Vir.Local
              ~span:expression.span ~project:true
          in
          let fresh_aggregate =
            { Vir.aggregate_type; aggregate_desc = Vir.Aggregate_symbol symbol }
          in
          let rec copy state equations = function
            | [] ->
                let state = with_assumptions state (List.rev equations) in
                let state =
                  match
                    ( transition,
                      context.verification_session,
                      find_owned_contents_origin evaluated.state old_aggregate
                    )
                  with
                  | Some _, Some session, Some predecessor -> (
                      match
                        Verification_session
                        .issue_successor_owned_contents_origin session
                          ~validated:context.validated
                          ~caller:context.current_definition ~expression
                          ~predecessor ~predecessor_root:old_aggregate
                          ~successor_root:fresh_aggregate
                          ~path_condition:evaluated.state.path_condition
                      with
                      | Ok origin ->
                          replace_owned_contents_origin state fresh_aggregate
                            origin
                      | Error _ -> state)
                  | Some _, (Some _ | None), None
                  | Some _, None, Some _
                  | None, (Some _ | None), (Some _ | None) ->
                      state
                in
                let successor = Aggregate_value fresh_aggregate in
                let* preserved =
                  match transition with
                  | None ->
                      Ok
                        {
                          obligations = [];
                          paths = [ { value = successor; state } ];
                        }
                  | Some transition -> (
                      match
                        invariant_for_typ context.invariants transition.root.typ
                      with
                      | None ->
                          Ok
                            {
                              obligations = [];
                              paths = [ { value = successor; state } ];
                            }
                      | Some handle ->
                          prove_invariant_validity context handle
                            (Vir.Transition_preservation
                               {
                                 transition_kind = Vir.Direct_root_transition;
                                 root_binding_id = transition.root.id;
                                 pre_version = transition.pre_version;
                                 successor_version =
                                   transition.successor_version;
                               })
                            context.function_ref expression.span successor state
                      )
                in
                Ok
                  {
                    obligations = preserved.obligations;
                    paths =
                      List.map
                        (fun preserved ->
                          {
                            value = Unit_value;
                            state =
                              replace_owned_root preserved.state root
                                preserved.value
                                (match transition with
                                | Some transition ->
                                    transition.successor_version
                                | None ->
                                    owned_root_version evaluated.state root
                                      old_aggregate
                                    + 1);
                          })
                        preserved.paths;
                  }
            | definition :: rest ->
                let* selected_new, state =
                  select_field state fresh_aggregate definition.Sst.field_id
                    definition.field_type
                in
                let* selected_old, state =
                  select_field state old_aggregate definition.field_id
                    definition.field_type
                in
                let rhs =
                  if definition.field_id = field then evaluated.value
                  else selected_old
                in
                let* equation =
                  match equality selected_new rhs with
                  | Some equation -> Ok equation
                  | None ->
                      error function_name expression.span
                        (Malformed_sst
                           "field-write value has the wrong field type")
                in
                copy state (equation :: equations) rest
          in
          copy state [] fields
        in
        let* advanced = evaluate_contexts advance evaluated_value.paths in
        Ok
          {
            obligations =
              append evaluated_value.obligations advanced.obligations;
            paths = advanced.paths;
          }
  | Sst.Shared_scalar_field_write { provenance; field = _; value; transition }
    ->
      if context.logical then
        error function_name expression.span
          (Malformed_sst "shared-scalar mutation is not logical")
      else
        let* session =
          match context.verification_session with
          | Some session when Verification_session.is_active session ->
              Ok session
          | Some _ | None ->
              error function_name expression.span
                (Malformed_sst
                   "shared-scalar mutation requires an active private session")
        in
        let hooks : Shared_scalar_heap_private.hooks =
          {
            issued =
              (fun () -> Verification_session.note_shared_heap_issuance session);
            write_authenticated =
              (fun () -> Verification_session.note_shared_heap_write session);
            read_logged =
              (fun () -> Verification_session.note_shared_heap_read_log session);
            epoch_advanced =
              (fun () ->
                Verification_session.note_shared_heap_epoch_advance session);
            torn_down =
              (fun () -> Verification_session.note_shared_heap_teardown session);
          }
        in
        let* state =
          match state.shared_scalar_heap with
          | Some _ -> Ok state
          | None -> (
              match
                Shared_scalar_heap_private.create ~hooks
                  ~session_token:
                    (Verification_session.shared_heap_session_token session)
                  ~program_snapshot:
                    (Verification_session.program_snapshot_digest session)
                  ~session_active:(fun () ->
                    Verification_session.is_active session)
                  transition
              with
              | Ok heap ->
                  Verification_session.register_shared_heap_teardown session
                    (fun () -> Shared_scalar_heap_private.destroy_if_live heap);
                  context.shared_heap_owner :=
                    heap :: !(context.shared_heap_owner);
                  Ok { state with shared_scalar_heap = Some heap }
              | Error message ->
                  error function_name expression.span (Malformed_sst message))
        in
        let* evaluated_value = evaluate context value state in
        let advance evaluated =
          let* location =
            match
              List.assoc_opt provenance.root.id evaluated.state.environment
            with
            | Some (Aggregate_value aggregate) -> Ok aggregate
            | Some _ | None ->
                error function_name expression.span
                  (Malformed_sst
                     "shared-scalar write target is not a live aggregate alias")
          in
          let* written =
            match evaluated.value with
            | Integer_value term -> Ok term
            | Unit_value | Boolean_value _ | Tuple_value _ | Aggregate_value _
            | Parametric_value _ | Function_value _ ->
                error function_name expression.span
                  (Malformed_sst
                     "shared-scalar write value changed from integer type")
          in
          let* heap =
            match evaluated.state.shared_scalar_heap with
            | Some heap -> Ok heap
            | None ->
                error function_name expression.span
                  (Malformed_sst
                     "shared-scalar heap disappeared before its write")
          in
          match
            Shared_scalar_heap_private.write_rebased heap
              ~base_epoch:context.shared_effect_base_epoch ~transition ~location
              ~value:written
          with
          | Error message ->
              error function_name expression.span (Malformed_sst message)
          | Ok heap ->
              let* () =
                if
                  Option.is_some
                    (frozen_spine_for_transition context.validated
                       context.current_definition.function_id transition)
                then (
                  if Option.is_none context.shared_old_heap_view then
                    Verification_session.note_invariant_cell_open session;
                  Verification_session.note_invariant_cell_update session;
                  Ok ())
                else
                  match
                    ( context.invariant_cell_authority,
                      Type_invariant.find_for_operation context.invariants
                        context.current_definition.function_id )
                  with
                  | Some authority, Some (_, Sst.Shared_invariant_transition)
                    -> (
                      match
                        Shared_invariant_cell_private.note_update authority
                          ~operation:
                            {
                              Sst.function_index =
                                transition.shared_function_index;
                              function_name = transition.shared_function_name;
                            }
                          ~transition
                          ~effective_predecessor_epoch:
                            (transition.shared_predecessor_epoch
                           + context.shared_effect_base_epoch)
                          ~effective_successor_epoch:
                            (transition.shared_successor_epoch
                           + context.shared_effect_base_epoch)
                      with
                      | Ok () -> Ok ()
                      | Error message ->
                          error function_name expression.span
                            (Malformed_sst message))
                  | (None | Some _), (None | Some _) -> Ok ()
              in
              Ok
                {
                  obligations = [];
                  paths =
                    [
                      {
                        value = Unit_value;
                        state =
                          {
                            evaluated.state with
                            shared_scalar_heap = Some heap;
                          };
                      };
                    ];
                }
        in
        let* advanced = evaluate_contexts advance evaluated_value.paths in
        Ok
          {
            obligations =
              append evaluated_value.obligations advanced.obligations;
            paths = advanced.paths;
          }
  | Sst.Owned_tree_nested_write { transition; value } ->
      if context.logical then
        error function_name expression.span
          (Malformed_sst "owned-tree mutation is not logical")
      else
        let root = transition.root in
        let cursor =
          match transition.cursor with
          | Some cursor -> cursor
          | None -> assert false
        in
        let* evaluated_value = evaluate context value state in
        let advance evaluated =
          let* root_aggregate =
            match List.assoc_opt root.id evaluated.state.environment with
            | Some (Aggregate_value aggregate) -> Ok aggregate
            | Some _ | None ->
                error function_name expression.span
                  (Malformed_sst "owned-tree root is not a live aggregate")
          in
          let* cursor_value, breadcrumbs, state =
            descend_owned_path function_name context expression.span
              evaluated.state root_aggregate cursor.guarded_path
          in
          let* cursor_aggregate =
            match cursor_value with
            | Aggregate_value aggregate -> Ok aggregate
            | _ ->
                error function_name expression.span
                  (Malformed_sst "owned-tree mutation cursor is not aggregate")
          in
          let* changed, state =
            rebuild_owned_fields function_name context expression.span root
              state cursor_aggregate transition.target_field.field_owner
              transition.target_field evaluated.value
          in
          let* successor, state =
            rebuild_owned_ancestors function_name context expression.span root
              state changed breadcrumbs
          in
          let* successor_aggregate =
            match successor with
            | Aggregate_value aggregate -> Ok aggregate
            | _ ->
                error function_name expression.span
                  (Malformed_sst "owned-tree successor root is not aggregate")
          in
          let state =
            match
              ( context.verification_session,
                find_owned_contents_origin evaluated.state root_aggregate )
            with
            | Some session, Some predecessor -> (
                match
                  Verification_session.issue_successor_owned_contents_origin
                    session ~validated:context.validated
                    ~caller:context.current_definition ~expression ~predecessor
                    ~predecessor_root:root_aggregate
                    ~successor_root:successor_aggregate
                    ~path_condition:evaluated.state.path_condition
                with
                | Ok origin ->
                    replace_owned_contents_origin state successor_aggregate
                      origin
                | Error _ -> state)
            | (Some _ | None), (Some _ | None) -> state
          in
          let changed_path =
            cursor.guarded_path
            @ [ Sst.Owned_tree_field transition.target_field ]
          in
          let () =
            if
              nested_owned_reconstruction_evidence context state
                ~root:root_aggregate ~changed_path
                ~target_field:transition.target_field ~written_expression:value
            then
              Option.iter
                Verification_session.note_owned_root_scalar_reconstruction
                context.verification_session
          in
          let successor = Aggregate_value successor_aggregate in
          let* preserved =
            match invariant_for_typ context.invariants transition.root.typ with
            | None ->
                Ok
                  { obligations = []; paths = [ { value = successor; state } ] }
            | Some handle ->
                prove_invariant_validity context handle
                  (Vir.Transition_preservation
                     {
                       transition_kind = Vir.Nested_transition;
                       root_binding_id = transition.root.id;
                       pre_version = transition.pre_version;
                       successor_version = transition.successor_version;
                     })
                  context.function_ref expression.span successor state
          in
          Ok
            {
              obligations = preserved.obligations;
              paths =
                List.map
                  (fun preserved ->
                    {
                      value = Unit_value;
                      state =
                        replace_owned_root preserved.state root preserved.value
                          transition.successor_version;
                    })
                  preserved.paths;
            }
        in
        let* advanced = evaluate_contexts advance evaluated_value.paths in
        Ok
          {
            obligations =
              append evaluated_value.obligations advanced.obligations;
            paths = advanced.paths;
          }
  | Sst.Owned_tree_rebase { transition } ->
      if context.logical then
        error function_name expression.span
          (Malformed_sst "owned-tree move is not logical")
      else
        let root = transition.root in
        let descendant =
          match transition.rhs_provenance with
          | Sst.Guarded_descendant_move cursor -> cursor
          | Sst.Ground_owned_tree_value -> assert false
        in
        let* root_aggregate =
          match List.assoc_opt root.id state.environment with
          | Some (Aggregate_value aggregate) -> Ok aggregate
          | Some _ | None ->
              error function_name expression.span
                (Malformed_sst "owned-tree move root is not a live aggregate")
        in
        let* descendant_value, _, state =
          descend_owned_path function_name context expression.span state
            root_aggregate descendant.guarded_path
        in
        let* successor, state =
          rebuild_owned_fields function_name context expression.span root state
            root_aggregate transition.target_field.field_owner
            transition.target_field descendant_value
        in
        let* successor =
          match successor with
          | Aggregate_value aggregate -> Ok (Aggregate_value aggregate)
          | _ ->
              error function_name expression.span
                (Malformed_sst "owned-tree move successor is not aggregate")
        in
        let state =
          match successor with
          | Aggregate_value successor_root -> (
              match
                ( context.verification_session,
                  find_owned_contents_origin state root_aggregate )
              with
              | Some session, Some predecessor -> (
                  match
                    Verification_session.issue_successor_owned_contents_origin
                      session ~validated:context.validated
                      ~caller:context.current_definition ~expression
                      ~predecessor ~predecessor_root:root_aggregate
                      ~successor_root ~path_condition:state.path_condition
                  with
                  | Ok origin ->
                      replace_owned_contents_origin state successor_root origin
                  | Error _ -> state)
              | (Some _ | None), (Some _ | None) -> state)
          | Unit_value | Integer_value _ | Boolean_value _ | Tuple_value _ ->
              state
          | Parametric_value _ | Function_value _ -> state
        in
        let* preserved =
          match invariant_for_typ context.invariants transition.root.typ with
          | None ->
              Ok { obligations = []; paths = [ { value = successor; state } ] }
          | Some handle ->
              prove_invariant_validity context handle
                (Vir.Transition_preservation
                   {
                     transition_kind = Vir.Rebase_transition;
                     root_binding_id = transition.root.id;
                     pre_version = transition.pre_version;
                     successor_version = transition.successor_version;
                   })
                context.function_ref expression.span successor state
        in
        Ok
          {
            obligations = preserved.obligations;
            paths =
              List.map
                (fun preserved ->
                  {
                    value = Unit_value;
                    state =
                      replace_owned_root preserved.state root preserved.value
                        transition.successor_version;
                  })
                preserved.paths;
          }
  | Sst.Let_mutable (binding, initial, body) ->
      if binding.uniqueness <> Sst.Definitely_unique then
        error function_name expression.span
          (Malformed_sst "mutable local lacks definite unique provenance")
      else
        let* initial = evaluate context initial state in
        let* bodies =
          evaluate_contexts
            (fun evaluated ->
              let* state =
                match bind_scalar evaluated.state binding evaluated.value with
                | Ok state -> Ok state
                | Error error -> Error { error with function_name }
              in
              evaluate context body
                ( forget_consumed_fact_for_value state evaluated.value
                |> fun state ->
                  forget_finite_receipt_for_value state evaluated.value ))
            initial.paths
        in
        Ok
          {
            obligations = append initial.obligations bodies.obligations;
            paths = bodies.paths;
          }
  | Sst.Mutable_read binding -> (
      match List.assoc_opt binding.id state.environment with
      | Some value -> Ok { obligations = []; paths = [ { value; state } ] }
      | None ->
          error function_name expression.span
            (Malformed_sst ("unbound mutable local " ^ binding.name)))
  | Sst.Mutable_write { provenance; value } ->
      if context.logical then
        error function_name expression.span
          (Malformed_sst "mutation is not allowed in a logical expression")
      else if
        provenance.binding_pattern_uniqueness <> Sst.Definitely_unique
        || (not provenance.field_is_local)
        || not provenance.field_is_mutable
      then
        error function_name expression.span
          (Malformed_sst "mutable local lacks definite provenance")
      else
        let root = provenance.root in
        let* evaluated = evaluate context value state in
        let assign evaluated =
          let state =
            forget_consumed_fact_for_value evaluated.state evaluated.value
            |> fun state ->
            forget_finite_receipt_for_value state evaluated.value
          in
          let* state =
            match bind_scalar state root evaluated.value with
            | Ok state -> Ok state
            | Error error -> Error { error with function_name }
          in
          Ok { obligations = []; paths = [ { value = Unit_value; state } ] }
        in
        let* assigned = evaluate_contexts assign evaluated.paths in
        Ok
          {
            obligations = append evaluated.obligations assigned.obligations;
            paths = assigned.paths;
          }
  | Sst.Let (bindings, body) ->
      let rec evaluate_bindings obligations contexts = function
        | [] ->
            let* body_results =
              evaluate_contexts
                (fun (state, values) ->
                  let* state =
                    List.fold_left2
                      (fun state ((pattern : Sst.pattern), value_expression)
                           value ->
                        let* state = state in
                        let* state =
                          match value with
                          | Aggregate_value aggregate ->
                              let* mode =
                                expression_instance_mode context
                                  value_expression
                              in
                              Immutable_fact_integration.derive_finite_pattern
                                context pattern.span state ~parent:aggregate
                                ~mode pattern value
                          | Unit_value | Integer_value _ | Boolean_value _
                          | Tuple_value _ | Parametric_value _ | Function_value _ ->
                              Ok state
                        in
                        if context.logical then
                          let* environment, ranges =
                            bind_pattern_direct
                              ~parametric_adts:state.parametric_adts
                              function_name state.environment pattern value
                          in
                          Ok
                            (with_assumptions { state with environment } ranges)
                        else bind_pattern function_name state pattern value)
                      (Ok state) bindings (List.rev values)
                  in
                  evaluate context body state)
                contexts
            in
            Ok
              {
                obligations = append obligations body_results.obligations;
                paths = body_results.paths;
              }
        | (_, value_expression) :: rest ->
            let* evaluated =
              evaluate_contexts
                (fun (state, values) ->
                  let* result = evaluate context value_expression state in
                  Ok
                    {
                      obligations = result.obligations;
                      paths =
                        List.map
                          (fun evaluated ->
                            (evaluated.state, evaluated.value :: values))
                          result.paths;
                    })
                contexts
            in
            evaluate_bindings
              (append obligations evaluated.obligations)
              evaluated.paths rest
      in
      evaluate_bindings [] [ (state, []) ] bindings
  | Sst.Sequence (first, second) ->
      let* first = evaluate context first state in
      let* second =
        evaluate_contexts
          (fun evaluated -> evaluate context second evaluated.state)
          first.paths
      in
      Ok
        {
          obligations = append first.obligations second.obligations;
          paths = second.paths;
        }
  | Sst.If (condition, consequent, alternative) ->
      let* condition = evaluate context condition state in
      let branch evaluated =
        let* condition_term =
          expect_boolean function_name expression.span evaluated.value
        in
        match condition_term with
        | Vir.Boolean_constant true ->
            evaluate context consequent evaluated.state
        | Vir.Boolean_constant false -> (
            match alternative with
            | Some alternative -> evaluate context alternative evaluated.state
            | None ->
                Ok
                  {
                    obligations = [];
                    paths = [ { value = Unit_value; state = evaluated.state } ];
                  })
        | condition_term ->
            let* consequent =
              evaluate context consequent
                (with_path evaluated.state condition_term)
            in
            let* alternative =
              match alternative with
              | Some alternative ->
                  evaluate context alternative
                    (with_path evaluated.state (Vir.Boolean_not condition_term))
              | None ->
                  Ok
                    {
                      obligations = [];
                      paths =
                        [
                          {
                            value = Unit_value;
                            state =
                              with_path evaluated.state
                                (Vir.Boolean_not condition_term);
                          };
                        ];
                    }
            in
            let paths =
              append consequent.paths alternative.paths
              |> intersect_consumed_receipt_facts |> intersect_finite_receipts
              |> intersect_proof_call_authority
              |> intersect_local_assertion_facts
                   ~current_mode:context.current_mode
            in
            Ok
              {
                obligations =
                  append consequent.obligations alternative.obligations;
                paths;
              }
      in
      let* branches = evaluate_contexts branch condition.paths in
      Ok
        {
          obligations = append condition.obligations branches.obligations;
          paths = branches.paths;
        }
  | Sst.Match (scrutinee, cases) ->
      let scrutinee_expression = scrutinee in
      let owned_runtime_match =
        context.owned_root_scalar_plan = None
        && List.exists
             (fun case -> pattern_has_owned_cursor case.Sst.case_pattern)
             cases
      in
      let* scrutinee = evaluate context scrutinee state in
      let choose condition state =
        match condition with
        | Vir.Boolean_constant true -> (Some state, None)
        | Vir.Boolean_constant false -> (None, Some state)
        | condition ->
            ( Some (with_path state condition),
              Some (with_path state (Vir.Boolean_not condition)) )
      in
      let rec execute_cases obligations completed remaining = function
        | [] ->
            (* The authenticated frontend admits only total matches.  A
               syntactically remaining symbolic path after the final case is
               therefore infeasible (for example [not (b = true)] and
               [not (b = false)]) and has no executable exit. *)
            Ok { obligations; paths = completed }
        | case :: rest ->
            let execute_one (state, scrutinee_value) =
              let outer_environment = state.environment in
              let* condition, bindings, ranges, state =
                match
                  ( owned_match context expression,
                    case.Sst.case_pattern.pattern_desc )
                with
                | Some matched, Sst.Constructor_pattern (constructor, arguments)
                  when List.mem constructor
                         (Owned_scalar.owned_scalar_match_constructors matched)
                  ->
                    let path =
                      {
                        Owned_scalar.owned_path_steps =
                          Owned_scalar.owned_scalar_match_path matched;
                        owned_path_terminal =
                          Owned_scalar.Owned_terminal_tag constructor;
                      }
                    in
                    let observed, state =
                      observe_owned_scalar context state path
                    in
                    let* condition =
                      expect_boolean function_name case.case_span observed
                    in
                    let root =
                      Option.get context.owned_root_scalar_plan
                      |> Verification_session.owned_root_scalar_plan_root
                    in
                    let bindings =
                      match arguments with
                      | [ { Sst.pattern_desc = Sst.Bind binding; _ } ] ->
                          [ (binding.id, Aggregate_value root) ]
                      | [ { Sst.pattern_desc = Sst.Wildcard; _ } ] -> []
                      | [] -> []
                      | _ -> assert false
                    in
                    Ok (condition, bindings, [], state)
                | Some _, _ -> assert false
                | None, Sst.Constructor_pattern (expected_constructor, _)
                  when Option.is_some context.owned_contents_stack -> (
                    match scrutinee_value with
                    | Aggregate_value aggregate -> (
                        let scope = Option.get context.owned_contents_stack in
                        let grammar =
                          Verification_session.owned_contents_candidate_grammar
                            scope.owned_contents_candidate
                        in
                        match
                          owned_contents_constructor state.assumptions grammar
                            aggregate
                        with
                        | Some constructor
                          when constructor = expected_constructor ->
                            let* _, bindings, ranges =
                              pattern_condition_and_bindings function_name
                                context.type_definitions state.parametric_adts
                                case.Sst.case_pattern scrutinee_value
                            in
                            Ok
                              ( Vir.Boolean_constant true,
                                bindings,
                                ranges,
                                state )
                        | Some _ ->
                            Ok (Vir.Boolean_constant false, [], [], state)
                        | None ->
                            let* condition, bindings, ranges =
                              pattern_condition_and_bindings function_name
                                context.type_definitions state.parametric_adts
                                case.Sst.case_pattern scrutinee_value
                            in
                            Ok (condition, bindings, ranges, state))
                    | Unit_value | Integer_value _ | Boolean_value _
                    | Tuple_value _ | Parametric_value _ | Function_value _ ->
                        error function_name case.case_span
                          (Malformed_sst
                             "owned-contents match scrutinee is not aggregate"))
                | None, _ ->
                    let* condition, bindings, ranges =
                      pattern_condition_and_bindings function_name
                        context.type_definitions state.parametric_adts
                        case.Sst.case_pattern scrutinee_value
                    in
                    let condition =
                      match
                        ( owned_runtime_match,
                          case.Sst.case_pattern.pattern_desc,
                          scrutinee_value )
                      with
                      | ( true,
                          Sst.Constructor_pattern (constructor, _),
                          (Aggregate_value aggregate as value) ) -> (
                          let contents_origin =
                            match owned_selector_root_and_path aggregate with
                            | Some (root, _) ->
                                find_owned_contents_origin state root
                            | None -> find_owned_contents_origin state aggregate
                          in
                          match contents_origin with
                          | Some _ ->
                              Option.value ~default:condition
                                (Option.map
                                   (fun reached -> Vir.Boolean_constant reached)
                                   (exact_owned_pattern_match state.assumptions
                                      case.Sst.case_pattern value))
                          | None ->
                              Option.value ~default:condition
                                (owned_runtime_tag aggregate constructor))
                      | _ -> condition
                    in
                    Ok (condition, bindings, ranges, state)
              in
              let state = with_assumptions state ranges in
              let matched, unmatched = choose condition state in
              let unmatched = Option.to_list unmatched in
              match matched with
              | None ->
                  Ok
                    {
                      obligations = [];
                      paths =
                        [
                          ( [],
                            List.map
                              (fun state -> (state, scrutinee_value))
                              unmatched );
                        ];
                    }
              | Some matched -> (
                  let* matched =
                    if not (immutable_pattern_reconstruction_enabled context)
                    then Ok matched
                    else
                      match scrutinee_value with
                      | Aggregate_value aggregate -> (
                          match
                            Immutable_aggregate_reconstruction_private
                            .pattern_reconstruction_equalities
                              (Sst_validation.program context.validated)
                                .parametric_adts context.type_definitions
                              case.case_pattern aggregate
                          with
                          | Ok equalities ->
                              Ok
                                (with_immutable_aggregate_facts matched
                                   equalities)
                          | Error message ->
                              error function_name case.case_span
                                (Malformed_sst message))
                      | Unit_value | Integer_value _ | Boolean_value _
                      | Tuple_value _ | Parametric_value _ | Function_value _ ->
                          Ok matched
                  in
                  let matched =
                    retain_ground_constructor_match_candidate context
                      scrutinee_expression case matched scrutinee_value
                  in
                  let* matched =
                    match scrutinee_value with
                    | Aggregate_value aggregate ->
                        let* mode =
                          expression_instance_mode context scrutinee_expression
                        in
                        Immutable_fact_integration.derive_finite_pattern context
                          case.case_span matched ~parent:aggregate ~mode
                          case.case_pattern scrutinee_value
                    | Tuple_value _ ->
                        Immutable_fact_integration
                        .derive_finite_ephemeral_pattern context case.case_span
                          matched scrutinee_expression case.case_pattern
                          scrutinee_value
                    | Unit_value | Integer_value _ | Boolean_value _
                    | Parametric_value _ | Function_value _ ->
                        Ok matched
                  in
                  let matched = add_pattern_bindings matched bindings in
                  let finish_body body_state =
                    let* body = evaluate context case.case_body body_state in
                    Ok
                      {
                        obligations = body.obligations;
                        paths =
                          [
                            ( List.map
                                (fun evaluated ->
                                  {
                                    evaluated with
                                    state =
                                      restore_pattern_environment
                                        outer_environment evaluated.state;
                                  })
                                body.paths,
                              List.map
                                (fun state -> (state, scrutinee_value))
                                unmatched );
                          ];
                      }
                  in
                  match case.case_guard with
                  | None -> finish_body matched
                  | Some guard ->
                      let* guard = evaluate context guard matched in
                      let* guarded =
                        evaluate_contexts
                          (fun evaluated ->
                            let* guard_term =
                              expect_boolean function_name case.case_span
                                evaluated.value
                            in
                            let selected, rejected =
                              choose guard_term evaluated.state
                            in
                            let* selected =
                              match selected with
                              | None -> Ok { obligations = []; paths = [] }
                              | Some selected ->
                                  let* body =
                                    evaluate context case.case_body selected
                                  in
                                  Ok
                                    {
                                      body with
                                      paths =
                                        List.map
                                          (fun evaluated ->
                                            {
                                              evaluated with
                                              state =
                                                restore_pattern_environment
                                                  outer_environment
                                                  evaluated.state;
                                            })
                                          body.paths;
                                    }
                            in
                            let rejected =
                              Option.to_list rejected
                              |> List.map (fun state ->
                                  ( restore_environment outer_environment state,
                                    scrutinee_value ))
                            in
                            Ok
                              {
                                obligations = selected.obligations;
                                paths = [ (selected.paths, rejected) ];
                              })
                          guard.paths
                      in
                      let bodies, rejected =
                        List.fold_left
                          (fun (bodies, rejected) (more_bodies, more_rejected)
                             ->
                            ( append bodies more_bodies,
                              append rejected more_rejected ))
                          ([], []) guarded.paths
                      in
                      Ok
                        {
                          obligations =
                            append guard.obligations guarded.obligations;
                          paths =
                            [
                              ( bodies,
                                append
                                  (List.map
                                     (fun state -> (state, scrutinee_value))
                                     unmatched)
                                  rejected );
                            ];
                        })
            in
            let* evaluated = evaluate_contexts execute_one remaining in
            let bodies, next_remaining =
              List.fold_left
                (fun (bodies, remaining) (more_bodies, more_remaining) ->
                  (append bodies more_bodies, append remaining more_remaining))
                ([], []) evaluated.paths
            in
            execute_cases
              (append obligations evaluated.obligations)
              (append completed bodies) next_remaining rest
      in
      let* matched =
        evaluate_contexts
          (fun evaluated ->
            let* matched =
              execute_cases [] [] [ (evaluated.state, evaluated.value) ] cases
            in
            Ok
              {
                matched with
                paths =
                  matched.paths |> intersect_consumed_receipt_facts
                  |> intersect_finite_receipts |> intersect_proof_call_authority
                  |> intersect_local_assertion_facts
                       ~current_mode:context.current_mode;
              })
          scrutinee.paths
      in
      Ok
        {
          obligations = append scrutinee.obligations matched.obligations;
          paths = matched.paths;
        }
  | Sst.Checked_arithmetic (operation, arguments) ->
      let rec evaluate_arguments obligations contexts = function
        | [] ->
            let results =
              List.map
                (fun (state, values) ->
                  let* mathematical_result =
                    arithmetic_term function_name expression.span operation
                      (List.rev values)
                  in
                  if context.logical then
                    Ok
                      {
                        obligations = [];
                        paths =
                          [
                            { value = Integer_value mathematical_result; state };
                          ];
                      }
                  else
                    Ok
                      (emit_checked function_ref expression.span operation
                         mathematical_result state))
                contexts
            in
            let rec collect obligations paths = function
              | [] -> Ok { obligations; paths }
              | result :: rest ->
                  let* result = result in
                  collect
                    (append obligations result.obligations)
                    (append paths result.paths)
                    rest
            in
            collect obligations [] results
        | argument :: rest ->
            let* evaluated =
              evaluate_contexts
                (fun (state, values) ->
                  let* result = evaluate context argument state in
                  Ok
                    {
                      obligations = result.obligations;
                      paths =
                        List.map
                          (fun evaluated ->
                            (evaluated.state, evaluated.value :: values))
                          result.paths;
                    })
                contexts
            in
            evaluate_arguments
              (append obligations evaluated.obligations)
              evaluated.paths rest
      in
      evaluate_arguments [] [ (state, []) ] arguments
  | Sst.Compare (comparison, left_expression, right) ->
      let* left = evaluate context left_expression state in
      let* evaluated =
        evaluate_contexts
          (fun left ->
            let* right = evaluate context right left.state in
            let rec make_paths paths = function
              | [] -> Ok (List.rev paths)
              | right :: rest ->
                  let* term =
                    match comparison with
                    | Sst.Equal | Sst.Not_equal -> (
                        match
                          comparison_equality_for right.state
                            left_expression.Sst.typ left.value right.value
                        with
                        | Some equality ->
                            Ok
                              (if comparison = Sst.Equal then equality
                               else Vir.Boolean_not equality)
                        | None ->
                            error function_name expression.span
                              (Malformed_sst "comparison operand type mismatch")
                        )
                    | Sst.Less_than | Sst.Less_or_equal | Sst.Greater_than
                    | Sst.Greater_or_equal -> (
                        match (left.value, right.value) with
                        | Integer_value left, Integer_value right ->
                            Ok
                              (Vir.Integer_compare
                                 (vir_comparison comparison, left, right))
                        | Boolean_value _, Boolean_value _ ->
                            error function_name expression.span
                              (Malformed_sst
                                 "ordered comparison applied to booleans")
                        | _ ->
                            error function_name expression.span
                              (Malformed_sst "comparison operand type mismatch")
                        )
                  in
                  let term, state =
                    consume_frozen_terminal_comparison right.state comparison
                      left_expression.Sst.typ left.value right.value term
                  in
                  make_paths
                    ({ value = Boolean_value term; state } :: paths)
                    rest
            in
            let* paths = make_paths [] right.paths in
            Ok { obligations = right.obligations; paths })
          left.paths
      in
      Ok
        {
          obligations = append left.obligations evaluated.obligations;
          paths = evaluated.paths;
        }
  | Sst.Boolean_not operand ->
      let* operand = evaluate context operand state in
      let* paths =
        let rec loop paths = function
          | [] -> Ok (List.rev paths)
          | evaluated :: rest ->
              let* term =
                expect_boolean function_name expression.span evaluated.value
              in
              loop
                ({
                   value = Boolean_value (Vir.Boolean_not term);
                   state = evaluated.state;
                 }
                :: paths)
                rest
        in
        loop [] operand.paths
      in
      Ok { obligations = operand.obligations; paths }
  | Sst.Boolean_binary (operation, left, right) ->
      let* left = evaluate context left state in
      let* right =
        evaluate_contexts
          (fun left ->
            let* left_term =
              expect_boolean function_name expression.span left.value
            in
            let short_circuit value =
              Ok
                {
                  obligations = [];
                  paths =
                    [
                      {
                        value = Boolean_value (Vir.Boolean_constant value);
                        state = left.state;
                      };
                    ];
                }
            in
            let evaluate_right state =
              let* right = evaluate context right state in
              let* paths =
                let rec loop paths = function
                  | [] -> Ok (List.rev paths)
                  | evaluated :: rest ->
                      let* right_term =
                        expect_boolean function_name expression.span
                          evaluated.value
                      in
                      loop
                        ({
                           value = Boolean_value right_term;
                           state = evaluated.state;
                         }
                        :: paths)
                        rest
                in
                loop [] right.paths
              in
              Ok { obligations = right.obligations; paths }
            in
            match (operation, left_term) with
            | Sst.And, Vir.Boolean_constant false -> short_circuit false
            | Sst.And, Vir.Boolean_constant true -> evaluate_right left.state
            | Sst.Or, Vir.Boolean_constant true -> short_circuit true
            | Sst.Or, Vir.Boolean_constant false -> evaluate_right left.state
            | Sst.And, left_term ->
                let short =
                  {
                    value = Boolean_value (Vir.Boolean_constant false);
                    state = with_path left.state (Vir.Boolean_not left_term);
                  }
                in
                let* evaluated =
                  evaluate_right (with_path left.state left_term)
                in
                Ok { evaluated with paths = short :: evaluated.paths }
            | Sst.Or, left_term ->
                let short =
                  {
                    value = Boolean_value (Vir.Boolean_constant true);
                    state = with_path left.state left_term;
                  }
                in
                let* evaluated =
                  evaluate_right
                    (with_path left.state (Vir.Boolean_not left_term))
                in
                Ok { evaluated with paths = short :: evaluated.paths })
          left.paths
      in
      Ok
        {
          obligations = append left.obligations right.obligations;
          paths = right.paths;
        }
  | Sst.Direct_call _
    when
      Spec_function_sst_private.lambda expression <> None
      || Spec_function_sst_private.application expression <> None
      || Spec_function_sst_private.is_reference expression ->
      if not context.logical then
        error function_name expression.span
          (Malformed_sst
             "specification-function value reached runtime evaluation")
      else
        let* permit =
          match
            Logical_spec_evaluation_private.authenticate_expression
              ~classify:(logical_spec_call_target context)
              expression
          with
          | Some permit -> Ok permit
          | None ->
              error function_name expression.span
                (Malformed_sst
                   "specification-function expression lacks logical authority")
        in
        let evaluate_recursive context expression state =
          let* evaluated = evaluate context expression state in
          match (evaluated.obligations, evaluated.paths) with
          | [], [ evaluated ] -> Ok (evaluated.value, evaluated.state)
          | [], [] | [], _ :: _ :: _ | _ :: _, _ ->
              error function_name expression.span
                (Malformed_sst
                   "recursive function application did not evaluate once")
        in
        let callbacks =
          logical_evaluation_callbacks context
            ~aggregate_type:(vir_aggregate_type_of_sst state.parametric_adts)
            ~option_instance:
              (Parametric_adt.option_instance state.parametric_adts)
            ~evaluate_recursive
            ~error:(fun span message ->
              { function_name; span; unsupported = Malformed_sst message })
        in
        let* value, state =
          Logical_spec_evaluation_private.evaluate permit callbacks context
            expression state
        in
        Ok { obligations = []; paths = [ { value; state } ] }
  | Sst.Direct_call { call_form = Sst.Proof_call; callee; _ }
    when suppressed_proof_edge suppress_proof_call_for_testing
           context.current_callable callee ->
      if expression.typ = Sst.Unit then
        Ok { obligations = []; paths = [ { value = Unit_value; state } ] }
      else
        error function_name expression.span
          (Malformed_sst "test-only suppressed Proof call has a non-unit result")
  | Sst.Direct_call { call_form = Sst.Proof_call; recursive = true; _ }
    when !suppress_recursive_proof_calls ->
      Ok { obligations = []; paths = [ { value = Unit_value; state } ] }
  | Sst.Direct_call
      {
        call_form = Sst.Specification_call;
        callee;
        arguments =
          [ Sst.Value_argument { label = None; value = actual_expression } ];
        recursive;
        type_arguments = [];
      }
    when Option.is_some
           (Sst_validation_private.Owned_recursive_contents_private
            .helper_for_program
              (Sst_validation.program context.validated)
              (match
                 List.assoc_opt callee.function_index context.definitions
               with
              | Some descriptor -> Sst_validation.callable_definition descriptor
              | None ->
                  (Sst_validation.program context.validated).Sst.functions
                  |> List.find (fun definition ->
                      same_function_id definition.Sst.function_id callee))) ->
      if not context.logical then
        error function_name expression.span
          (Malformed_sst "owned-contents helper reached runtime evaluation")
      else
        let program = Sst_validation.program context.validated in
        let* helper =
          match
            program.Sst.functions
            |> List.find_opt (fun definition ->
                same_function_id definition.Sst.function_id callee)
          with
          | Some helper -> Ok helper
          | None -> error function_name expression.span (Missing_summary callee)
        in
        let* grammar =
          match
            Sst_validation_private.Owned_recursive_contents_private
            .helper_for_program program helper
          with
          | Some grammar -> Ok grammar
          | None ->
              error function_name expression.span
                (Malformed_sst
                   "owned-contents helper lacks its exact authenticated grammar")
        in
        let* actual = evaluate context actual_expression state in
        let expand evaluated =
          match evaluated.value with
          | Aggregate_value raw_aggregate ->
              let aggregate =
                if
                  same_function_id context.current_callable
                    (Sst_validation_private.Owned_recursive_contents_private
                     .model grammar)
                      .Sst.function_id
                then
                  match
                    List.assoc_opt
                      (Sst_validation_private.Owned_recursive_contents_private
                       .root_formal grammar)
                        .id evaluated.state.environment
                  with
                  | Some (Aggregate_value root) ->
                      Option.value ~default:raw_aggregate
                        (owned_aggregate_at_path context
                           evaluated.state.assumptions root
                           (Sst_validation_private
                            .Owned_recursive_contents_private
                            .root_path grammar
                           |> List.map (fun field -> Sst.Owned_tree_field field)
                           ))
                  | Some
                      ( Unit_value | Integer_value _ | Boolean_value _
                      | Tuple_value _ | Parametric_value _ | Function_value _ )
                  | None ->
                      raw_aggregate
                else
                  normalize_owned_contents_aggregate evaluated.state.assumptions
                    128 raw_aggregate
              in
              let aggregate =
                normalize_owned_contents_aggregate evaluated.state.assumptions
                  128 aggregate
              in
              let* session, scope =
                match
                  (context.verification_session, context.owned_contents_stack)
                with
                | Some session, Some scope
                  when Verification_session.owned_contents_candidate_grammar
                         scope.owned_contents_candidate
                       == grammar ->
                    Ok (session, scope)
                | (Some _ | None), (Some _ | None) ->
                    error function_name expression.span
                      (Malformed_sst
                         "owned-contents helper lacks its exact session \
                          candidate")
              in
              let root_call = not recursive in
              let aggregate =
                if root_call then scope.owned_contents_node else aggregate
              in
              let* child_scope =
                if root_call then Ok scope
                else
                  let* parent_constructor =
                    match
                      owned_contents_constructor evaluated.state.assumptions
                        grammar scope.owned_contents_node
                    with
                    | Some constructor -> Ok constructor
                    | None ->
                        error function_name expression.span
                          (Malformed_sst
                             "owned-contents recursive call lost its exact \
                              parent constructor")
                  in
                  let matching_edges =
                    Sst_validation_private.Owned_recursive_contents_private
                    .cases grammar
                    |> List.filter (fun case ->
                        Sst_validation_private.Owned_recursive_contents_private
                        .case_carrier_constructor case
                        = parent_constructor)
                    |> List.concat_map
                         Sst_validation_private.Owned_recursive_contents_private
                         .case_sources
                    |> List.filter_map (function
                      | Sst_validation_private.Owned_recursive_contents_private
                        .Recursive_source
                          (field, source)
                        when source == expression ->
                          Some field
                      | Sst_validation_private.Owned_recursive_contents_private
                        .Scalar_source
                          _
                      | Sst_validation_private.Owned_recursive_contents_private
                        .Constant_source
                      | Sst_validation_private.Owned_recursive_contents_private
                        .Recursive_source
                          _ ->
                          None)
                    |> List.sort_uniq compare
                  in
                  let* field =
                    match matching_edges with
                    | [ field ] -> Ok field
                    | [] | _ :: _ :: _ ->
                        error function_name expression.span
                          (Malformed_sst
                             "owned-contents recursive call is not one exact \
                              authenticated grammar edge")
                  in
                  let* permit =
                    match
                      Verification_session.issue_owned_contents_permit session
                        scope.owned_contents_candidate
                        ~parent_path:scope.owned_contents_path ~field
                        ~parent:scope.owned_contents_node ~child:aggregate
                        ~call:expression ~call_span:expression.span
                        ~path_condition:evaluated.state.path_condition
                    with
                    | Ok permit -> Ok permit
                    | Error message ->
                        error function_name expression.span
                          (Malformed_sst message)
                  in
                  let* () =
                    match
                      Verification_session.consume_owned_contents_permit session
                        permit ~candidate:scope.owned_contents_candidate
                        ~parent_path:scope.owned_contents_path ~field
                        ~parent:scope.owned_contents_node ~child:aggregate
                        ~call:expression ~call_span:expression.span
                        ~path_condition:evaluated.state.path_condition
                    with
                    | Ok () -> Ok ()
                    | Error message ->
                        error function_name expression.span
                          (Malformed_sst message)
                  in
                  Ok
                    {
                      scope with
                      owned_contents_path =
                        scope.owned_contents_path @ [ field ];
                      owned_contents_node = aggregate;
                    }
              in
              let* () =
                match
                  Verification_session.note_owned_contents_recursive_route
                    session child_scope.owned_contents_candidate
                with
                | Ok () -> Ok ()
                | Error message ->
                    error function_name expression.span (Malformed_sst message)
              in
              (match
                 owned_contents_constructor evaluated.state.assumptions grammar
                   aggregate
               with
                | Some _ -> Ok ()
                | None ->
                    error function_name expression.span
                      (Malformed_sst
                         "owned-contents helper actual is not one exact closed \
                          topology node"))
              |> fun authenticated ->
              let* () = authenticated in
              let helper_body =
                match helper.Sst.body with
                | Sst.Recursive_spec_definition { body; _ } -> body.expression
                | _ -> assert false
              in
              let* environment, ranges =
                bind_pattern_direct function_name []
                  {
                    Sst.pattern_desc =
                      Sst.Bind
                        (Sst_validation_private.Owned_recursive_contents_private
                         .helper_formal grammar);
                    typ =
                      Sst.Aggregate
                        (Sst_validation_private.Owned_recursive_contents_private
                         .carrier_type grammar);
                    span = actual_expression.span;
                  }
                  (Aggregate_value aggregate)
              in
              let caller_environment = evaluated.state.environment in
              let helper_context =
                {
                  context with
                  current_definition = helper;
                  current_callable = helper.function_id;
                  entry_environment = environment;
                  logical = true;
                  old_environment = None;
                  owned_contents_stack = Some child_scope;
                }
              in
              let* expanded =
                evaluate helper_context helper_body
                  (with_assumptions { evaluated.state with environment } ranges)
              in
              let* () =
                match
                  Verification_session.note_owned_contents_ground_equation
                    session child_scope.owned_contents_candidate
                with
                | Ok () -> Ok ()
                | Error message ->
                    error function_name expression.span (Malformed_sst message)
              in
              let* () =
                if not root_call then Ok ()
                else
                  let* () =
                    match
                      Verification_session.finish_owned_contents_candidate
                        session child_scope.owned_contents_candidate
                    with
                    | Ok () -> Ok ()
                    | Error message ->
                        error function_name expression.span
                          (Malformed_sst message)
                  in
                  match
                    Verification_session.note_owned_contents_model_result
                      session child_scope.owned_contents_candidate
                  with
                  | Ok () -> Ok ()
                  | Error message ->
                      error function_name expression.span
                        (Malformed_sst message)
              in
              Ok
                {
                  expanded with
                  paths =
                    List.map
                      (fun evaluated ->
                        {
                          evaluated with
                          state =
                            restore_environment caller_environment
                              evaluated.state;
                        })
                      expanded.paths;
                }
          | Unit_value | Integer_value _ | Boolean_value _ | Tuple_value _
          | Parametric_value _ | Function_value _ ->
              error function_name expression.span
                (Malformed_sst
                   "owned-contents helper actual is not its exact carrier")
        in
        let* expanded = evaluate_contexts expand actual.paths in
        Ok
          {
            obligations = append actual.obligations expanded.obligations;
            paths = expanded.paths;
          }
  | Sst.Direct_call
      {
        call_form = Sst.Specification_call;
        callee;
        arguments;
        recursive = false;
        type_arguments;
      } ->
      let* arguments =
          plain_args function_name expression.span arguments
      in
      let imported_aggregate_call =
        Option.fold ~none:false
          ~some:(fun registration ->
            Option.fold ~none:false
              ~some:Imported_callable.call_is_aggregate_model
              (Imported_callable.find_call registration expression))
          context.imports
      in
      if (not context.logical) && not imported_aggregate_call then
        error function_name expression.span
          (Malformed_sst "specification call reached runtime evaluation")
      else
        let* definition =
          match List.assoc_opt callee.function_index context.definitions with
          | Some descriptor
            when String.equal
                   (Sst_validation.callable_id descriptor).function_name
                   callee.function_name ->
              Ok (call_contract_view context.imports expression descriptor)
          | _ -> error function_name expression.span (Missing_summary callee)
        in
        let* disposition =
          if imported_aggregate_call then Ok `Opaque_imported
          else match (definition.mode, definition.body) with
          | Sst.Spec, Sst.Spec_definition body -> (
              match
                Type_invariant.find_for_operation context.invariants
                  definition.function_id
              with
              | Some (handle, Sst.Abstract_invariant) ->
                  Ok (`Opaque_invariant handle)
              | Some
                  ( _,
                    ( Sst.Abstract_constructor | Sst.Abstract_model
                    | Sst.Current_model | Sst.Terminal_read
                    | Sst.Current_terminal_read | Sst.Terminal_snapshot
                    | Sst.Unique_transition | Sst.Shared_invariant_transition )
                  )
              | None ->
                  if List.mem callee.function_index context.spec_call_stack then
                    error function_name expression.span
                      (Malformed_sst
                         "cyclic specification expansion reached evaluation")
                  else if
                    List.length context.spec_call_stack
                    >= context.spec_expansion_limit
                  then
                    error function_name expression.span
                      (Malformed_sst
                         "specification expansion depth exceeded validated \
                          graph")
                  else
                    let permit =
                      match
                        logical_spec_call_target context definition.function_id
                      with
                      | Logical_spec_evaluation_private.Local_nonrecursive _ ->
                          Logical_spec_evaluation_private.authenticate
                            ~classify:(logical_spec_call_target context)
                            definition
                      | Logical_spec_evaluation_private.Opaque_recursive
                      | Logical_spec_evaluation_private.Captured_model _
                      | Logical_spec_evaluation_private.Unsupported ->
                          None
                    in
                    Ok (`Expand (body.expression, permit)))
          | Sst.Spec, Sst.Recursive_spec_definition _ -> Ok `Opaque_recursive
          | _ ->
              error function_name expression.span
                (Malformed_sst
                   "specification call target has no validated spec definition")
        in
        let* parameters, result_type, disposition =
          view function_name disposition definition type_arguments arguments expression
        in
        let rec evaluate_arguments obligations contexts = function
          | [] -> Ok { obligations; paths = contexts }
          | (_, argument) :: rest ->
              let* evaluated =
                evaluate_contexts
                  (fun (state, values) ->
                    let* argument = evaluate context argument state in
                    Ok
                      {
                        obligations = argument.obligations;
                        paths =
                          List.map
                            (fun evaluated ->
                              (evaluated.state, evaluated.value :: values))
                            argument.paths;
                      })
                  contexts
              in
              evaluate_arguments
                (append obligations evaluated.obligations)
                evaluated.paths rest
        in
        let* actuals = evaluate_arguments [] [ (state, []) ] arguments in
        let* actual_paths =
          authorize_finite_formal_call context ~callee_definition:definition
            ~arguments ~call_span:expression.span actuals.paths
        in
        let actuals = { actuals with paths = actual_paths } in
        let expand (caller_state, reversed_actuals) =
          let caller_environment = caller_state.environment in
          let actuals = List.rev reversed_actuals in
          if List.length actuals <> List.length parameters then
            error function_name expression.span
              (Malformed_sst "specification-call argument count mismatch")
          else
            let* _definition_environment, _ranges =
              List.fold_left2
                (fun environment parameter actual ->
                  let* environment, ranges = environment in
                  let* environment, nested_ranges =
                    bind_pattern_direct function_name environment
                      parameter.Sst.pattern actual
                  in
                  Ok (environment, ranges @ nested_ranges))
                (Ok ([], []))
                parameters actuals
            in
            let* () =
              authorize_frozen_formal_call context ~callee_definition:definition
                ~call_form:Sst.Specification_call ~arguments
                ~call_span:expression.span caller_state actuals
            in
            match disposition with
            | `Opaque_imported ->
                let* definition_environment, ranges =
                  List.fold_left2
                    (fun environment parameter actual ->
                      let* environment, ranges = environment in
                      let* environment, nested_ranges =
                        bind_pattern_direct function_name environment
                          parameter.Sst.pattern actual
                      in
                      Ok (environment, ranges @ nested_ranges))
                    (Ok ([], []))
                    parameters actuals
                in
                let summary_context =
                  {
                    context with
                    entry_environment = definition_environment;
                    logical = true;
                    old_environment = None;
                  }
                in
                let initial =
                  with_assumptions
                    { caller_state with environment = definition_environment }
                    ranges
                in
                let rec prove_requires obligations states = function
                  | [] -> Ok { obligations; paths = states }
                  | (clause : Sst.predicate_clause) :: rest ->
                      let* evaluated =
                        evaluate_contexts
                          (fun state ->
                            let* predicate =
                              evaluate summary_context
                                clause.predicate.expression state
                            in
                            let* paths =
                              let rec goals completed = function
                                | [] -> Ok (List.rev completed)
                                | evaluated :: remaining ->
                                    let* goal =
                                      expect_boolean function_name clause.span
                                        evaluated.value
                                    in
                                    let obligation, state =
                                      emit_goal context.function_ref
                                        (Vir.Call_precondition
                                           {
                                             callee =
                                               vir_function_ref
                                                 definition.function_id;
                                             precondition_ordinal =
                                               clause.clause_index;
                                             declaration_span = clause.span;
                                             call_span = expression.span;
                                           })
                                        expression.span goal evaluated.state
                                    in
                                    goals
                                      ((obligation, state) :: completed)
                                      remaining
                              in
                              goals [] predicate.paths
                            in
                            let emitted, states = List.split paths in
                            Ok
                              {
                                obligations =
                                  append predicate.obligations emitted;
                                paths = states;
                              })
                          states
                      in
                      prove_requires
                        (append obligations evaluated.obligations)
                        evaluated.paths rest
                in
                let* required =
                  prove_requires [] [ initial ] definition.contracts.requires
                in
                let instantiate_result state =
                  let* result, state =
                    match expression.typ with
                    | (Sst.Aggregate _ | Sst.Application _) as model_result
                      when imported_aggregate_call ->
                        let* registration, session =
                          match (context.imports, context.verification_session) with
                          | Some registration, Some session ->
                              Ok (registration, session)
                          | (Some _ | None), (Some _ | None) ->
                              error function_name expression.span
                                (Malformed_sst
                                   "retained aggregate model application lacks \
                                    its exact consumer session")
                        in
                        let actual_types =
                          List.map snd arguments
                          |> List.map (fun argument -> argument.Sst.typ)
                        in
                        let* arguments =
                          List.fold_left
                            (fun result actual ->
                              let* arguments = result in
                              match actual with
                              | Integer_value term ->
                                  Ok
                                    (Vir.Recursive_integer_argument term
                                    :: arguments)
                              | Boolean_value term ->
                                  Ok
                                    (Vir.Recursive_boolean_argument term
                                    :: arguments)
                              | Aggregate_value term ->
                                  Ok
                                    (Vir.Recursive_aggregate_argument term
                                    :: arguments)
                              | Unit_value | Tuple_value _ | Parametric_value _
                              | Function_value _ ->
                                  error function_name expression.span
                                    (Malformed_sst
                                       "retained aggregate model actual is not \
                                        an exact first-order logical term"))
                            (Ok []) actuals
                        in
                        let arguments = List.rev arguments in
                        let result_type = exact_agg_type context model_result in
                        let application_snapshot =
                          Vir.imported_model_application_snapshot
                            ~callee:definition.function_id ~arguments
                            ~result_type ~span:expression.span
                        in
                        let* ( summary,
                               call_snapshot,
                               registration_snapshot,
                               application_identity ) =
                          match
                            Imported_callable.consume_aggregate_model_call
                              registration
                              ~session:
                                (Verification_session.shared_heap_session_token
                                   session)
                              ~expression ~actual_types ~application_snapshot
                          with
                          | Ok admitted -> Ok admitted
                          | Error message ->
                              error function_name expression.span
                                (Malformed_sst message)
                        in
                        let* model =
                          match summary.model with
                          | Some model -> Ok model
                          | None ->
                              error function_name expression.span
                                (Malformed_sst
                                   "retained aggregate application lacks exact \
                                    public model provenance")
                        in
                        let application =
                          {
                            Vir.aggregate_type = result_type;
                            aggregate_desc =
                              Vir.Aggregate_imported_model_application
                                {
                                  callee = definition.function_id;
                                  callable_path = summary.path;
                                  callable_uid = summary.binding_uid;
                                  provider_unit = summary.provider_unit;
                                  provider_interface =
                                    summary.provider_interface;
                                  provider_source = summary.provider_source;
                                  provider_family = summary.provider_family;
                                  provider_import = summary.provider_import;
                                  summary_digest = summary.summary_digest;
                                  closure_digest = model.closure_digest;
                                  call_snapshot;
                                  registration_snapshot;
                                  invocation_ordinal =
                                    Option.value ~default:(-1)
                                      (Option.map
                                         Imported_callable
                                         .call_invocation_ordinal
                                         (Imported_callable.find_call
                                            registration expression));
                                  application_identity;
                                  arguments;
                                  result_type;
                                  span = expression.span;
                                };
                          }
                        in
                        Ok (Aggregate_value application, state)
                    | Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _ ->
                        fresh_value state
                          ~source_name:
                            (definition.function_id.function_name
                           ^ ".spec-result")
                          ~role:Vir.Result ~span:expression.span ~project:true
                          expression.typ
                    | Sst.Parameter _ | Sst.Application _ | Sst.Aggregate _ ->
                        unsupported_retained_generic state definition expression
                  in
                  let rec assume_ensures obligations states = function
                    | [] ->
                        Ok
                          {
                            obligations;
                            paths =
                              List.map
                                (fun state ->
                                  {
                                    value = result;
                                    state =
                                      restore_environment caller_environment
                                        state;
                                  })
                                states;
                          }
                    | (clause : Sst.ensures_clause) :: rest ->
                        let* evaluated =
                          evaluate_contexts
                            (fun state ->
                              let* environment, binder_ranges =
                                match clause.binder with
                                | None -> Ok (definition_environment, [])
                                | Some binder ->
                                    bind_pattern_direct function_name
                                      definition_environment binder result
                              in
                              let post_context =
                                {
                                  summary_context with
                                  old_environment = Some definition_environment;
                                }
                              in
                              let* predicate = evaluate_contract_formula_root post_context definition Logical_spec_evaluation_private.Ensures clause.clause_index clause.predicate.expression
                                  (with_assumptions { state with environment }
                                     binder_ranges)
                              in
                              let* paths =
                                let rec assumptions completed = function
                                  | [] -> Ok (List.rev completed)
                                  | evaluated :: remaining ->
                                      let* assumption =
                                        expect_boolean function_name clause.span
                                          evaluated.value
                                      in
                                      assumptions
                                        (with_assumptions evaluated.state
                                           [ assumption ]
                                        :: completed)
                                        remaining
                                in
                                assumptions [] predicate.paths
                              in
                              Ok { obligations = predicate.obligations; paths })
                            states
                        in
                        assume_ensures
                          (append obligations evaluated.obligations)
                          evaluated.paths rest
                  in
                  assume_ensures [] [ state ] definition.contracts.ensures
                in
                let* summarized =
                  evaluate_contexts instantiate_result required.paths
                in
                Ok
                  {
                    obligations =
                      append required.obligations summarized.obligations;
                    paths = summarized.paths;
                  }
            | `Opaque_invariant handle -> (
                match actuals with
                | [ Aggregate_value value ] ->
                    Ok
                      {
                        obligations = [];
                        paths =
                          [
                            {
                              value =
                                Boolean_value
                                  (Vir.Boolean_invariant_application
                                     {
                                       invariant_id =
                                         Type_invariant.invariant_id handle;
                                       model =
                                         Type_invariant.model_callable handle;
                                       predicate =
                                         Type_invariant.predicate_callable
                                           handle;
                                       value;
                                     });
                              state = caller_state;
                            };
                          ];
                      }
                | _ ->
                    error function_name expression.span
                      (Malformed_sst
                         "invariant predicate received a non-exact value"))
            | `Opaque_recursive ->
                let actual_expressions = List.map snd arguments in
                let frozen_recursive =
                  match
                    List.assoc_opt callee.function_index context.definitions
                  with
                  | Some descriptor
                    when same_function_id
                           (Sst_validation.callable_id descriptor)
                           callee -> (
                      match Sst_validation.callable_decrease descriptor with
                      | Sst_validation.Direct_frozen_spine_decrease (_, frozen)
                        ->
                          Some frozen
                      | Sst_validation.Direct_parametric_decrease _
                      | Sst_validation.No_decrease
                      | Sst_validation.Direct_integer_decrease _
                      | Sst_validation.Direct_structural_decrease _
                      | Sst_validation.Missing_decrease _
                      | Sst_validation.Duplicate_decrease _
                      | Sst_validation.Inapplicable_decrease _
                      | Sst_validation.Non_integer_decrease _
                      | Sst_validation.Recursive_decrease _ ->
                          None)
                  | Some _ | None -> None
                in
                let* authenticated =
                  List.fold_left
                    (fun result
                         (ordinal, (value, (actual_expression : Sst.expression)))
                       ->
                      let* arguments = result in
                      match value with
                      | Integer_value term ->
                          Ok
                            (`Argument (Vir.Recursive_integer_argument term)
                           :: arguments)
                      | Boolean_value term ->
                          Ok
                            (`Argument (Vir.Recursive_boolean_argument term)
                           :: arguments)
                      | Aggregate_value aggregate -> (
                          match
                            Logical_adt_evaluation_private
                            .exact_constructor_under caller_state.assumptions
                              aggregate
                          with
                          | Some constructed
                            when rank_free_type context actual_expression.typ ->
                              Ok
                                (`Argument
                                   (Vir.Recursive_aggregate_argument constructed)
                                :: arguments)
                          | Some _ | None ->
                          match frozen_recursive with
                          | Some frozen
                            when actual_expression.typ
                                 = Sst.Aggregate frozen.frozen_root -> (
                              if
                                same_function_id
                                  context.current_definition.function_id
                                  frozen.frozen_constructor
                              then
                                Ok
                                  (`Argument
                                     (Vir.Recursive_aggregate_argument aggregate)
                                 :: arguments)
                              else
                                match
                                  ( context.verification_session,
                                    context.frozen_observation_scope )
                                with
                                | Some session, Some scope -> (
                                    let call_path, path_condition =
                                      match
                                        Sys.getenv_opt
                                          "VEROCAML_TEST_FROZEN_OBSERVATION_ATTACK"
                                      with
                                      | Some "call" ->
                                          ( scope.frozen_scope_call_path
                                            @ [ expression.span ],
                                            scope.frozen_scope_path_condition )
                                      | Some "path" ->
                                          ( scope.frozen_scope_call_path,
                                            Vir.Boolean_constant false
                                            :: scope.frozen_scope_path_condition
                                          )
                                      | Some "replay" | Some _ | None ->
                                          ( scope.frozen_scope_call_path,
                                            scope.frozen_scope_path_condition )
                                    in
                                    let consume () =
                                      Verification_session
                                      .consume_frozen_observation session
                                        scope.frozen_scope_permit
                                        ~definition:
                                          scope.frozen_scope_definition
                                        ~frozen:scope.frozen_scope_descriptor
                                        ~root:scope.frozen_scope_root ~call_path
                                        ~path_condition
                                        ~epoch:scope.frozen_scope_epoch
                                    in
                                    let consumed =
                                      match consume () with
                                      | Error _ as error -> error
                                      | Ok () -> (
                                          match
                                            Sys.getenv_opt
                                              "VEROCAML_TEST_FROZEN_OBSERVATION_ATTACK"
                                          with
                                          | Some "replay" -> consume ()
                                          | Some _ | None -> Ok ())
                                    in
                                    match consumed with
                                    | Ok () ->
                                        Ok
                                          (`Argument
                                             (Vir.Recursive_aggregate_argument
                                                aggregate) :: arguments)
                                    | Error message ->
                                        error function_name expression.span
                                          (Malformed_sst message))
                                | (Some _ | None), (Some _ | None) ->
                                    error function_name expression.span
                                      (Malformed_sst
                                         "frozen-spine observation has no \
                                          constructor-origin or \
                                          formal-transfer authority"))
                          | Some _ | None ->
                              let* domain =
                                rank_domain_for_type context
                                  actual_expression.Sst.span
                                  actual_expression.typ
                              in
                              let* domain =
                                match domain with
                                | Some domain -> Ok domain
                                | None ->
                                    error function_name expression.span
                                      (Malformed_sst
                                         "recursive aggregate actual has no \
                                          exact local rank domain")
                              in
                              let* rank =
                                finite_rank_snapshot context
                                  actual_expression.span domain
                              in
                              let* registry, callable =
                                finite_registry context actual_expression.span
                              in
                              let* mode =
                                expression_instance_mode context
                                  actual_expression
                              in
                              let* receipt, mode =
                                match
                                  authenticate_exact_source_mode registry
                                    ~facts:caller_state.finite_receipts
                                    ~callable ~value:aggregate
                                    ~preferred_mode:mode
                                    ~typ:actual_expression.typ ~rank
                                with
                                | Ok authenticated -> Ok authenticated
                                | Error message ->
                                    error function_name expression.span
                                      (Malformed_sst message)
                              in
                              Ok
                                (`Aggregate
                                   ( ordinal,
                                     registry,
                                     callable,
                                     aggregate,
                                     mode,
                                     actual_expression.typ,
                                     rank,
                                     receipt )
                                :: arguments))
                      | Parametric_value term ->
                          Ok
                            (`Argument
                               (Vir.Recursive_parametric_argument term)
                            :: arguments)
                      | Function_value function_ ->
                          Ok
                            (`Argument
                               (Vir.Recursive_parametric_argument
                                  function_.function_term)
                            :: arguments)
                      | Unit_value | Tuple_value _ ->
                          error function_name expression.span
                            (Malformed_sst
                               "recursive specification argument is not scalar"))
                    (Ok [])
                    (List.mapi
                       (fun ordinal pair -> (ordinal, pair))
                       (List.combine actuals actual_expressions))
                in
                let authenticated = List.rev authenticated in
                let argument_receipts =
                  List.filter_map
                    (function
                      | `Argument _ -> None
                      | `Aggregate
                          ( ordinal,
                            _registry,
                            _callable,
                            aggregate,
                            _mode,
                            _typ,
                            _rank,
                            receipt ) ->
                          Some (ordinal, aggregate, receipt))
                    authenticated
                in
                let* arguments =
                  List.fold_left
                    (fun result -> function
                      | `Argument argument ->
                          let* arguments = result in
                          Ok (argument :: arguments)
                      | `Aggregate
                          ( _ordinal,
                            registry,
                            callable,
                            aggregate,
                            mode,
                            typ,
                            rank,
                            _authenticated ) ->
                          let* arguments = result in
                          let* _ =
                            if
                              !suppress_recursive_spec_result_consumption_for_testing
                              && Finite_value_registry
                                 .receipt_has_recursive_spec_result_origin
                                   _authenticated
                            then
                              error function_name expression.span
                                (Malformed_sst
                                   "recursive Spec result receipt consumption \
                                    is suppressed")
                            else
                              match
                                Finite_value_registry.consume registry
                                  ~facts:caller_state.finite_receipts ~callable
                                  ~value:aggregate ~mode ~typ ~rank
                              with
                              | Ok receipt -> Ok receipt
                              | Error message ->
                                  error function_name expression.span
                                    (Malformed_sst message)
                          in
                          Ok
                            (Vir.Recursive_aggregate_argument aggregate
                           :: arguments))
                    (Ok []) authenticated
                in
                let arguments = List.rev arguments in
                let has_nested_aggregate_recursive_argument =
                  List.exists
                    (function
                      | Vir.Recursive_aggregate_argument aggregate ->
                          aggregate_contains_recursive_specification aggregate
                      | Vir.Recursive_integer_argument _
                      | Vir.Recursive_boolean_argument _
                      | Vir.Recursive_parametric_argument _ ->
                          false)
                    arguments
                in
                let has_sealed_structural_rank_projection =
                  match
                    List.assoc_opt callee.function_index context.definitions
                  with
                  | Some descriptor
                    when same_function_id
                           (Sst_validation.callable_id descriptor)
                           callee -> (
                      match Sst_validation.callable_decrease descriptor with
                      | Sst_validation.Direct_structural_decrease _ -> true
                      | Sst_validation.Direct_parametric_decrease _
                      | Sst_validation.Direct_frozen_spine_decrease _ ->
                          false
                      | Sst_validation.Direct_integer_decrease _
                      | Sst_validation.No_decrease
                      | Sst_validation.Missing_decrease _
                      | Sst_validation.Duplicate_decrease _
                      | Sst_validation.Inapplicable_decrease _
                      | Sst_validation.Non_integer_decrease _
                      | Sst_validation.Recursive_decrease _ ->
                          false)
                  | Some _ | None -> false
                in
                (if has_nested_aggregate_recursive_argument then
                   (* Keep the two retained-source observations disjoint: the
                     integer observer must own a sealed structural decrease,
                     whose normal termination lowering is a rank projection;
                     the Boolean observer records the aggregate-actual route
                     itself. *)
                   match result_type with
                   | Sst.Int when has_sealed_structural_rank_projection ->
                       incr aggregate_recursive_rank_route_observations
                   | Sst.Bool ->
                       incr aggregate_recursive_argument_route_observations
                   | Sst.Int | Sst.Unit | Sst.Tuple _ | Sst.Aggregate _
                   | Sst.Parameter _ | Sst.Application _ ->
                       ());
                let* value, caller_state =
                  match result_type with
                  | Sst.Int ->
                      Ok
                        ( Integer_value
                            (recursive_integer_application callee type_arguments
                               arguments expression.span),
                          caller_state )
                  | Sst.Bool ->
                      Ok
                        ( Boolean_value
                            (recursive_boolean_application callee type_arguments
                               arguments expression.span),
                          caller_state )
                  | (Sst.Aggregate _ | Sst.Application _) as result_type -> (
                      let* () =
                        if
                          !suppress_aggregate_recursive_source_traversal_for_testing
                        then
                          error function_name expression.span
                            (Malformed_sst
                               "aggregate recursive source traversal is \
                                suppressed")
                        else Ok ()
                      in
                      let application_identity =
                        if
                          !recursive_spec_application_identity_forgery_for_testing
                        then
                          Recursive_spec_application_identity.For_testing.forged
                            ()
                        else Recursive_spec_application_identity.issue ()
                      in
                      let aggregate_type = exact_agg_type context result_type in
                      let result =
                        {
                          Vir.aggregate_type;
                          aggregate_desc =
                            Vir.Aggregate_recursive_spec_application
                              {
                                callee;
                                arguments;
                                result_type = aggregate_type;
                                span = expression.span;
                                application_identity;
                                type_arguments;
                              };
                        }
                      in
                      let* caller_state, observed_result =
                        match (frozen_recursive, arguments) with
                        | Some frozen, [ Vir.Recursive_aggregate_argument root ]
                          ->
                            let payload location =
                              match
                                selected_value_without_state location
                                  (field_selector frozen.frozen_payload_field)
                                  [] Sst.Int
                              with
                              | Integer_value entry -> (
                                  match caller_state.shared_scalar_heap with
                                  | Some heap
                                    when Shared_scalar_heap_private.accepts_read
                                           heap
                                           ~field:frozen.frozen_payload_field
                                           ~location -> (
                                      match
                                        Shared_scalar_heap_private.read heap
                                          ~view:context.shared_heap_view
                                          ~field:frozen.frozen_payload_field
                                          ~location ~entry
                                      with
                                      | Ok current -> Ok current
                                      | Error message ->
                                          error function_name expression.span
                                            (Malformed_sst message))
                                  | Some _ | None -> Ok entry)
                              | Unit_value | Boolean_value _ | Tuple_value _
                              | Aggregate_value _ | Parametric_value _ | Function_value _ ->
                                  error function_name expression.span
                                    (Malformed_sst
                                       "frozen-spine payload selector changed \
                                        sort")
                            in
                            let rec normalize fuel term =
                              if fuel = 0 then term
                              else
                                match
                                  Logical_adt.find_aggregate_equation
                                    caller_state.assumptions term
                                with
                                | Some
                                    ({
                                       Vir.aggregate_desc =
                                         Vir.Aggregate_symbol _;
                                       _;
                                     } as replacement)
                                  when replacement <> term ->
                                    normalize (fuel - 1) replacement
                                | Some _ | None -> term
                            in
                            let root = normalize 32 root in
                            let edge =
                              (match
                                 selected_value_without_state root
                                   (field_selector frozen.frozen_edge_field)
                                   [] (Sst.Aggregate frozen.frozen_link)
                               with
                                | Aggregate_value edge -> edge
                                | Unit_value | Integer_value _ | Boolean_value _
                                | Tuple_value _ | Parametric_value _ | Function_value _ ->
                                    assert false)
                              |> normalize 32
                            in
                            let child =
                              (match
                                 selected_value_without_state edge
                                   (argument_selector
                                      frozen.frozen_next_constructor 0)
                                   [] (Sst.Aggregate frozen.frozen_root)
                               with
                                | Aggregate_value child -> child
                                | Unit_value | Integer_value _ | Boolean_value _
                                | Tuple_value _ | Parametric_value _ | Function_value _ ->
                                    assert false)
                              |> normalize 32
                            in
                            let child_edge =
                              (match
                                 selected_value_without_state child
                                   (field_selector frozen.frozen_edge_field)
                                   [] (Sst.Aggregate frozen.frozen_link)
                               with
                                | Aggregate_value child_edge -> child_edge
                                | Unit_value | Integer_value _ | Boolean_value _
                                | Tuple_value _ | Parametric_value _ | Function_value _ ->
                                    assert false)
                              |> normalize 32
                            in
                            let* root_payload = payload root in
                            let* child_payload = payload child in
                            let constructor_scope =
                              same_function_id
                                context.current_definition.function_id
                                frozen.frozen_constructor
                            in
                            let has_tag (aggregate : Vir.aggregate_term)
                                (constructor : Sst.constructor_id) =
                              List.exists
                                (function
                                  | Vir.Integer_compare
                                      ( Vir.Equal,
                                        Vir.Aggregate_tag (_, candidate),
                                        Vir.Integer_constant ordinal ) ->
                                      candidate = aggregate
                                      && Z.equal ordinal
                                           (Z.of_int
                                              constructor.Sst.constructor_index)
                                  | _ -> false)
                                caller_state.assumptions
                            in
                            let constructor_terminal =
                              constructor_scope
                              && has_tag edge frozen.frozen_nil_constructor
                            in
                            let constructor_two_node =
                              constructor_scope
                              && has_tag edge frozen.frozen_next_constructor
                              && has_tag child_edge
                                   frozen.frozen_nil_constructor
                            in
                            let* () =
                              if
                                (not constructor_scope) || constructor_terminal
                                || constructor_two_node
                              then Ok ()
                              else
                                error function_name expression.span
                                  (Malformed_sst
                                     "frozen-spine constructor-local model \
                                      observation lacks its exact terminal or \
                                      two-node shape")
                            in
                            let end_ =
                              {
                                Vir.aggregate_type =
                                  vir_aggregate_type frozen.frozen_result_type;
                                aggregate_desc =
                                  Vir.Aggregate_constructor
                                    {
                                      constructor =
                                        frozen.frozen_end_constructor;
                                      arguments = [];
                                    };
                              }
                            in
                            let more head tail =
                              {
                                Vir.aggregate_type =
                                  vir_aggregate_type frozen.frozen_result_type;
                                aggregate_desc =
                                  Vir.Aggregate_constructor
                                    {
                                      constructor =
                                        frozen.frozen_more_constructor;
                                      arguments =
                                        [
                                          Vir.Recursive_integer_argument head;
                                          Vir.Recursive_aggregate_argument tail;
                                        ];
                                    };
                              }
                            in
                            let tail =
                              if constructor_terminal then end_
                              else more child_payload end_
                            in
                            let expected = more root_payload tail in
                            let result_type =
                              vir_aggregate_type frozen.frozen_result_type
                            in
                            let tag value constructor =
                              Vir.Integer_compare
                                ( Vir.Equal,
                                  Vir.Aggregate_tag (result_type, value),
                                  Vir.Integer_constant
                                    (Z.of_int constructor.Sst.constructor_index)
                                )
                            in
                            let select_head value head =
                              Vir.Integer_compare
                                ( Vir.Equal,
                                  Vir.Integer_selector
                                    ( argument_selector
                                        frozen.frozen_more_constructor 0 []
                                        Vir.Integer,
                                      value ),
                                  head )
                            in
                            let select_tail value selected =
                              Vir.Aggregate_equal
                                ( {
                                    Vir.aggregate_type = result_type;
                                    aggregate_desc =
                                      Vir.Aggregate_selector
                                        ( argument_selector
                                            frozen.frozen_more_constructor 1 []
                                            (Vir.Aggregate result_type),
                                          value );
                                  },
                                  selected )
                            in
                            let assumptions =
                              (if constructor_terminal then []
                               else
                                 [
                                   Vir.Integer_compare
                                     ( Vir.Equal,
                                       Vir.Aggregate_tag
                                         ( vir_aggregate_type frozen.frozen_link,
                                           edge ),
                                       Vir.Integer_constant
                                         (Z.of_int
                                            frozen.frozen_next_constructor
                                              .constructor_index) );
                                 ])
                              @ [ tag end_ frozen.frozen_end_constructor ]
                              @ (if constructor_terminal then []
                                 else
                                   [
                                     tag tail frozen.frozen_more_constructor;
                                     select_head tail child_payload;
                                     select_tail tail end_;
                                   ])
                              @ [
                                  tag expected frozen.frozen_more_constructor;
                                  select_head expected root_payload;
                                  select_tail expected tail;
                                ]
                              @ (match caller_state.shared_scalar_heap with
                                | Some heap
                                  when Shared_scalar_heap_private.current_epoch
                                         heap
                                       > 0 ->
                                    []
                                | Some _ | None ->
                                    [ Vir.Aggregate_equal (result, expected) ])
                              @
                              if
                                constructor_terminal
                                || !suppress_frozen_spine_child_witness_for_testing
                              then []
                              else
                                [
                                  Vir.Integer_compare
                                    ( Vir.Equal,
                                      Vir.Aggregate_tag
                                        ( vir_aggregate_type frozen.frozen_link,
                                          child_edge ),
                                      Vir.Integer_constant
                                        (Z.of_int
                                           frozen.frozen_nil_constructor
                                             .constructor_index) );
                                  Vir.Boolean_not
                                    (Vir.Aggregate_equal (root, child));
                                ]
                            in
                            Ok
                              (record_frozen_observation context constructor_scope
                                 frozen expected root_payload
                                 (if constructor_terminal then None
                                  else Some child_payload)
                                 assumptions caller_state)
                        | Some _, _ ->
                            error function_name expression.span
                              (Malformed_sst
                                 "frozen-spine helper lost its exact root \
                                  argument")
                        | None, _ -> Ok (caller_state, result)
                      in
                      let* domain =
                        rank_domain_for_type context expression.span
                          result_type
                      in
                      match domain with
                      | None ->
                          rank_free_recursive_result context expression.span
                            result_type observed_result caller_state
                      | Some domain ->
                          let* rank =
                            finite_rank_snapshot context expression.span domain
                          in
                          let* session =
                            match context.verification_session with
                            | Some session -> Ok session
                            | None ->
                                error function_name expression.span
                                  (Malformed_sst
                                     "recursive aggregate result lacks the \
                                      private session")
                          in
                          let* caller_callable =
                            match context.canonical_callable with
                            | Some callable -> Ok callable
                            | None ->
                                error function_name expression.span
                                  (Malformed_sst
                                     "recursive aggregate result lacks caller \
                                      identity")
                          in
                          let* receipt =
                            match
                              Verification_session.issue_recursive_spec_result
                                session ~caller_callable
                                ~caller:context.current_callable ~callee
                                ~call_span:expression.span
                                ~path_condition:caller_state.path_condition
                                ~application_identity ~argument_receipts ~result
                                ~mode:Sst.Ghost_instance
                                ~typ:definition.result_type ~rank
                            with
                            | Ok receipt -> Ok receipt
                            | Error message ->
                                error function_name expression.span
                                  (Malformed_sst message)
                          in
                          Ok
                            ( Aggregate_value observed_result,
                              {
                                caller_state with
                                finite_receipts =
                                  receipt :: caller_state.finite_receipts;
                              } ))
                  | Sst.Unit | Sst.Tuple _ | Sst.Parameter _ ->
                      error function_name expression.span
                        (Malformed_sst
                           "recursive specification result is outside its \
                            exact ABI")
                in
                let () = record_authority_observation Recursive_spec_lowering in
                Ok
                  {
                    obligations = [];
                    paths = [ { value; state = caller_state } ];
                  }
            | `Expand (definition_body, logical_spec_permit) ->
                let* caller_state =
                  let rec derive state parameters actuals expressions =
                    match (parameters, actuals, expressions) with
                    | [], [], [] -> Ok state
                    | ( (parameter : Sst.value_parameter) :: parameters,
                        actual :: actuals,
                        (_, (actual_expression : Sst.expression)) :: expressions
                      ) ->
                        let* state =
                          match actual with
                          | Aggregate_value aggregate ->
                              let* mode =
                                expression_instance_mode context
                                  actual_expression
                              in
                              Immutable_fact_integration.derive_finite_pattern
                                context parameter.pattern.span state
                                ~parent:aggregate ~mode parameter.pattern actual
                          | Unit_value | Integer_value _ | Boolean_value _
                          | Tuple_value _ | Parametric_value _ | Function_value _ ->
                              Ok state
                        in
                        derive state parameters actuals expressions
                    | _ ->
                        error function_name expression.span
                          (Malformed_sst
                             "expanded specification argument count mismatch")
                  in
                  derive caller_state parameters actuals arguments
                in
                let* owned_root_scalar_plan =
                  match Sst_validation.find_model context.validated callee with
                  | Some model -> (
                      match
                        Sst_validation_private.Public.owned_root_scalar_model
                          model
                      with
                      | None -> Ok None
                      | Some _ -> (
                          match
                            (context.verification_session, arguments, actuals)
                          with
                          | ( Some session,
                              [ (None, actual_expression) ],
                              [ Aggregate_value root ] ) -> (
                              match actual_expression.Sst.expression_desc with
                              | Sst.Variable { binding = root_binding; _ } ->
                                  let root_version =
                                    owned_root_version caller_state root_binding
                                      root
                                  in
                                  let plan_caller =
                                    Sst_validation.call_edge_descriptors
                                      context.validated
                                    |> List.find_map (fun edge ->
                                        let edge_callee =
                                          Sst_validation.call_edge_callee edge
                                          |> Sst_validation.callable_id
                                        in
                                        if
                                          edge_callee = callee
                                          && Sst_validation.call_edge_span edge
                                             = expression.span
                                          && Sst_validation.call_edge_form edge
                                             = Sst.Specification_call
                                        then
                                          Some
                                            (Sst_validation.call_edge_caller
                                               edge
                                            |> Sst_validation
                                               .callable_definition)
                                        else None)
                                    |> Option.value
                                         ~default:context.current_definition
                                  in
                                  let* plan =
                                    match
                                      Verification_session
                                      .issue_owned_root_scalar_observation_plan
                                        session ~validated:context.validated
                                        ~model ~caller:plan_caller
                                        ~call:expression
                                        ~actual:actual_expression ~root_binding
                                        ~root ~root_version
                                        ~path_condition:
                                          caller_state.path_condition
                                    with
                                    | Ok plan -> Ok plan
                                    | Error message ->
                                        error function_name expression.span
                                          (Malformed_sst message)
                                  in
                                  let* () =
                                    match
                                      Verification_session
                                      .consume_owned_root_scalar_observation_plan
                                        session plan
                                        ~validated:context.validated ~model
                                        ~caller:plan_caller ~call:expression
                                        ~actual:actual_expression ~root_binding
                                        ~root ~root_version
                                        ~path_condition:
                                          caller_state.path_condition
                                    with
                                    | Ok () -> Ok ()
                                    | Error message ->
                                        error function_name expression.span
                                          (Malformed_sst message)
                                  in
                                  Ok (Some plan)
                              | _ ->
                                  error function_name expression.span
                                    (Malformed_sst
                                       "owned-root scalar model actual is not \
                                        the exact current root"))
                          | _ ->
                              error function_name expression.span
                                (Malformed_sst
                                   "owned-root scalar model lacks its one \
                                    affine root plan")))
                  | None -> Ok None
                in
                let* owned_contents_stack =
                  let program = Sst_validation.program context.validated in
                  match
                    Sst_validation_private.Owned_recursive_contents_private
                    .model_for_program program definition
                  with
                  | None -> Ok context.owned_contents_stack
                  | Some grammar -> (
                      match
                        (context.verification_session, arguments, actuals)
                      with
                      | ( Some session,
                          [ (None, actual_expression) ],
                          [ Aggregate_value raw_root ] ) -> (
                          match actual_expression.Sst.expression_desc with
                          | Sst.Variable { binding = root_binding; _ } ->
                              let normalized_root =
                                normalize_owned_contents_aggregate
                                  caller_state.assumptions 128 raw_root
                              in
                              let* origin =
                                match
                                  ( find_owned_contents_origin caller_state
                                      raw_root,
                                    find_owned_contents_origin caller_state
                                      normalized_root )
                                with
                                | Some origin, (Some _ | None)
                                | None, Some origin ->
                                    Ok origin
                                | None, None ->
                                    error function_name expression.span
                                      (Malformed_sst
                                         "owned-contents current root has no \
                                          exact private construction or \
                                          predecessor capability")
                              in
                              let root =
                                Verification_session.owned_contents_origin_root
                                  origin
                              in
                              let root_version =
                                Verification_session
                                .owned_contents_origin_version origin
                              in
                              let* carrier =
                                match
                                  owned_contents_carrier_from_root context
                                    caller_state.assumptions root
                                    (Sst_validation_private
                                     .Owned_recursive_contents_private
                                     .root_path grammar)
                                with
                                | Some carrier -> Ok carrier
                                | None ->
                                    error function_name expression.span
                                      (Malformed_sst
                                         "owned-contents root has no exact \
                                          current carrier construction")
                              in
                              let* topology =
                                match
                                  owned_contents_topology
                                    caller_state.assumptions grammar carrier
                                with
                                | Ok topology -> Ok topology
                                | Error message ->
                                    error function_name expression.span
                                      (Malformed_sst message)
                              in
                              let* candidate =
                                match
                                  Verification_session
                                  .issue_owned_contents_candidate session
                                    ~validated:context.validated ~grammar
                                    ~caller:context.execution_definition
                                    ~call:expression ~actual:actual_expression
                                    ~root_binding ~root ~root_version
                                    ~path_condition:caller_state.path_condition
                                    ~topology ~origin
                                with
                                | Ok candidate -> Ok candidate
                                | Error message ->
                                    error function_name expression.span
                                      (Malformed_sst message)
                              in
                              let* () =
                                match
                                  Verification_session
                                  .consume_owned_contents_candidate session
                                    candidate
                                with
                                | Ok () -> Ok ()
                                | Error message ->
                                    error function_name expression.span
                                      (Malformed_sst message)
                              in
                              Ok
                                (Some
                                   {
                                     owned_contents_candidate = candidate;
                                     owned_contents_path = [];
                                     owned_contents_node =
                                       Verification_session
                                       .owned_contents_candidate_topology_root
                                         candidate;
                                   })
                          | _ ->
                              error function_name expression.span
                                (Malformed_sst
                                   "owned-contents model actual is not the \
                                    exact current root"))
                      | (Some _ | None), _, _ ->
                          error function_name expression.span
                            (Malformed_sst
                               "owned-contents model lacks its exact session \
                                root candidate"))
                in
                let* frozen_observation_scope =
                  match frozen_spine_for_model context.validated callee with
                  | None -> Ok None
                  | Some frozen
                    when same_function_id context.current_definition.function_id
                           frozen.frozen_constructor ->
                      Ok None
                  | Some frozen -> (
                      match
                        ( context.verification_session,
                          actuals,
                          Option.value ~default:[]
                            (Sst.value_parameters definition.parameters) )
                      with
                      | ( Some session,
                          [ Aggregate_value root ],
                          [
                            {
                              pattern = { typ = Sst.Aggregate root_type; _ };
                              _;
                            };
                          ] )
                        when same_type_id root_type frozen.frozen_root -> (
                          let call_path =
                            context.frozen_observation_call_path
                            @ [ expression.span ]
                          in
                          let epoch =
                            match caller_state.shared_scalar_heap with
                            | Some heap ->
                                Shared_scalar_heap_private.current_epoch heap
                            | None -> 0
                          in
                          match
                            Verification_session.seal_frozen_observation session
                              ~definition:context.current_definition ~frozen
                              ~root ~call_path
                              ~path_condition:caller_state.path_condition ~epoch
                          with
                          | Ok permit ->
                              Ok
                                (Some
                                   {
                                     frozen_scope_permit = permit;
                                     frozen_scope_definition =
                                       context.current_definition;
                                     frozen_scope_descriptor = frozen;
                                     frozen_scope_root = root;
                                     frozen_scope_call_path = call_path;
                                     frozen_scope_path_condition =
                                       caller_state.path_condition;
                                     frozen_scope_epoch = epoch;
                                   })
                          | Error message ->
                              error function_name expression.span
                                (Malformed_sst message))
                      | (Some _ | None), _, _ ->
                          error function_name expression.span
                            (Malformed_sst
                               "frozen-spine model call lacks its exact \
                                formal-transfer observation scope"))
                in
                let* definition_environment, ranges =
                  List.fold_left2
                    (fun environment parameter actual ->
                      let* environment, ranges = environment in
                      let* environment, nested_ranges =
                        bind_pattern_direct function_name environment
                          parameter.Sst.pattern actual
                      in
                      Ok (environment, ranges @ nested_ranges))
                    (Ok ([], []))
                    parameters
                    actuals
                in
                let spec_context =
                  {
                    context with
                    entry_environment = definition_environment;
                    logical = true;
                    ground_retry_candidate_scope = false;
                    old_environment = None;
                    spec_call_stack =
                      callee.function_index :: context.spec_call_stack;
                    frozen_observation_call_path =
                      context.frozen_observation_call_path @ [ expression.span ];
                    frozen_observation_scope;
                    owned_root_scalar_plan;
                    owned_contents_stack;
                  }
                in
                let* expanded =
                  let initial =
                    with_assumptions
                      { caller_state with environment = definition_environment }
                      ranges
                  in
                  match permit_for_body logical_spec_permit definition_body with
                  | None ->
                      evaluate spec_context definition_body initial
                  | Some permit ->
                      let evaluate_opaque_recursive context expression state =
                        let* evaluated = evaluate context expression state in
                        match (evaluated.obligations, evaluated.paths) with
                        | [], [ evaluated ] ->
                            Ok (evaluated.value, evaluated.state)
                        | _ ->
                            error function_name expression.span
                              (Malformed_sst
                                 "authenticated recursive logical term did not \
                                  evaluate once")
                      in
                      let aggregate_type, option_instance =
                        logical_type_callbacks_for_call spec_context.validated
                          (definition, type_arguments)
                      in
                      let callbacks =
                        logical_evaluation_callbacks spec_context ~aggregate_type
                          ~option_instance
                          ~evaluate_recursive:evaluate_opaque_recursive
                          ~error:(fun span message ->
                            {
                              function_name;
                              span;
                              unsupported = Malformed_sst message;
                            })
                      in
                      let* value, state =
                        Logical_spec_evaluation_private.evaluate permit callbacks
                          spec_context definition_body initial
                      in
                      Ok { obligations = []; paths = [ { value; state } ] }
                in
                Ok
                  {
                    expanded with
                    paths =
                      List.map
                        (fun evaluated ->
                          {
                            evaluated with
                            state =
                              restore_environment caller_environment
                                evaluated.state;
                          })
                        expanded.paths;
                  }
        in
        let* expanded = evaluate_contexts expand actuals.paths in
        Ok
          {
            obligations = append actuals.obligations expanded.obligations;
            paths = expanded.paths;
          }
  | Sst.Direct_call
      {
        call_form = (Sst.Exec_call | Sst.Proof_call) as call_form;
        callee;
        arguments;
        recursive;
        type_arguments;
      } ->
      let proof_summary_assumption_prefix = state.assumptions in
      let* summary =
        match find_summary context.summaries callee with
        | None -> error function_name expression.span (Missing_summary callee)
        | Some summary -> Ok summary
      in
      let finite_result_demanded =
        call_form = Sst.Exec_call
        &&
        match summary.definition.body with
        | Sst.Checked_exec { provenance = Sst.Authenticated_typedtree _; _ } ->
            List.exists
              (fun site ->
                site.finite_result_callee.function_index = callee.function_index
                && String.equal site.finite_result_callee.function_name
                     callee.function_name
                && site.finite_result_call_span = expression.span)
              context.finite_result_demand_sites
        | Sst.Checked_exec { provenance = Sst.Raw_semantic_body _; _ }
        | Sst.Spec_definition _ | Sst.Recursive_spec_definition _
        | Sst.Proof_body _ | Sst.External_specification _
        | Sst.Trusted_external_spec_target _ | Sst.Trusted_external_body _
        | Sst.Symbolic_declaration _ ->
            false
      in
      let* () =
        match (call_form, summary.definition.mode, context.logical) with
        | Sst.Exec_call, Sst.Exec, false | Sst.Proof_call, Sst.Proof, true ->
            Ok ()
        | Sst.Exec_call, Sst.Exec, true -> (
            match Sst_validation.find_callable context.validated callee with
            | Some descriptor -> (
                match
                  Sst_validation.result_instance_mode context.validated
                    descriptor
                with
                | Sst.Ghost_instance | Sst.Tracked_instance -> Ok ()
                | Sst.Exec_instance
                  when frozen_spine_terminal context.validated callee ->
                    Ok ()
                | Sst.Exec_instance ->
                    error function_name expression.span
                      (Malformed_sst
                         "runtime-result executable call reached logical \
                          evaluation"))
            | None ->
                error function_name expression.span (Missing_summary callee))
        | _ ->
            error function_name expression.span
              (Malformed_sst "call form reached the wrong evaluation stage")
      in
      let* consumed_root =
        match summary.definition.returns_unique_parameter with
        | None -> Ok None
        | Some parameter_index -> (
            match
              unique_parameter_actual summary.definition arguments
                parameter_index
            with
            | ( Some
                  {
                    Sst.pattern =
                      { pattern_desc = Sst.Bind parameter_binding; _ };
                    _;
                  },
                Some
                  ( _,
                    {
                      Sst.expression_desc =
                        Sst.Variable
                          {
                            binding = argument_binding;
                            use_uniqueness = Sst.Definitely_unique;
                          };
                      _;
                    } ) )
              when parameter_binding.uniqueness = Sst.Definitely_unique
                   && argument_binding.uniqueness = Sst.Definitely_unique ->
                Ok (Some argument_binding)
            | Some _, Some (_, argument) -> (
                match argument.Sst.expression_desc with
                | Sst.Record_value _ | Sst.Constructor_value _ -> Ok None
                | _ ->
                    error function_name expression.span
                      (Malformed_sst
                         "unique returned state is not supplied by a definite \
                          unique root"))
            | _ ->
                error function_name expression.span
                  (Malformed_sst
                     "unique return provenance names an invalid parameter"))
      in
      if recursive && context.logical && call_form <> Sst.Proof_call then
        error function_name expression.span (Recursion_awaits_totality callee)
      else if Sst_callback_private.has_callback_arguments arguments then
        let* summary =
          instantiate_call_summary function_name expression summary
            type_arguments
        in
        callback_direct evaluate context expression summary call_form arguments
          state
      else
        let* summary, arguments, parameters =
          call_summary function_name expression summary type_arguments arguments
        in
        let rec evaluate_arguments obligations contexts = function
          | [] -> Ok { obligations; paths = contexts }
          | (_, argument) :: rest ->
              let* evaluated =
                evaluate_contexts
                  (fun (state, values) ->
                    let* argument = evaluate context argument state in
                    Ok
                      {
                        obligations = argument.obligations;
                        paths =
                          List.map
                            (fun evaluated ->
                              (evaluated.state, evaluated.value :: values))
                            argument.paths;
                      })
                  contexts
              in
              evaluate_arguments
                (append obligations evaluated.obligations)
                evaluated.paths rest
        in
        let source_arguments = arguments in
        let* arguments = evaluate_arguments [] [ (state, []) ] arguments in
        let* argument_paths =
          authorize_finite_formal_call context
            ~callee_definition:summary.definition ~arguments:source_arguments
            ~call_span:expression.span arguments.paths
        in
        let arguments = { arguments with paths = argument_paths } in
        let instantiate (caller_state, reversed_actuals) =
          let actuals = List.rev reversed_actuals in
          if List.length actuals <> List.length parameters then
            error function_name expression.span
              (Malformed_sst "direct-call argument count mismatch")
          else
            let actual_snapshot =
              Digest.string (Marshal.to_string actuals [ Marshal.No_sharing ])
              |> Digest.to_hex
            in
            let* ( actual_environment,
                   actual_ranges,
                   caller_state,
                   default_obligations ) =
              bind_call_actuals evaluate context summary type_arguments
                caller_state function_name expression.span actuals
            in
            let caller_state = with_assumptions caller_state actual_ranges in
            let* finite_induction =
              Direct_recursion_induction.prepare context ~recursive ~call_form
                ~callee ~callee_definition:summary.definition
                ~arguments:source_arguments ~actuals
                ~call_span:expression.span caller_state
            in
            let* () =
              authorize_frozen_formal_call context
                ~callee_definition:summary.definition ~call_form
                ~arguments:source_arguments ~call_span:expression.span
                caller_state actuals
            in
            let* caller_state, proof_visit =
              if
                (not recursive) || summary.ensures = []
                || context.current_definition.mode <> Sst.Proof
              then Ok (caller_state, None)
              else
                match
                  Termination.find_edge_intent context.termination
                    ~caller:context.current_callable ~callee
                    ~span:expression.span
                with
                | Some edge_intent -> (
                    match Termination.edge_domain edge_intent with
                    | Termination.Integer_height -> Ok (caller_state, None)
                    | Termination.Parametric_direct_edge _ ->
                        Ok (caller_state, None)
                    | Termination.Frozen_spine_direct_edge _ ->
                        Ok (caller_state, None)
                    | Termination.Structural_rank _ -> (
                        let decrease =
                          Sst_validation.decrease_clause
                            (Termination.edge_measure edge_intent)
                        in
                        let decrease_expression =
                          Sst_validation.contract_clause_expression decrease
                        in
                        let* measure_binding =
                          match decrease_expression.Sst.expression_desc with
                          | Sst.Variable { binding; _ } -> Ok binding
                          | _ ->
                              error function_name expression.span
                                (Malformed_sst
                                   "structural recursive proof measure is not \
                                    an exact aggregate formal")
                        in
                        let measured_ordinal =
                          parameters
                          |> List.mapi (fun ordinal parameter ->
                              match parameter.Sst.pattern.pattern_desc with
                              | Sst.Bind binding
                                when binding.id = measure_binding.id ->
                                  Some ordinal
                              | Sst.Bind _ | Sst.Wildcard | Sst.Unit_pattern
                              | Sst.Tuple_pattern _ | Sst.Record_pattern _
                              | Sst.Constructor_pattern _ | Sst.Int_pattern _
                              | Sst.Bool_pattern _
                              | Sst.Owned_tree_cursor_pattern _
                              | Sst.Or_pattern _ ->
                                  None)
                          |> List.find_map Fun.id
                        in
                        let* measured_ordinal =
                          match measured_ordinal with
                          | Some ordinal -> Ok ordinal
                          | None ->
                              error function_name expression.span
                                (Malformed_sst
                                   "structural recursive proof measure does \
                                    not name a simple aggregate formal")
                        in
                        let* parent =
                          match
                            List.assoc_opt measure_binding.id
                              context.entry_environment
                          with
                          | Some (Aggregate_value parent) -> Ok parent
                          | Some
                              ( Unit_value | Integer_value _ | Boolean_value _
                              | Tuple_value _ | Parametric_value _ | Function_value _ )
                          | None ->
                              error function_name expression.span
                                (Malformed_sst
                                   "structural recursive proof parent is \
                                    absent from its exact entry environment")
                        in
                        let* child, child_expression, measured_type =
                          let argument_expressions =
                            List.map snd source_arguments
                          in
                          match
                            ( List.nth_opt actuals measured_ordinal,
                              List.nth_opt argument_expressions measured_ordinal,
                              List.nth_opt parameters measured_ordinal )
                          with
                          | ( Some (Aggregate_value child),
                              Some child_expression,
                              Some parameter ) ->
                              Ok
                                ( child,
                                  child_expression,
                                  parameter.Sst.pattern.typ )
                          | _ ->
                              error function_name expression.span
                                (Malformed_sst
                                   "structural recursive proof child actual is \
                                    not an exact aggregate")
                        in
                        let* selector =
                          match child.Vir.aggregate_desc with
                          | Vir.Aggregate_selector (selector, actual_parent)
                            when actual_parent = parent ->
                              Ok selector
                          | Vir.Aggregate_symbol _ | Vir.Aggregate_selector _ ->
                              error function_name expression.span
                                (Malformed_sst
                                   "structural recursion must select an \
                                    authenticated immediate child")
                          | Vir.Aggregate_constructor _ | Vir.Aggregate_record _
                          | Vir.Aggregate_conditional _
                          | Vir.Aggregate_symbolic_application _
                          | Vir.Aggregate_imported_model_application _
                          | Vir.Aggregate_recursive_spec_application _ ->
                              error function_name expression.span
                                (Malformed_sst
                                   "structural recursion cannot descend \
                                    through an unauthenticated aggregate term")
                        in
                        let* domain =
                          rank_domain_for_type context child_expression.span
                            measured_type
                        in
                        let* domain =
                          match domain with
                          | Some domain
                            when Vir.rank_selector_is_positive_child domain
                                   selector ->
                              Ok domain
                          | Some _ | None ->
                              error function_name expression.span
                                (Malformed_sst
                                   "structural recursive proof child has no \
                                    authenticated strict rank selector")
                        in
                        let* rank =
                          finite_rank_snapshot context expression.span domain
                        in
                        let* registry, callable =
                          finite_registry context expression.span
                        in
                        let* parent_receipt, _ =
                          match
                            authenticate_exact_source_mode registry
                              ~facts:caller_state.finite_receipts ~callable
                              ~value:parent ~preferred_mode:Sst.Ghost_instance
                              ~typ:measured_type ~rank
                          with
                          | Ok authenticated -> Ok authenticated
                          | Error message ->
                              error function_name expression.span
                                (Malformed_sst message)
                        in
                        let* child_mode =
                          expression_instance_mode context child_expression
                        in
                        let* child_receipt, _ =
                          match
                            authenticate_exact_source_mode registry
                              ~facts:caller_state.finite_receipts ~callable
                              ~value:child ~preferred_mode:child_mode
                              ~typ:measured_type ~rank
                          with
                          | Ok authenticated -> Ok authenticated
                          | Error message ->
                              error function_name expression.span
                                (Malformed_sst message)
                        in
                        match
                          Finite_value_registry.issue_proof_call_visit registry
                            ~facts:caller_state.finite_receipts
                            ~existing:caller_state.spent_proof_call_visits
                            ~callable ~parent:parent_receipt
                            ~child:child_receipt ~selector ~rank
                            ~branch:caller_state.path_condition
                            ~call_span:expression.span
                        with
                        | Ok visit ->
                            let () =
                              match
                                Sys.getenv_opt "VEROCAML_TEST_PROOF_CALL_TRACE"
                              with
                              | Some "1" ->
                                  [%log.debug "proof call visit"
                                    ~callable_digest:
                                      (Delator.Field.string
                                         (Digest.string callable
                                         |> Digest.to_hex))
                                    ~selector_owner:
                                      (Delator.Field.string
                                         selector.Vir.selector_namespace)
                                    ~selector_ordinal:
                                      (Delator.Field.int
                                         selector.selector_index)
                                    ~selector_name:
                                      (Delator.Field.string
                                         selector.selector_name)
                                    ~branch_digest:
                                      (Delator.Field.string
                                         (Finite_value_registry
                                          .result_path_digest
                                            caller_state.path_condition))
                                    ~call_file:
                                      (Delator.Field.string
                                         (Filename.basename
                                            expression.span.file))
                                    ~call_line:
                                      (Delator.Field.int
                                         expression.span.start_pos.line)
                                    ~call_column:
                                      (Delator.Field.int
                                         expression.span.start_pos.column)]
                              | Some _ | None -> ()
                            in
                            incr observed_proof_call_spent_visits;
                            Ok
                              ( {
                                  caller_state with
                                  proof_call_visits =
                                    visit :: caller_state.proof_call_visits;
                                  spent_proof_call_visits =
                                    visit
                                    :: caller_state.spent_proof_call_visits;
                                },
                                Some visit )
                        | Error message ->
                            error function_name expression.span
                              (Malformed_sst message)))
                | None ->
                    error function_name expression.span
                      (Malformed_sst
                         "recursive call has no validated termination edge \
                          intent")
            in
            (* Ordinary calls still mint no invariant authority.  An
               authenticated terminal snapshot is narrower: it must consume
               the exact already-closed caller fact and emits its own boundary
               VC without creating a predecessor capability. *)
            let* argument_obligations, caller_state =
              match
                Type_invariant.find_for_operation context.invariants callee
              with
              | Some (handle, Sst.Terminal_snapshot) ->
                  let rec observe ordinal obligations state actuals =
                    match actuals with
                    | [] -> Ok (List.rev obligations, state)
                    | actual :: rest -> (
                        match closed_invariant_application handle actual with
                        | Error _ ->
                            observe (ordinal + 1) obligations state rest
                        | Ok closed ->
                            let* () =
                              if List.mem closed state.closed_invariant_facts
                              then Ok ()
                              else
                                error function_name expression.span
                                  (Malformed_sst
                                     "terminal invariant snapshot has no exact \
                                      completed-result fact")
                            in
                            let callee_ref = vir_function_ref callee in
                            let* obligation, state =
                              emit_closed_invariant_goal
                                ~verified_assumptions:[ closed ] context handle
                                (Vir.Terminal_observation
                                   { operation = callee_ref; snapshot = true })
                                callee_ref expression.span actual state
                            in
                            observe (ordinal + 1)
                              (obligation :: obligations)
                              state rest)
                  in
                  observe 0 [] caller_state actuals
              | Some (_, _) | None -> Ok ([], caller_state)
            in
            let spec_context =
              {
                context with
                function_ref;
                entry_environment = actual_environment;
                logical = true;
                ground_retry_candidate_scope = false;
                old_environment = None;
                frozen_observation_call_path =
                  context.frozen_observation_call_path @ [ expression.span ];
                frozen_observation_scope = None;
              }
            in
            let rec prove_requires obligations states = function
              | [] -> Ok { obligations; paths = states }
              | clause :: rest ->
                  let* evaluated =
                    evaluate_contexts
                      (fun state ->
                        let caller_environment = state.environment in
                        let spec_state =
                          { state with environment = actual_environment }
                        in
                        let* predicate = evaluate_contract_formula_root spec_context summary.definition Logical_spec_evaluation_private.Requires clause.ordinal clause.payload spec_state
                        in
                        let* paths =
                          let rec loop paths = function
                            | [] -> Ok (List.rev paths)
                            | evaluated :: remaining ->
                                let* goal =
                                  expect_boolean function_name clause.span
                                    evaluated.value
                                in
                                let kind =
                                  Vir.Call_precondition
                                    {
                                      callee =
                                        {
                                          Vir.function_index =
                                            summary.definition.function_id
                                              .function_index;
                                          function_name =
                                            summary.definition.function_id
                                              .function_name;
                                        };
                                      precondition_ordinal = clause.ordinal;
                                      declaration_span = clause.span;
                                      call_span = expression.span;
                                    }
                                in
                                let obligation, state =
                                  emit_goal function_ref kind expression.span
                                    goal evaluated.state
                                in
                                loop
                                  (( obligation,
                                     restore_environment caller_environment
                                       state )
                                  :: paths)
                                  remaining
                          in
                          loop [] predicate.paths
                        in
                        Ok { obligations = predicate.obligations; paths })
                      states
                  in
                  let emitted, states = List.split evaluated.paths in
                  prove_requires
                    (append obligations (append evaluated.obligations emitted))
                    states rest
            in
            let* proved = prove_requires [] [ caller_state ] summary.requires in
            let proved =
              {
                proved with
                obligations = append argument_obligations proved.obligations;
              }
            in
            let* proved =
              if not recursive then Ok proved
              else
                match
                  Termination.find_edge_intent context.termination
                    ~caller:context.current_callable ~callee
                    ~span:expression.span
                with
                | Some edge_intent ->
                    let measure =
                      Sst_validation.decrease_clause
                        (Termination.edge_measure edge_intent)
                      |> fun clause ->
                      {
                        ordinal = Sst_validation.contract_clause_index clause;
                        span = Sst_validation.contract_clause_span clause;
                        binder = Sst_validation.contract_clause_binder clause;
                        payload =
                          Sst_validation.contract_clause_expression clause;
                      }
                    in
                    let domain = Termination.edge_domain edge_intent in
                    let measure_context =
                      {
                        spec_context with
                        logical = false;
                        old_environment = None;
                      }
                    in
                    let* evaluated =
                      evaluate_contexts
                        (fun state ->
                          let caller_environment = state.environment in
                          let* measured =
                            evaluate measure_context measure.payload
                              { state with environment = actual_environment }
                          in
                          let* paths =
                            let rec loop paths = function
                              | [] -> Ok (List.rev paths)
                              | measured :: remaining ->
                                  let* call_measure =
                                    ranked_measure function_name measure.span
                                      context.rank_domains domain measured.value
                                  in
                                  let* () =
                                    match (domain, call_measure) with
                                    | ( Termination.Structural_rank _,
                                        Vir.Integer_rank_project
                                          ( rank_domain,
                                            {
                                              Vir.aggregate_desc =
                                                Vir.Aggregate_selector
                                                  (selector, _);
                                              _;
                                            } ) )
                                      when Vir.rank_selector_is_positive_child
                                             rank_domain selector ->
                                        Ok ()
                                    | Termination.Structural_rank _, _ ->
                                        error function_name expression.span
                                          (Malformed_sst
                                             "structural recursion must select \
                                              an authenticated immediate child")
                                    | ( Termination.Frozen_spine_direct_edge _,
                                        Vir.Integer_constant value )
                                      when Z.equal value Z.zero ->
                                        Ok ()
                                    | Termination.Frozen_spine_direct_edge _, _
                                      ->
                                        error function_name expression.span
                                          (Malformed_sst
                                             "frozen-spine recursion must \
                                              select its exact direct child")
                                    | ( Termination.Parametric_direct_edge _,
                                        measure ) ->
                                        validate_parametric_recursive_measure
                                          function_name expression.span measure
                                    | Termination.Integer_height, _ -> Ok ()
                                  in
                                  let* entry_measure =
                                    match measured.state.entry_measure with
                                    | Some (entry_domain, entry_measure)
                                      when same_decrease_domain entry_domain
                                             domain ->
                                        Ok entry_measure
                                    | Some _ ->
                                        error function_name expression.span
                                          (Malformed_sst
                                             "recursive call measure domain \
                                              differs from its entry domain")
                                    | None ->
                                        error function_name expression.span
                                          (Malformed_sst
                                             "recursive call reached before \
                                              entry measure evaluation")
                                  in
                                  let measured_state =
                                    restore_environment caller_environment
                                      measured.state
                                  in
                                  let callee_ref =
                                    {
                                      Vir.function_index =
                                        summary.definition.function_id
                                          .function_index;
                                      function_name =
                                        summary.definition.function_id
                                          .function_name;
                                    }
                                  in
                                  let nonnegative =
                                    Vir.Integer_compare
                                      ( Vir.Less_or_equal,
                                        Vir.Integer_constant Z.zero,
                                        call_measure )
                                  in
                                  let nonnegative_obligation, measured_state =
                                    emit_goal function_ref
                                      (Vir.Recursive_call_measure_nonnegative
                                         {
                                           callee = callee_ref;
                                           declaration_span = measure.span;
                                           call_span = expression.span;
                                         })
                                      expression.span nonnegative measured_state
                                  in
                                  let descent =
                                    Vir.Integer_compare
                                      ( Vir.Less_than,
                                        call_measure,
                                        entry_measure )
                                  in
                                  let descent_obligation, measured_state =
                                    emit_goal function_ref
                                      (Vir.Recursive_call_strict_descent
                                         {
                                           callee = callee_ref;
                                           declaration_span = measure.span;
                                           call_span = expression.span;
                                         })
                                      expression.span descent measured_state
                                  in
                                  let* measured_state =
                                    Direct_recursion_induction.bind_hypothesis
                                      context finite_induction ~callee
                                      ~call_span:expression.span
                                      ~actual_snapshot
                                      ~nonnegative_obligation:
                                        nonnegative_obligation
                                          .provisional_obligation
                                      ~strict_descent_obligation:
                                        descent_obligation
                                          .provisional_obligation measured_state
                                  in
                                  loop
                                    (( [
                                         nonnegative_obligation;
                                         descent_obligation;
                                       ],
                                       measured_state )
                                    :: paths)
                                    remaining
                            in
                            loop [] measured.paths
                          in
                          let obligations, states = List.split paths in
                          Ok
                            {
                              obligations =
                                append measured.obligations
                                  (List.concat obligations);
                              paths = states;
                            })
                        proved.paths
                    in
                    let () =
                      match domain with
                      | Termination.Structural_rank _ ->
                          record_authority_observation
                            Recursive_proof_rank_lowering
                      | Termination.Integer_height
                      | Termination.Parametric_direct_edge _
                      | Termination.Frozen_spine_direct_edge _ ->
                          ()
                    in
                    Ok
                      {
                        obligations =
                          append proved.obligations evaluated.obligations;
                        paths = evaluated.paths;
                      }
                | None ->
                    error function_name expression.span
                      (Malformed_sst
                         "recursive call has no validated termination edge \
                          intent")
            in
            let operation_role =
              Type_invariant.find_for_operation context.invariants
                summary.definition.function_id
              |> Option.map snd
            in
            let shared_invariant_effect =
              match operation_role with
              | Some Sst.Shared_invariant_transition ->
                  Sst.function_shared_scalar_transitions summary.definition
              | Some _ | None -> []
            in
            let* proved =
              match shared_invariant_effect with
              | [] -> Ok proved
              | transitions ->
                  let apply state =
                    let* state =
                      match state.shared_scalar_heap with
                      | None -> Ok state
                      | Some heap -> (
                          match Shared_scalar_heap_private.fork heap with
                          | Ok heap ->
                              Ok { state with shared_scalar_heap = Some heap }
                          | Error message ->
                              error function_name expression.span
                                (Malformed_sst message))
                    in
                    let caller_environment = state.environment in
                    let base_epoch =
                      match state.shared_scalar_heap with
                      | Some heap ->
                          Shared_scalar_heap_private.current_epoch heap
                      | None -> 0
                    in
                    let* handle, identity, location =
                      match
                        ( Type_invariant.find_for_operation context.invariants
                            summary.definition.function_id,
                          transitions )
                      with
                      | ( Some (handle, Sst.Shared_invariant_transition),
                          transition :: _ ) -> (
                          let identity =
                            {
                              Shared_invariant_cell_private.abstract_type =
                                Type_invariant.abstract_type handle;
                              hidden_type = transition.shared_record_type;
                              field = transition.shared_target_field;
                              model = Type_invariant.model_callable handle;
                              invariant =
                                Type_invariant.predicate_callable handle;
                            }
                          in
                          let root = List.hd transition.shared_formal_roots in
                          let actual =
                            List.combine parameters actuals
                            |> List.find_map (fun (parameter, actual) ->
                                match parameter.Sst.pattern.pattern_desc with
                                | Sst.Bind formal when formal.id = root.id ->
                                    Some actual
                                | Sst.Bind _ | Sst.Wildcard | Sst.Unit_pattern
                                | Sst.Tuple_pattern _ | Sst.Record_pattern _
                                | Sst.Constructor_pattern _ | Sst.Int_pattern _
                                | Sst.Bool_pattern _
                                | Sst.Owned_tree_cursor_pattern _
                                | Sst.Or_pattern _ ->
                                    None)
                          in
                          match actual with
                          | Some (Aggregate_value location) ->
                              Ok (handle, identity, location)
                          | Some _ | None ->
                              error function_name expression.span
                                (Malformed_sst
                                   "shared invariant effect has no exact \
                                    aggregate actual"))
                      | Some (_, _), _ | None, _ ->
                          error function_name expression.span
                            (Malformed_sst
                               "shared invariant effect lost its authenticated \
                                identity")
                    in
                    let* () =
                      if
                        Option.is_some
                          (frozen_spine_for_transition context.validated
                             summary.definition.function_id
                             (List.hd transitions))
                      then (
                        Option.iter
                          (fun session ->
                            Verification_session.note_invariant_cell_open
                              session;
                            Verification_session
                            .note_invariant_cell_effect_instantiation session)
                          context.verification_session;
                        Ok ())
                      else
                        match context.invariant_cell_authority with
                        | None -> Ok ()
                        | Some authority -> (
                            match
                              Shared_invariant_cell_private.open_cell authority
                                ~identity
                                ~operation:summary.definition.function_id
                                ~location ~entry_epoch:base_epoch
                            with
                            | Error message ->
                                error function_name expression.span
                                  (Malformed_sst message)
                            | Ok () -> (
                                match
                                  Shared_invariant_cell_private
                                  .note_effect_instantiation authority
                                with
                                | Ok () -> Ok ()
                                | Error message ->
                                    error function_name expression.span
                                      (Malformed_sst message)))
                    in
                    let effect_context =
                      {
                        spec_context with
                        current_definition = summary.definition;
                        current_callable = summary.definition.function_id;
                        current_mode = Sst.Exec;
                        entry_environment = actual_environment;
                        logical = false;
                        old_environment = None;
                        shared_heap_view =
                          Shared_scalar_heap_private.Current_view;
                        shared_old_heap_view =
                          Some
                            (Shared_scalar_heap_private.Epoch_view base_epoch);
                        shared_effect_base_epoch = base_epoch;
                        shared_entry_transitions = transitions;
                      }
                    in
                    let* effected =
                      evaluate effect_context summary.executable_body
                        { state with environment = actual_environment }
                    in
                    let* () =
                      if
                        Option.is_some
                          (frozen_spine_for_transition context.validated
                             summary.definition.function_id
                             (List.hd transitions))
                      then (
                        Option.iter
                          Verification_session.note_invariant_cell_close
                          context.verification_session;
                        Ok ())
                      else
                        match context.invariant_cell_authority with
                        | None -> Ok ()
                        | Some authority -> (
                            match
                              Shared_invariant_cell_private.close_cell authority
                                ~operation:summary.definition.function_id
                                ~final_epoch:
                                  (base_epoch + List.length transitions)
                            with
                            | Ok () -> Ok ()
                            | Error message ->
                                error function_name expression.span
                                  (Malformed_sst message))
                    in
                    Ok
                      {
                        obligations = effected.obligations;
                        paths =
                          List.map
                            (fun effected ->
                              let state =
                                restore_environment caller_environment
                                  effected.state
                              in
                              match
                                closed_invariant_application handle
                                  (Aggregate_value location)
                              with
                              | Ok closed ->
                                  with_closed_invariant_fact state closed
                              | Error _ -> state)
                            effected.paths;
                      }
                  in
                  let* effected = evaluate_contexts apply proved.paths in
                  Ok
                    {
                      obligations =
                        append proved.obligations effected.obligations;
                      paths = effected.paths;
                    }
            in
            let assume_summary state =
              let state =
                match consumed_root with
                | None -> state
                | Some binding ->
                    {
                      state with
                      environment =
                        remove_binding binding.Sst.id state.environment;
                    }
              in
              if
                Option.is_some proof_visit
                && !suppress_recursive_proof_summaries
              then
                Ok
                  {
                    obligations = [];
                    paths = [ { value = Unit_value; state } ];
                  }
              else
                let* state =
                  match proof_visit with
                  | None -> Ok state
                  | Some visit -> (
                      let* registry, _ =
                        finite_registry context expression.span
                      in
                      match
                        Finite_value_registry.issue_proof_call_summary registry
                          ~visit
                      with
                      | Ok issued_summary ->
                          observed_proof_call_ledger_summaries :=
                            ( context.current_callable.function_name,
                              summary.definition.function_id.function_name )
                            :: !observed_proof_call_ledger_summaries;
                          Ok
                            {
                              state with
                              proof_call_summaries =
                                issued_summary :: state.proof_call_summaries;
                            }
                      | Error message ->
                          error function_name expression.span
                            (Malformed_sst message))
                in
                let* state =
                  match summary.definition.body with
                  | Sst.Trusted_external_spec_target
                      (Sst.Same_unit_target
                         {
                           wrapper;
                           target;
                           target_span;
                           declaration_span;
                           witness_span;
                         }) ->
                      Ok
                        {
                          state with
                          trusted_summary_uses =
                            append state.trusted_summary_uses
                              [
                                Vir.Trusted_external_specification_use
                                  {
                                    target = vir_function_ref target;
                                    wrapper = vir_function_ref wrapper;
                                    target_span;
                                    wrapper_span = declaration_span;
                                    witness_span;
                                    call_span = expression.span;
                                    requires_count =
                                      List.length summary.requires;
                                    ensures_count = List.length summary.ensures;
                                  };
                              ];
                        }
                  | Sst.Trusted_external_spec_target _ ->
                      error function_name expression.span
                        (Malformed_sst
                           "trusted external target has an unresolved linkage")
                  | Sst.Trusted_external_body
                      (Sst.Authenticated_external_body
                         { declaration_span; witness_span; _ }) ->
                      Ok
                        {
                          state with
                          trusted_summary_uses =
                            append state.trusted_summary_uses
                              [
                                Vir.Trusted_external_body_use
                                  {
                                    function_ref =
                                      vir_function_ref
                                        summary.definition.function_id;
                                    mode = summary.definition.mode;
                                    call_form;
                                    broadcast_use = false;
                                    declaration_span;
                                    witness_span;
                                    call_span = expression.span;
                                    requires_count =
                                      List.length summary.requires;
                                    ensures_count = List.length summary.ensures;
                                  };
                              ];
                        }
                  | Sst.Trusted_external_body (Sst.Raw_external_body _) ->
                      error function_name expression.span
                        (Malformed_sst
                           "trusted external body has raw provenance")
                  | Sst.Checked_exec _ | Sst.Proof_body _ -> Ok state
                  | Sst.Spec_definition _ | Sst.Recursive_spec_definition _
                  | Sst.External_specification _
                  | Sst.Symbolic_declaration _ ->
                      error function_name expression.span
                        (Malformed_sst
                           "call summary has a noncallable body disposition")
                in
                let* terminal_obligations, result, state =
                  match (call_form, operation_role) with
                  | Sst.Exec_call, Some Sst.Current_terminal_read -> (
                      let caller_environment = state.environment in
                      let terminal_context =
                        {
                          spec_context with
                          current_definition = summary.definition;
                          current_callable = summary.definition.function_id;
                          current_mode = Sst.Exec;
                          entry_environment = actual_environment;
                          logical = false;
                          old_environment = None;
                          shared_heap_view =
                            Shared_scalar_heap_private.Current_view;
                        }
                      in
                      let* terminal_evaluation =
                        evaluate terminal_context summary.executable_body
                          { state with environment = actual_environment }
                      in
                      match terminal_evaluation.paths with
                      | [ terminal ] ->
                          let* () =
                            if
                              frozen_spine_terminal context.validated
                                summary.definition.function_id
                            then (
                              Option.iter
                                Verification_session
                                .note_invariant_cell_terminal_read
                                context.verification_session;
                              Ok ())
                            else
                              match context.invariant_cell_authority with
                              | None -> Ok ()
                              | Some authority -> (
                                  let* handle =
                                    match
                                      Type_invariant.find_for_operation
                                        context.invariants
                                        summary.definition.function_id
                                    with
                                    | Some (handle, Sst.Current_terminal_read)
                                      ->
                                        Ok handle
                                    | Some (_, _) | None ->
                                        error function_name expression.span
                                          (Malformed_sst
                                             "terminal read lost its \
                                              invariant-cell role")
                                  in
                                  let* transition =
                                    Type_invariant.public_operations handle
                                    |> List.find_map
                                         (fun
                                           ((operation : Sst.function_id), role)
                                         ->
                                           if
                                             role
                                             <> Sst.Shared_invariant_transition
                                           then None
                                           else
                                             match
                                               List.assoc_opt
                                                 operation.function_index
                                                 context.definitions
                                             with
                                             | Some descriptor -> (
                                                 match
                                                   Sst
                                                   .function_shared_scalar_transitions
                                                     (Sst_validation
                                                      .callable_definition
                                                        descriptor)
                                                 with
                                                 | transition :: _ ->
                                                     Some transition
                                                 | [] -> None)
                                             | None -> None)
                                    |> function
                                    | Some transition -> Ok transition
                                    | None ->
                                        error function_name expression.span
                                          (Malformed_sst
                                             "terminal read has no shared \
                                              operation descriptor")
                                  in
                                  let identity =
                                    {
                                      Shared_invariant_cell_private
                                      .abstract_type =
                                        Type_invariant.abstract_type handle;
                                      hidden_type =
                                        transition.shared_record_type;
                                      field = transition.shared_target_field;
                                      model =
                                        Type_invariant.model_callable handle;
                                      invariant =
                                        Type_invariant.predicate_callable handle;
                                    }
                                  in
                                  let* location =
                                    actuals
                                    |> List.find_map (function
                                      | Aggregate_value location
                                        when location.aggregate_type
                                             = vir_aggregate_type
                                                 (Type_invariant.abstract_type
                                                    handle) ->
                                          Some location
                                      | Unit_value | Integer_value _
                                      | Boolean_value _ | Tuple_value _
                                      | Aggregate_value _ | Parametric_value _
                                      | Function_value _ ->
                                          None)
                                    |> function
                                    | Some location -> Ok location
                                    | None ->
                                        error function_name expression.span
                                          (Malformed_sst
                                             "terminal read has no exact cell \
                                              actual")
                                  in
                                  let epoch =
                                    match terminal.state.shared_scalar_heap with
                                    | Some heap ->
                                        Shared_scalar_heap_private.current_epoch
                                          heap
                                    | None -> 0
                                  in
                                  match
                                    Shared_invariant_cell_private
                                    .note_terminal_read authority ~identity
                                      ~operation:summary.definition.function_id
                                      ~location ~epoch
                                  with
                                  | Ok () -> Ok ()
                                  | Error message ->
                                      error function_name expression.span
                                        (Malformed_sst message))
                          in
                          Ok
                            ( terminal_evaluation.obligations,
                              terminal.value,
                              restore_environment caller_environment
                                terminal.state )
                      | [] | _ :: _ :: _ ->
                          error function_name expression.span
                            (Malformed_sst
                               "current terminal read did not retain one exact \
                                path"))
                  | Sst.Proof_call, _ when expression.typ = Sst.Unit ->
                      Ok ([], Unit_value, state)
                  | Sst.Proof_call, _ ->
                      let* result, state =
                        fresh_value state
                          ~source_name:
                            (summary.definition.function_id.function_name
                           ^ ".proof-result")
                          ~role:Vir.Result ~span:expression.span ~project:true
                          expression.typ
                      in
                      Ok ([], result, state)
                  | Sst.Exec_call, _ ->
                      let* result, state =
                        fresh_value state
                          ~source_name:
                            (summary.definition.function_id.function_name
                           ^ ".result")
                          ~role:Vir.Result ~span:expression.span ~project:true
                          expression.typ
                      in
                      Ok ([], result, state)
                  | (Sst.Specification_call | Sst.Unclassified_call), _ ->
                      assert false
                in
                let* () =
                  match
                    ( call_form,
                      frozen_spine_for_constructor context.validated
                        summary.definition.function_id,
                      context.verification_session,
                      result )
                  with
                  | Sst.Exec_call, Some _, Some session, Aggregate_value root
                    -> (
                      match
                        Verification_session
                        .issue_frozen_constructor_result_instance session
                          ~definition:context.current_definition
                          ~callee:summary.definition
                          ~call_path:
                            (context.frozen_observation_call_path
                           @ [ expression.span ])
                          ~path_condition:state.path_condition ~root ~epoch:0
                          ~allow_branch_reuse:true
                      with
                      | Ok () -> Ok ()
                      | Error message ->
                          error function_name expression.span
                            (Malformed_sst message))
                  | ( Sst.Exec_call,
                      Some _,
                      (Some _ | None),
                      ( Unit_value | Integer_value _ | Boolean_value _
                      | Tuple_value _ | Parametric_value _ | Function_value _ ) ) ->
                      error function_name expression.span
                        (Malformed_sst
                           "frozen-spine constructor call has no exact \
                            aggregate result")
                  | Sst.Exec_call, Some _, None, Aggregate_value _ ->
                      error function_name expression.span
                        (Malformed_sst
                           "frozen-spine constructor call requires the private \
                            verification session")
                  | ( (Sst.Exec_call | Sst.Proof_call),
                      (Some _ | None),
                      (Some _ | None),
                      ( Unit_value | Integer_value _ | Boolean_value _
                      | Tuple_value _ | Aggregate_value _ | Parametric_value _
                      | Function_value _
                        ) ) ->
                      Ok ()
                  | (Sst.Specification_call | Sst.Unclassified_call), _, _, _ ->
                      assert false
                in
                let* state =
                  match
                    ( summary.definition.returns_unique_parameter,
                      expression.typ,
                      result )
                  with
                  | Some _, Sst.Aggregate type_id, Aggregate_value aggregate ->
                      constrain_immediate_record_fields function_name
                        expression.span context.type_definitions state type_id
                        aggregate
                  | Some _, _, _ ->
                      error function_name expression.span
                        (Malformed_sst
                           "unique returned parameter has a non-record result")
                  | None, _, _ -> Ok state
                in
                let* state =
                  Direct_recursion_induction.issue_result context expression
                    result state
                in
                let* state =
                  Finite_result_integration.consume_published context
                    ~demanded:finite_result_demanded ~recursive
                    ~callee_definition:summary.definition
                    ~call_span:expression.span result state
                in
                let* result_boundary, state =
                  match
                    ( context.verification_session,
                      invariant_for_typ context.invariants expression.typ )
                  with
                  | None, _ | Some _, None -> Ok ([], state)
                  | Some verification_session, Some handle -> (
                      let* descriptor =
                        match
                          Sst_validation.find_callable context.validated
                            summary.definition.function_id
                        with
                        | Some descriptor -> Ok descriptor
                        | None ->
                            error function_name expression.span
                              (Malformed_sst
                                 "receipt callee is absent from validated \
                                  authority")
                      in
                      match
                        Sst_validation.result_instance_mode context.validated
                          descriptor
                      with
                      | Sst.Ghost_instance -> Ok ([], state)
                      | Sst.Exec_instance | Sst.Tracked_instance -> (
                          match result with
                          | Aggregate_value aggregate ->
                              let callee_ref =
                                vir_function_ref summary.definition.function_id
                              in
                              let* callee_snapshot =
                                match
                                  Verification_session.callee_snapshot
                                    verification_session
                                    ~validated:context.validated
                                    ~invariants:context.invariants
                                    summary.definition
                                with
                                | Ok snapshot -> Ok snapshot
                                | Error message ->
                                    error function_name expression.span
                                      (Malformed_sst message)
                              in
                              let* consumed =
                                match
                                  Verification_session.consume
                                    verification_session callee_snapshot
                                    ~caller:context.current_callable
                                    ~call_span:expression.span
                                    ~path_condition:state.path_condition
                                    ~result:aggregate
                                with
                                | Ok consumed -> Ok consumed
                                | Error message ->
                                    error function_name expression.span
                                      (Malformed_sst message)
                              in
                              let closed =
                                Verification_session.consumed_closed_fact
                                  consumed
                              in
                              let* obligation, state =
                                emit_closed_invariant_goal
                                  ~verified_assumptions:[ closed ] context
                                  handle
                                  (Vir.Call_result { callee = callee_ref })
                                  callee_ref expression.span result state
                              in
                              Ok
                                ( [ obligation ],
                                  with_consumed_receipt_fact state consumed )
                          | Unit_value | Integer_value _ | Boolean_value _
                          | Tuple_value _ | Parametric_value _ | Function_value _ ->
                              error function_name expression.span
                                (Malformed_sst
                                   "receipt result changed from its exact \
                                    aggregate type")))
                in
                let* state =
                  match
                    ( operation_role,
                      context.invariant_cell_authority,
                      result,
                      Type_invariant.find_for_operation context.invariants
                        summary.definition.function_id )
                  with
                  | ( Some Sst.Abstract_constructor,
                      Some authority,
                      Aggregate_value location,
                      Some (handle, Sst.Abstract_constructor) ) -> (
                      let transition =
                        Type_invariant.public_operations handle
                        |> List.find_map
                             (fun ((operation : Sst.function_id), role) ->
                               if role <> Sst.Shared_invariant_transition then
                                 None
                               else
                                 match
                                   List.assoc_opt operation.function_index
                                     context.definitions
                                 with
                                 | Some descriptor -> (
                                     match
                                       Sst.function_shared_scalar_transitions
                                         (Sst_validation.callable_definition
                                            descriptor)
                                     with
                                     | transition :: _ -> Some transition
                                     | [] -> None)
                                 | None -> None)
                      in
                      match transition with
                      | Some transition -> (
                          let identity =
                            {
                              Shared_invariant_cell_private.abstract_type =
                                Type_invariant.abstract_type handle;
                              hidden_type = transition.shared_record_type;
                              field = transition.shared_target_field;
                              model = Type_invariant.model_callable handle;
                              invariant =
                                Type_invariant.predicate_callable handle;
                            }
                          in
                          match
                            Shared_invariant_cell_private.issue_constructor
                              authority ~identity
                              ~constructor:summary.definition.function_id
                              ~call_span:expression.span ~location
                          with
                          | Ok () -> Ok state
                          | Error message ->
                              error function_name expression.span
                                (Malformed_sst message))
                      | None -> Ok state)
                  | ( (None | Some _),
                      (None | Some _),
                      ( Unit_value | Integer_value _ | Boolean_value _
                      | Tuple_value _ | Aggregate_value _ | Parametric_value _
                      | Function_value _
                        ),
                      (None | Some _) ) ->
                      Ok state
                in
                let actual_entry = actual_environment in
                let rec assume_posts states = function
                  | [] ->
                      Ok
                        {
                          obligations =
                            append terminal_obligations result_boundary;
                          paths =
                            List.map
                              (fun state -> { value = result; state })
                              states;
                        }
                  | clause :: rest ->
                      let* evaluated =
                        evaluate_contexts
                          (fun state ->
                            let caller_environment = state.environment in
                            let* spec_environment, binder_ranges =
                              match clause.binder with
                              | None -> Ok (actual_entry, [])
                              | Some binder ->
                                  bind_pattern_direct function_name actual_entry
                                    binder result
                            in
                            let post_context =
                              {
                                spec_context with
                                old_environment =
                                  Some spec_context.entry_environment;
                                shared_old_heap_view =
                                  (match shared_invariant_effect with
                                  | [] -> spec_context.shared_old_heap_view
                                  | writes ->
                                      Option.map
                                        (fun heap ->
                                          Shared_scalar_heap_private.Epoch_view
                                            (Shared_scalar_heap_private
                                             .current_epoch heap
                                            - List.length writes))
                                        state.shared_scalar_heap);
                              }
                            in
                            let* predicate =
                              evaluate_contract_formula_root post_context summary.definition Logical_spec_evaluation_private.Ensures clause.ordinal clause.payload
                                (with_assumptions
                                   { state with environment = spec_environment }
                                   binder_ranges)
                            in
                            let* paths =
                              let rec loop paths = function
                                | [] -> Ok (List.rev paths)
                                | evaluated :: remaining ->
                                    let* assumption =
                                      expect_boolean function_name clause.span
                                        evaluated.value
                                    in
                                    let assumptions =
                                      match
                                        !suppress_exec_ensure_assumption_for_callable
                                      with
                                      | Some callable
                                        when call_form = Sst.Exec_call
                                             && String.equal callable
                                                  summary.definition.function_id
                                                    .function_name ->
                                          []
                                      | Some _ | None -> [ assumption ]
                                    in
                                    let state =
                                      with_assumptions evaluated.state
                                        assumptions
                                    in
                                    let state =
                                      restore_environment caller_environment
                                        state
                                    in
                                    loop (state :: paths) remaining
                              in
                              loop [] predicate.paths
                            in
                            Ok { obligations = predicate.obligations; paths })
                          states
                      in
                      let* rest_result = assume_posts evaluated.paths rest in
                      Ok
                        {
                          obligations =
                            append evaluated.obligations rest_result.obligations;
                          paths = rest_result.paths;
                        }
                in
                let ensures =
                  match !suppress_exec_summary_for_callable with
                  | Some callable
                    when call_form = Sst.Exec_call
                         && String.equal callable
                              summary.definition.function_id.function_name ->
                      []
                  | Some _ | None ->
                      if
                        call_form = Sst.Proof_call
                        && suppressed_proof_edge
                             suppress_proof_summary_for_testing
                             context.current_callable
                             summary.definition.function_id
                      then []
                      else summary.ensures
                in
                assume_posts [ state ] ensures
            in
            let* summarized = evaluate_contexts assume_summary proved.paths in
            Ok
              {
                obligations =
                  append default_obligations
                    (append proved.obligations summarized.obligations);
                paths = summarized.paths;
              }
        in
        let* instantiated = evaluate_contexts instantiate arguments.paths in
        let paths =
          if call_form <> Sst.Proof_call then instantiated.paths
          else
            (* Keep the logical closure of the ledgered proof summary, including
               constructor facts for its arguments, without exporting any
               receipt or finite authority from the proof region. *)
            let rec drop_prefix prefix assumptions =
              match (prefix, assumptions) with
              | [], assumptions -> assumptions
              | expected :: prefix, actual :: assumptions when expected = actual
                ->
                  drop_prefix prefix assumptions
              | _ -> []
            in
            List.map
              (fun evaluated ->
                let exported =
                  drop_prefix proof_summary_assumption_prefix
                    evaluated.state.assumptions
                in
                if
                  exported <> []
                  && context.current_definition.mode = Sst.Exec
                  && Option.is_some evaluated.state.proof_activation_authority
                then
                  observed_exec_region_proof_summary_exports :=
                    ( context.current_callable.function_name,
                      summary.definition.function_id.function_name )
                    :: !observed_exec_region_proof_summary_exports;
                {
                  evaluated with
                  state =
                    {
                      evaluated.state with
                      exported_proof_summary_facts =
                        append evaluated.state.exported_proof_summary_facts
                          exported;
                    };
                })
              instantiated.paths
        in
        Ok
          {
            obligations = append arguments.obligations instantiated.obligations;
            paths;
          }
  | Sst.Direct_call _ ->
      error function_name expression.span
        (Malformed_sst "unclassified call reached symbolic execution")
  | Sst.Use_type_invariant { value; _ } ->
      if not context.logical then
        error function_name expression.span
          (Malformed_sst "use_type_invariant reached runtime evaluation")
      else
        let* handle =
          match value.typ with
          | Sst.Aggregate type_id -> (
              match Type_invariant.find_for_type context.invariants type_id with
              | Some handle -> Ok handle
              | None ->
                  error function_name expression.span
                    (Malformed_sst
                       "use_type_invariant has no authenticated exact-type \
                        handle"))
          | Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _ | Sst.Parameter _
          | Sst.Application _ ->
              error function_name expression.span
                (Malformed_sst
                   "use_type_invariant value is not an abstract aggregate")
        in
        let* evaluated_value = evaluate context value state in
        let assume evaluated =
          match evaluated.value with
          | Aggregate_value _ ->
              let* closed =
                match closed_invariant_application handle evaluated.value with
                | Ok closed -> Ok closed
                | Error message ->
                    error function_name value.span (Malformed_sst message)
              in
              let* () =
                if List.mem closed evaluated.state.closed_invariant_facts then
                  Ok ()
                else
                  error function_name value.span
                    (Malformed_sst
                       "use_type_invariant requires a closed fact for this \
                        exact Exec/Tracked symbolic instance")
              in
              let* predicate =
                evaluate_invariant_predicate context handle evaluated.value
                  evaluated.state
              in
              let rec instantiate paths = function
                | [] -> Ok (List.rev paths)
                | evaluated :: rest ->
                    let* assumption =
                      expect_boolean function_name expression.span
                        evaluated.value
                    in
                    instantiate
                      ({
                         value = Unit_value;
                         state =
                           with_local_invariant_assumption evaluated.state
                             assumption;
                       }
                      :: paths)
                      rest
              in
              let* paths = instantiate [] predicate.paths in
              Ok { obligations = predicate.obligations; paths }
          | Unit_value | Integer_value _ | Boolean_value _ | Tuple_value _
          | Parametric_value _ | Function_value _ ->
              error function_name expression.span
                (Malformed_sst
                   "use_type_invariant value changed from its abstract type")
        in
        let* assumed = evaluate_contexts assume evaluated_value.paths in
        Ok
          {
            obligations = append evaluated_value.obligations assumed.obligations;
            paths = assumed.paths;
          }
  | Sst.Local_assert { assertion_ordinal; predicate } ->
      evaluate_local_assertion context expression assertion_ordinal predicate
        state
  | Sst.Proof_region proof_body ->
      if context.logical then
        error function_name expression.span
          (Malformed_sst "nested proof region reached proof evaluation")
      else
        let* region_authority =
          match context.verification_session with
          | None -> Ok None
          | Some session -> (
              match
                Verification_session.exec_proof_region_activation_authority
                  session ~validated:context.validated
                  context.current_definition ~region:expression
              with
              | Ok authority -> Ok (Some authority)
              | Error message ->
                  error function_name expression.span (Malformed_sst message))
        in
        let proof_context =
          { context with logical = true; ground_retry_candidate_scope = true }
        in
        let outer_state = state in
        let region_state =
          {
            state with
            proof_activation_authority = region_authority;
            proof_activations = context.proof_entry_activations;
            reached_proof_activations = [];
            ground_constructor_matches = [];
            pending_ground_constructor_matches = [];
            exported_proof_summary_facts = [];
            exported_local_assertion_facts = [];
          }
        in
        let* evaluated = evaluate proof_context proof_body region_state in
        Ok
          {
            evaluated with
            paths =
              List.map
                (fun evaluated ->
                  let exported =
                    evaluated.state.exported_local_assertion_facts
                  in
                  let proof_summaries =
                    evaluated.state.exported_proof_summary_facts
                  in
                  {
                    value = Unit_value;
                    state =
                      {
                        evaluated.state with
                        environment = outer_state.environment;
                        assumptions =
                          append outer_state.assumptions
                            (append proof_summaries exported);
                        local_invariant_assumptions =
                          outer_state.local_invariant_assumptions;
                        closed_invariant_facts =
                          outer_state.closed_invariant_facts;
                        consumed_receipt_facts =
                          outer_state.consumed_receipt_facts;
                        finite_receipts = outer_state.finite_receipts;
                        pending_finite_induction =
                          outer_state.pending_finite_induction;
                        required_preceding_safety =
                          outer_state.required_preceding_safety;
                        entry_measure = outer_state.entry_measure;
                        proof_activation_authority =
                          outer_state.proof_activation_authority;
                        proof_activations = outer_state.proof_activations;
                        reached_proof_activations =
                          outer_state.reached_proof_activations;
                        ground_constructor_matches =
                          outer_state.ground_constructor_matches;
                        pending_ground_constructor_matches =
                          outer_state.pending_ground_constructor_matches;
                        exported_proof_summary_facts =
                          append outer_state.exported_proof_summary_facts
                            proof_summaries;
                        exported_local_assertion_facts =
                          append outer_state.exported_local_assertion_facts
                            exported;
                      };
                  })
                evaluated.paths;
          }
  | Sst.Old payload when context.logical -> (
      match context.old_environment with
      | None ->
          error function_name expression.span
            (Malformed_sst "old reached a non-postcondition specification")
      | Some entry_environment ->
          let current_environment = state.environment in
          let old_context =
            {
              context with
              shared_heap_view =
                Option.value ~default:Shared_scalar_heap_private.Entry_view
                  context.shared_old_heap_view;
            }
          in
          let* evaluated =
            evaluate old_context payload
              { state with environment = entry_environment }
          in
          Ok
            {
              evaluated with
              paths =
                List.map
                  (fun evaluated ->
                    {
                      evaluated with
                      state =
                        restore_environment current_environment evaluated.state;
                    })
                  evaluated.paths;
            })
  | Sst.Forall _ | Sst.Exists _ | Sst.Callback_call _
  | Sst.Callback_requires _ | Sst.Callback_ensures _ ->
      logical_expression evaluate context expression state
  | Sst.Old _ ->
      error function_name expression.span
        (Malformed_sst "old reached a runtime expression")
and evaluate_local_assertion context expression assertion_ordinal predicate
    state =
  let function_name = context.function_ref.Vir.function_name in
  let direct_exec_builtin =
    (not context.logical)
    && context.current_mode = Sst.Exec
    && Verification_session.authenticate_direct_exec_local_assertion_source
         ~validated:context.validated ~definition:context.current_definition
         ~expression
  in
  if (not context.logical) && not direct_exec_builtin then
    error function_name expression.span
      (Malformed_sst "local assertion reached a non-proof evaluation context")
  else
    let caller_environment = state.environment in
    let* evaluated = evaluate context predicate state in
    let emit evaluated =
      let* goal = expect_boolean function_name predicate.span evaluated.value in
      let* issued_state, continuation_ground_matches =
        issue_retry_ground_constructor_match context goal evaluated.state
      in
      let evaluated = { evaluated with state = issued_state } in
      let assumptions = effective_assumptions evaluated.state in
      let* session =
        match context.verification_session with
        | Some session -> Ok session
        | None ->
            error function_name expression.span
              (Malformed_sst
                 "local assertion lacks a private verification session")
      in
      let* direct_scope =
        match
          Verification_session.issue_and_consume_local_assertion session
            ~direct_exec:direct_exec_builtin
            ~authority:evaluated.state.proof_activation_authority
            ~validated:context.validated ~definition:context.current_definition
            ~expression ~ordinal:assertion_ordinal ~assumptions
            ~required_preceding_safety:evaluated.state.required_preceding_safety
            ~path_condition:evaluated.state.path_condition ~goal
        with
        | Ok scope -> Ok scope
        | Error message ->
            error function_name expression.span (Malformed_sst message)
      in
      let proof_activations = evaluated.state.proof_activations in
      let local_state = evaluated.state in
      let obligation, state =
        emit_goal context.function_ref
          (Vir.Local_assertion { local_assertion_ordinal = assertion_ordinal })
          expression.span goal local_state
      in
      let* () =
        match direct_scope with
        | None -> Ok ()
        | Some scope -> (
            match
              Verification_session.close_direct_exec_local_assertion_scope
                session scope
            with
            | Ok () -> Ok ()
            | Error message ->
                error function_name expression.span (Malformed_sst message))
      in
      let state =
        { state with ground_constructor_matches = continuation_ground_matches }
      in
      let suppress_successor =
        !suppress_local_assertion_successor_for_testing
        || List.exists
             (fun (suppressed_function, suppressed_ordinal) ->
               String.equal context.function_ref.function_name
                 suppressed_function
               && assertion_ordinal = suppressed_ordinal)
             !suppress_local_assertion_successor_sites_for_testing
      in
      if not suppress_successor then incr observed_local_assertion_exports;
      let state =
        {
          state with
          assumptions =
            (if suppress_successor then local_state.assumptions
             else state.assumptions);
          proof_activations;
          exported_local_assertion_facts =
            (if suppress_successor then
               local_state.exported_local_assertion_facts
             else append local_state.exported_local_assertion_facts [ goal ]);
        }
      in
      Ok
        {
          obligations = [ obligation ];
          paths =
            [
              {
                value = Unit_value;
                state = restore_environment caller_environment state;
              };
            ];
        }
    in
    let* emitted = evaluate_contexts emit evaluated.paths in
    Ok
      {
        obligations = append evaluated.obligations emitted.obligations;
        paths = emitted.paths;
      }
and evaluate_formula_root ?(strict = true) context identity
    (expression : Sst.expression) state =
  match List.find_opt (fun (entry : Logical_spec_admission_private.registry_entry) -> entry.identity = identity) context.formula_registry with
  | None -> error context.function_ref.function_name expression.span (Malformed_sst "formula root has no exact authenticated registry token")
  | Some ({ disposition = Logical_spec_admission_private.Abstain reason; _ } : Logical_spec_admission_private.registry_entry) ->
      Logical_spec_capability_private.For_testing.trace_abstention identity ~reason
        ~authority_snapshot:(Option.fold ~none:"none" ~some:Verification_session.render_counters context.verification_session);
      evaluate context expression state
  | Some ({ disposition = Logical_spec_admission_private.Eligible _; _ } : Logical_spec_admission_private.registry_entry)
    when not strict ->
      Logical_spec_capability_private.For_testing.trace_abstention identity
        ~reason:"instantiated-generic-contract"
        ~authority_snapshot:
          (Option.fold ~none:"none" ~some:Verification_session.render_counters
             context.verification_session);
      evaluate context expression state
  | Some ({ root = formula_root; disposition = Logical_spec_admission_private.Eligible permit; _ } : Logical_spec_admission_private.registry_entry) ->
      evaluate_permitted_formula context identity state formula_root permit
and evaluate_permitted_formula context identity state formula_root permit =
  let function_name = context.function_ref.function_name in
  let before = Option.map Verification_session.counters context.verification_session in
  let callbacks = logical_evaluation_callbacks context ~aggregate_type:(vir_aggregate_type_of_sst state.parametric_adts) ~option_instance:(Parametric_adt.option_instance state.parametric_adts) ~evaluate_recursive:(fun _ recursive _ -> error function_name recursive.Sst.span (Malformed_sst "strict invariant-contract permit exposed recursive escape")) ~error:(fun span message -> { function_name; span; unsupported = Malformed_sst message }) in
  let callbacks = { callbacks with Logical_spec_evaluation_private.observe_field_read = (fun context state field _ value -> observe_formula_model_field context state field value) } in
  Logical_spec_capability_private.For_testing.note_evaluation identity;
  let* value, state = Logical_spec_evaluation_private.evaluate_invariant_contract permit ~validated:context.validated ~root_identity:identity callbacks context formula_root state in
  let after = Option.map Verification_session.counters context.verification_session in
  if before <> after then error function_name formula_root.span (Malformed_sst "permitted invariant-contract formula changed authority state")
  else (Logical_spec_capability_private.For_testing.trace_evaluation identity ~function_name ~authority_unchanged:true; Ok { obligations = []; paths = [ { value; state } ] })
and evaluate_contract_formula_root context definition clause_kind ordinal expression state =
  let identity =
    Logical_spec_admission_private.contract_identity definition clause_kind
      ordinal
  in
  let exact_root =
    List.exists
      (fun (entry : Logical_spec_admission_private.registry_entry) ->
        entry.identity = identity && entry.root == expression)
      context.formula_registry
  in
  evaluate_formula_root
    ~strict:(definition.type_binders = [] || exact_root)
    context identity expression state
and evaluate_invariant_predicate context handle value state =
  let function_name = context.function_ref.function_name in
  let predicate = Type_invariant.predicate_definition handle in
  let* parameter =
    match predicate.parameters with
    | [ parameter ] -> Ok parameter
    | _ ->
        error function_name predicate.span
          (Malformed_sst "authenticated invariant predicate arity changed")
  in
  let parameter = Sst.require_value_parameter parameter in
  let caller_environment = state.environment in
  let* environment, ranges =
    bind_pattern_direct function_name [] parameter.pattern value
  in
  let predicate_body =
    match predicate.body with
    | Sst.Spec_definition body -> body.expression
    | _ -> assert false
  in
  let predicate_context =
    {
      context with
      entry_environment = environment;
      logical = true;
      old_environment = None;
    }
  in
  let* predicate =
    evaluate_formula_root predicate_context (Invariant_formula_capture.identity handle) predicate_body
      (with_assumptions { state with environment } ranges)
  in
  Ok
    {
      predicate with
      paths =
        List.map
          (fun evaluated ->
            {
              evaluated with
              state = restore_environment caller_environment evaluated.state;
            })
          predicate.paths;
    }
and prove_invariant_validity context handle boundary operation span value state
    =
  let boundary_state = state in
  let* predecessor =
    match boundary with
    | Vir.Transition_preservation { root_binding_id; pre_version; _ } ->
        let* predecessor_value =
          match List.assoc_opt root_binding_id state.environment with
          | Some value -> Ok value
          | None ->
              error context.function_ref.function_name span
                (Malformed_sst
                   "invariant transition predecessor is not a live root")
        in
        let* closed =
          match closed_invariant_application handle predecessor_value with
          | Ok closed -> Ok closed
          | Error message ->
              error context.function_ref.function_name span
                (Malformed_sst message)
        in
        let* () =
          if List.mem closed state.closed_invariant_facts then Ok ()
          else
            let* session =
              match context.verification_session with
              | Some session -> Ok session
              | None ->
                  error context.function_ref.function_name span
                    (Malformed_sst
                       "invariant transition predecessor has no authenticated \
                        closed validity fact")
            in
            let* descriptor =
              match
                Sst_validation.find_callable context.validated
                  context.current_definition.function_id
              with
              | Some descriptor -> Ok descriptor
              | None ->
                  error context.function_ref.function_name span
                    (Malformed_sst
                       "transition predecessor callee is absent from validated \
                        authority")
            in
            let* ordinal, formal =
              context.current_definition.parameters
              |> List.mapi (fun ordinal parameter -> (ordinal, parameter))
              |> List.find_map (fun (ordinal, parameter) ->
                  let parameter = Sst.require_value_parameter parameter in
                  match parameter.Sst.pattern.pattern_desc with
                  | Sst.Bind binding when binding.id = root_binding_id ->
                      Some (ordinal, binding)
                  | Sst.Bind _ | Sst.Wildcard | Sst.Unit_pattern
                  | Sst.Tuple_pattern _ | Sst.Record_pattern _
                  | Sst.Constructor_pattern _ | Sst.Int_pattern _
                  | Sst.Bool_pattern _ | Sst.Owned_tree_cursor_pattern _
                  | Sst.Or_pattern _ ->
                      None)
              |> function
              | Some formal -> Ok formal
              | None ->
                  error context.function_ref.function_name span
                    (Malformed_sst
                       "transition predecessor root is not an exact formal")
            in
            let mode =
              Sst_validation.formal_instance_mode context.validated descriptor
                ordinal
                (List.nth context.current_definition.parameters ordinal)
            in
            match
              Verification_session.consume_transition_predecessors session
                ~validated:context.validated ~invariants:context.invariants
                ~callee:context.current_definition ~formal ~root:formal
                ~root_value:
                  (match predecessor_value with
                  | Aggregate_value aggregate -> aggregate
                  | Unit_value | Integer_value _ | Boolean_value _
                  | Tuple_value _ | Parametric_value _ | Function_value _ ->
                      assert false)
                ~mode ~typ:formal.typ ~owned_version:pre_version
                ~obligation_snapshot:
                  (transition_obligation_snapshot context.current_definition
                     handle)
            with
            | Ok () -> Ok ()
            | Error message ->
                error context.function_ref.function_name span
                  (Malformed_sst message)
        in
        let* expanded =
          evaluate_invariant_predicate context handle predecessor_value state
        in
        let* paths =
          let rec assume paths = function
            | [] -> Ok (List.rev paths)
            | evaluated :: rest ->
                let* fact =
                  expect_boolean context.function_ref.function_name span
                    evaluated.value
                in
                assume (with_assumptions evaluated.state [ fact ] :: paths) rest
          in
          assume [] expanded.paths
        in
        Ok { obligations = expanded.obligations; paths }
    | Vir.Constructor_establishment | Vir.Call_argument _ | Vir.Call_result _
    | Vir.Function_return | Vir.Shared_invariant_close _
    | Vir.Terminal_observation _ ->
        Ok { obligations = []; paths = [ state ] }
  in
  let* predicate =
    evaluate_contexts
      (fun state -> evaluate_invariant_predicate context handle value state)
      predecessor.paths
  in
  let rec loop obligations paths = function
    | [] -> Ok { obligations = List.rev obligations; paths = List.rev paths }
    | predicate :: rest ->
        let* goal =
          expect_boolean context.function_ref.function_name span predicate.value
        in
        let kind =
          Vir.Invariant_validity
            {
              invariant_id = Type_invariant.invariant_id handle;
              abstract_type =
                Type_invariant.abstract_type handle |> vir_aggregate_type;
              model = Type_invariant.model_callable handle |> vir_function_ref;
              predicate =
                Type_invariant.predicate_callable handle |> vir_function_ref;
              operation;
              boundary;
            }
        in
        let obligation, state =
          emit_goal context.function_ref kind span goal predicate.state
        in
        let* closed =
          match closed_invariant_application handle value with
          | Ok closed -> Ok closed
          | Error message ->
              error context.function_ref.function_name span
                (Malformed_sst message)
        in
        let state =
          match boundary with
          | Vir.Transition_preservation _ ->
              {
                state with
                environment = boundary_state.environment;
                assumptions = boundary_state.assumptions;
                local_invariant_assumptions =
                  boundary_state.local_invariant_assumptions;
                closed_invariant_facts =
                  append boundary_state.closed_invariant_facts [ closed ];
                required_preceding_safety =
                  boundary_state.required_preceding_safety;
                path_condition = boundary_state.path_condition;
                projection_symbols = boundary_state.projection_symbols;
                trusted_summary_uses = boundary_state.trusted_summary_uses;
                entry_measure = boundary_state.entry_measure;
              }
          | Vir.Constructor_establishment | Vir.Call_argument _
          | Vir.Call_result _ | Vir.Function_return
          | Vir.Shared_invariant_close _ | Vir.Terminal_observation _ ->
              with_closed_invariant_fact state closed
        in
        loop (obligation :: obligations) ({ value; state } :: paths) rest
  in
  let* proved = loop [] [] predicate.paths in
  Ok
    {
      obligations =
        append predecessor.obligations
          (append predicate.obligations proved.obligations);
      paths = proved.paths;
    }
let rec materialize_result ?(project = false) state span name value =
  match value with
  | Unit_value -> Ok (Vir.Unit_result, state)
  | Integer_value value ->
      let symbol, state =
        fresh_symbol state ~source_name:name ~sort:Vir.Integer ~role:Vir.Result
          ~span ~project
      in
      let term = Vir.Integer_symbol symbol in
      let equation = Vir.Integer_compare (Equal, term, value) in
      let state = with_assumptions state (equation :: Vir.integer_range term) in
      Ok (Vir.Integer_result symbol, state)
  | Boolean_value value ->
      let symbol, state =
        fresh_symbol state ~source_name:name ~sort:Vir.Boolean ~role:Vir.Result
          ~span ~project
      in
      let state =
        with_assumptions state
          [ Vir.Boolean_equal (Vir.Boolean_symbol symbol, value) ]
      in
      Ok (Vir.Boolean_result symbol, state)
  | Tuple_value values ->
      let rec loop index state results = function
        | [] -> Ok (Vir.Tuple_result (List.rev results), state)
        | value :: rest ->
            let* result, state =
              materialize_result ~project state span
                (Printf.sprintf "%s.%d" name index)
                value
            in
            loop (index + 1) state (result :: results) rest
      in
      loop 0 state [] values
  | Parametric_value value ->
      let symbol, state =
        fresh_symbol state ~source_name:name
          ~sort:(Vir.Parametric value.parametric_sort) ~role:Vir.Result ~span
          ~project:false
      in
      let* term =
        match Parametric_logic_private.of_symbol symbol with
        | Ok term -> Ok term
        | Error message -> error name span (Malformed_sst message)
      in
      let* equation =
        match Parametric_logic_private.equal term value with
        | Ok equality -> Ok equality
        | Error message -> error name span (Malformed_sst message)
      in
      Ok (Vir.Parametric_result symbol, with_assumptions state [ equation ])
  | Aggregate_value value ->
      let symbol, state =
        fresh_symbol state ~source_name:name
          ~sort:(Vir.Aggregate value.aggregate_type) ~role:Vir.Result ~span
          ~project
      in
      let term =
        {
          Vir.aggregate_type = value.aggregate_type;
          aggregate_desc = Vir.Aggregate_symbol symbol;
        }
      in
      let state =
        with_assumptions state [ Vir.Aggregate_equal (term, value) ]
      in
      Ok (Vir.Aggregate_result symbol, state)
  | Function_value _ ->
      error name span
        (Malformed_sst
           "specification-function values cannot cross the runtime result ABI")
let rec value_of_result = function
  | Vir.Unit_result -> Unit_value
  | Vir.Integer_result symbol -> Integer_value (Vir.Integer_symbol symbol)
  | Vir.Boolean_result symbol -> Boolean_value (Vir.Boolean_symbol symbol)
  | Vir.Tuple_result components ->
      Tuple_value (List.map value_of_result components)
  | Vir.Parametric_result symbol -> (
      match Parametric_logic_private.of_symbol symbol with
      | Ok term -> Parametric_value term
      | Error _ -> assert false)
  | Vir.Aggregate_result symbol ->
      let aggregate_type =
        match symbol.Vir.sort with
        | Vir.Aggregate aggregate_type -> aggregate_type
        | Vir.Integer | Vir.Boolean | Vir.Parametric _ -> assert false
      in
      Aggregate_value
        { Vir.aggregate_type; aggregate_desc = Vir.Aggregate_symbol symbol }
let summary_of_callable descriptor =
  let definition = Sst_validation.callable_definition descriptor in
  let contract = Sst_validation.callable_contract descriptor in
  let contract_clause clause =
    {
      ordinal = Sst_validation.contract_clause_index clause;
      span = Sst_validation.contract_clause_span clause;
      binder = Sst_validation.contract_clause_binder clause;
      payload = Sst_validation.contract_clause_expression clause;
    }
  in
  let requires =
    List.map contract_clause (Sst_validation.contract_requires contract)
  and ensures =
    List.map contract_clause (Sst_validation.contract_ensures contract)
  and assertions =
    List.map contract_clause (Sst_validation.contract_assertions contract)
  in
  match definition.body with
  | Sst.Checked_exec { body = { expression = executable_body; _ }; _ }
  | Sst.Proof_body { body = { expression = executable_body; _ }; _ } ->
      Ok { definition; requires; ensures; assertions; executable_body }
  | Sst.Trusted_external_spec_target _ | Sst.Trusted_external_body _ ->
      Ok
        {
          definition;
          requires;
          ensures;
          assertions;
          executable_body =
            {
              Sst.expression_desc = Sst.Unit_constant;
              typ = Sst.Unit;
              span = definition.span;
            };
        }
  | Sst.External_specification (Sst.Imported_unverified_target _) ->
      let definition, executable_body =
        External_target_specification_use_private.executable_summary definition
      in
      Ok { definition; requires; ensures; assertions; executable_body }
  | Sst.Spec_definition _ | Sst.Recursive_spec_definition _
  | Sst.External_specification _ | Sst.Symbolic_declaration _ ->
      error definition.function_id.function_name definition.span
        (Malformed_sst "validated declaration has no checked executable body")
let termination_clause termination summary =
  match
    Termination.find_entry_intent termination summary.definition.Sst.function_id
  with
  | None -> None
  | Some intent ->
      let decrease = Termination.entry_measure intent in
      let clause = Sst_validation.decrease_clause decrease in
      Some
        ( Termination.entry_domain intent,
          {
            ordinal = Sst_validation.contract_clause_index clause;
            span = Sst_validation.contract_clause_span clause;
            binder = Sst_validation.contract_clause_binder clause;
            payload = Sst_validation.contract_clause_expression clause;
          } )
let lower_summary ?imports ?verification_session
    ?(finite_result_eligible = false) ?(finite_result_candidate = false)
    ?(finite_result_demand_sites = [])
    ?(formula_registry = [])
    ?(proof_entry_activations = fun (_ : Sst.function_id) -> []) validated
    rank_domains invariants type_definitions definitions summaries termination
    summary =
  let definition = summary.definition in
  let termination_clause = termination_clause termination summary in
  let function_ref = function_ref definition in
  let reached_callback_calls = ref [] in
  let* callable =
    match Sst_validation.find_callable validated definition.function_id with
    | Some callable -> Ok callable
    | None ->
        error function_ref.function_name definition.span
          (Malformed_sst "validated callable is absent from mode authority")
  in
  let* canonical_callable =
    match verification_session with
    | None -> Ok None
    | Some session -> (
        match
          Verification_session.canonical_callable_key session definition
        with
        | Ok callable -> Ok (Some callable)
        | Error message ->
            error function_ref.function_name definition.span
              (Malformed_sst message))
  in
  let* proof_activation_authority =
    match (definition.mode, verification_session) with
    | Sst.Proof, Some session -> (
        match
          Verification_session.proof_activation_authority session ~validated
            definition
        with
        | Ok authority -> Ok (Some authority)
        | Error message ->
            error function_ref.function_name definition.span
              (Malformed_sst message))
    | (Sst.Exec | Sst.Spec), (None | Some _) | Sst.Proof, None -> Ok None
  in
  let invariant_cell_authority =
    Option.map
      (fun session ->
        let hooks : Shared_invariant_cell_private.hooks =
          {
            entry_eligibility_issued =
              (fun () ->
                Verification_session.note_invariant_cell_entry_eligibility
                  session);
            constructor_eligibility_issued =
              (fun () ->
                Verification_session.note_invariant_cell_constructor_eligibility
                  session);
            closed_initialized =
              (fun () ->
                Verification_session.note_invariant_cell_closed_initialization
                  session);
            opened =
              (fun () -> Verification_session.note_invariant_cell_open session);
            updated =
              (fun () ->
                Verification_session.note_invariant_cell_update session);
            closed =
              (fun () -> Verification_session.note_invariant_cell_close session);
            effect_instantiated =
              (fun () ->
                Verification_session.note_invariant_cell_effect_instantiation
                  session);
            terminal_read =
              (fun () ->
                Verification_session.note_invariant_cell_terminal_read session);
            torn_down =
              (fun () ->
                Verification_session.note_invariant_cell_teardown session);
          }
        in
        let authority =
          Shared_invariant_cell_private.create ~hooks
            ~session_token:
              (Verification_session.shared_heap_session_token session)
            ~program_snapshot:
              (Verification_session.program_snapshot_digest session)
            ~session_active:(fun () -> Verification_session.is_active session)
            ~owner:definition.function_id
        in
        Verification_session.register_shared_heap_teardown session (fun () ->
            Shared_invariant_cell_private.destroy_if_live authority);
        authority)
      verification_session
  in
  let invariant_cell_descriptor type_id =
    match Type_invariant.find_for_type invariants type_id with
    | None -> None
    | Some handle ->
        Type_invariant.public_operations handle
        |> List.find_map (fun ((operation : Sst.function_id), role) ->
            if role <> Sst.Shared_invariant_transition then None
            else
              match List.assoc_opt operation.function_index definitions with
              | Some descriptor
                when (Sst_validation.callable_id descriptor).function_name
                     = operation.function_name -> (
                  match
                    Sst.function_shared_scalar_transitions
                      (Sst_validation.callable_definition descriptor)
                  with
                  | transition :: _ ->
                      Some
                        ( handle,
                          {
                            Shared_invariant_cell_private.abstract_type =
                              Type_invariant.abstract_type handle;
                            hidden_type = transition.shared_record_type;
                            field = transition.shared_target_field;
                            model = Type_invariant.model_callable handle;
                            invariant = Type_invariant.predicate_callable handle;
                          } )
                  | [] -> None)
              | Some _ | None -> None)
  in
  let shared_heap_owner = ref [] in
  let shared_entry_transitions =
    Sst.function_shared_scalar_transitions definition
  in
  let shared_entry_reads = ref [] in
  let make_context ~entry_environment ~logical =
    {
      validated;
      imports;
      function_ref;
      execution_definition = definition;
      current_definition = definition;
      current_callable = definition.function_id;
      canonical_callable;
      current_mode = definition.mode;
      definitions;
      summaries;
      termination;
      type_definitions;
      entry_environment;
      logical;
      old_environment = None;
      spec_call_stack = [];
      spec_expansion_limit = List.length definitions + 1;
      rank_domains;
      invariants;
      verification_session;
      proof_entry_activations = proof_entry_activations definition.function_id;
      finite_result_eligible;
      finite_result_candidate;
      finite_result_demand_sites;
      frozen_observation_call_path = [];
      frozen_observation_scope = None;
      owned_root_scalar_plan = None;
      owned_contents_stack = None;
      ground_retry_candidate_scope = false;
      shared_heap_view = Shared_scalar_heap_private.Current_view;
      shared_old_heap_view = None;
      shared_effect_base_epoch = 0;
      invariant_cell_authority;
      shared_heap_owner;
      shared_entry_transitions;
      shared_entry_reads;
      reached_callback_calls;
      callback_environment = [];
      formula_registry;
    }
  in
  let initial =
    {
      parametric_adts = (Sst_validation.program validated).parametric_adts;
      environment = [];
      owned_root_versions = [];
      owned_contents_origins = [];
      shared_scalar_heap = None;
      assumptions = [];
      immutable_aggregate_facts = [];
      local_invariant_assumptions = [];
      closed_invariant_facts = [];
      consumed_receipt_facts = [];
      finite_receipts = [];
      proof_call_visits = [];
      spent_proof_call_visits = [];
      proof_call_summaries = [];
      exported_proof_summary_facts = [];
      exported_local_assertion_facts = [];
      pending_finite_induction = None;
      required_preceding_safety = [];
      path_condition = [];
      projection_symbols = [];
      trusted_summary_uses = [];
      next_symbol = 0;
      next_obligation = 0;
      entry_measure = None;
      proof_activation_authority;
      proof_activations =
        (match definition.mode with
        | Sst.Proof -> proof_entry_activations definition.function_id
        | Sst.Exec | Sst.Spec -> []);
      reached_proof_activations = [];
      ground_constructor_matches = [];
      pending_ground_constructor_matches = [];
      frozen_terminal_observations = [];
    }
  in
  let rec parameters index obligations state = function
    | [] -> Ok (obligations, state)
    | Sst.Callback_parameter _ :: rest ->
        parameters (index + 1) obligations state rest
    | Sst.Value_parameter parameter :: rest ->
        let* value, state =
          fresh_parameter function_ref.function_name state index
            parameter.Sst.pattern
        in
        let* state, obligations =
          match parameter.optional_default with
          | None -> Ok (state, obligations)
          | Some optional_default -> (
              match value with
              | Aggregate_value aggregate ->
                  let logical = definition.mode = Sst.Proof in
                  let context =
                    make_context ~entry_environment:state.environment ~logical
                  in
                  let* environment, ranges, state, obligations =
                    resolve_optional_formal
                      ~evaluate_default:(fun state ->
                        evaluate context optional_default.optional_expression
                          state)
                      ~function_name:function_ref.function_name
                      ~span:parameter.pattern.span
                      ~environment:state.environment ~state ~obligations
                      ~carrier_type:parameter.pattern.typ optional_default
                      aggregate
                  in
                  Ok
                    ( with_assumptions { state with environment } ranges,
                      obligations )
              | _ ->
                  error function_ref.function_name parameter.pattern.span
                    (Malformed_sst
                       "optional-default formal has a non-carrier type"))
        in
        let* () =
          match
            ( invariant_cell_authority,
              parameter.Sst.pattern.pattern_desc,
              parameter.pattern.typ,
              value )
          with
          | ( Some authority,
              Sst.Bind formal,
              Sst.Aggregate type_id,
              Aggregate_value location ) -> (
              match invariant_cell_descriptor type_id with
              | Some (_, identity) -> (
                  match
                    Shared_invariant_cell_private.issue_entry authority
                      ~identity ~formal ~ordinal:index ~location
                  with
                  | Ok () -> Ok ()
                  | Error message ->
                      error function_ref.function_name parameter.pattern.span
                        (Malformed_sst message))
              | None -> Ok ())
          | (None | Some _), _, _, _ -> Ok ()
        in
        let* state =
          match
            ( invariant_for_typ invariants parameter.Sst.pattern.typ,
              Sst_validation.formal_has_closed_invariant_authority validated
                callable index )
          with
          | None, _ | Some _, false -> Ok state
          | Some handle, true -> (
              match closed_invariant_application handle value with
              | Ok closed -> Ok (with_closed_invariant_fact state closed)
              | Error message ->
                  error function_ref.function_name parameter.pattern.span
                    (Malformed_sst message))
        in
        let* state =
          match
            Sst_validation.finite_formal_requirement validated callable index
          with
          | None -> Ok state
          | Some requirement -> (
              match (verification_session, canonical_callable, value) with
              | Some session, Some callable_key, Aggregate_value aggregate -> (
                  let certificate =
                    Sst_validation.finite_formal_rank_domain requirement
                  in
                  let* domain =
                    match
                      List.find_opt
                        (fun domain ->
                          String.equal
                            (Vir.rank_domain_id domain)
                            (Sst_validation.rank_domain_id certificate)
                          && String.equal
                               (Vir.rank_domain_version domain)
                               (Sst_validation.rank_domain_version certificate)
                          && String.equal
                               (Vir.rank_domain_digest domain)
                               (Sst_validation.rank_snapshot_digest certificate))
                        rank_domains
                    with
                    | Some domain -> Ok domain
                    | None ->
                        error function_ref.function_name parameter.pattern.span
                          (Malformed_sst
                             "finite formal entry rank domain is absent or \
                              stale")
                  in
                  let rank =
                    finite_rank_snapshot_of_certificate domain certificate
                  in
                  let* registry =
                    match Verification_session.finite_registry session with
                    | Ok registry -> Ok registry
                    | Error message ->
                        error function_ref.function_name parameter.pattern.span
                          (Malformed_sst message)
                  in
                  let* slot =
                    match
                      Finite_value_registry.find_formal registry
                        ~callee:callable_key ~ordinal:index
                    with
                    | Some slot -> Ok slot
                    | None ->
                        error function_ref.function_name parameter.pattern.span
                          (Malformed_sst
                             "finite formal entry slot is absent or stale")
                  in
                  let mode = Sst_validation.finite_formal_mode requirement in
                  match
                    Finite_value_registry.assume_formal registry ~slot
                      ~callable:callable_key ~value:aggregate ~mode
                      ~typ:parameter.pattern.typ ~rank
                  with
                  | Ok receipt -> Ok (with_finite_receipt state receipt)
                  | Error message ->
                      error function_ref.function_name parameter.pattern.span
                        (Malformed_sst message))
              | ( _,
                  _,
                  ( Unit_value | Integer_value _ | Boolean_value _
                  | Tuple_value _ | Aggregate_value _ | Parametric_value _ | Function_value _ ) )
                ->
                  error function_ref.function_name parameter.pattern.span
                    (Malformed_sst
                       "finite formal entry requires the private session and \
                        an exact aggregate formal"))
        in
        let* () =
          match
            Sst_validation.frozen_formal_requirement validated callable index
          with
          | None -> Ok ()
          | Some requirement -> (
              let type_id = Sst_validation.frozen_formal_type requirement in
              match
                ( verification_session,
                  parameter.Sst.pattern.pattern_desc,
                  parameter.pattern.typ,
                  value )
              with
              | ( Some session,
                  Sst.Bind formal,
                  Sst.Aggregate actual_type,
                  Aggregate_value root )
                when actual_type = type_id -> (
                  match
                    Verification_session.register_frozen_formal_scope session
                      ~definition ~formal ~ordinal:index ~root
                      ~path_condition:state.path_condition
                  with
                  | Ok () -> Ok ()
                  | Error message ->
                      error function_ref.function_name parameter.pattern.span
                        (Malformed_sst message))
              | (Some _ | None), _, _, _ ->
                  error function_ref.function_name parameter.pattern.span
                    (Malformed_sst
                       "frozen-spine formal has no exact conditional scope"))
        in
        parameters (index + 1) obligations state rest
  in
  let* parameter_obligations, state =
    parameters 0 [] initial definition.parameters
  in
  let* captures =
    Sst_callback_private.authenticated_captures
      (Sst_validation.program validated)
      definition
    |> Result.map_error (fun (span, message) ->
        {
          function_name = function_ref.function_name;
          span;
          unsupported = Malformed_sst message;
        })
  in
  let* state =
    List.fold_left
      (fun state ((capture : Callback_certificate_private.capture), binding) ->
        let* state = state in
        let* value, state =
          fresh_value state ~source_name:binding.Sst.name ~role:Vir.Input
            ~span:binding.span ~project:true capture.typ
        in
        Ok { state with environment = (binding.id, value) :: state.environment })
      (Ok state) captures
  in
  let entry_environment = state.environment in
  let shared_invariant_handle =
    match
      ( Type_invariant.find_for_operation invariants definition.function_id,
        shared_entry_transitions )
    with
    | Some (handle, Sst.Shared_invariant_transition), transition :: _
      when Option.is_none
             (frozen_spine_for_transition validated definition.function_id
                transition) ->
        Some handle
    | Some (_, Sst.Shared_invariant_transition), (_ :: _ | [])
    | Some (_, _), _
    | None, _ ->
        None
  in
  let shared_invariant_final_epoch =
    match (shared_invariant_handle, List.rev shared_entry_transitions) with
    | Some _, final :: _ -> Some final.Sst.shared_successor_epoch
    | Some _, [] -> None
    | None, _ -> None
  in
  let logical_context = make_context ~entry_environment ~logical:true in
  let runtime_context = { logical_context with logical = false } in
  let body_context =
    match definition.mode with
    | Sst.Proof -> { logical_context with ground_retry_candidate_scope = true }
    | Sst.Exec -> runtime_context
    | Sst.Spec -> assert false
  in
  let rec assume_requires obligations states = function
    | [] -> Ok { obligations; paths = states }
    | clause :: rest ->
        let* evaluated =
          evaluate_contexts
            (fun state ->
              let current_environment = state.environment in
              let* predicate = evaluate_contract_formula_root logical_context definition Logical_spec_evaluation_private.Requires clause.ordinal clause.payload state in
              let* paths =
                let rec loop paths = function
                  | [] -> Ok (List.rev paths)
                  | evaluated :: remaining ->
                      let* assumption =
                        expect_boolean function_ref.function_name clause.span
                          evaluated.value
                      in
                      let state =
                        with_assumptions evaluated.state [ assumption ]
                        |> restore_environment current_environment
                      in
                      loop (state :: paths) remaining
                in
                loop [] predicate.paths
              in
              Ok { obligations = predicate.obligations; paths })
            states
        in
        assume_requires
          (append obligations evaluated.obligations)
          evaluated.paths rest
  in
  let* required =
    assume_requires parameter_obligations [ state ] summary.requires
  in
  let* measured =
    match termination_clause with
    | None -> Ok required
    | Some (domain, measure) ->
        let* evaluated =
          evaluate_contexts
            (fun state ->
              let current_environment = state.environment in
              let* measured =
                evaluate runtime_context measure.payload
                  { state with environment = entry_environment }
              in
              let* paths =
                let rec loop paths = function
                  | [] -> Ok (List.rev paths)
                  | measured :: remaining ->
                      let* entry_measure =
                        ranked_measure function_ref.function_name measure.span
                          runtime_context.rank_domains domain measured.value
                      in
                      let measured_state =
                        restore_environment current_environment measured.state
                      in
                      let nonnegative =
                        Vir.Integer_compare
                          ( Vir.Less_or_equal,
                            Vir.Integer_constant Z.zero,
                            entry_measure )
                      in
                      let obligation, measured_state =
                        emit_goal function_ref
                          (Vir.Entry_measure_nonnegative
                             { declaration_span = measure.span })
                          measure.span nonnegative measured_state
                      in
                      let measured_state =
                        match domain with
                        | Termination.Structural_rank _ ->
                            with_assumptions measured_state [ nonnegative ]
                        | Termination.Integer_height
                        | Termination.Parametric_direct_edge _
                        | Termination.Frozen_spine_direct_edge _ ->
                            measured_state
                      in
                      let measured_state =
                        {
                          measured_state with
                          entry_measure = Some (domain, entry_measure);
                        }
                      in
                      loop ((obligation, measured_state) :: paths) remaining
                in
                loop [] measured.paths
              in
              let obligations, states = List.split paths in
              Ok
                {
                  obligations = append measured.obligations obligations;
                  paths = states;
                })
            required.paths
        in
        Ok
          {
            obligations = append required.obligations evaluated.obligations;
            paths = evaluated.paths;
          }
  in
  let rec prove_assertions obligations states = function
    | [] -> Ok { obligations; paths = states }
    | clause :: rest ->
        let* evaluated =
          evaluate_contexts
            (fun state ->
              let current_environment = state.environment in
              let* predicate = evaluate logical_context clause.payload state in
              let* paths =
                let rec loop paths = function
                  | [] -> Ok (List.rev paths)
                  | evaluated :: remaining ->
                      let* goal =
                        expect_boolean function_ref.function_name clause.span
                          evaluated.value
                      in
                      let obligation, state =
                        emit_goal function_ref
                          (Vir.Assertion { assertion_ordinal = clause.ordinal })
                          clause.span goal evaluated.state
                      in
                      loop
                        (( obligation,
                           restore_environment current_environment state )
                        :: paths)
                        remaining
                in
                loop [] predicate.paths
              in
              let emitted, states = List.split paths in
              Ok
                {
                  obligations = append predicate.obligations emitted;
                  paths = states;
                })
            states
        in
        prove_assertions
          (append obligations evaluated.obligations)
          evaluated.paths rest
  in
  let* asserted =
    prove_assertions measured.obligations measured.paths summary.assertions
  in
  let* opened =
    match (shared_invariant_handle, shared_entry_transitions) with
    | Some handle, transition :: _ ->
        let* cell =
          match transition.Sst.shared_formal_roots with
          | [ formal ] -> (
              match List.assoc_opt formal.id entry_environment with
              | Some (Aggregate_value cell) -> Ok (Aggregate_value cell)
              | Some _ | None ->
                  error function_ref.function_name formal.span
                    (Malformed_sst
                       "shared invariant operation formal is not an exact \
                        aggregate"))
          | [] | _ :: _ ->
              error function_ref.function_name definition.span
                (Malformed_sst
                   "shared invariant operation does not have one exact cell \
                    formal")
        in
        let* () =
          match (invariant_cell_authority, cell) with
          | Some authority, Aggregate_value location -> (
              match
                invariant_cell_descriptor (Type_invariant.abstract_type handle)
              with
              | Some (_, identity) -> (
                  match
                    Shared_invariant_cell_private.open_cell authority ~identity
                      ~operation:definition.function_id ~location ~entry_epoch:0
                  with
                  | Ok () -> Ok ()
                  | Error message ->
                      error function_ref.function_name definition.span
                        (Malformed_sst message))
              | None ->
                  error function_ref.function_name definition.span
                    (Malformed_sst
                       "shared invariant operation lost its exact descriptor"))
          | None, _ -> Ok ()
          | Some _, _ ->
              error function_ref.function_name definition.span
                (Malformed_sst
                   "shared invariant operation cell changed from aggregate")
        in
        let assume_entry state =
          let* predicate =
            evaluate_invariant_predicate logical_context handle cell state
          in
          let rec assume paths = function
            | [] -> Ok (List.rev paths)
            | evaluated :: rest ->
                let* fact =
                  expect_boolean function_ref.function_name definition.span
                    evaluated.value
                in
                assume (with_assumptions evaluated.state [ fact ] :: paths) rest
          in
          let* paths = assume [] predicate.paths in
          Ok { obligations = predicate.obligations; paths }
        in
        let* opened = evaluate_contexts assume_entry asserted.paths in
        Ok
          {
            obligations = append asserted.obligations opened.obligations;
            paths = opened.paths;
          }
    | None, [] | None, _ :: _ -> Ok asserted
    | Some _, [] ->
        error function_ref.function_name definition.span
          (Malformed_sst
             "shared invariant operation has no body-derived hidden effect")
  in
  let* body =
    evaluate_contexts
      (fun state -> evaluate body_context summary.executable_body state)
      opened.paths
  in
  let evaluated_obligations = append opened.obligations body.obligations in
  let* body =
    match (shared_invariant_handle, shared_entry_transitions) with
    | Some handle, transition :: _ ->
        let final_epoch =
          match shared_invariant_final_epoch with
          | Some final_epoch -> final_epoch
          | None -> assert false
        in
        let* cell =
          match transition.shared_formal_roots with
          | [ formal ] -> (
              match List.assoc_opt formal.id entry_environment with
              | Some value -> Ok value
              | None ->
                  error function_ref.function_name formal.span
                    (Malformed_sst
                       "shared invariant close lost its exact cell formal"))
          | [] | _ :: _ ->
              error function_ref.function_name definition.span
                (Malformed_sst
                   "shared invariant close has an invalid cell formal set")
        in
        let close evaluated =
          let body_value = evaluated.value in
          let* closed =
            prove_invariant_validity runtime_context handle
              (Vir.Shared_invariant_close { entry_epoch = 0; final_epoch })
              function_ref definition.span cell evaluated.state
          in
          Ok
            {
              obligations = closed.obligations;
              paths =
                List.map
                  (fun closed -> { value = body_value; state = closed.state })
                  closed.paths;
            }
        in
        evaluate_contexts close body.paths
    | None, [] | None, _ :: _ -> Ok { obligations = []; paths = body.paths }
    | Some _, [] -> assert false
  in
  let evaluated_obligations = append evaluated_obligations body.obligations in
  let prove_postconditions evaluated =
    let* return_obligations, state =
      match
        ( invariant_for_typ invariants definition.result_type,
          Sst_validation.result_instance_mode validated callable )
      with
      | None, _ | Some _, Sst.Ghost_instance -> Ok ([], evaluated.state)
      | Some handle, (Sst.Exec_instance | Sst.Tracked_instance) ->
          let* verified_assumptions =
            let* closed =
              match closed_invariant_application handle evaluated.value with
              | Ok closed -> Ok closed
              | Error message ->
                  error function_ref.function_name definition.span
                    (Malformed_sst message)
            in
            if List.mem closed evaluated.state.closed_invariant_facts then Ok []
            else
              match
                ( Type_invariant.find_for_operation invariants
                    definition.function_id,
                  verification_session )
              with
              | Some (_, Sst.Unique_transition), Some session -> (
                  let* ordinal, formal, root_value =
                    definition.parameters
                    |> List.mapi (fun ordinal parameter -> (ordinal, parameter))
                    |> List.find_map (fun (ordinal, parameter) ->
                        let parameter = Sst.require_value_parameter parameter in
                        match parameter.Sst.pattern.pattern_desc with
                        | Sst.Bind binding -> (
                            match
                              List.assoc_opt binding.id entry_environment
                            with
                            | Some (Aggregate_value aggregate)
                              when evaluated.value = Aggregate_value aggregate
                              ->
                                Some (ordinal, binding, aggregate)
                            | Some _ | None -> None)
                        | Sst.Wildcard | Sst.Unit_pattern | Sst.Tuple_pattern _
                        | Sst.Record_pattern _ | Sst.Constructor_pattern _
                        | Sst.Int_pattern _ | Sst.Bool_pattern _
                        | Sst.Owned_tree_cursor_pattern _ | Sst.Or_pattern _ ->
                            None)
                    |> function
                    | Some exact -> Ok exact
                    | None ->
                        error function_ref.function_name definition.span
                          (Malformed_sst
                             "transition return has neither a preserved \
                              successor nor an exact predecessor")
                  in
                  let mode =
                    Sst_validation.formal_instance_mode validated callable
                      ordinal
                      (List.nth definition.parameters ordinal)
                  in
                  match
                    Verification_session.consume_transition_predecessors session
                      ~validated ~invariants ~callee:definition ~formal
                      ~root:formal ~root_value ~mode ~typ:formal.typ
                      ~owned_version:
                        (owned_root_version evaluated.state formal root_value)
                      ~obligation_snapshot:
                        (transition_obligation_snapshot definition handle)
                  with
                  | Ok () -> Ok [ closed ]
                  | Error message ->
                      error function_ref.function_name definition.span
                        (Malformed_sst message))
              | Some (_, _), (None | Some _) | None, (None | Some _) -> Ok []
          in
          let* obligation, state =
            emit_closed_invariant_goal ~verified_assumptions logical_context
              handle Vir.Function_return function_ref definition.span
              evaluated.value evaluated.state
          in
          Ok ([ obligation ], state)
    in
    let* result, state =
      materialize_result ~project:(summary.ensures <> []) state
        summary.executable_body.span "result" evaluated.value
    in
    let result_value = value_of_result result in
    let state =
      match (evaluated.value, result_value) with
      | Aggregate_value source, Aggregate_value materialized -> (
          let state =
            match
              List.find_map
                (fun (binding_id, candidate, version) ->
                  if candidate = source then Some (binding_id, version)
                  else None)
                state.owned_root_versions
            with
            | Some (binding_id, version) ->
                {
                  state with
                  owned_root_versions =
                    (binding_id, materialized, version)
                    :: state.owned_root_versions;
                }
            | None -> state
          in
          match find_owned_contents_origin state source with
          | Some origin ->
              replace_owned_contents_origin state materialized origin
          | None -> state)
      | ( ( Unit_value | Integer_value _ | Boolean_value _ | Tuple_value _
          | Aggregate_value _ | Parametric_value _ | Function_value _ ),
          ( Unit_value | Integer_value _ | Boolean_value _ | Tuple_value _
          | Aggregate_value _ | Parametric_value _ | Function_value _ ) ) ->
          state
    in
    let* state =
      match verification_session with
      | None -> Ok state
      | Some session ->
          Finite_result_integration.record_materialized_exit logical_context
            ~eligible:finite_result_eligible ~validated ~callable ~definition
            ~session ~source_value:evaluated.value ~result_value state
    in
    let rec prove obligations states = function
      | [] ->
          Ok
            {
              obligations;
              paths =
                List.map
                  (fun state ->
                    {
                      Vir.assumptions = state.assumptions;
                      path_condition = state.path_condition;
                      result;
                      projection_symbols = state.projection_symbols;
                      trusted_summary_uses = state.trusted_summary_uses;
                    })
                  states;
            }
      | clause :: rest ->
          let* evaluated =
            evaluate_contexts
              (fun state ->
                let current_environment = state.environment in
                let* environment, binder_ranges =
                  match clause.binder with
                  | None -> Ok (current_environment, [])
                  | Some binder ->
                      bind_pattern_direct ~parametric_adts:state.parametric_adts
                        function_ref.function_name current_environment binder
                        result_value
                in
                let post_context =
                  {
                    logical_context with
                    old_environment = Some logical_context.entry_environment;
                  }
                in
                let* predicate =
                  evaluate_contract_formula_root post_context definition Logical_spec_evaluation_private.Ensures clause.ordinal clause.payload
                    (with_assumptions { state with environment } binder_ranges)
                in
                let* paths =
                  let rec loop paths = function
                    | [] -> Ok (List.rev paths)
                    | evaluated :: remaining ->
                        let* goal =
                          expect_boolean function_ref.function_name clause.span
                            evaluated.value
                        in
                        let obligation, state =
                          emit_goal function_ref
                            (Vir.Postcondition
                               {
                                 postcondition_ordinal = clause.ordinal;
                                 declaration_span = clause.span;
                               })
                            clause.span goal evaluated.state
                        in
                        loop
                          (( obligation,
                             restore_environment current_environment state )
                          :: paths)
                          remaining
                  in
                  loop [] predicate.paths
                in
                let emitted, states = List.split paths in
                Ok
                  {
                    obligations = append predicate.obligations emitted;
                    paths = states;
                  })
              states
          in
          prove (append obligations evaluated.obligations) evaluated.paths rest
    in
    prove return_obligations [ state ] summary.ensures
  in
  let* postconditions = evaluate_contexts prove_postconditions body.paths in
  let broadcast_malformed span message =
    {
      function_name = function_ref.function_name;
      span;
      unsupported = Malformed_sst message;
    }
  in
  let broadcast_lowering =
    Broadcast_vc_private.make_evaluator_lowering
      ~error:broadcast_malformed
      ~aggregate_of_type:
        (vir_aggregate_type_of_sst initial.parametric_adts)
      ~integer_value:(fun term -> Integer_value term)
      ~boolean_value:(fun term -> Boolean_value term)
      ~parametric_value:(fun term -> Parametric_value term)
      ~spec_function_value:(fun function_arrow function_term ->
        Function_value
          {
            Logical_spec_evaluation_private.function_term;
            function_arrow;
            function_closure =
              Logical_spec_evaluation_private.Abstract_function;
          })
      ~aggregate_value:(fun term -> Aggregate_value term)
      ~map_expression:(fun substitution ->
        Sst.map_expression_types (Parametric_type.substitute substitution))
      ~environment:(fun state -> state.environment)
      ~reset:(fun environment ->
          {
            initial with
            environment;
            assumptions = [];
            path_condition = [];
            projection_symbols = [];
            proof_activations = [];
            reached_proof_activations = [];
          })
      ~context:(fun environment ->
        make_context ~entry_environment:environment ~logical:true)
      ~evaluate_formula:(fun context expression state ->
        evaluate_logical_formula evaluate context function_ref.function_name
          expression state)
      ~prepare_ensure:(fun state binder ->
          let outer_environment = state.environment in
          let* environment, ranges =
            match binder with
            | None -> Ok (outer_environment, [])
            | Some binder ->
                bind_pattern_direct ~parametric_adts:state.parametric_adts
                  function_ref.function_name outer_environment binder Unit_value
          in
          Ok (with_assumptions { state with environment } ranges))
      ~restore:restore_environment
      ~evaluate_trigger_formula:(evaluate_explicit_trigger evaluate)
  in
  (* Branches copy immutable allocator state.  Renumbering the final traversal
     gives every emitted VC a deterministic function-local identity without a
     mutable global counter or branch-order side channel. *)
  let* obligations, proof_activation_members, proof_activation_manifests =
    let rec finalize obligation_index obligations members manifests = function
      | [] -> Ok (List.rev obligations, List.rev members, List.rev manifests)
      | emitted :: rest ->
          let* obligation =
            Broadcast_vc_private.materialize_and_attach
              ~program:(Sst_validation.program validated)
              ~function_id:definition.function_id
              ~descriptors:
                (Sst_validation.program validated).parametric_adts
              ~obligation:
                {
                  emitted.provisional_obligation with
                  Vir.obligation_index;
                }
              ~build:(fun theorem vector ->
                Broadcast_vc_private.build_quantifier broadcast_lowering theorem
                  vector
                |> Result.map_error (fun error ->
                       match error.unsupported with
                       | Malformed_sst message -> message
                       | Ghost_call | Or_pattern | Missing_summary _
                       | Recursion_awaits_totality _ | Missing_decreases
                       | Duplicate_decreases | Inapplicable_decreases
                       | Non_integer_decreases ->
                           "broadcast theorem lowering failed"))
              ~map_error:
                (broadcast_malformed emitted.provisional_obligation.span)
          in
          let* manifest =
            let snapshot =
              match !proof_activation_snapshot_attack_for_testing with
              | Some "missing-snapshot"
                when Option.is_some emitted.expected_proof_activation_authority
                ->
                  None
              | Some _ | None -> emitted.proof_activation_snapshot
            in
            match
              ( verification_session,
                emitted.expected_proof_activation_authority,
                snapshot )
            with
            | Some session, Some _authority, Some snapshot -> (
                match
                  Verification_session.finalize_proof_activation session
                    snapshot obligation
                with
                | Ok manifest -> Ok (Some manifest)
                | Error message ->
                    error function_ref.function_name obligation.span
                      (Malformed_sst message))
            | Some _, Some _, None ->
                error function_ref.function_name obligation.span
                  (Malformed_sst
                     "scoped proof obligation lacks reached activation snapshot")
            | Some _, None, None when definition.mode = Sst.Proof ->
                error function_ref.function_name obligation.span
                  (Malformed_sst
                     "Proof obligation lacks reached activation authority")
            | (None | Some _), None, None -> Ok None
            | _, None, Some _ | None, Some _, (None | Some _) ->
                error function_ref.function_name obligation.span
                  (Malformed_sst
                     "Proof activation snapshot lacks its authority/session")
          in
          finalize (obligation_index + 1)
            (obligation :: obligations)
            (match emitted.expected_proof_activation_authority with
            | Some authority -> (authority, obligation) :: members
            | None -> members)
            (match manifest with
            | Some manifest -> manifest :: manifests
            | None -> manifests)
            rest
    in
    let emitted = append evaluated_obligations postconditions.obligations in
    if
      List.exists
        (fun emitted ->
          Option.is_some emitted.expected_proof_activation_authority)
        emitted
    then
      Option.iter
        (fun observer -> observer ())
        !proof_activation_prefinalizer_observer_for_testing;
    finalize 0 [] [] [] emitted
  in
  let body_provenance =
    match definition.body with
    | Sst.Checked_exec { provenance; _ } -> provenance
    | Sst.Proof_body { provenance; _ } -> provenance
    | _ -> assert false
  in
  let trusted_summary_uses =
    let add uses use =
      if List.mem use uses then uses else append uses [ use ]
    in
    let dynamic =
      List.fold_left
        (fun uses (exit : Vir.exit) ->
          List.fold_left add uses exit.trusted_summary_uses)
        [] postconditions.paths
    in
    let direct =
      List.fold_left add dynamic
        (External_target_specification_use_private.collect ~validated
           ~caller:definition.function_id)
    in
    obligations
    |> List.fold_left
         (fun uses (obligation : Vir.obligation) ->
           Broadcast_vc_private.report obligation
           |> Option.fold ~none:uses
                ~some:(fun (report : Broadcast_vc_private.report) ->
                  report.inserted
                  |> List.fold_left
                       (fun uses
                            (inserted : Broadcast_vc_private.inserted) ->
                         match (inserted.trusted, inserted.witness_span) with
                         | true, Some witness_span ->
                             [%log.debug "retain trusted broadcast axiom use"
                               ~broadcast_id:
                                 (Delator.Field.string inserted.broadcast_id)
                               ~function_name:
                                 (Delator.Field.string
                                    inserted.theorem_function_id.function_name)
                               ~obligation_index:
                                 (Delator.Field.int
                                    obligation.Vir.obligation_index)];
                             add uses
                               (Vir.Trusted_external_body_use
                                  {
                                    function_ref =
                                      vir_function_ref
                                        inserted.theorem_function_id;
                                    mode = Sst.Proof;
                                    call_form = Sst.Proof_call;
                                    broadcast_use = true;
                                    declaration_span =
                                      inserted.declaration_span;
                                    witness_span;
                                    call_span = definition.span;
                                    requires_count = inserted.requires_count;
                                    ensures_count = inserted.ensures_count;
                                  })
                         | false, _ | true, None -> uses)
                       uses))
         direct
  in
  let* shared_scalar_heap_reads, shared_scalar_heap_writes =
    let rec collect reads writes = function
      | [] -> Ok (List.rev reads, List.rev writes)
      | heap :: rest -> (
          let heap_reads =
            Shared_scalar_heap_private.read_events heap
            |> List.map (fun (event : Shared_scalar_heap_private.read_event) ->
                {
                  Vir.shared_read_field = event.read_field;
                  shared_read_path_id = event.read_path_id;
                  shared_read_epoch = event.read_epoch;
                  shared_read_location = event.read_location;
                  shared_read_term = event.read_term;
                  shared_read_entry_view = event.read_entry_view;
                })
          in
          let heap_writes =
            Shared_scalar_heap_private.write_events heap
            |> List.map (fun (event : Shared_scalar_heap_private.write_event) ->
                {
                  Vir.shared_write_transition = event.write_transition;
                  shared_write_location = event.write_location;
                  shared_write_value = event.write_value;
                })
          in
          match Shared_scalar_heap_private.destroy heap with
          | Ok () ->
              collect
                (List.rev_append heap_reads reads)
                (List.rev_append heap_writes writes)
                rest
          | Error message ->
              error function_ref.function_name definition.span
                (Malformed_sst message))
    in
    let* reads, writes = collect [] [] !shared_heap_owner in
    Ok (List.rev !shared_entry_reads @ reads, writes)
  in
  let provisional_invariant_cell_close =
    match
      ( invariant_cell_authority,
        shared_invariant_handle,
        shared_invariant_final_epoch )
    with
    | Some authority, Some _, Some final_epoch ->
        Some { authority; operation = definition.function_id; final_epoch }
    | (None | Some _), (None | Some _), (None | Some _) -> None
  in
  let* () =
    match (invariant_cell_authority, provisional_invariant_cell_close) with
    | None, _ | Some _, Some _ -> Ok ()
    | Some authority, None -> (
        match Shared_invariant_cell_private.destroy authority with
        | Ok () -> Ok ()
        | Error message ->
            error function_ref.function_name definition.span
              (Malformed_sst message))
  in
  let execution =
    {
      Vir.function_ref;
      mode = definition.mode;
      body_provenance;
      policy = definition.policy;
      trusted_summary_uses;
      reached_callback_calls = List.rev !reached_callback_calls;
      owned_tree_transitions = owned_tree_transitions summary.executable_body;
      shared_scalar_heap_reads;
      shared_scalar_heap_writes;
      obligations;
      exits = postconditions.paths;
    }
  in
  let proof_activation_batch =
    Option.map
      (fun session ->
        Verification_session.proof_activation_batch session
          proof_activation_members proof_activation_manifests)
      verification_session
  in
  Ok { execution; proof_activation_batch; provisional_invariant_cell_close }
let error_of_termination termination_error =
  let function_name =
    Option.fold ~none:"program"
      ~some:(fun id -> id.Sst.function_name)
      termination_error.Termination.function_id
  in
  let unsupported =
    match termination_error.kind with
    | Termination.Missing_measure -> Missing_decreases
    | Termination.Duplicate_measure -> Duplicate_decreases
    | Termination.Inapplicable_measure -> Inapplicable_decreases
    | Termination.Non_integer_measure -> Non_integer_decreases
    | Termination.Recursive_measure | Termination.Unsupported_mutual_scc _
    | Termination.Unsupported_recursive_mode _ | Termination.Raw_analysis_only
      ->
        Malformed_sst (Termination.error_to_string termination_error)
  in
  error function_name termination_error.span unsupported
let prepare_termination validated =
  match
    Sst_validation_private.Public.with_owned_contents_helpers_hidden (fun () ->
        Termination.prepare (Termination.analyze validated))
  with
  | Ok termination -> Ok termination
  | Error termination -> error_of_termination termination
let prepare_invariants validated =
  match Type_invariant.authenticate validated with
  | Ok invariants -> Ok invariants
  | Error invariant ->
      error "program" invariant.span
        (Malformed_sst (Type_invariant.error_to_string invariant))
let is_legacy_recursive_graph_error = function
  | Sst_validation.Invalid_call detail ->
      String.equal detail "cyclic pure specification call graph"
      || String.equal detail "cyclic proof call graph"
  | _ -> false
let validate_with_raw_termination (program : Sst.program) =
  let raw_termination = Termination.analyze_raw program in
  match Sst_validation.validate program with
  | Ok validated -> Ok validated
  | Error validation when is_legacy_recursive_graph_error validation.kind -> (
      match Termination.precheck raw_termination with
      | Error termination -> error_of_termination termination
      | Ok () ->
          error
            (Option.fold ~none:"program"
               ~some:(fun id -> id.Sst.function_name)
               validation.function_id)
            validation.span
            (Malformed_sst (Sst_validation.error_to_string validation)))
  | Error validation ->
      error
        (Option.fold ~none:"program"
           ~some:(fun id -> id.Sst.function_name)
           validation.function_id)
        validation.span
        (Malformed_sst (Sst_validation.error_to_string validation))
let lower_function (definition : Sst.function_definition) =
  let program =
    {
      Sst.policy = definition.policy;
      parametric_adts = [];
      types = [];
      functions = [ definition ];
    }
  in
  let* validated = validate_with_raw_termination program in
  let* termination = prepare_termination validated in
  let* invariants = prepare_invariants validated in
  let declarations = Sst_validation.callable_descriptors validated in
  let* descriptor =
    match Sst_validation.find_callable validated definition.function_id with
    | Some descriptor -> Ok descriptor
    | None ->
        error definition.function_id.function_name definition.span
          (Malformed_sst "validated callable is absent from environment")
  in
  let* summary = summary_of_callable descriptor in
  let definitions =
    List.map
      (fun descriptor ->
        ((Sst_validation.callable_id descriptor).function_index, descriptor))
      declarations
  in
  let* summaries =
    let rec loop lowered = function
      | [] -> Ok (List.rev lowered)
      | descriptor :: rest ->
          let definition = Sst_validation.callable_definition descriptor in
          let* summary = summary_of_callable descriptor in
          loop
            ((definition.Sst.function_id.function_index, summary) :: lowered)
            rest
    in
    loop [] declarations
  in
  let* lowered =
    lower_summary validated [] invariants [] definitions summaries termination
      summary
  in
  Ok lowered.execution
let lower_program (program : Sst.program) =
  let* validated = validate_with_raw_termination program in
  let* termination = prepare_termination validated in
  let* invariants = prepare_invariants validated in
  let descriptor_declarations = Sst_validation.callable_descriptors validated in
  let declarations =
    List.map Sst_validation.callable_definition descriptor_declarations
  in
  let type_definitions =
    List.map Sst_validation.type_definition
      (Sst_validation.type_descriptors validated)
  in
  let rank_domains = Vir.rank_domains_of_validated validated in
  let definitions =
    List.map
      (fun descriptor ->
        ((Sst_validation.callable_id descriptor).function_index, descriptor))
      descriptor_declarations
  in
  let* summaries =
    let rec loop lowered = function
      | [] -> Ok (List.rev lowered)
      | descriptor :: rest -> (
          let definition = Sst_validation.callable_definition descriptor in
          match definition.Sst.body with
          | Sst.Checked_exec _ ->
              let* summary = summary_of_callable descriptor in
              loop
                ((definition.Sst.function_id.function_index, summary) :: lowered)
                rest
          | Sst.Spec_definition _ | Sst.Recursive_spec_definition _
          | Sst.External_specification _ | Sst.Symbolic_declaration _ ->
              loop lowered rest
          | Sst.Proof_body _ ->
              let* summary = summary_of_callable descriptor in
              loop
                ((definition.Sst.function_id.function_index, summary) :: lowered)
                rest
          | Sst.Trusted_external_spec_target _ ->
              let* summary = summary_of_callable descriptor in
              loop
                ((definition.Sst.function_id.function_index, summary) :: lowered)
                rest
          | Sst.Trusted_external_body _ ->
              let* summary = summary_of_callable descriptor in
              loop
                ((definition.Sst.function_id.function_index, summary) :: lowered)
                rest)
    in
    loop [] descriptor_declarations
  in
  let rec loop functions = function
    | [] ->
        let trusted_external_body_declarations =
          List.filter_map
            (fun definition ->
              match definition.Sst.body with
              | Sst.Trusted_external_body
                  (Sst.Authenticated_external_body
                     { declaration_span; witness_span; _ }) ->
                  Some
                    {
                      Vir.function_ref = vir_function_ref definition.function_id;
                      mode = definition.mode;
                      declaration_span;
                      witness_span;
                      requires_count = List.length definition.contracts.requires;
                      ensures_count = List.length definition.contracts.ensures;
                    }
              | Sst.Trusted_external_body (Sst.Raw_external_body _) ->
                  assert false
              | _ -> None)
            declarations
        in
        Ok
          {
            Vir.policy = program.policy;
            rank_domains;
            trusted_external_body_declarations;
            functions = List.rev functions;
          }
    | definition :: rest -> (
        match definition.Sst.body with
        | Sst.Spec_definition _ | Sst.Recursive_spec_definition _
        | Sst.External_specification _ | Sst.Trusted_external_spec_target _
        | Sst.Symbolic_declaration _ ->
            loop functions rest
        | Sst.Checked_exec _ | Sst.Proof_body _ ->
            let* summary =
              match
                List.assoc_opt definition.Sst.function_id.function_index
                  summaries
              with
              | Some summary -> Ok summary
              | None ->
                  error definition.function_id.function_name definition.span
                    (Malformed_sst "validated callable is absent from registry")
            in
            let* lowered =
              lower_summary validated rank_domains invariants type_definitions
                definitions summaries termination summary
            in
            loop (lowered.execution :: functions) rest
        | Sst.Trusted_external_body _ -> loop functions rest)
  in
  loop [] declarations
let invariant_cell_expression_children =
  Sst_callback_private.expression_children
let invariant_cell_transition_prerequisites invariants
    (definition : Sst.function_definition) =
  let rec collect prerequisites (expression : Sst.expression) =
    let prerequisites =
      match expression.expression_desc with
      | Sst.Direct_call { callee; _ } -> (
          match Type_invariant.find_for_operation invariants callee with
          | Some (_, Sst.Shared_invariant_transition) ->
              callee.function_index :: prerequisites
          | Some
              ( _,
                ( Sst.Abstract_constructor | Sst.Abstract_model
                | Sst.Abstract_invariant | Sst.Terminal_read
                | Sst.Terminal_snapshot | Sst.Unique_transition
                | Sst.Current_model | Sst.Current_terminal_read ) )
          | None ->
              prerequisites)
      | Sst.Callback_call _ | Sst.Callback_requires _ | Sst.Callback_ensures _
      | Sst.Int_constant _ | Sst.Bool_constant _ | Sst.Unit_constant
      | Sst.Variable _ | Sst.Tuple_value _ | Sst.Record_value _
      | Sst.Constructor_value _ | Sst.Field_read _ | Sst.Field_write _
      | Sst.Shared_scalar_field_write _ | Sst.Checked_arithmetic _
      | Sst.Boolean_not _ | Sst.Boolean_binary _ | Sst.Compare _
      | Sst.Let_mutable _ | Sst.Mutable_read _ | Sst.Mutable_write _ | Sst.Let _
      | Sst.Sequence _ | Sst.If _ | Sst.Match _ | Sst.Use_type_invariant _
      | Sst.Owned_tree_nested_write _ | Sst.Owned_tree_rebase _ | Sst.Reveal _
      | Sst.Reveal_with_fuel _ | Sst.Local_assert _ | Sst.Proof_region _
      | Sst.Old _ | Sst.Optional_absent | Sst.Optional_present _
      | Sst.Optional_forward _ | Sst.Forall _ | Sst.Exists _
      | Sst.Symbolic_application _ ->
          prerequisites
    in
    match expression.expression_desc with
    | Sst.Forall _ | Sst.Exists _ -> prerequisites
    | Sst.Int_constant _ | Sst.Bool_constant _ | Sst.Unit_constant
    | Sst.Variable _ | Sst.Tuple_value _ | Sst.Record_value _
    | Sst.Constructor_value _ | Sst.Field_read _ | Sst.Field_write _
    | Sst.Shared_scalar_field_write _ | Sst.Owned_tree_nested_write _
    | Sst.Owned_tree_rebase _ | Sst.Let_mutable _ | Sst.Mutable_read _
    | Sst.Mutable_write _ | Sst.Let _ | Sst.Sequence _ | Sst.If _
    | Sst.Match _ | Sst.Checked_arithmetic _ | Sst.Compare _
    | Sst.Boolean_not _ | Sst.Boolean_binary _ | Sst.Direct_call _
    | Sst.Callback_call _ | Sst.Callback_requires _ | Sst.Callback_ensures _
    | Sst.Optional_absent | Sst.Optional_present _ | Sst.Optional_forward _
    | Sst.Reveal _ | Sst.Reveal_with_fuel _ | Sst.Use_type_invariant _
    | Sst.Local_assert _ | Sst.Proof_region _ | Sst.Old _
    | Sst.Symbolic_application _ ->
        List.fold_left collect prerequisites
          (invariant_cell_expression_children expression)
  in
  match definition.body with
  | Sst.Checked_exec { body; _ } | Sst.Proof_body { body; _ } ->
      collect [] body.expression |> List.sort_uniq Int.compare
  | Sst.Spec_definition _ | Sst.Recursive_spec_definition _
  | Sst.External_specification _ | Sst.Trusted_external_spec_target _
  | Sst.Trusted_external_body _ | Sst.Symbolic_declaration _ ->
      []
type transition_predecessor_origin = {
  transition_origin_source : Sst.function_definition;
  transition_origin_call_span : Diagnostic.span;
  transition_origin_version : int;
  transition_origin_path : string;
  transition_origin_intersected : bool;
}
type transition_predecessor_site = {
  transition_site_caller : Sst.function_definition;
  transition_site_callee : Sst.function_definition;
  transition_site_source : Sst.function_definition;
  transition_site_call_span : Diagnostic.span;
  transition_site_source_call_span : Diagnostic.span;
  transition_site_call_path : string;
  transition_site_actual : Sst.binding;
  transition_site_actual_version : int;
  transition_site_actual_path : string;
  transition_site_formal : Sst.binding;
  transition_site_root : Sst.binding;
  transition_site_mode : Sst.instance_mode;
  transition_site_type : Sst.typ;
  transition_site_owned_version : int;
  transition_site_obligation_snapshot : string list;
  transition_site_branch_intersection : bool;
}
type transition_predecessor_requirement = {
  transition_requirement_callee : Sst.function_definition;
  transition_requirement_formal : Sst.binding;
  transition_requirement_root : Sst.binding;
  transition_requirement_mode : Sst.instance_mode;
  transition_requirement_type : Sst.typ;
  transition_requirement_owned_version : int;
  transition_requirement_obligation_snapshot : string list;
  transition_requirement_sites : transition_predecessor_site list;
}
type scheduled_function = {
  definition : Sst.function_definition;
  canonical_key : string;
  finite_result_eligible : bool;
  invariant_receipt_source : bool;
  finite_result_source : bool;
  receipt_source : bool;
  receipt_dependent : bool;
  receipt_prerequisites : int list;
  invariant_receipt_prerequisites : int list;
  finite_result_prerequisites : int list;
  finite_result_demand_sites : finite_result_call_site list;
  frozen_formal_prerequisites : int list;
  invariant_cell_transition_prerequisites : int list;
  transition_predecessor_requirement : transition_predecessor_requirement option;
}
type prepared_program = {
  session : Verification_session.t;
  imports : Imported_callable.registration option;
  program : Sst.program;
  validated : Sst_validation.validated_program;
  termination : Termination.plan;
  invariants : Type_invariant.environment;
  declarations : Sst.function_definition list;
  type_definitions : Sst.type_definition list;
  rank_domains : Vir.rank_domain list;
  definitions : (int * Sst_validation.callable_descriptor) list;
  summaries : (int * summary) list;
  formula_registry : Logical_spec_admission_private.registry_entry list;
  scheduled : scheduled_function list;
  established_invariant_cell_transitions : int list ref;
  proof_entry_activations : Sst.function_id -> Spec_unfolding.activation list;
}
let receipt_edge_error edge message =
  let caller =
    Sst_validation.call_edge_caller edge |> Sst_validation.callable_id
  in
  error caller.function_name
    (Sst_validation.call_edge_span edge)
    (Malformed_sst message)
let finite_result_eligible validated rank_domains type_definitions descriptor =
  let definition = Sst_validation.callable_definition descriptor in
  match
    ( definition.body,
      definition.result_type,
      Sst_validation.result_instance_mode validated descriptor )
  with
  | ( Sst.Checked_exec { provenance = Sst.Authenticated_typedtree _; _ },
      ((Sst.Aggregate _ | Sst.Application _) as typ),
      (Sst.Exec_instance | Sst.Tracked_instance) ) ->
      Option.is_some
        (static_rank_domain_for_type
           (Sst_validation.program validated).Sst.parametric_adts rank_domains
           type_definitions typ)
  | ( ( Sst.Checked_exec _ | Sst.Spec_definition _
      | Sst.Recursive_spec_definition _ | Sst.Proof_body _
      | Sst.External_specification _ | Sst.Trusted_external_spec_target _
      | Sst.Trusted_external_body _ | Sst.Symbolic_declaration _ ),
      ( Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _ | Sst.Aggregate _
      | Sst.Parameter _ | Sst.Application _ ),
      (Sst.Exec_instance | Sst.Tracked_instance | Sst.Ghost_instance) ) ->
      false
let immutable_field_id type_definitions field_id =
  List.exists
    (fun (definition : Sst.type_definition) ->
      let fields =
        match definition.type_kind with
        | Sst.Record_definition fields -> fields
        | Sst.Variant_definition constructors ->
            List.concat_map
              (fun (constructor : Sst.constructor_definition) ->
                constructor.constructor_fields)
              constructors
      in
      List.exists
        (fun (field : Sst.field_definition) ->
          field.field_id = field_id
          && Finite_value_registry.Finite_domain.immutable_field field)
        fields)
    type_definitions
type finite_demand_context = {
  finite_demand_validated : Sst_validation.validated_program;
  finite_demand_types : Sst.type_definition list;
  finite_demand_descriptors : Sst_validation.callable_descriptor list;
  finite_demand_eligible_ids : int list;
  finite_demand_rank_domains : Vir.rank_domain list;
}
let finite_demand_descriptor context id =
  List.find_opt
    (fun descriptor ->
      same_function_id (Sst_validation.callable_id descriptor) id)
    context.finite_demand_descriptors
let finite_demand_eligible context id =
  List.mem id.Sst.function_index context.finite_demand_eligible_ids
let finite_demand_ranked context typ =
  Option.is_some
    (static_rank_domain_for_type
       (Sst_validation.program context.finite_demand_validated).Sst.parametric_adts
       context.finite_demand_rank_domains
       context.finite_demand_types typ)
let add_finite_sites left right = List.sort_uniq compare (left @ right)
let intersect_finite_sites = function
  | [] -> []
  | first :: rest ->
      List.fold_left
        (fun shared sites ->
          List.filter (fun site -> List.mem site sites) shared)
        first rest
let bind_finite_origin_pattern environment pattern origins reconstructed =
  let rec bind environment (pattern : Sst.pattern) =
    match pattern.pattern_desc with
    | Sst.Bind binding
    | Sst.Owned_tree_cursor_pattern { cursor_binding = binding; _ } ->
        (binding.id, (origins, reconstructed))
        :: List.remove_assoc binding.id environment
    | Sst.Tuple_pattern components ->
        List.fold_left
          (fun environment (_, pattern) -> bind environment pattern)
          environment components
    | Sst.Record_pattern fields ->
        List.fold_left
          (fun environment (_, pattern) -> bind environment pattern)
          environment fields
    | Sst.Constructor_pattern (_, patterns) ->
        List.fold_left bind environment patterns
    | Sst.Wildcard | Sst.Int_pattern _ | Sst.Bool_pattern _ | Sst.Unit_pattern
    | Sst.Or_pattern _ ->
        environment
  in
  bind environment pattern
let bind_finite_authority_pattern environment pattern authority =
  let rec bind environment (pattern : Sst.pattern) =
    match pattern.pattern_desc with
    | Sst.Bind binding
    | Sst.Owned_tree_cursor_pattern { cursor_binding = binding; _ } ->
        (binding.id, authority) :: List.remove_assoc binding.id environment
    | Sst.Tuple_pattern components ->
        List.fold_left
          (fun environment (_, pattern) -> bind environment pattern)
          environment components
    | Sst.Record_pattern fields ->
        List.fold_left
          (fun environment (_, pattern) -> bind environment pattern)
          environment fields
    | Sst.Constructor_pattern (_, patterns) ->
        List.fold_left bind environment patterns
    | Sst.Or_pattern (left, right) -> bind (bind environment left) right
    | Sst.Wildcard | Sst.Int_pattern _ | Sst.Bool_pattern _ | Sst.Unit_pattern
      ->
        environment
  in
  bind environment pattern
let combine_finite_authority_uses analyses =
  List.fold_left
    (fun (authority, demand) (next_authority, next_demand) ->
      (authority || next_authority, demand || next_demand))
    (false, false) analyses
let finite_authority_call context ~recurse ~visited ~environment ~call_form
    ~callee ~arguments =
  let analyzed =
    List.map
      (fun (_, argument) -> recurse visited environment argument)
      arguments
  in
  let _, nested_demand = combine_finite_authority_uses analyzed in
  let descriptor = finite_demand_descriptor context callee in
  let direct_demand =
    match descriptor with
    | None -> false
    | Some callee_descriptor ->
        List.mapi
          (fun ordinal (_, (argument : Sst.expression)) ->
            let authority, _ = List.nth analyzed ordinal in
            authority
            &&
            let finite_formal =
              Option.is_some
                (Sst_validation.finite_formal_requirement
                   context.finite_demand_validated callee_descriptor ordinal)
            in
            finite_formal
            || (call_form = Sst.Specification_call || call_form = Sst.Proof_call)
               && (Sst_validation.callable_definition callee_descriptor)
                    .recursive
               && finite_demand_ranked context argument.typ)
          arguments
        |> List.exists Fun.id
  in
  let expanded_authority, expanded_demand =
    match descriptor with
    | Some callee_descriptor
      when call_form = Sst.Specification_call
           && not (List.mem callee.function_index visited) -> (
        let definition = Sst_validation.callable_definition callee_descriptor in
        match definition.body with
        | Sst.Spec_definition body
          when List.length definition.parameters = List.length analyzed ->
            let helper_environment =
              List.fold_left2
                (fun helper_environment parameter (authority, _) ->
                  bind_finite_authority_pattern helper_environment
                    parameter.Sst.pattern authority)
                []
                (value_parameters definition)
                analyzed
            in
            recurse
              (callee.function_index :: visited)
              helper_environment body.expression
        | Sst.Spec_definition _ | Sst.Recursive_spec_definition _
        | Sst.Checked_exec _ | Sst.Proof_body _ | Sst.External_specification _
        | Sst.Trusted_external_spec_target _ | Sst.Trusted_external_body _
        | Sst.Symbolic_declaration _ ->
            (false, false))
    | Some _ | None -> (false, false)
  in
  (expanded_authority, nested_demand || direct_demand || expanded_demand)
let rec finite_authority_use context visited environment
    (expression : Sst.expression) =
  let visit = finite_authority_use context visited environment in
  let combine expressions =
    List.map visit expressions |> combine_finite_authority_uses
  in
  match expression.expression_desc with
  | Sst.Variable { binding; _ } ->
      ( Option.value ~default:false (List.assoc_opt binding.id environment),
        false )
  | Sst.Field_read { record; field }
    when immutable_field_id context.finite_demand_types field ->
      visit record
  | Sst.Field_read { record; _ } -> (false, snd (visit record))
  | Sst.Direct_call { call_form; callee; arguments; _ } ->
      finite_authority_call context
        ~recurse:(finite_authority_use context)
        ~visited ~environment ~call_form ~callee
        ~arguments:
          (List.filter_map
             (function
               | Sst.Value_argument { label; value } -> Some (label, value)
               | Sst.Callback_argument _ -> None)
             arguments)
  | Sst.Let (bindings, body) ->
      let environment, binding_demand =
        List.fold_left
          (fun (environment, demand) (pattern, value) ->
            let authority, next_demand =
              finite_authority_use context visited environment value
            in
            ( bind_finite_authority_pattern environment pattern authority,
              demand || next_demand ))
          (environment, false) bindings
      in
      let authority, body_demand =
        finite_authority_use context visited environment body
      in
      (authority, binding_demand || body_demand)
  | Sst.Match (scrutinee, cases) ->
      let scrutinee_authority, scrutinee_demand = visit scrutinee in
      let analyses =
        List.map
          (fun case ->
            let case_environment =
              bind_finite_authority_pattern environment case.Sst.case_pattern
                scrutinee_authority
            in
            let guard_demand =
              Option.fold ~none:false
                ~some:(fun guard ->
                  snd
                    (finite_authority_use context visited case_environment guard))
                case.case_guard
            in
            let authority, body_demand =
              finite_authority_use context visited case_environment
                case.case_body
            in
            (authority, guard_demand || body_demand))
          cases
      in
      let authority =
        match analyses with
        | [] -> false
        | (first, _) :: rest ->
            first && List.for_all (fun (next, _) -> next) rest
      in
      (authority, scrutinee_demand || List.exists snd analyses)
  | Sst.If (condition, consequent, alternative) ->
      let _, condition_demand = visit condition in
      let consequent_authority, consequent_demand = visit consequent in
      let alternative_authority, alternative_demand =
        Option.fold ~none:(false, false) ~some:visit alternative
      in
      ( consequent_authority && alternative_authority,
        condition_demand || consequent_demand || alternative_demand )
  | Sst.Sequence (left, right) ->
      let _, left_demand = visit left in
      let authority, right_demand = visit right in
      (authority, left_demand || right_demand)
  | Sst.Proof_region body | Sst.Old body -> visit body
  | Sst.Tuple_value components ->
      let _, demand = combine (List.map snd components) in
      (false, demand)
  | Sst.Record_value { fields; _ } ->
      let _, demand = combine (List.map snd fields) in
      (false, demand)
  | Sst.Constructor_value { arguments; _ }
  | Sst.Checked_arithmetic (_, arguments) ->
      let _, demand = combine arguments in
      (false, demand)
  | Sst.Compare (_, left, right) | Sst.Boolean_binary (_, left, right) ->
      let _, demand = combine [ left; right ] in
      (false, demand)
  | Sst.Boolean_not operand
  | Sst.Local_assert { predicate = operand; _ }
  | Sst.Use_type_invariant { value = operand; _ }
  | Sst.Optional_present operand
  | Sst.Optional_forward operand ->
      (false, snd (visit operand))
  | Sst.Let_mutable (_, initial, body) ->
      let _, initial_demand = visit initial in
      let authority, body_demand = visit body in
      (authority, initial_demand || body_demand)
  | Sst.Field_write { value; _ }
  | Sst.Shared_scalar_field_write { value; _ }
  | Sst.Owned_tree_nested_write { value; _ }
  | Sst.Mutable_write { value; _ } ->
      (false, snd (visit value))
  | Sst.Symbolic_application application ->
      let _, demand =
        combine (Symbolic_application_private.arguments application)
      in
      (false, demand)
  | Sst.Callback_call _ | Sst.Callback_requires _ | Sst.Callback_ensures _
  | Sst.Forall _ | Sst.Exists _
  | Sst.Int_constant _ | Sst.Bool_constant _ | Sst.Unit_constant
  | Sst.Optional_absent | Sst.Mutable_read _ | Sst.Owned_tree_rebase _
  | Sst.Reveal _ | Sst.Reveal_with_fuel _ ->
      (false, false)
let finite_ensures_require_result context descriptor =
  let definition = Sst_validation.callable_definition descriptor in
  List.exists
    (fun (clause : Sst.ensures_clause) ->
      let environment =
        Option.fold ~none:[]
          ~some:(fun binder -> bind_finite_authority_pattern [] binder true)
          clause.binder
      in
      snd
        (finite_authority_use context
           [ definition.function_id.function_index ]
           environment clause.predicate.expression))
    definition.contracts.ensures
let combine_finite_origin_analyses analyses =
  List.fold_left
    (fun (origins, reconstructed, demands)
         (next_origins, next_reconstructed, next_demands) ->
      ( add_finite_sites origins next_origins,
        add_finite_sites reconstructed next_reconstructed,
        demands @ next_demands ))
    ([], [], []) analyses
let finite_demand_call context ~analyze ~caller ~environment ~call_form ~callee
    ~arguments ~call_span =
  let analyzed =
    List.map
      (fun (_, argument) -> analyze caller environment argument)
      arguments
  in
  let _, _, nested_demands = combine_finite_origin_analyses analyzed in
  let descriptor = finite_demand_descriptor context callee in
  let sink_demands =
    match descriptor with
    | None -> []
    | Some callee_descriptor ->
        List.mapi
          (fun ordinal (_, (argument : Sst.expression)) ->
            let origins, _, _ = List.nth analyzed ordinal in
            let finite_formal =
              Option.is_some
                (Sst_validation.finite_formal_requirement
                   context.finite_demand_validated callee_descriptor ordinal)
            in
            let structural =
              (call_form = Sst.Specification_call || call_form = Sst.Proof_call)
              && (Sst_validation.callable_definition callee_descriptor)
                   .recursive
              && finite_demand_ranked context argument.typ
            in
            if finite_formal || structural then
              List.map (fun site -> (caller, site)) origins
            else [])
          arguments
        |> List.concat
  in
  let origins =
    if call_form = Sst.Exec_call && finite_demand_eligible context callee then
      [ { finite_result_callee = callee; finite_result_call_span = call_span } ]
    else []
  in
  let summary_demands =
    match descriptor with
    | Some descriptor
      when origins <> [] && finite_ensures_require_result context descriptor ->
        List.map (fun site -> (caller, site)) origins
    | Some _ | None -> []
  in
  (origins, [], nested_demands @ sink_demands @ summary_demands)
let rec finite_demand_analyze context caller environment
    (expression : Sst.expression) =
  let visit = finite_demand_analyze context caller environment in
  let combine expressions =
    List.map visit expressions |> combine_finite_origin_analyses
  in
  match expression.expression_desc with
  | Sst.Variable { binding; _ } ->
      let origins, reconstructed =
        Option.value ~default:([], []) (List.assoc_opt binding.id environment)
      in
      (origins, reconstructed, [])
  | Sst.Field_read { record; field }
    when immutable_field_id context.finite_demand_types field ->
      visit record
  | Sst.Direct_call { call_form; callee; arguments; _ } ->
      finite_demand_call context
        ~analyze:(finite_demand_analyze context)
        ~caller ~environment ~call_form ~callee
        ~arguments:
          (List.filter_map
             (function
               | Sst.Value_argument { label; value } -> Some (label, value)
               | Sst.Callback_argument _ -> None)
             arguments)
        ~call_span:expression.span
  | Sst.Let (bindings, body) ->
      let environment, demands =
        List.fold_left
          (fun (environment, demands) (pattern, value) ->
            let origins, reconstructed, next_demands =
              finite_demand_analyze context caller environment value
            in
            ( bind_finite_origin_pattern environment pattern origins
                reconstructed,
              demands @ next_demands ))
          (environment, []) bindings
      in
      let origins, reconstructed, body_demands =
        finite_demand_analyze context caller environment body
      in
      (origins, reconstructed, demands @ body_demands)
  | Sst.Match (scrutinee, cases) ->
      let scrutinee_origins, scrutinee_reconstructed, scrutinee_demands =
        visit scrutinee
      in
      let case_analyses =
        List.map
          (fun case ->
            let case_environment =
              bind_finite_origin_pattern environment case.Sst.case_pattern
                scrutinee_origins scrutinee_reconstructed
            in
            let _, _, guard_demands =
              Option.fold ~none:([], [], [])
                ~some:(finite_demand_analyze context caller case_environment)
                case.case_guard
            in
            let origins, reconstructed, body_demands =
              finite_demand_analyze context caller case_environment
                case.case_body
            in
            (origins, reconstructed, guard_demands @ body_demands))
          cases
      in
      let origins =
        List.map (fun (origins, _, _) -> origins) case_analyses
        |> intersect_finite_sites
      in
      let _, reconstructed, demands =
        combine_finite_origin_analyses case_analyses
      in
      (origins, reconstructed, scrutinee_demands @ demands)
  | Sst.If (condition, consequent, alternative) ->
      let _, _, condition_demands = visit condition in
      let consequent_analysis = visit consequent in
      let alternative_analysis =
        Option.fold ~none:([], [], []) ~some:visit alternative
      in
      let consequent_origins, _, _ = consequent_analysis in
      let alternative_origins, _, _ = alternative_analysis in
      let _, reconstructed, branch_demands =
        combine_finite_origin_analyses
          [ consequent_analysis; alternative_analysis ]
      in
      ( intersect_finite_sites [ consequent_origins; alternative_origins ],
        reconstructed,
        condition_demands @ branch_demands )
  | Sst.Sequence (left, right) ->
      let _, _, left_demands = visit left in
      let origins, reconstructed, right_demands = visit right in
      (origins, reconstructed, left_demands @ right_demands)
  | Sst.Proof_region body | Sst.Old body -> visit body
  | Sst.Tuple_value components ->
      let _, _, demands = combine (List.map snd components) in
      ([], [], demands)
  | Sst.Record_value { fields; _ } ->
      let _, _, demands = combine (List.map snd fields) in
      ([], [], demands)
  | Sst.Constructor_value { arguments; _ }
  | Sst.Checked_arithmetic (_, arguments) ->
      let _, _, demands = combine arguments in
      ([], [], demands)
  | Sst.Compare (_, left, right) | Sst.Boolean_binary (_, left, right) ->
      let _, _, demands = combine [ left; right ] in
      ([], [], demands)
  | Sst.Boolean_not operand
  | Sst.Local_assert { predicate = operand; _ }
  | Sst.Use_type_invariant { value = operand; _ }
  | Sst.Optional_present operand
  | Sst.Optional_forward operand ->
      let _, _, demands = visit operand in
      ([], [], demands)
  | Sst.Let_mutable (_, initial, body) ->
      let _, _, initial_demands = visit initial in
      let origins, reconstructed, body_demands = visit body in
      (origins, reconstructed, initial_demands @ body_demands)
  | Sst.Field_write { value; _ }
  | Sst.Shared_scalar_field_write { value; _ }
  | Sst.Owned_tree_nested_write { value; _ }
  | Sst.Mutable_write { value; _ } ->
      let _, _, demands = visit value in
      ([], [], demands)
  | Sst.Symbolic_application application ->
      let _, _, demands =
        combine (Symbolic_application_private.arguments application)
      in
      ([], [], demands)
  | Sst.Callback_call _ | Sst.Callback_requires _ | Sst.Callback_ensures _
  | Sst.Forall _ | Sst.Exists _
  | Sst.Int_constant _ | Sst.Bool_constant _ | Sst.Unit_constant
  | Sst.Optional_absent | Sst.Mutable_read _ | Sst.Owned_tree_rebase _
  | Sst.Reveal _ | Sst.Reveal_with_fuel _ | Sst.Field_read _ ->
      ([], [], [])
let finite_demand_rows context =
  List.filter_map
    (fun descriptor ->
      let definition = Sst_validation.callable_definition descriptor in
      match definition.body with
      | Sst.Checked_exec { body; _ } ->
          let returns, _, demands =
            finite_demand_analyze context definition.function_id []
              body.expression
          in
          Some
            ( definition.function_id.function_index,
              definition.function_id,
              returns,
              demands,
              finite_ensures_require_result context descriptor )
      | Sst.Proof_body _ | Sst.Spec_definition _
      | Sst.Recursive_spec_definition _ | Sst.External_specification _
      | Sst.Trusted_external_spec_target _ | Sst.Trusted_external_body _
      | Sst.Symbolic_declaration _ ->
          None)
    context.finite_demand_descriptors
let close_finite_demands rows demands =
  let rec close demands =
    let demanded_ids =
      List.map
        (fun (_, site) -> site.finite_result_callee.function_index)
        demands
      |> List.sort_uniq Int.compare
    in
    let expanded =
      List.concat_map
        (fun (index, caller, returns, _, local_post_demand) ->
          if local_post_demand || List.mem index demanded_ids then
            List.map (fun site -> (caller, site)) returns
          else [])
        rows
    in
    let next = List.sort_uniq compare (demands @ expanded) in
    if next = demands then demands else close next
  in
  close (List.sort_uniq compare demands)
let finite_result_demand_dependencies ~validated ~type_definitions ~eligible_ids
    ~rank_domains descriptors =
  let context =
    {
      finite_demand_validated = validated;
      finite_demand_types = type_definitions;
      finite_demand_descriptors = descriptors;
      finite_demand_eligible_ids = eligible_ids;
      finite_demand_rank_domains = rank_domains;
    }
  in
  let rows = finite_demand_rows context in
  let direct_demands =
    List.concat_map (fun (_, _, _, demands, _) -> demands) rows
  in
  let demands = close_finite_demands rows direct_demands in
  let local_post_ids =
    List.filter_map
      (fun (index, _, _, _, local_post_demand) ->
        if local_post_demand then Some index else None)
      rows
  in
  (demands, local_post_ids)
let topological_order ~key_for_index dependencies remaining =
  let rec loop ordered remaining =
    match remaining with
    | [] -> Some (List.rev ordered)
    | _ -> (
        let ready =
          List.filter
            (fun index ->
              Option.value ~default:[] (List.assoc_opt index dependencies)
              |> List.for_all (fun (callee : Sst.function_id) ->
                  not (List.mem callee.function_index remaining)))
            remaining
          |> List.sort (fun left right ->
              String.compare (key_for_index left) (key_for_index right))
        in
        match ready with
        | [] -> None
        | next :: _ ->
            loop (next :: ordered) (List.filter (( <> ) next) remaining))
  in
  loop [] remaining
let transition_span_string (span : Diagnostic.span) =
  Printf.sprintf "%s:%d:%d-%d:%d" span.file span.start_pos.line
    span.start_pos.column span.end_pos.line span.end_pos.column
let transition_predecessor_analysis ~validated ~invariants descriptors =
  let descriptor_for_id id =
    List.find_opt
      (fun descriptor ->
        same_function_id (Sst_validation.callable_id descriptor) id)
      descriptors
  in
  let result_version definition =
    match definition.Sst.body with
    | Sst.Checked_exec { body; _ } -> (
        match List.rev (owned_tree_transitions body.expression) with
        | transition :: _ -> transition.Sst.successor_version
        | [] -> 0)
    | Sst.Spec_definition _ | Sst.Recursive_spec_definition _ | Sst.Proof_body _
    | Sst.External_specification _ | Sst.Trusted_external_spec_target _
    | Sst.Trusted_external_body _ | Sst.Symbolic_declaration _ ->
        0
  in
  let result_is_local_closed definition =
    match descriptor_for_id definition.Sst.function_id with
    | Some descriptor -> (
        match
          ( definition.body,
            definition.result_type,
            Sst_validation.result_instance_mode validated descriptor )
        with
        | ( Sst.Checked_exec { provenance = Sst.Authenticated_typedtree _; _ },
            Sst.Aggregate type_id,
            (Sst.Exec_instance | Sst.Tracked_instance) ) ->
            Option.is_some (Type_invariant.find_for_type invariants type_id)
        | ( ( Sst.Checked_exec _ | Sst.Spec_definition _
            | Sst.Recursive_spec_definition _ | Sst.Proof_body _
            | Sst.External_specification _ | Sst.Trusted_external_spec_target _
            | Sst.Trusted_external_body _ | Sst.Symbolic_declaration _ ),
            ( Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _ | Sst.Aggregate _
            | Sst.Parameter _ | Sst.Application _ ),
            (Sst.Exec_instance | Sst.Tracked_instance | Sst.Ghost_instance) ) ->
            false)
    | None -> false
  in
  let* base_requirements =
    let rec collect requirements = function
      | [] -> Ok requirements
      | descriptor :: rest -> (
          let definition = Sst_validation.callable_definition descriptor in
          match
            Type_invariant.find_for_operation invariants definition.function_id
          with
          | Some (handle, Sst.Unique_transition) ->
              let transitions =
                match definition.body with
                | Sst.Checked_exec { body; _ } ->
                    owned_tree_transitions body.expression
                | Sst.Spec_definition _ | Sst.Recursive_spec_definition _
                | Sst.Proof_body _ | Sst.External_specification _
                | Sst.Trusted_external_spec_target _
                | Sst.Trusted_external_body _ | Sst.Symbolic_declaration _ ->
                    []
              in
              let* first =
                match transitions with
                | transition :: _ -> Ok transition
                | [] ->
                    error definition.function_id.function_name definition.span
                      (Malformed_sst
                         "transition predecessor requirement has no \
                          preservation boundary")
              in
              let* ordinal, formal =
                definition.parameters
                |> List.mapi (fun ordinal parameter -> (ordinal, parameter))
                |> List.find_map (fun (ordinal, parameter) ->
                    let parameter = Sst.require_value_parameter parameter in
                    match parameter.Sst.pattern.pattern_desc with
                    | Sst.Bind binding when binding.id = first.root.id ->
                        Some (ordinal, binding)
                    | Sst.Bind _ | Sst.Wildcard | Sst.Unit_pattern
                    | Sst.Tuple_pattern _ | Sst.Record_pattern _
                    | Sst.Constructor_pattern _ | Sst.Int_pattern _
                    | Sst.Bool_pattern _ | Sst.Owned_tree_cursor_pattern _
                    | Sst.Or_pattern _ ->
                        None)
                |> function
                | Some formal -> Ok formal
                | None ->
                    error definition.function_id.function_name definition.span
                      (Malformed_sst
                         "transition predecessor root is not an exact local \
                          formal")
              in
              let mode =
                Sst_validation.formal_instance_mode validated descriptor ordinal
                  (List.nth definition.parameters ordinal)
              in
              let* () =
                if
                  List.for_all
                    (fun transition ->
                      transition.Sst.root.id = formal.id
                      && transition.root.typ = formal.typ)
                    transitions
                  && (mode = Sst.Exec_instance || mode = Sst.Tracked_instance)
                then Ok ()
                else
                  error definition.function_id.function_name definition.span
                    (Malformed_sst
                       "transition predecessor roots or mode are not exact")
              in
              collect
                ({
                   transition_requirement_callee = definition;
                   transition_requirement_formal = formal;
                   transition_requirement_root = formal;
                   transition_requirement_mode = mode;
                   transition_requirement_type = formal.typ;
                   transition_requirement_owned_version = first.pre_version;
                   transition_requirement_obligation_snapshot =
                     transition_obligation_snapshot definition handle;
                   transition_requirement_sites = [];
                 }
                :: requirements)
                rest
          | Some (_, _) | None -> collect requirements rest)
    in
    collect [] descriptors
  in
  let requirement_for_id id =
    List.find_opt
      (fun requirement ->
        same_function_id requirement.transition_requirement_callee.function_id
          id)
      base_requirements
  in
  let same_origin left right =
    same_function_id left.transition_origin_source.function_id
      right.transition_origin_source.function_id
    && left.transition_origin_call_span = right.transition_origin_call_span
    && left.transition_origin_version = right.transition_origin_version
    && String.equal left.transition_origin_path right.transition_origin_path
  in
  let intersect_environments environments =
    match environments with
    | [] -> []
    | first :: rest ->
        List.filter_map
          (fun (binding_id, origin) ->
            if
              List.for_all
                (fun environment ->
                  match List.assoc_opt binding_id environment with
                  | Some candidate -> same_origin origin candidate
                  | None -> false)
                rest
            then
              Some
                ( binding_id,
                  { origin with transition_origin_intersected = true } )
            else None)
          first
  in
  let bind_origin environment pattern origin =
    match pattern.Sst.pattern_desc with
    | Sst.Bind binding -> (
        match origin with
        | Some origin ->
            (binding.id, origin) :: List.remove_assoc binding.id environment
        | None -> List.remove_assoc binding.id environment)
    | Sst.Wildcard | Sst.Unit_pattern | Sst.Tuple_pattern _
    | Sst.Record_pattern _ | Sst.Constructor_pattern _ | Sst.Int_pattern _
    | Sst.Bool_pattern _ | Sst.Owned_tree_cursor_pattern _ | Sst.Or_pattern _ ->
        environment
  in
  let add_path path label = String.concat "/" (path @ [ label ]) in
  let rec analyze caller path branch_depth environment
      (expression : Sst.expression) =
    let analyze_list path environment expressions =
      List.fold_left
        (fun result expression ->
          let* environment, sites = result in
          let* _, environment, next =
            analyze caller path branch_depth environment expression
          in
          Ok (environment, sites @ next))
        (Ok (environment, []))
        expressions
    in
    match expression.expression_desc with
    | Sst.Variable _ -> Ok (None, environment, [])
    | Sst.Direct_call
        { call_form = Sst.Exec_call; callee; arguments; recursive = false; _ }
      -> (
        let* environment, nested_sites =
          analyze_list path environment
            (List.filter_map
               (function
                 | Sst.Value_argument { value; _ } -> Some value
                 | Sst.Callback_argument _ -> None)
               arguments)
        in
        let callee_definition =
          Option.map Sst_validation.callable_definition
            (descriptor_for_id callee)
        in
        match (requirement_for_id callee, callee_definition) with
        | Some requirement, Some callee_definition -> (
            let actual_expression =
              List.nth_opt arguments
                (match callee_definition.returns_unique_parameter with
                | Some ordinal -> ordinal
                | None -> 0)
            in
            let exact =
              match actual_expression with
              | Some
                  (Sst.Value_argument
                     {
                       value =
                         {
                           Sst.expression_desc =
                             Sst.Variable
                               {
                                 binding = actual;
                                 use_uniqueness = Sst.Definitely_unique;
                               };
                           _;
                         };
                       _;
                     }) -> (
                  match List.assoc_opt actual.id environment with
                  | Some origin
                    when branch_depth = 0
                         && origin.transition_origin_source.function_id
                            <> callee ->
                      Some (actual, origin)
                  | Some _ | None -> None)
              | Some _ | None -> None
            in
            match exact with
            | Some (actual, origin) ->
                let call_path =
                  add_path path
                    ("call:" ^ transition_span_string expression.span)
                in
                let site =
                  {
                    transition_site_caller = caller;
                    transition_site_callee = callee_definition;
                    transition_site_source = origin.transition_origin_source;
                    transition_site_call_span = expression.span;
                    transition_site_source_call_span =
                      origin.transition_origin_call_span;
                    transition_site_call_path = call_path;
                    transition_site_actual = actual;
                    transition_site_actual_version =
                      origin.transition_origin_version;
                    transition_site_actual_path = origin.transition_origin_path;
                    transition_site_formal =
                      requirement.transition_requirement_formal;
                    transition_site_root =
                      requirement.transition_requirement_root;
                    transition_site_mode =
                      requirement.transition_requirement_mode;
                    transition_site_type =
                      requirement.transition_requirement_type;
                    transition_site_owned_version =
                      requirement.transition_requirement_owned_version;
                    transition_site_obligation_snapshot =
                      requirement.transition_requirement_obligation_snapshot;
                    transition_site_branch_intersection =
                      origin.transition_origin_intersected;
                  }
                in
                let environment = List.remove_assoc actual.id environment in
                Ok
                  ( Some
                      {
                        transition_origin_source = callee_definition;
                        transition_origin_call_span = expression.span;
                        transition_origin_version =
                          result_version callee_definition;
                        transition_origin_path = call_path;
                        transition_origin_intersected = false;
                      },
                    environment,
                    nested_sites @ [ site ] )
            | None ->
                error caller.function_id.function_name expression.span
                  (Malformed_sst
                     "transition predecessor call has no exact direct local \
                      established argument"))
        | None, Some callee_definition
          when result_is_local_closed callee_definition ->
            let call_path =
              add_path path ("result:" ^ transition_span_string expression.span)
            in
            Ok
              ( Some
                  {
                    transition_origin_source = callee_definition;
                    transition_origin_call_span = expression.span;
                    transition_origin_version = result_version callee_definition;
                    transition_origin_path = call_path;
                    transition_origin_intersected = false;
                  },
                environment,
                nested_sites )
        | (None | Some _), (None | Some _) ->
            Ok (None, environment, nested_sites))
    | Sst.Direct_call { arguments; _ } ->
        let* environment, sites =
          analyze_list path environment
            (List.filter_map
               (function
                 | Sst.Value_argument { value; _ } -> Some value
                 | Sst.Callback_argument _ -> None)
               arguments)
        in
        Ok (None, environment, sites)
    | Sst.Let (bindings, body) ->
        let* environment, sites =
          List.fold_left
            (fun result (pattern, value) ->
              let* environment, sites = result in
              let* origin, environment, next =
                analyze caller path branch_depth environment value
              in
              let origin =
                match value.Sst.expression_desc with
                | Sst.Variable _ -> None
                | _ -> origin
              in
              Ok (bind_origin environment pattern origin, sites @ next))
            (Ok (environment, []))
            bindings
        in
        let* origin, environment, body_sites =
          analyze caller path branch_depth environment body
        in
        Ok (origin, environment, sites @ body_sites)
    | Sst.Sequence (left, right) ->
        let* _, environment, left_sites =
          analyze caller path branch_depth environment left
        in
        let* origin, environment, right_sites =
          analyze caller path branch_depth environment right
        in
        Ok (origin, environment, left_sites @ right_sites)
    | Sst.If (condition, consequent, alternative) ->
        let* _, environment, condition_sites =
          analyze caller path branch_depth environment condition
        in
        let* left_origin, left_environment, left_sites =
          analyze caller (path @ [ "if:true" ]) (branch_depth + 1) environment
            consequent
        in
        let left_origin =
          match consequent.Sst.expression_desc with
          | Sst.Variable { binding; _ } -> List.assoc_opt binding.id environment
          | _ -> left_origin
        in
        let* right_origin, right_environment, right_sites =
          match alternative with
          | Some alternative ->
              analyze caller (path @ [ "if:false" ]) (branch_depth + 1)
                environment alternative
          | None -> Ok (None, environment, [])
        in
        let right_origin =
          match alternative with
          | Some { Sst.expression_desc = Sst.Variable { binding; _ }; _ } ->
              List.assoc_opt binding.id environment
          | Some _ | None -> right_origin
        in
        let origin =
          match (left_origin, right_origin) with
          | Some left, Some right when same_origin left right ->
              Some { left with transition_origin_intersected = true }
          | (None | Some _), (None | Some _) -> None
        in
        Ok
          ( origin,
            intersect_environments [ left_environment; right_environment ],
            condition_sites @ left_sites @ right_sites )
    | Sst.Match (scrutinee, cases) ->
        let* _, environment, scrutinee_sites =
          analyze caller path branch_depth environment scrutinee
        in
        let* analyses =
          let rec loop ordinal analyses = function
            | [] -> Ok (List.rev analyses)
            | case :: rest ->
                let case_path = path @ [ Printf.sprintf "match:%d" ordinal ] in
                let* _, environment, guard_sites =
                  match case.Sst.case_guard with
                  | Some guard ->
                      analyze caller case_path (branch_depth + 1) environment
                        guard
                  | None -> Ok (None, environment, [])
                in
                let* origin, environment, body_sites =
                  analyze caller case_path (branch_depth + 1) environment
                    case.case_body
                in
                loop (ordinal + 1)
                  ((origin, environment, guard_sites @ body_sites) :: analyses)
                  rest
          in
          loop 0 [] cases
        in
        let origins = List.filter_map (fun (origin, _, _) -> origin) analyses in
        let origin =
          match (origins, analyses) with
          | first :: rest, _
            when List.length origins = List.length analyses
                 && List.for_all (same_origin first) rest ->
              Some { first with transition_origin_intersected = true }
          | _ -> None
        in
        Ok
          ( origin,
            intersect_environments
              (List.map (fun (_, environment, _) -> environment) analyses),
            scrutinee_sites
            @ List.concat_map (fun (_, _, sites) -> sites) analyses )
    | Sst.Field_write { provenance; value; _ } ->
        let* _, environment, sites =
          analyze caller path branch_depth environment value
        in
        Ok (None, List.remove_assoc provenance.root.id environment, sites)
    | Sst.Shared_scalar_field_write { value; _ } ->
        let* _, environment, sites =
          analyze caller path branch_depth environment value
        in
        Ok (None, environment, sites)
    | Sst.Owned_tree_nested_write { transition; value } ->
        let* _, environment, sites =
          analyze caller path branch_depth environment value
        in
        Ok (None, List.remove_assoc transition.root.id environment, sites)
    | Sst.Owned_tree_rebase { transition } ->
        Ok (None, List.remove_assoc transition.root.id environment, [])
    | Sst.Mutable_write { provenance; value } ->
        let* _, environment, sites =
          analyze caller path branch_depth environment value
        in
        Ok (None, List.remove_assoc provenance.root.id environment, sites)
    | Sst.Let_mutable (_, initial, body) ->
        let* _, environment, initial_sites =
          analyze caller path branch_depth environment initial
        in
        let* origin, environment, body_sites =
          analyze caller path branch_depth environment body
        in
        Ok (origin, environment, initial_sites @ body_sites)
    | Sst.Tuple_value components ->
        let* environment, sites =
          analyze_list path environment (List.map snd components)
        in
        Ok (None, environment, sites)
    | Sst.Record_value { fields; _ } ->
        let* environment, sites =
          analyze_list path environment (List.map snd fields)
        in
        Ok (None, environment, sites)
    | Sst.Constructor_value { arguments; _ }
    | Sst.Checked_arithmetic (_, arguments) ->
        let* environment, sites = analyze_list path environment arguments in
        Ok (None, environment, sites)
    | Sst.Field_read { record; _ }
    | Sst.Old record
    | Sst.Proof_region record
    | Sst.Boolean_not record
    | Sst.Local_assert { predicate = record; _ }
    | Sst.Use_type_invariant { value = record; _ }
    | Sst.Optional_present record
    | Sst.Optional_forward record ->
        let* _, environment, sites =
          analyze caller path branch_depth environment record
        in
        Ok (None, environment, sites)
    | Sst.Compare (_, left, right) | Sst.Boolean_binary (_, left, right) ->
        let* environment, sites =
          analyze_list path environment [ left; right ]
        in
        Ok (None, environment, sites)
    | Sst.Callback_call _ | Sst.Callback_requires _ | Sst.Callback_ensures _
    | Sst.Forall _ | Sst.Exists _
    | Sst.Int_constant _ | Sst.Bool_constant _ | Sst.Unit_constant
    | Sst.Optional_absent | Sst.Mutable_read _ | Sst.Reveal _
    | Sst.Reveal_with_fuel _ ->
        Ok (None, environment, [])
    | Sst.Symbolic_application application ->
        let* environment, sites =
          analyze_list path environment
            (Symbolic_application_private.arguments application)
        in
        Ok (None, environment, sites)
  in
  let* sites =
    let rec collect sites = function
      | [] -> Ok sites
      | descriptor :: rest -> (
          let definition = Sst_validation.callable_definition descriptor in
          match definition.body with
          | Sst.Checked_exec { body; _ } ->
              let* _, _, next =
                analyze definition
                  [ definition.function_id.function_name ]
                  0 [] body.expression
              in
              collect (sites @ next) rest
          | Sst.Spec_definition _ | Sst.Recursive_spec_definition _
          | Sst.Proof_body _ | Sst.External_specification _
          | Sst.Trusted_external_spec_target _ | Sst.Trusted_external_body _
          | Sst.Symbolic_declaration _ ->
              collect sites rest)
    in
    collect [] descriptors
  in
  Ok
    (List.map
       (fun requirement ->
         {
           requirement with
           transition_requirement_sites =
             List.filter
               (fun site ->
                 same_function_id site.transition_site_callee.function_id
                   requirement.transition_requirement_callee.function_id)
               sites;
         })
       base_requirements)
let prepare_program ~imports ~session ~validated ~invariants
    ?(proof_entry_activations = fun (_ : Sst.function_id) -> [])
    (program : Sst.program) =
  if Sst_validation.program validated != program then
    error "program"
      (match program.functions with
      | definition :: _ -> definition.span
      | [] -> Diagnostic.file_span "<semantic-sst>")
      (Malformed_sst
         "private receipt scheduler requires the exact validated program")
  else
    let imported_registration = imports in
    let imported function_id =
      Option.fold ~none:false
        ~some:(fun registration ->
          Imported_callable.is_imported registration function_id)
        imported_registration
    in
    let all_descriptor_declarations =
      Sst_validation.callable_descriptors validated
    in
    let descriptor_declarations =
      List.filter
        (fun descriptor ->
          not (imported (Sst_validation.callable_id descriptor)))
        all_descriptor_declarations
    in
    let declarations =
      List.map Sst_validation.callable_definition descriptor_declarations
    in
    let type_definitions =
      List.map Sst_validation.type_definition
        (Sst_validation.type_descriptors validated)
    in
    let rank_domains = Vir.rank_domains_of_validated validated in
    let definitions =
      List.map
        (fun descriptor ->
          ((Sst_validation.callable_id descriptor).function_index, descriptor))
        all_descriptor_declarations
    in
    let formula_registry = prepare_formula_registry ~imports ~session ~validated ~invariants ~type_definitions definitions in
    let* termination = prepare_termination validated in
    let* summaries =
      let rec loop lowered = function
        | [] -> Ok (List.rev lowered)
        | descriptor :: rest -> (
            let definition = Sst_validation.callable_definition descriptor in
            match definition.Sst.body with
            | Sst.Checked_exec _ | Sst.Proof_body _
            | Sst.Trusted_external_spec_target _ | Sst.Trusted_external_body _
            | Sst.External_specification (Sst.Imported_unverified_target _) ->
                let* summary = summary_of_callable descriptor in
                loop
                  ((definition.Sst.function_id.function_index, summary)
                  :: lowered)
                  rest
            | Sst.Spec_definition _ | Sst.Recursive_spec_definition _
            | Sst.External_specification _ | Sst.Symbolic_declaration _ ->
                loop lowered rest)
      in
      loop [] all_descriptor_declarations
    in
    let* transition_predecessor_requirements =
      transition_predecessor_analysis ~validated ~invariants
        descriptor_declarations
    in
    let finite_result_eligible_ids =
      List.filter_map
        (fun descriptor ->
          let id = Sst_validation.callable_id descriptor in
          if
            (not (imported id))
            && finite_result_eligible validated rank_domains type_definitions
                 descriptor
          then Some id.function_index
          else None)
        all_descriptor_declarations
    in
    let invariant_receipt_edges =
      Sst_validation.call_edge_descriptors validated
      |> List.filter_map (fun edge ->
          if
            Sst_validation.call_edge_region edge <> Sst_validation.Body_region
            || Sst_validation.call_edge_form edge <> Sst.Exec_call
            || imported
                 (Sst_validation.callable_id
                    (Sst_validation.call_edge_callee edge))
          then None
          else
            let callee = Sst_validation.call_edge_callee edge in
            let definition = Sst_validation.callable_definition callee in
            match
              ( definition.result_type,
                Sst_validation.result_instance_mode validated callee )
            with
            | Sst.Aggregate type_id, (Sst.Exec_instance | Sst.Tracked_instance)
              when Option.is_some
                     (Type_invariant.find_for_type invariants type_id) ->
                Some edge
            | ( ( Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _ | Sst.Aggregate _
                | Sst.Parameter _ | Sst.Application _ ),
                (Sst.Exec_instance | Sst.Tracked_instance | Sst.Ghost_instance)
              ) ->
                None)
    in
    let* invariant_dependencies =
      let rec collect dependencies = function
        | [] -> Ok dependencies
        | edge :: rest -> (
            let caller =
              Sst_validation.call_edge_caller edge
              |> Sst_validation.callable_definition
            in
            let callee =
              Sst_validation.call_edge_callee edge
              |> Sst_validation.callable_definition
            in
            if
              Sst_validation.call_edge_recursive edge
              || same_function_id caller.function_id callee.function_id
            then
              receipt_edge_error edge
                "recursive verified-result receipt edge is unsupported"
            else
              match callee.body with
              | Sst.Checked_exec
                  { provenance = Sst.Authenticated_typedtree _; _ } ->
                  let previous =
                    Option.value ~default:[]
                      (List.assoc_opt caller.function_id.function_index
                         dependencies)
                  in
                  let previous =
                    if
                      List.exists (same_function_id callee.function_id) previous
                    then previous
                    else callee.function_id :: previous
                  in
                  collect
                    ((caller.function_id.function_index, previous)
                    :: List.remove_assoc caller.function_id.function_index
                         dependencies)
                    rest
              | Sst.Checked_exec { provenance = Sst.Raw_semantic_body _; _ } ->
                  receipt_edge_error edge
                    "raw checked body cannot be a receipt dependency"
              | Sst.Proof_body _ ->
                  receipt_edge_error edge
                    "proof result cannot be a verified-callee receipt source"
              | Sst.Spec_definition _ | Sst.Recursive_spec_definition _
              | Sst.External_specification _
              | Sst.Trusted_external_spec_target _ | Sst.Trusted_external_body _
              | Sst.Symbolic_declaration _
                ->
                  receipt_edge_error edge
                    "imported, external, or trusted result cannot be a receipt \
                     dependency")
      in
      collect [] invariant_receipt_edges
    in
    let transition_dependencies =
      List.filter_map
        (fun requirement ->
          let sources =
            requirement.transition_requirement_sites
            |> List.map (fun site -> site.transition_site_source.function_id)
            |> List.sort_uniq compare
          in
          if sources = [] then None
          else
            Some
              ( requirement.transition_requirement_callee.function_id
                  .function_index,
                sources ))
        transition_predecessor_requirements
    in
    let invariant_dependencies =
      List.fold_left
        (fun dependencies (caller, callees) ->
          let previous =
            Option.value ~default:[] (List.assoc_opt caller dependencies)
          in
          let combined =
            List.fold_left
              (fun ids id ->
                if List.exists (same_function_id id) ids then ids else id :: ids)
              previous callees
          in
          (caller, combined) :: List.remove_assoc caller dependencies)
        invariant_dependencies transition_dependencies
    in
    let finite_demand_rows, _finite_local_post_ids =
      finite_result_demand_dependencies ~validated ~type_definitions
        ~eligible_ids:finite_result_eligible_ids ~rank_domains
        all_descriptor_declarations
    in
    let invariant_finite_consumption_rows =
      List.filter_map
        (fun edge ->
          let caller =
            Sst_validation.call_edge_caller edge |> Sst_validation.callable_id
          in
          let callee =
            Sst_validation.call_edge_callee edge |> Sst_validation.callable_id
          in
          if List.mem callee.function_index finite_result_eligible_ids then
            Some
              ( caller,
                {
                  finite_result_callee = callee;
                  finite_result_call_span = Sst_validation.call_edge_span edge;
                } )
          else None)
        invariant_receipt_edges
    in
    let finite_result_consumption_rows =
      (* The pre-existing invariant-result path is already an exact,
         authenticated same-CMT demand. Preserve its independent finite
         consumption while keeping it out of finite-only demand inference. *)
      List.sort_uniq compare
        (finite_demand_rows @ invariant_finite_consumption_rows)
    in
    let* finite_dependencies =
      let rec collect dependencies = function
        | [] -> Ok dependencies
        | ( (caller_id : Sst.function_id),
            ({ finite_result_callee; finite_result_call_span = _ } :
              finite_result_call_site) )
          :: rest -> (
            let callee =
              List.find_opt
                (fun descriptor ->
                  same_function_id
                    (Sst_validation.callable_id descriptor)
                    finite_result_callee)
                all_descriptor_declarations
            in
            match callee with
            | None ->
                error caller_id.function_name
                  (List.find
                     (fun definition ->
                       same_function_id definition.Sst.function_id caller_id)
                     declarations)
                    .span
                  (Malformed_sst
                     "finite-result demand references an absent local callee")
            | Some callee ->
                let callee_id = Sst_validation.callable_id callee in
                let caller_definition =
                  List.find
                    (fun definition ->
                      same_function_id definition.Sst.function_id caller_id)
                    declarations
                in
                if same_direct_recursion termination caller_id callee_id then
                  collect dependencies rest
                else if imported callee_id then
                  error caller_id.function_name caller_definition.span
                    (Malformed_sst
                       "imported finite-result transfer is unsupported")
                else
                  let previous =
                    Option.value ~default:[]
                      (List.assoc_opt caller_id.function_index dependencies)
                  in
                  let previous =
                    if List.exists (same_function_id callee_id) previous then
                      previous
                    else callee_id :: previous
                  in
                  collect
                    ((caller_id.function_index, previous)
                    :: List.remove_assoc caller_id.function_index dependencies)
                    rest)
      in
      collect [] finite_demand_rows
    in
    let finite_dependencies =
      (* An invariant-bearing result is already a finite-result transfer
         demand.  Keep that established edge when the callee
         also satisfies the stricter finite-result eligibility predicate;
         invariant and finite completion remain independently authorized. *)
      List.fold_left
        (fun dependencies (caller, callees) ->
          let eligible_callees =
            List.filter
              (fun (callee : Sst.function_id) ->
                List.mem callee.function_index finite_result_eligible_ids)
              callees
          in
          let previous =
            Option.value ~default:[] (List.assoc_opt caller dependencies)
          in
          let combined =
            List.fold_left
              (fun ids id ->
                if List.exists (same_function_id id) ids then ids else id :: ids)
              previous eligible_callees
          in
          if combined = [] then dependencies
          else (caller, combined) :: List.remove_assoc caller dependencies)
        finite_dependencies invariant_dependencies
    in
    let dependencies =
      List.fold_left
        (fun dependencies (caller, callees) ->
          let previous =
            Option.value ~default:[] (List.assoc_opt caller dependencies)
          in
          let combined =
            List.fold_left
              (fun ids id ->
                if List.exists (same_function_id id) ids then ids else id :: ids)
              previous callees
          in
          (caller, combined) :: List.remove_assoc caller dependencies)
        invariant_dependencies finite_dependencies
    in
    let dependencies =
      Broadcast_scope_private.add_proved_dependencies ~program
        ~definitions:declarations dependencies
    in
    let receipt_dependencies = dependencies in
    let frozen_constructor_ids =
      program.types
      |> List.filter_map (fun (type_definition : Sst.type_definition) ->
          match type_definition.representation with
          | Sst.Abstract_with_evidence
              (Sst.Authenticated_same_cmt_abstraction evidence) ->
              Sst.frozen_spine_prerequisite evidence
              |> Option.map (fun (frozen : Sst.frozen_spine_prerequisite) ->
                  frozen.frozen_constructor)
          | Sst.Revealed
          | Sst.Abstract_with_evidence
              ( Sst.Incomplete_abstraction_evidence _
              | Sst.Proposed_same_cmt_abstraction _ ) ->
              None)
    in
    let executable_definition definition =
      match definition.Sst.body with
      | Sst.Checked_exec _ | Sst.Proof_body _ -> true
      | Sst.Spec_definition _ | Sst.Recursive_spec_definition _
      | Sst.External_specification _ | Sst.Trusted_external_spec_target _
      | Sst.Trusted_external_body _ | Sst.Symbolic_declaration _ ->
          false
    in
    let callable_has_frozen_formal descriptor =
      let definition = Sst_validation.callable_definition descriptor in
      definition.parameters
      |> List.mapi (fun ordinal _ ->
          Sst_validation.frozen_formal_requirement validated descriptor ordinal)
      |> List.exists Option.is_some
    in
    let add_frozen_dependency dependencies caller callee =
      if
        same_function_id caller callee
        || not
             (List.exists
                (fun definition ->
                  same_function_id definition.Sst.function_id callee
                  && executable_definition definition)
                declarations)
      then dependencies
      else
        let previous =
          Option.value ~default:[]
            (List.assoc_opt caller.function_index dependencies)
        in
        let previous =
          if List.exists (same_function_id callee) previous then previous
          else callee :: previous
        in
        (caller.function_index, previous)
        :: List.remove_assoc caller.function_index dependencies
    in
    let frozen_dependencies =
      List.fold_left
        (fun dependencies definition ->
          if not (executable_definition definition) then dependencies
          else
            match
              Sst_validation.find_callable validated definition.function_id
            with
            | Some descriptor when callable_has_frozen_formal descriptor ->
                List.fold_left
                  (fun dependencies constructor ->
                    add_frozen_dependency dependencies definition.function_id
                      constructor)
                  dependencies frozen_constructor_ids
            | Some _ | None -> dependencies)
        [] declarations
      |> fun dependencies ->
      List.fold_left
        (fun dependencies edge ->
          if Sst_validation.call_edge_recursive edge then dependencies
          else
            let caller =
              Sst_validation.call_edge_caller edge
              |> Sst_validation.callable_definition
            in
            let callee_descriptor = Sst_validation.call_edge_callee edge in
            let callee = Sst_validation.callable_definition callee_descriptor in
            if
              executable_definition caller
              && (callable_has_frozen_formal callee_descriptor
                 || List.exists
                      (same_function_id callee.function_id)
                      frozen_constructor_ids)
            then
              add_frozen_dependency dependencies caller.function_id
                callee.function_id
            else dependencies)
        dependencies
        (Sst_validation.call_edge_descriptors validated)
    in
    let dependencies =
      List.fold_left
        (fun dependencies (caller, callees) ->
          List.fold_left
            (fun dependencies callee ->
              let caller =
                declarations
                |> List.find (fun definition ->
                    definition.Sst.function_id.function_index = caller)
                |> fun definition -> definition.Sst.function_id
              in
              add_frozen_dependency dependencies caller callee)
            dependencies callees)
        dependencies frozen_dependencies
    in
    let closure_ids =
      List.fold_left
        (fun ids (caller, callees) ->
          caller
          :: List.fold_left
               (fun ids (callee : Sst.function_id) ->
                 callee.function_index :: ids)
               ids callees)
        [] dependencies
      |> List.sort_uniq Int.compare
    in
    let definition_for_index index =
      List.find_opt
        (fun definition -> definition.Sst.function_id.function_index = index)
        declarations
    in
    let* canonical_keys =
      let rec collect keys = function
        | [] -> Ok keys
        | definition :: rest ->
            let* key =
              match
                Verification_session.canonical_callable_key session definition
              with
              | Ok key -> Ok key
              | Error message ->
                  error definition.function_id.function_name definition.span
                    (Malformed_sst message)
            in
            collect
              ((definition.Sst.function_id.function_index, key) :: keys)
              rest
      in
      collect []
        (List.map Sst_validation.callable_definition all_descriptor_declarations)
    in
    let key_for_index index =
      match List.assoc_opt index canonical_keys with
      | Some key -> key
      | None -> assert false
    in
    let* registry =
      match Verification_session.finite_registry session with
      | Ok registry -> Ok registry
      | Error message ->
          error "program"
            (match declarations with
            | definition :: _ -> definition.span
            | [] -> Diagnostic.file_span "<semantic-sst>")
            (Malformed_sst message)
    in
    let* () =
      let rec register_callables = function
        | [] -> Ok ()
        | callable :: rest ->
            let definition = Sst_validation.callable_definition callable in
            let rec register_formals ordinal = function
              | [] -> Ok ()
              | _parameter :: parameters -> (
                  match
                    Sst_validation.finite_formal_requirement validated callable
                      ordinal
                  with
                  | None -> register_formals (ordinal + 1) parameters
                  | Some requirement ->
                      let certificate =
                        Sst_validation.finite_formal_rank_domain requirement
                      in
                      let* domain =
                        match
                          List.find_opt
                            (fun domain ->
                              String.equal
                                (Vir.rank_domain_id domain)
                                (Sst_validation.rank_domain_id certificate)
                              && String.equal
                                   (Vir.rank_domain_version domain)
                                   (Sst_validation.rank_domain_version
                                      certificate)
                              && String.equal
                                   (Vir.rank_domain_digest domain)
                                   (Sst_validation.rank_snapshot_digest
                                      certificate))
                            rank_domains
                        with
                        | Some domain -> Ok domain
                        | None ->
                            error definition.function_id.function_name
                              definition.span
                              (Malformed_sst
                                 "finite formal slot rank domain is absent or \
                                  stale")
                      in
                      let rank =
                        finite_rank_snapshot_of_certificate domain certificate
                      in
                      let* _ =
                        match
                          Finite_value_registry.register_formal registry
                            ~callee:
                              (key_for_index
                                 definition.function_id.function_index)
                            ~ordinal
                            ~label:
                              (Sst_validation.finite_formal_label requirement)
                            ~pattern_digest:
                              (Sst_validation.finite_formal_pattern_digest
                                 requirement)
                            ~binding_ids:
                              (Sst_validation.finite_formal_binding_ids
                                 requirement)
                            ~mode:
                              (Sst_validation.finite_formal_mode requirement)
                            ~typ:(Sst_validation.finite_formal_type requirement)
                            ~rank
                            ~requirement_digest:
                              (Sst_validation.finite_formal_digest requirement)
                        with
                        | Ok slot -> Ok slot
                        | Error message ->
                            error definition.function_id.function_name
                              definition.span (Malformed_sst message)
                      in
                      register_formals (ordinal + 1) parameters)
            in
            let* () = register_formals 0 definition.parameters in
            register_callables rest
      in
      register_callables all_descriptor_declarations
    in
    let* closure_order =
      match topological_order ~key_for_index dependencies closure_ids with
      | Some order -> Ok order
      | None ->
          error "program"
            (match declarations with
            | definition :: _ -> definition.span
            | [] -> Diagnostic.file_span "<semantic-sst>")
            (Malformed_sst
               "recursive or mutually dependent predecessor/verified-result \
                receipt SCC is unsupported")
    in
    let executable =
      List.filter
        (fun definition ->
          match definition.Sst.body with
          | Sst.Checked_exec _ | Sst.Proof_body _ -> true
          | Sst.Spec_definition _ | Sst.Recursive_spec_definition _
          | Sst.External_specification _ | Sst.Trusted_external_spec_target _
          | Sst.Trusted_external_body _ | Sst.Symbolic_declaration _ ->
              false)
        declarations
    in
    let ordinary =
      List.filter
        (fun definition ->
          not (List.mem definition.Sst.function_id.function_index closure_ids))
        executable
    in
    let invariant_source_ids =
      List.concat_map snd invariant_dependencies
      |> List.map (fun id -> id.Sst.function_index)
      |> List.sort_uniq Int.compare
    in
    let finite_source_ids =
      List.concat_map snd finite_dependencies
      |> List.map (fun id -> id.Sst.function_index)
      |> List.sort_uniq Int.compare
    in
    let source_ids =
      List.sort_uniq Int.compare (invariant_source_ids @ finite_source_ids)
    in
    let dependent_ids =
      List.map fst receipt_dependencies |> List.sort_uniq Int.compare
    in
    let scheduled_of_definition definition =
      let index = definition.Sst.function_id.function_index in
      {
        definition;
        canonical_key = key_for_index index;
        finite_result_eligible = List.mem index finite_result_eligible_ids;
        invariant_receipt_source = List.mem index invariant_source_ids;
        finite_result_source = List.mem index finite_source_ids;
        receipt_source = List.mem index source_ids;
        receipt_dependent = List.mem index dependent_ids;
        receipt_prerequisites =
          Option.value ~default:[] (List.assoc_opt index dependencies)
          |> List.map (fun id -> id.Sst.function_index);
        invariant_receipt_prerequisites =
          Option.value ~default:[] (List.assoc_opt index invariant_dependencies)
          |> List.map (fun id -> id.Sst.function_index);
        finite_result_prerequisites =
          Option.value ~default:[] (List.assoc_opt index finite_dependencies)
          |> List.map (fun id -> id.Sst.function_index);
        finite_result_demand_sites =
          List.filter_map
            (fun (caller, site) ->
              if same_function_id caller definition.function_id then Some site
              else None)
            finite_result_consumption_rows;
        frozen_formal_prerequisites =
          Option.value ~default:[] (List.assoc_opt index frozen_dependencies)
          |> List.map (fun id -> id.Sst.function_index);
        invariant_cell_transition_prerequisites =
          invariant_cell_transition_prerequisites invariants definition;
        transition_predecessor_requirement =
          List.find_opt
            (fun requirement ->
              same_function_id
                requirement.transition_requirement_callee.function_id
                definition.function_id)
            transition_predecessor_requirements;
      }
    in
    let closure =
      List.filter_map definition_for_index closure_order
      |> List.map scheduled_of_definition
    in
    let scheduled = closure @ List.map scheduled_of_definition ordinary in
    Ok
      {
        session;
        imports;
        program;
        validated;
        termination;
        invariants;
        declarations;
        type_definitions;
        rank_domains;
        definitions;
        summaries;
        formula_registry;
        scheduled;
        established_invariant_cell_transitions = ref [];
        proof_entry_activations;
      }
let scheduled_functions prepared = prepared.scheduled
let scheduled_definition scheduled = scheduled.definition
let scheduled_canonical_key scheduled = scheduled.canonical_key
let scheduled_is_receipt_source scheduled = scheduled.receipt_source
let scheduled_is_finite_result_eligible scheduled =
  scheduled.finite_result_eligible
let scheduled_is_invariant_receipt_source scheduled =
  scheduled.invariant_receipt_source
let scheduled_is_finite_result_source scheduled = scheduled.finite_result_source
let scheduled_is_receipt_dependent scheduled = scheduled.receipt_dependent
let scheduled_is_frozen_constructor prepared scheduled =
  (Sst_validation.program prepared.validated).Sst.types
  |> List.exists (fun (type_definition : Sst.type_definition) ->
      match type_definition.representation with
      | Sst.Abstract_with_evidence
          (Sst.Authenticated_same_cmt_abstraction evidence) -> (
          match Sst.frozen_spine_prerequisite evidence with
          | Some frozen ->
              same_function_id scheduled.definition.function_id
                frozen.frozen_constructor
          | None -> false)
      | Sst.Revealed
      | Sst.Abstract_with_evidence
          ( Sst.Incomplete_abstraction_evidence _
          | Sst.Proposed_same_cmt_abstraction _ ) ->
          false)
let scheduled_requires_frozen_constructor prepared scheduled =
  match
    Sst_validation.find_callable prepared.validated
      scheduled.definition.function_id
  with
  | Some callable ->
      List.mapi
        (fun ordinal _ ->
          Sst_validation.frozen_formal_requirement prepared.validated callable
            ordinal)
        scheduled.definition.parameters
      |> List.exists Option.is_some
  | None -> false
let scheduled_receipt_prerequisites scheduled = scheduled.receipt_prerequisites
let scheduled_invariant_receipt_prerequisites scheduled =
  scheduled.invariant_receipt_prerequisites
let scheduled_finite_result_prerequisites scheduled =
  scheduled.finite_result_prerequisites
let scheduled_frozen_formal_prerequisites scheduled =
  scheduled.frozen_formal_prerequisites
let scheduled_has_unestablished_invariant_cell_transition prepared scheduled =
  List.exists
    (fun prerequisite ->
      not
        (List.mem prerequisite
           !(prepared.established_invariant_cell_transitions)))
    scheduled.invariant_cell_transition_prerequisites
let transfer_transition_predecessors prepared scheduled =
  match scheduled.transition_predecessor_requirement with
  | None -> Ok ()
  | Some requirement ->
      let rec preview capabilities = function
        | [] -> (
            if capabilities = [] then
              error scheduled.definition.function_id.function_name
                scheduled.definition.span
                (Malformed_sst
                   "transition predecessor transfer has no exact direct local \
                    established argument")
            else
              match
                Verification_session.activate_transition_predecessors
                  prepared.session (List.rev capabilities)
              with
              | Ok () -> Ok ()
              | Error message ->
                  error scheduled.definition.function_id.function_name
                    scheduled.definition.span (Malformed_sst message))
        | site :: rest -> (
            match
              Verification_session.preview_transition_predecessor
                prepared.session ~validated:prepared.validated
                ~invariants:prepared.invariants
                ~caller:site.transition_site_caller
                ~callee:site.transition_site_callee
                ~source:site.transition_site_source
                ~call_span:site.transition_site_call_span
                ~source_call_span:site.transition_site_source_call_span
                ~call_path:site.transition_site_call_path
                ~actual:site.transition_site_actual
                ~actual_version:site.transition_site_actual_version
                ~actual_path:site.transition_site_actual_path
                ~formal:site.transition_site_formal
                ~root:site.transition_site_root ~mode:site.transition_site_mode
                ~typ:site.transition_site_type
                ~owned_version:site.transition_site_owned_version
                ~obligation_snapshot:site.transition_site_obligation_snapshot
                ~branch_intersection:site.transition_site_branch_intersection
            with
            | Ok capability -> preview (capability :: capabilities) rest
            | Error message ->
                error scheduled.definition.function_id.function_name
                  site.transition_site_call_span (Malformed_sst message))
      in
      preview [] requirement.transition_requirement_sites
let lower_scheduled prepared scheduled =
  let definition = scheduled.definition in
  let* summary =
    match
      List.assoc_opt definition.Sst.function_id.function_index
        prepared.summaries
    with
    | Some summary -> Ok summary
    | None ->
        error definition.function_id.function_name definition.span
          (Malformed_sst "validated callable is absent from private scheduler")
  in
  let* lowered = lower_summary ~verification_session:prepared.session ?imports:prepared.imports ~formula_registry:prepared.formula_registry
    ~proof_entry_activations:prepared.proof_entry_activations ~finite_result_eligible:scheduled.finite_result_eligible ~finite_result_candidate:scheduled.finite_result_source
    ~finite_result_demand_sites:scheduled.finite_result_demand_sites prepared.validated prepared.rank_domains prepared.invariants
    prepared.type_definitions prepared.definitions prepared.summaries prepared.termination summary in
  Logical_spec_capability_private.For_testing.trace_authority definition.function_id.function_name;
  Ok lowered
let lowered_execution lowered = lowered.execution
let lowered_proof_activation_batch lowered = lowered.proof_activation_batch
let finalize_invariant_cell_close_owned prepared scheduled lowered results =
  match lowered.provisional_invariant_cell_close with
  | None ->
      let frozen_transition =
        match Sst.function_shared_scalar_transitions scheduled.definition with
        | transition :: _ ->
            Option.is_some
              (frozen_spine_for_transition prepared.validated
                 scheduled.definition.function_id transition)
        | [] -> false
      in
      if not frozen_transition then Ok None
      else
        let established =
          List.length results = List.length lowered.execution.Vir.obligations
          && List.for_all
               (fun (result : Solver_backend.obligation_result) ->
                 result.outcome = Solver_backend.Verified)
               results
        in
        if established then (
          prepared.established_invariant_cell_transitions :=
            scheduled.definition.function_id.function_index
            :: !(prepared.established_invariant_cell_transitions);
          Verification_session.note_invariant_cell_close prepared.session);
        Ok (Some established)
  | Some provisional ->
      let established =
        List.exists
          (fun (result : Solver_backend.obligation_result) ->
            result.outcome = Solver_backend.Verified
            &&
            match result.obligation.Vir.kind with
            | Vir.Invariant_validity
                {
                  operation;
                  boundary =
                    Vir.Shared_invariant_close { entry_epoch = 0; final_epoch };
                  _;
                } ->
                operation.function_index = provisional.operation.function_index
                && String.equal operation.function_name
                     provisional.operation.function_name
                && final_epoch = provisional.final_epoch
            | Vir.Arithmetic_safety _ | Vir.Assertion _ | Vir.Local_assertion _
            | Vir.Postcondition _ | Vir.Call_precondition _
            | Vir.Callback_precondition _ | Vir.Invariant_validity _
            | Vir.Entry_measure_nonnegative _
            | Vir.Recursive_call_measure_nonnegative _
            | Vir.Recursive_call_strict_descent _ ->
                false)
          results
      in
      let* () =
        if not established then Ok ()
        else
          match
            Shared_invariant_cell_private.close_cell provisional.authority
              ~operation:provisional.operation
              ~final_epoch:provisional.final_epoch
          with
          | Ok () -> Ok ()
          | Error message ->
              error scheduled.definition.function_id.function_name
                scheduled.definition.span (Malformed_sst message)
      in
      let* () =
        match Shared_invariant_cell_private.destroy provisional.authority with
        | Ok () -> Ok ()
        | Error message ->
            error scheduled.definition.function_id.function_name
              scheduled.definition.span (Malformed_sst message)
      in
      if established then
        prepared.established_invariant_cell_transitions :=
          provisional.operation.function_index
          :: !(prepared.established_invariant_cell_transitions);
      Ok (Some established)
let finalize_invariant_cell_close prepared scheduled lowered results =
  let finalized =
    finalize_invariant_cell_close_owned prepared scheduled lowered results
  in
  match finalized with
  | Error _ -> finalized
  | Ok _ ->
      let verified =
        List.length results = List.length lowered.execution.Vir.obligations
        && List.for_all
             (fun (result : Solver_backend.obligation_result) ->
               result.outcome = Solver_backend.Verified)
             results
      in
      Broadcast_scope_private.mark_completed ~program:prepared.program
        scheduled.definition.function_id ~verified;
      finalized
type receipt_manifests = {
  invariant_manifest : Verification_session.obligation_manifest option;
  finite_result_manifest : Verification_session.finite_result_manifest option;
  frozen_constructor_manifest :
    Verification_session.frozen_constructor_manifest option;
  owned_contents_manifest : Verification_session.owned_contents_manifest option;
}
let finite_result_snapshot_for_scheduled prepared scheduled =
  let* domain =
    match
      static_rank_domain_for_type prepared.program.parametric_adts
        prepared.rank_domains prepared.type_definitions
        scheduled.definition.result_type
    with
    | Some domain -> Ok domain
    | None ->
        error scheduled.definition.function_id.function_name
          scheduled.definition.span
          (Malformed_sst "finite result rank domain is absent or ambiguous")
  in
  let* certificate =
    match
      Sst_validation.rank_domains prepared.validated
      |> List.find_opt (fun certificate ->
          String.equal
            (Sst_validation.rank_domain_id certificate)
            (Vir.rank_domain_id domain)
          && String.equal
               (Sst_validation.rank_domain_version certificate)
               (Vir.rank_domain_version domain)
          && String.equal
               (Sst_validation.rank_snapshot_digest certificate)
               (Vir.rank_domain_digest domain))
    with
    | Some certificate -> Ok certificate
    | None ->
        error scheduled.definition.function_id.function_name
          scheduled.definition.span
          (Malformed_sst "finite result rank snapshot is absent or stale")
  in
  let rank = finite_rank_snapshot_of_certificate domain certificate in
  match
    Verification_session.finite_result_snapshot prepared.session
      ~validated:prepared.validated scheduled.definition ~rank
  with
  | Ok snapshot -> Ok snapshot
  | Error message ->
      error scheduled.definition.function_id.function_name
        scheduled.definition.span (Malformed_sst message)
let authorize_receipt_obligations prepared scheduled execution =
  let* owned_contents_manifest =
    match
      Verification_session.authorize_owned_contents_obligations prepared.session
        scheduled.definition execution
    with
    | Ok manifest -> Ok manifest
    | Error message ->
        error scheduled.definition.function_id.function_name
          scheduled.definition.span (Malformed_sst message)
  in
  let* frozen_constructor_manifest =
    match
      Verification_session.authorize_frozen_constructor_obligations
        prepared.session scheduled.definition execution
    with
    | Ok manifest -> Ok manifest
    | Error message ->
        error scheduled.definition.function_id.function_name
          scheduled.definition.span (Malformed_sst message)
  in
  let* invariant_manifest =
    if not scheduled.invariant_receipt_source then Ok None
    else
      let* snapshot =
        match
          Verification_session.callee_snapshot prepared.session
            ~validated:prepared.validated ~invariants:prepared.invariants
            scheduled.definition
        with
        | Ok snapshot -> Ok snapshot
        | Error message ->
            error scheduled.definition.function_id.function_name
              scheduled.definition.span (Malformed_sst message)
      in
      match
        Verification_session.authorize_obligations prepared.session snapshot
          execution
      with
      | Ok manifest -> Ok (Some manifest)
      | Error message ->
          error scheduled.definition.function_id.function_name
            scheduled.definition.span (Malformed_sst message)
  in
  let* finite_result_manifest =
    if not scheduled.finite_result_source then Ok None
    else
      let* snapshot = finite_result_snapshot_for_scheduled prepared scheduled in
      match
        Verification_session.authorize_finite_result_obligations
          prepared.session snapshot execution
      with
      | Ok manifest -> Ok manifest
      | Error message ->
          error scheduled.definition.function_id.function_name
            scheduled.definition.span (Malformed_sst message)
  in
  Ok
    {
      invariant_manifest;
      finite_result_manifest;
      frozen_constructor_manifest;
      owned_contents_manifest;
    }
let complete_owned_contents prepared scheduled manifest execution results =
  match
    Verification_session.complete_owned_contents prepared.session
      manifest.owned_contents_manifest execution results
  with
  | Ok completed -> Ok completed
  | Error message ->
      error scheduled.definition.function_id.function_name
        scheduled.definition.span (Malformed_sst message)
let issue_receipt prepared scheduled manifest results =
  let* invariant_issued =
    if not scheduled.invariant_receipt_source then Ok true
    else if
      match !suppress_invariant_completion_for_callable_for_testing with
      | Some callable ->
          String.equal scheduled.definition.function_id.function_name callable
      | None -> false
    then Ok false
    else
      let* manifest =
        match manifest.invariant_manifest with
        | Some manifest -> Ok manifest
        | None ->
            error scheduled.definition.function_id.function_name
              scheduled.definition.span
              (Malformed_sst
                 "invariant receipt source has no authenticated obligation \
                  manifest")
      in
      let* completion =
        match
          Verification_session.complete prepared.session manifest results
        with
        | Ok completion -> Ok completion
        | Error message ->
            error scheduled.definition.function_id.function_name
              scheduled.definition.span (Malformed_sst message)
      in
      match Verification_session.issue prepared.session completion with
      | Ok () -> Ok true
      | Error message ->
          error scheduled.definition.function_id.function_name
            scheduled.definition.span (Malformed_sst message)
  in
  let* finite_issued =
    if not scheduled.finite_result_source then Ok true
    else
      match manifest.finite_result_manifest with
      | None -> Ok false
      | Some manifest -> (
          let* completion =
            match
              Verification_session.complete_finite_result prepared.session
                manifest results
            with
            | Ok completion -> Ok completion
            | Error message ->
                error scheduled.definition.function_id.function_name
                  scheduled.definition.span (Malformed_sst message)
          in
          match
            Verification_session.issue_finite_result prepared.session completion
          with
          | Ok () -> Ok true
          | Error message ->
              error scheduled.definition.function_id.function_name
                scheduled.definition.span (Malformed_sst message))
  in
  Ok (invariant_issued, finite_issued)
let trusted_external_body_declarations prepared =
  List.filter_map
    (fun definition ->
      match definition.Sst.body with
      | Sst.Trusted_external_body
          (Sst.Authenticated_external_body { declaration_span; witness_span; _ })
        ->
          Some
            {
              Vir.function_ref = vir_function_ref definition.function_id;
              mode = definition.mode;
              declaration_span;
              witness_span;
              requires_count = List.length definition.contracts.requires;
              ensures_count = List.length definition.contracts.ensures;
            }
      | Sst.Trusted_external_body (Sst.Raw_external_body _) -> assert false
      | _ -> None)
    prepared.declarations
let staged_program prepared functions =
  {
    Vir.policy = prepared.program.policy;
    rank_domains = prepared.rank_domains;
    trusted_external_body_declarations =
      trusted_external_body_declarations prepared;
    functions;
  }
let error_to_string error =
  let unsupported =
    match error.unsupported with
    | Ghost_call -> "frontend contract carrier reached symbolic execution"
    | Or_pattern -> "or-patterns await symbolic pattern lowering"
    | Missing_summary callee ->
        Printf.sprintf "missing same-unit summary for %s#%d"
          callee.function_name callee.function_index
    | Recursion_awaits_totality callee ->
        Printf.sprintf "recursion awaits totality for %s#%d"
          callee.function_name callee.function_index
    | Missing_decreases ->
        "direct recursion requires exactly one decreases measure"
    | Duplicate_decreases ->
        "direct recursion has more than one decreases measure"
    | Inapplicable_decreases ->
        "decreases measure is not allowed without a resolved direct self-call"
    | Non_integer_decreases -> "decreases measure must have type int"
    | Malformed_sst message -> "malformed SST: " ^ message
  in
  let span = error.span in
  Printf.sprintf "%s: %s at %s:%d:%d-%d:%d" error.function_name unsupported
    (Filename.basename span.file)
    span.start_pos.line span.start_pos.column span.end_pos.line
    span.end_pos.column
module For_testing = struct
  let reset_authority_observation () = authority_observation_events := Some []
  let authority_observation = authority_observation
  let suppress_recursive_proof_summaries_for_testing suppress =
    suppress_recursive_proof_summaries := suppress
  let suppress_recursive_proof_calls_for_testing suppress =
    suppress_recursive_proof_calls := suppress
  let suppress_proof_call_for_testing edge =
    suppress_proof_call_for_testing := edge
  let suppress_proof_summary_for_testing edge =
    suppress_proof_summary_for_testing := edge
  let suppress_local_assertion_successor_for_testing suppress =
    suppress_local_assertion_successor_for_testing := suppress
  let suppress_local_assertion_successor_sites_for_testing sites =
    suppress_local_assertion_successor_sites_for_testing := sites
  let proof_activation_snapshot_attack_for_testing attack =
    proof_activation_snapshot_attack_for_testing := attack
  let reset_local_assertion_export_observation () =
    observed_local_assertion_exports := 0
  let local_assertion_export_count () = !observed_local_assertion_exports
  let observe_proof_activation_prefinalizer_for_testing observer callback =
    if !proof_activation_prefinalizer_observer_for_testing <> None then
      invalid_arg "proof activation prefinalizer observer is already active";
    proof_activation_prefinalizer_observer_for_testing := Some observer;
    Fun.protect
      ~finally:(fun () ->
        proof_activation_prefinalizer_observer_for_testing := None)
      callback
  let reset_proof_call_spent_visit_observation () =
    observed_proof_call_spent_visits := 0;
    observed_proof_call_ledger_summaries := [];
    observed_exec_region_proof_summary_exports := []
  let proof_call_spent_visit_count () = !observed_proof_call_spent_visits
  let count_proof_call_observations ~caller ~callee observations =
    List.fold_left
      (fun count (observed_caller, observed_callee) ->
        if
          String.equal caller observed_caller
          && String.equal callee observed_callee
        then count + 1
        else count)
      0 observations
  let proof_call_ledger_summary_count ~caller ~callee =
    count_proof_call_observations ~caller ~callee
      !observed_proof_call_ledger_summaries
  let exec_region_proof_summary_export_count ~caller ~callee =
    count_proof_call_observations ~caller ~callee
      !observed_exec_region_proof_summary_exports
  let suppress_exec_summary_for_callable_for_testing callable =
    suppress_exec_summary_for_callable := callable
  let suppress_exec_ensure_assumption_for_callable_for_testing callable =
    suppress_exec_ensure_assumption_for_callable := callable
  let suppress_invariant_completion_for_callable_for_testing callable =
    suppress_invariant_completion_for_callable_for_testing := callable
  let suppress_recursive_spec_result_consumption_for_testing suppress =
    suppress_recursive_spec_result_consumption_for_testing := suppress
  let recursive_spec_application_identity_forgery_for_testing forge =
    recursive_spec_application_identity_forgery_for_testing := forge
  let suppress_aggregate_recursive_source_traversal_for_testing suppress =
    suppress_aggregate_recursive_source_traversal_for_testing := suppress
  let suppress_frozen_spine_child_witness_for_testing suppress =
    suppress_frozen_spine_child_witness_for_testing := suppress
  let source_route_attack_observation_for_testing enabled =
    source_route_attack_observation_enabled_for_testing := enabled
  let source_route_attack_observation_active () =
    !source_route_attack_observation_enabled_for_testing
  let aggregate_recursive_rank_route_observation_count () =
    !aggregate_recursive_rank_route_observations
  let aggregate_recursive_argument_route_observation_count () =
    !aggregate_recursive_argument_route_observations
  let reset_aggregate_recursive_route_observations () =
    aggregate_recursive_rank_route_observations := 0;
    aggregate_recursive_argument_route_observations := 0
  let id function_index function_name = { Sst.function_index; function_name }
  let rejected dependencies remaining =
    topological_order
      ~key_for_index:(fun index -> Printf.sprintf "key-%04d" index)
      dependencies remaining
    = None
  let scheduler_matrix () =
    let self = id 0 "self" in
    let left = id 0 "left" in
    let right = id 1 "right" in
    let dependencies = [ (2, [ left ]); (3, [ right ]) ] in
    let key_for_index = function
      | 0 -> "a"
      | 1 -> "b"
      | 2 -> "c"
      | 3 -> "d"
      | _ -> assert false
    in
    let forward =
      topological_order ~key_for_index dependencies [ 0; 1; 2; 3 ]
    in
    let reversed =
      topological_order ~key_for_index dependencies [ 3; 2; 1; 0 ]
    in
    [
      Printf.sprintf "scheduler=self-scc result=%s"
        (if rejected [ (0, [ self ]) ] [ 0 ] then "rejected" else "accepted");
      Printf.sprintf "scheduler=mutual-scc result=%s"
        (if rejected [ (0, [ right ]); (1, [ left ]) ] [ 0; 1 ] then "rejected"
         else "accepted");
      Printf.sprintf "scheduler=input-permutation stable=%b" (forward = reversed);
    ]
end
