type certified_structural_rank = Sst_validation.validated_rank_domain

type decrease_domain =
  | Integer_height
  | Structural_rank of certified_structural_rank
  | Parametric_direct_edge of Parametric_adt.t
  | Frozen_spine_direct_edge of Sst.frozen_spine_prerequisite

type callable = {
  id : Sst.function_id;
  mode : Sst.verification_mode;
  span : Diagnostic.span;
  validated : Sst_validation.callable_descriptor option;
}

type call_edge = {
  caller : callable;
  callee : callable;
  call_form : Sst.call_form;
  recursive : bool;
  span : Diagnostic.span;
  region : Sst_validation.call_edge_region;
}

type call_graph = {
  callables : callable list;
  edges : call_edge list;
}

type scc = {
  members : callable list;
  edges : call_edge list;
  recursive : bool;
}

type analysis = {
  graph : call_graph;
  components : scc list;
  authenticated : bool;
}

type recursion_kind =
  | Direct_self
  | Mutual

type error_kind =
  | Missing_measure
  | Duplicate_measure
  | Inapplicable_measure
  | Non_integer_measure
  | Recursive_measure
  | Unsupported_mutual_scc of Sst.function_id list
  | Unsupported_recursive_mode of
      Sst.verification_mode * Sst.function_id list
  | Raw_analysis_only

type error = {
  function_id : Sst.function_id option;
  span : Diagnostic.span;
  kind : error_kind;
}

type edge_intent = {
  edge : call_edge;
  measure : Sst_validation.decrease_descriptor;
  domain : decrease_domain;
}

type entry_intent = {
  callable : callable;
  measure : Sst_validation.decrease_descriptor;
  domain : decrease_domain;
}

type pending_summary = {
  callable : callable;
  group : scc;
  entry : entry_intent;
  edges : edge_intent list;
}

type plan = {
  pending : pending_summary list;
}

let ( let* ) result continuation =
  match result with
  | Ok value -> continuation value
  | Error _ as error -> error

let same_function_id (left : Sst.function_id) (right : Sst.function_id) =
  left.function_index = right.function_index
  && String.equal left.function_name right.function_name

let same_span (left : Diagnostic.span) (right : Diagnostic.span) =
  String.equal left.file right.file
  && left.start_pos.line = right.start_pos.line
  && left.start_pos.column = right.start_pos.column
  && left.end_pos.line = right.end_pos.line
  && left.end_pos.column = right.end_pos.column

let callable_id (callable : callable) = callable.id
let callable_mode (callable : callable) = callable.mode
let call_edge_caller (edge : call_edge) = edge.caller
let call_edge_callee (edge : call_edge) = edge.callee
let call_edge_form (edge : call_edge) = edge.call_form
let call_edge_recursive (edge : call_edge) = edge.recursive
let call_edge_span (edge : call_edge) = edge.span
let call_edge_region (edge : call_edge) = edge.region

let edge_caller_id edge =
  callable_id (call_edge_caller edge)

let edge_callee_id edge =
  callable_id (call_edge_callee edge)

let adjacency edges id =
  List.filter_map
    (fun edge ->
      if same_function_id (edge_caller_id edge) id then
        Some (edge_callee_id edge)
      else None)
    edges

let reachable edges source target =
  let rec visit seen = function
    | [] -> false
    | current :: rest ->
        if same_function_id current target then true
        else if List.exists (same_function_id current) seen then
          visit seen rest
        else
          visit (current :: seen) (adjacency edges current @ rest)
  in
  visit [] [ source ]

let contains_id members id =
  List.exists
    (fun member -> same_function_id (callable_id member) id)
    members

let component_edges edges members =
  List.filter
    (fun edge ->
      contains_id members (edge_caller_id edge)
      && contains_id members (edge_callee_id edge))
    edges

let build_components (graph : call_graph) =
  let rec loop components remaining =
    match remaining with
    | [] -> List.rev components
    | first :: _ ->
        let first_id = callable_id first in
        let members =
          List.filter
            (fun candidate ->
              let candidate_id = callable_id candidate in
              reachable graph.edges first_id candidate_id
              && reachable graph.edges candidate_id first_id)
            remaining
        in
        let remaining =
          List.filter
            (fun candidate ->
              not (contains_id members (callable_id candidate)))
            remaining
        in
        let edges = component_edges graph.edges members in
        let recursive =
          match members with
          | [ member ] ->
              let id = callable_id member in
              List.exists
                (fun edge ->
                  same_function_id (edge_caller_id edge) id
                  && same_function_id (edge_callee_id edge) id)
                edges
          | _ :: _ :: _ -> true
          | [] -> assert false
        in
        loop ({ members; edges; recursive } :: components) remaining
  in
  loop [] graph.callables

let callable_of_descriptor descriptor =
  let definition = Sst_validation.callable_definition descriptor in
  {
    id = Sst_validation.callable_id descriptor;
    mode = Sst_validation.callable_mode descriptor;
    span = definition.Sst.span;
    validated = Some descriptor;
  }

let find_callable callables id =
  List.find_opt (fun callable -> same_function_id callable.id id) callables

let edge_of_descriptor callables descriptor =
  let caller_id =
    Sst_validation.call_edge_caller descriptor
    |> Sst_validation.callable_id
  in
  let callee_id =
    Sst_validation.call_edge_callee descriptor
    |> Sst_validation.callable_id
  in
  let caller = Option.get (find_callable callables caller_id) in
  let callee = Option.get (find_callable callables callee_id) in
  {
    caller;
    callee;
    call_form = Sst_validation.call_edge_form descriptor;
    recursive = Sst_validation.call_edge_recursive descriptor;
    span = Sst_validation.call_edge_span descriptor;
    region = Sst_validation.call_edge_region descriptor;
  }

let analyze validated =
  let callables =
    List.map callable_of_descriptor
      (Sst_validation.callable_descriptors validated)
  in
  let edges =
    List.map (edge_of_descriptor callables)
      (Sst_validation.call_edge_descriptors validated)
  in
  let graph =
    {
      callables;
      edges;
    }
  in
  { graph; components = build_components graph; authenticated = true }

let expression_children (expression : Sst.expression) =
  match expression.expression_desc with
  | Sst.Bv_literal _ -> []
  | Sst.Bv_int_to_bv_mod { input; _ }
  | Sst.Bv_to_int_unsigned input
  | Sst.Bv_to_int_signed input
  | Sst.Bv_not input ->
      [ input ]
  | Sst.Bv_binary (_, left, right) | Sst.Bv_compare (_, left, right) ->
      [ left; right ]
  | Sst.Lift_runtime_int operand -> [ operand ]
  | Sst.Tuple_value values -> List.map snd values
  | Sst.Record_value { fields; _ } -> List.map snd fields
  | Sst.Constructor_value { arguments; _ } -> arguments
  | Sst.Field_read { record; _ } -> [ record ]
  | Sst.Field_write { value; _ }
  | Sst.Shared_scalar_field_write { value; _ }
  | Sst.Mutable_write { value; _ } ->
      [ value ]
  | Sst.Owned_tree_nested_write { value; _ } -> [ value ]
  | Sst.Owned_tree_rebase _ -> []
  | Sst.Let_mutable (_, initial, body) -> [ initial; body ]
  | Sst.Let (bindings, body) -> List.map snd bindings @ [ body ]
  | Sst.Sequence (first, second)
  | Sst.Compare (_, first, second)
  | Sst.Boolean_binary (_, first, second) ->
      [ first; second ]
  | Sst.If (condition, consequent, alternative) ->
      condition :: consequent :: Option.to_list alternative
  | Sst.Match (scrutinee, cases) ->
      scrutinee
      :: List.concat_map
           (fun case -> Option.to_list case.Sst.case_guard @ [ case.case_body ])
           cases
  | Sst.Checked_arithmetic (_, operands) -> operands
  | Sst.Boolean_not operand -> [ operand ]
  | Sst.Direct_call { arguments; _ } ->
      List.filter_map
        (function
          | Sst.Value_argument { value; _ } -> Some value
          | Sst.Callback_argument _ -> None)
        arguments
  | Sst.Callback_call { arguments; _ } | Sst.Callback_requires { arguments; _ } ->
      List.map snd arguments
  | Sst.Callback_ensures { application = { arguments; _ }; result } ->
      List.map snd arguments @ [ result ]
  | Sst.Symbolic_application application ->
      Symbolic_application_private.arguments application
  | Sst.Forall quantifier | Sst.Exists quantifier ->
      [ quantifier.quantifier_body ]
  | Sst.Use_type_invariant { value; _ } -> [ value ]
  | Sst.Reveal _ | Sst.Reveal_with_fuel _ -> []
  | Sst.Proof_region body -> [ body ]
  | Sst.Local_assert { predicate; _ } -> [ predicate ]
  | Sst.Old payload -> [ payload ]
  | Sst.Int_constant _ | Sst.Bool_constant _ | Sst.Unit_constant
  | Sst.Variable _ | Sst.Mutable_read _ | Sst.Optional_absent
  | Sst.Logical_constant_reference _ ->
      []
  | Sst.Optional_present payload | Sst.Optional_forward payload -> [ payload ]

let rec raw_expression_edges callables caller region expression =
  let current =
    match expression.Sst.expression_desc with
    | Sst.Direct_call _
      when Spec_function_sst_private.semantic_children expression <> None ->
        []
    | Sst.Direct_call { call_form; callee; recursive; _ } -> (
        match find_callable callables callee with
        | None -> []
        | Some callee ->
            [
              {
                caller;
                callee;
                call_form;
                recursive;
                span = expression.span;
                region;
              };
            ])
    | _ -> []
  in
  let children =
    Option.value ~default:(expression_children expression)
      (Spec_function_sst_private.semantic_children expression)
  in
  current
  @ List.concat_map
      (raw_expression_edges callables caller region)
      children

let raw_callable_expressions (definition : Sst.function_definition) =
  let contracts = definition.contracts in
  List.map
    (fun (clause : Sst.predicate_clause) ->
      (Sst_validation.Requires_region, clause.predicate.expression))
    contracts.requires
  @ List.map
      (fun (clause : Sst.ensures_clause) ->
        (Sst_validation.Ensures_region, clause.predicate.expression))
      contracts.ensures
  @ List.map
      (fun (clause : Sst.predicate_clause) ->
        (Sst_validation.Decreases_region, clause.predicate.expression))
      contracts.decreases
  @ List.map
      (fun (clause : Sst.predicate_clause) ->
        (Sst_validation.Assertions_region, clause.predicate.expression))
      contracts.assertions
  @
  match definition.body with
  | Sst.Checked_exec { body; _ }
  | Sst.Spec_definition body
  | Sst.Recursive_spec_definition { body; _ }
  | Sst.Proof_body { body; _ } ->
      [ (Sst_validation.Body_region, body.expression) ]
  | Sst.External_specification _
  | Sst.Trusted_external_spec_target _
  | Sst.Trusted_external_body _
  | Sst.Symbolic_declaration _ ->
      []

(* Raw graph handles are deliberately distinct from validated descriptors.
   They retain cycle evidence for pre-VIR diagnostics but cannot issue a plan
   or cross the Sst_validation callable boundary. *)
let analyze_raw (program : Sst.program) =
  let callables =
    List.map
      (fun (definition : Sst.function_definition) ->
        {
          id = definition.function_id;
          mode = definition.mode;
          span = definition.span;
          validated = None;
        })
      program.functions
  in
  let edges =
    List.concat_map
      (fun callable ->
        let definition =
          List.find
            (fun (definition : Sst.function_definition) ->
              same_function_id definition.function_id callable.id)
            program.functions
        in
        List.concat_map
          (fun (region, expression) ->
            raw_expression_edges callables callable region expression)
          (raw_callable_expressions definition))
      callables
  in
  let graph = { callables; edges } in
  { graph; components = build_components graph; authenticated = false }

let graph (analysis : analysis) = analysis.graph
let graph_callables (graph : call_graph) = graph.callables
let graph_edges (graph : call_graph) = graph.edges
let sccs (analysis : analysis) = analysis.components
let scc_members (component : scc) = component.members
let scc_edges (component : scc) = component.edges

let scc_modes component =
  List.map callable_mode component.members

let scc_is_recursive component = component.recursive

let scc_recursion_kind component =
  if not component.recursive then None
  else
    match component.members with
    | [ _ ] -> Some Direct_self
    | _ :: _ :: _ -> Some Mutual
    | [] -> assert false

let error ?function_id span kind = Error { function_id; span; kind }

let first_function (component : scc) =
  match component.members with
  | member :: _ -> callable_id member
  | [] -> assert false

let first_edge_span (component : scc) =
  match component.edges with
  | edge :: _ -> call_edge_span edge
  | [] -> (List.hd component.members).span

let find_component analysis id =
  List.find_opt
    (fun component -> contains_id component.members id)
    analysis.components

let clause_span decrease =
  Sst_validation.decrease_clause decrease
  |> Sst_validation.contract_clause_span

let direct_body_edges (component : scc) =
  List.filter
    (fun edge -> call_edge_region edge = Sst_validation.Body_region)
    component.edges

let non_body_recursive_edges (component : scc) =
  List.filter
    (fun edge -> call_edge_region edge <> Sst_validation.Body_region)
    component.edges

let direct_measure callable =
  let id = callable_id callable in
  let descriptor = Option.get callable.validated in
  match Sst_validation.callable_decrease descriptor with
  | Sst_validation.Direct_integer_decrease measure ->
      Ok (measure, Integer_height)
  | Sst_validation.Direct_structural_decrease (measure, domain) ->
      Ok (measure, Structural_rank domain)
  | Sst_validation.Direct_parametric_decrease (measure, descriptor) ->
      Ok (measure, Parametric_direct_edge descriptor)
  | Sst_validation.Direct_frozen_spine_decrease (measure, frozen) ->
      Ok (measure, Frozen_spine_direct_edge frozen)
  | Sst_validation.No_decrease ->
      let definition = Sst_validation.callable_definition descriptor in
      error ~function_id:id definition.span Missing_measure
  | Sst_validation.Missing_decrease span ->
      error ~function_id:id span Missing_measure
  | Sst_validation.Duplicate_decrease span ->
      error ~function_id:id span Duplicate_measure
  | Sst_validation.Inapplicable_decrease span ->
      error ~function_id:id span Inapplicable_measure
  | Sst_validation.Non_integer_decrease span ->
      error ~function_id:id span Non_integer_measure
  | Sst_validation.Recursive_decrease span ->
      error ~function_id:id span Recursive_measure

let reject_mutual analysis =
  match
    List.find_opt
      (fun component ->
        match scc_recursion_kind component with
        | Some Mutual -> true
        | Some Direct_self | None -> false)
      analysis.components
  with
  | None -> Ok ()
  | Some component ->
      let ids = List.map callable_id component.members in
      error ~function_id:(first_function component)
        (first_edge_span component)
        (Unsupported_mutual_scc ids)

let reject_unsupported_modes analysis =
  match
    List.find_opt
      (fun component ->
        component.recursive
        && List.exists
             (fun member ->
               let mode = callable_mode member in
               mode <> Sst.Exec && mode <> Sst.Proof && mode <> Sst.Spec)
             component.members)
      analysis.components
  with
  | None -> Ok ()
  | Some component ->
      let first = List.hd component.members in
      let ids = List.map callable_id component.members in
      error ~function_id:first.id (first_edge_span component)
        (Unsupported_recursive_mode (first.mode, ids))

let precheck analysis =
  let* () = reject_mutual analysis in
  reject_unsupported_modes analysis

let reject_inapplicable_decreases analysis =
  let rec loop = function
    | [] -> Ok ()
    | callable :: rest ->
        let id = callable_id callable in
        let descriptor = Option.get callable.validated in
        let contract = Sst_validation.callable_contract descriptor in
        let decreases = Sst_validation.contract_decreases contract in
        let recursive =
          match find_component analysis id with
          | Some component -> component.recursive
          | None -> false
        in
        if decreases <> [] && not recursive then
          error ~function_id:id (clause_span (List.hd decreases))
            Inapplicable_measure
        else loop rest
  in
  loop analysis.graph.callables

let prepare_direct (component : scc) =
  match component.members with
  | [ callable ] ->
      let id = callable_id callable in
      let non_body = non_body_recursive_edges component in
      let body_edges = direct_body_edges component in
      let recursive_measure =
        List.find_opt
          (fun edge ->
            call_edge_region edge = Sst_validation.Decreases_region)
          non_body
      in
      (match recursive_measure with
      | Some edge ->
          error ~function_id:id (call_edge_span edge) Recursive_measure
      | None -> (
          match (body_edges, non_body) with
          | [], edge :: _ ->
              error ~function_id:id (call_edge_span edge)
                Inapplicable_measure
          | [], [] -> assert false
          | _ :: _, edge :: _ ->
              error ~function_id:id (call_edge_span edge)
                Inapplicable_measure
          | _ :: _, [] ->
              let* measure, domain = direct_measure callable in
              let entry = { callable; measure; domain } in
              let edges =
                List.map (fun edge -> { edge; measure; domain }) body_edges
              in
              Ok { callable; group = component; entry; edges }))
  | _ -> assert false

let prepare analysis =
  let* () = precheck analysis in
  let* () =
    if analysis.authenticated then Ok ()
    else
      let span =
        match analysis.graph.callables with
        | callable :: _ -> callable.span
        | [] -> Diagnostic.file_span "<semantic-sst>"
      in
      error span Raw_analysis_only
  in
  let* () = reject_inapplicable_decreases analysis in
  let rec loop pending = function
    | [] -> Ok { pending = List.rev pending }
    | component :: rest -> (
        match scc_recursion_kind component with
        | None -> loop pending rest
        | Some Direct_self ->
            let* summary = prepare_direct component in
            loop (summary :: pending) rest
        | Some Mutual -> assert false)
  in
  loop [] analysis.components

let pending_summaries (plan : plan) = plan.pending
let pending_callable (pending : pending_summary) = pending.callable
let pending_group (pending : pending_summary) = pending.group
let pending_domain (pending : pending_summary) = pending.entry.domain
let structural_rank_id = Sst_validation.rank_domain_id

let find_pending_summary (plan : plan) id =
  List.find_opt
    (fun (pending : pending_summary) ->
      same_function_id (callable_id pending.callable) id)
    plan.pending

let find_entry_intent plan id =
  Option.map
    (fun pending -> pending.entry)
    (find_pending_summary plan id)

let find_edge_intent (plan : plan) ~caller ~callee ~span =
  List.find_map
    (fun (pending : pending_summary) ->
      List.find_opt
        (fun intent ->
          same_function_id (edge_caller_id intent.edge) caller
          && same_function_id (edge_callee_id intent.edge) callee
          && same_span (call_edge_span intent.edge) span)
        pending.edges)
    plan.pending

let entry_callable (intent : entry_intent) = intent.callable
let entry_measure (intent : entry_intent) = intent.measure
let entry_domain (intent : entry_intent) = intent.domain
let edge_descriptor (intent : edge_intent) = intent.edge
let edge_measure (intent : edge_intent) = intent.measure
let edge_domain (intent : edge_intent) = intent.domain

let function_name id =
  Printf.sprintf "%s#%d" id.Sst.function_name id.function_index

let mode_name = function
  | Sst.Spec -> "specification"
  | Sst.Proof -> "proof"
  | Sst.Exec -> "executable"

let error_to_string error =
  match error.kind with
  | Missing_measure ->
      "direct recursion requires exactly one decreases measure"
  | Duplicate_measure ->
      "direct recursion has more than one decreases measure"
  | Inapplicable_measure ->
      "decreases measure is not applicable to direct body self recursion"
  | Non_integer_measure -> "decreases measure must have type int"
  | Recursive_measure -> "decreases measure contains a recursive call"
  | Unsupported_mutual_scc ids ->
      Printf.sprintf "unsupported mutual recursion SCC: %s"
        (String.concat ", " (List.map function_name ids))
  | Unsupported_recursive_mode (mode, ids) ->
      Printf.sprintf "unsupported recursive %s SCC: %s"
        (mode_name mode)
        (String.concat ", " (List.map function_name ids))
  | Raw_analysis_only ->
      "raw termination analysis cannot issue pending totality descriptors"
