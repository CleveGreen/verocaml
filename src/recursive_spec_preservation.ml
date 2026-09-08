module Finite_expression = struct
  type origin =
    | Exact_formal
    | Exact_alias
    | Immutable_construction
    | Immutable_projection
    | Immutable_pattern
    | Finite_branch_join
    | Completed_local_summary
    | Smaller_direct_call
    | Immutable_recursive_spec

  type 'fact premise =
    | Exact_fact of 'fact
    | Missing_fact of string

  type origin_kind =
    | Projection_origin
    | Pattern_origin

  type authority_kind =
    | Completed_summary_authority
    | Strict_descent_induction_authority
    | Recursive_spec_authority

  type ('fact, 'authority, 'node) source =
    | Existing_fact of 'fact
    | Exact_alias of 'fact
    | Exact_let of 'fact
    | Exact_materialization of 'fact
    | Immutable_constructor of 'node * 'fact premise list
    | Immutable_record of 'node * 'fact premise list
    | Immutable_projection of 'node * 'fact
    | Immutable_pattern of 'node * 'fact
    | All_feasible_branches of 'fact premise list
    | Completed_nonrecursive_summary of 'authority
    | Strictly_smaller_direct_call of {
        designated_actuals : 'fact premise list;
        induction : 'authority;
      }
    | Immutable_recursive_spec_result of {
        arguments : 'fact premise list;
        capability : 'authority;
      }

  type 'fact derivation = { fact : 'fact }

  type ('fact, 'authority, 'node) callbacks = {
    authenticate_fact : 'fact -> (unit, string) result;
    authenticate_node : 'node -> (unit, string) result;
    authenticate_origin :
      origin_kind -> 'node -> 'fact -> (unit, string) result;
    authenticate_authority :
      authority_kind -> 'authority -> (unit, string) result;
    issue : origin -> 'fact;
  }

  let ( let* ) result continuation =
    match result with Ok value -> continuation value | Error _ as error -> error

  let rule_error rule message =
    Error (Printf.sprintf "finite-expression %s rejected: %s" rule message)

  let authenticate_fact callbacks rule fact =
    match callbacks.authenticate_fact fact with
    | Ok () -> Ok fact
    | Error message -> rule_error rule message

  let authenticate_premises callbacks rule premises =
    let rec loop facts = function
      | [] -> Ok (List.rev facts)
      | Missing_fact message :: _ -> rule_error rule message
      | Exact_fact fact :: rest ->
          let* fact = authenticate_fact callbacks rule fact in
          loop (fact :: facts) rest
    in
    loop [] premises

  let authenticate_node callbacks rule node =
    match callbacks.authenticate_node node with
    | Ok () -> Ok ()
    | Error message -> rule_error rule message

  let authenticate_origin callbacks rule kind node fact =
    let* _ = authenticate_fact callbacks rule fact in
    match callbacks.authenticate_origin kind node fact with
    | Ok () -> Ok ()
    | Error message -> rule_error rule message

  let authenticate_authority callbacks rule kind authority =
    match callbacks.authenticate_authority kind authority with
    | Ok () -> Ok ()
    | Error message -> rule_error rule message

  let derive source ~callbacks =
    match source with
    | Existing_fact fact | Exact_alias fact | Exact_let fact ->
        let* fact = authenticate_fact callbacks "exact fact" fact in
        Ok { fact }
    | Exact_materialization fact ->
        let* _ = authenticate_fact callbacks "exact materialization" fact in
        Ok { fact = callbacks.issue Exact_alias }
    | Immutable_constructor (node, children)
    | Immutable_record (node, children) ->
        let* () = authenticate_node callbacks "immutable construction" node in
        let* _ =
          authenticate_premises callbacks "immutable construction child" children
        in
        Ok { fact = callbacks.issue Immutable_construction }
    | Immutable_projection (node, parent) ->
        let* () = authenticate_node callbacks "immutable projection" node in
        let* () =
          authenticate_origin callbacks "immutable projection"
            Projection_origin node parent
        in
        Ok { fact = callbacks.issue Immutable_projection }
    | Immutable_pattern (node, parent) ->
        let* () = authenticate_node callbacks "immutable pattern" node in
        let* () =
          authenticate_origin callbacks "immutable pattern" Pattern_origin node
            parent
        in
        Ok { fact = callbacks.issue Immutable_pattern }
    | All_feasible_branches [] ->
        rule_error "branch join" "no feasible normal exit was supplied"
    | All_feasible_branches branches ->
        let* _ =
          authenticate_premises callbacks "feasible result branch" branches
        in
        Ok { fact = callbacks.issue Finite_branch_join }
    | Completed_nonrecursive_summary authority ->
        let* () =
          authenticate_authority callbacks "completed summary"
            Completed_summary_authority authority
        in
        Ok { fact = callbacks.issue Completed_local_summary }
    | Strictly_smaller_direct_call { designated_actuals; induction } ->
        let* _ =
          authenticate_premises callbacks "designated recursive actual"
            designated_actuals
        in
        let* () =
          authenticate_authority callbacks "strict recursive call"
            Strict_descent_induction_authority induction
        in
        Ok { fact = callbacks.issue Smaller_direct_call }
    | Immutable_recursive_spec_result { arguments; capability } ->
        let* _ =
          authenticate_premises callbacks "recursive Spec argument" arguments
        in
        let* () =
          authenticate_authority callbacks "recursive Spec result"
            Recursive_spec_authority capability
        in
        Ok { fact = callbacks.issue Immutable_recursive_spec }

  let fact derivation = derivation.fact

  type route_counts = { base : int; recursive : int; construction : int }

  let empty_counts = { base = 0; recursive = 0; construction = 0 }

  let add_counts left right =
    {
      base = left.base + right.base;
      recursive = left.recursive + right.recursive;
      construction = left.construction + right.construction;
    }

  let same_function left right =
    left.Sst.function_index = right.Sst.function_index
    && String.equal left.function_name right.function_name

  type local_fact = {
    local_issuer : unit ref;
    local_token : unit ref;
  }

  type local_node =
    | Local_constructor of Sst.expression
    | Local_record of Sst.expression
    | Local_projection of Sst.expression * local_fact
    | Local_pattern of Sst.pattern * local_fact

  type local_authority = Local_strict_edge of Diagnostic.span

  let local_issuer = ref ()
  let issue_local _ = { local_issuer; local_token = ref () }

  let local_callbacks strict_edge_spans =
    {
      authenticate_fact =
        (fun fact ->
          if fact.local_issuer == local_issuer && fact.local_token != local_issuer
          then Ok ()
          else Error "fact was not issued by this expression judgment");
      authenticate_node =
        (function
        | Local_constructor
            { Sst.expression_desc = Sst.Constructor_value _; _ }
        | Local_record { expression_desc = Sst.Record_value _; _ }
        | Local_projection ({ expression_desc = Sst.Field_read _; _ }, _)
        | Local_pattern _ ->
            Ok ()
        | Local_constructor _ ->
            Error "constructor rule was not bound to a constructor expression"
        | Local_record _ ->
            Error "record rule was not bound to a record expression"
        | Local_projection _ ->
            Error "projection rule was not bound to a field read");
      authenticate_origin =
        (fun kind node fact ->
          let exact_parent expected =
            fact.local_issuer == expected.local_issuer
            && fact.local_token == expected.local_token
          in
          match (kind, node) with
          | Projection_origin, Local_projection (_, expected)
          | Pattern_origin, Local_pattern (_, expected)
            when exact_parent expected ->
              Ok ()
          | Projection_origin, Local_projection _
          | Pattern_origin, Local_pattern _ ->
              Error "projection/pattern origin is not the exact finite parent"
          | Projection_origin, _
          | Pattern_origin, _ ->
              Error "projection/pattern authority kind is mismatched");
      authenticate_authority =
        (fun kind (Local_strict_edge span) ->
          match kind with
          | Strict_descent_induction_authority
            when List.exists (( = ) span) strict_edge_spans ->
              Ok ()
          | Strict_descent_induction_authority ->
              Error "recursive call has no authenticated strict-descent edge"
          | Completed_summary_authority | Recursive_spec_authority ->
              Error "authority kind is not available in recursive-Spec checking");
      issue = issue_local;
    }

  let derive_local callbacks source =
    let* derivation = derive source ~callbacks in
    Ok (fact derivation)

  let bind_pattern callbacks environment source (pattern : Sst.pattern) =
    let rec bind environment pattern =
      match pattern.Sst.pattern_desc with
      | Sst.Bind binding ->
          (match binding.typ with
          | Sst.Aggregate _ | Sst.Application _ ->
              let* finite =
                match source with
                | `Let finite -> derive_local callbacks (Exact_let finite)
                | `Pattern finite ->
                    derive_local callbacks
                      (Immutable_pattern
                         (Local_pattern (pattern, finite), finite))
              in
              Ok ((binding.id, finite) :: environment)
          | Sst.Unit | Sst.Bool | Sst.Int | Sst.Mathematical_int | Sst.Bit_vector _ | Sst.Tuple _
          | Sst.Parameter _ -> Ok environment)
      | Sst.Record_pattern fields ->
          List.fold_left
            (fun result (_, nested) ->
              let* environment = result in
              bind environment nested)
            (Ok environment) fields
      | Sst.Constructor_pattern (_, arguments) ->
          List.fold_left
            (fun result nested ->
              let* environment = result in
              bind environment nested)
            (Ok environment) arguments
      | Sst.Wildcard | Sst.Int_pattern _ | Sst.Bool_pattern _
      | Sst.Unit_pattern | Sst.Tuple_pattern _
      | Sst.Owned_tree_cursor_pattern _ | Sst.Or_pattern _ ->
          Ok environment
    in
    bind environment pattern

  let check_immutable_recursive_spec
      ~(definition : Sst.function_definition) ~expanded_body ~strict_edge_spans =
    let callbacks = local_callbacks strict_edge_spans in
    let initial_environment =
      definition.parameters
      |> List.fold_left
           (fun environment parameter ->
             let parameter = Sst.require_value_parameter parameter in
             match parameter.Sst.pattern.pattern_desc with
             | Sst.Bind binding -> (
                 match binding.typ with
                 | Sst.Aggregate _ | Sst.Application _ ->
                     (binding.id, issue_local Exact_formal) :: environment
                 | Sst.Unit | Sst.Bool | Sst.Int | Sst.Mathematical_int | Sst.Bit_vector _
                 | Sst.Tuple _
                 | Sst.Parameter _ -> environment)
             | _ -> environment)
           []
    in
    let rec check environment (expression : Sst.expression) =
      match expression.typ with
      | Sst.Unit | Sst.Bool | Sst.Int | Sst.Mathematical_int | Sst.Bit_vector _ ->
          Error "scalar escaped aggregate recursive-Spec finite checking"
      | Sst.Tuple _ | Sst.Parameter _ ->
          Error "non-aggregate escaped immutable recursive-Spec finite checking"
      | Sst.Aggregate _ | Sst.Application _ -> (
          match expression.expression_desc with
          | Sst.Variable { binding; _ } -> (
              match List.assoc_opt binding.id environment with
              | Some fact ->
                  let* finite = derive_local callbacks (Existing_fact fact) in
                  Ok (finite, { empty_counts with base = 1 })
              | None ->
                  Error "recursive-Spec value lacks an exact finite binding")
          | Sst.Record_value { fields; _ } ->
              construction environment
                (fun facts ->
                  Immutable_record
                    (Local_record expression, facts))
                (List.map snd fields)
          | Sst.Constructor_value { arguments; _ } ->
              construction environment
                (fun facts ->
                  Immutable_constructor
                    (Local_constructor expression, facts))
                arguments
          | Sst.Field_read { record; _ } ->
              let* parent, counts = check environment record in
              let* finite =
                derive_local callbacks
                  (Immutable_projection
                     (Local_projection (expression, parent), parent))
              in
              Ok (finite, counts)
          | Sst.If (_, consequent, Some alternative) ->
              let* left_finite, left = check environment consequent in
              let* right_finite, right = check environment alternative in
              let* finite =
                derive_local callbacks
                  (All_feasible_branches
                     [ Exact_fact left_finite; Exact_fact right_finite ])
              in
              Ok (finite, add_counts left right)
          | Sst.Let (bindings, body) ->
              let* environment, binding_counts =
                List.fold_left
                  (fun result (pattern, value) ->
                    let* environment, counts = result in
                    match value.Sst.typ with
                    | Sst.Unit | Sst.Bool | Sst.Int | Sst.Mathematical_int | Sst.Bit_vector _ ->
                        Ok (environment, counts)
                    | Sst.Tuple _ | Sst.Parameter _ ->
                        Error
                          "non-aggregate let escaped immutable recursive-Spec finite checking"
                    | Sst.Aggregate _ | Sst.Application _ ->
                        let* finite, next = check environment value in
                        let* environment =
                          bind_pattern callbacks environment (`Let finite) pattern
                        in
                        Ok (environment, add_counts counts next))
                  (Ok (environment, empty_counts)) bindings
              in
              let* finite, body_counts = check environment body in
              Ok (finite, add_counts binding_counts body_counts)
          | Sst.Match (scrutinee, cases) ->
              let* scrutinee_finite, scrutinee_counts =
                check environment scrutinee
              in
              let* branch_facts, branch_counts =
                List.fold_left
                  (fun result (case : Sst.case) ->
                    let* facts, counts = result in
                    let* environment =
                      bind_pattern callbacks environment
                        (`Pattern scrutinee_finite) case.case_pattern
                    in
                    let* finite, next = check environment case.case_body in
                    Ok (finite :: facts, add_counts counts next))
                  (Ok ([], empty_counts)) cases
              in
              let* finite =
                derive_local callbacks
                  (All_feasible_branches
                     (List.rev_map
                        (fun fact -> Exact_fact fact)
                        branch_facts))
              in
              Ok (finite, add_counts scrutinee_counts branch_counts)
          | Sst.Direct_call
              {
                callee;
                arguments;
                recursive = true;
                call_form = Sst.Specification_call;
                _;
              }
            when same_function callee definition.function_id ->
              let* facts, counts =
                List.fold_left
                  (fun result argument ->
                    let _, actual = Sst.require_value_argument argument in
                    let* facts, counts = result in
                    match actual.Sst.typ with
                    | Sst.Aggregate _ | Sst.Application _ ->
                        let* finite, next = check environment actual in
                        Ok (finite :: facts, add_counts counts next)
                    | Sst.Unit | Sst.Bool | Sst.Int | Sst.Mathematical_int | Sst.Bit_vector _
                    | Sst.Tuple _
                    | Sst.Parameter _ ->
                        Ok (facts, counts))
                  (Ok ([], empty_counts)) arguments
              in
              let* finite =
                derive_local callbacks
                  (Strictly_smaller_direct_call
                     {
                       designated_actuals =
                         List.rev_map (fun fact -> Exact_fact fact) facts;
                       induction = Local_strict_edge expression.span;
                     })
              in
              Ok (finite, { counts with recursive = counts.recursive + 1 })
          | Sst.Direct_call _ ->
              Error "unexpanded helper escaped recursive-Spec finite checking"
          | _ -> Error "aggregate path escaped recursive-Spec finite checking")
    and construction environment source children =
      let* facts, counts =
        List.fold_left
          (fun result child ->
            let* facts, counts = result in
            match child.Sst.typ with
            | Sst.Aggregate _ | Sst.Application _ ->
                let* finite, next = check environment child in
                Ok (Exact_fact finite :: facts, add_counts counts next)
            | Sst.Unit | Sst.Bool | Sst.Int | Sst.Mathematical_int | Sst.Bit_vector _ ->
                Ok (facts, counts)
            | Sst.Tuple _ | Sst.Parameter _ ->
                Error "non-aggregate child escaped immutable finite construction")
          (Ok ([], empty_counts)) children
      in
      let* finite = derive_local callbacks (source (List.rev facts)) in
      Ok (finite, { counts with construction = counts.construction + 1 })
    in
    let* _, counts = check initial_environment expanded_body in
    Ok counts

  let ordinary_checker_entry = "Recursive_spec_preservation.Finite_expression.derive/ordinary"

  let immutable_recursive_spec_checker_entry =
    "Recursive_spec_preservation.Finite_expression.check_immutable_recursive_spec"
end

type capability = {
  issuer : unit ref;
  token : unit ref;
  program : Sst.program;
  definition : Sst.function_definition;
  function_id : Sst.function_id;
  body_snapshot : string;
  helper_closure_snapshot : string;
  result_type : Sst.typ;
  rank_domain_id : string;
  rank_domain_version : string;
  rank_domain_digest : string;
  required_aggregate_ordinals : int list;
  base_route_count : int;
  recursive_route_count : int;
  construction_route_count : int;
}

type counts = {
  base : int;
  recursive : int;
  construction : int;
}

let private_issuer = ref ()

let ( let* ) result continuation =
  match result with Ok value -> continuation value | Error _ as error -> error

let same_function_id (left : Sst.function_id) (right : Sst.function_id) =
  left.function_index = right.function_index
  && String.equal left.function_name right.function_name

let aggregate_type_of_sst (type_id : Sst.type_id) =
  {
    Vir.aggregate_type_index = type_id.type_index;
    aggregate_type_name = type_id.type_name;
      aggregate_type_arguments = [];
  }

let aggregate_type_of_typ program typ =
  Logical_spec_evaluation_private.vir_aggregate_type_of_sst
    program.Sst.parametric_adts typ

let fields_of_kind = function
  | Sst.Record_definition fields -> fields
  | Sst.Variant_definition constructors ->
      List.concat_map
        (fun (constructor : Sst.constructor_definition) ->
          constructor.constructor_fields)
        constructors

let authenticated_logical_application program = function
  | Sst.Application (constructor, arguments) as application -> (
      match Parametric_adt.find program.Sst.parametric_adts constructor with
      | Some descriptor
        when Parametric_adt.same_application descriptor application
             && Result.is_ok
                  (Logical_adt_schema_private.instantiate
                     ~descriptors:program.parametric_adts
                     ~applications:
                       [
                         ( (Parametric_adt.type_id descriptor).type_index,
                           arguments );
                       ]) ->
          true
      | Some _ | None -> false)
  | Sst.Unit | Sst.Bool | Sst.Int | Sst.Mathematical_int | Sst.Bit_vector _ | Sst.Tuple _
  | Sst.Aggregate _
  | Sst.Parameter _ ->
      false

let rank_domain_for_result program rank_domains result_type =
  let find_type type_id =
    List.find_opt
      (fun definition -> definition.Sst.type_id = type_id)
      program.Sst.types
  in
  let merge left right =
    match (left, right) with
    | None, value | value, None -> Some value
    | Some left, Some right
      when String.equal (Vir.rank_domain_id left) (Vir.rank_domain_id right)
           && String.equal
                (Vir.rank_domain_digest left)
                (Vir.rank_domain_digest right) ->
        Some (Some left)
    | Some _, Some _ -> None
  in
  let rec classify visiting = function
    | Sst.Unit | Sst.Bool | Sst.Int | Sst.Mathematical_int | Sst.Bit_vector _ -> Some None
    | Sst.Tuple _ | Sst.Parameter _ -> None
    | Sst.Application _ as application -> (
        match aggregate_type_of_typ program application with
        | None -> None
        | Some aggregate ->
            rank_domains
            |> List.find_opt (fun domain ->
                   List.mem aggregate (Vir.rank_domain_component domain))
            |> Option.map Option.some)
    | Sst.Aggregate type_id -> (
        match
          List.find_opt
            (fun domain ->
              List.mem
                (aggregate_type_of_sst type_id)
                (Vir.rank_domain_component domain))
            rank_domains
        with
        | Some domain -> Some (Some domain)
        | None when List.mem type_id visiting -> Some None
        | None -> (
            match find_type type_id with
            | Some
                {
                  representation = Sst.Revealed;
                  type_kind;
                  _;
                } ->
                List.fold_left
                  (fun selected (field : Sst.field_definition) ->
                    Option.bind selected (fun selected ->
                        Option.bind
                          (classify (type_id :: visiting) field.field_type)
                          (merge selected)))
                  (Some None) (fields_of_kind type_kind)
            | Some { representation = Sst.Abstract_with_evidence _; _ }
            | None ->
                None))
  in
  match classify [] result_type with
  | Some (Some domain) -> Some domain
  | Some None | None -> None

let frozen_helper program (definition : Sst.function_definition) =
  List.exists
    (fun (type_definition : Sst.type_definition) ->
      match type_definition.representation with
      | Sst.Abstract_with_evidence
          (Sst.Authenticated_same_cmt_abstraction evidence) -> (
          match Sst.frozen_spine_prerequisite evidence with
          | Some frozen ->
              frozen.frozen_helper = definition.function_id
              && frozen.frozen_root = type_definition.type_id
          | None -> false)
      | Sst.Revealed
      | Sst.Abstract_with_evidence
          (Sst.Incomplete_abstraction_evidence _
          | Sst.Proposed_same_cmt_abstraction _) ->
          false)
    program.Sst.types

let analyze ~program ~(definition : Sst.function_definition) ~expanded_body
    ~helper_closure_snapshot ~rank_domains ~termination_obligations =
  match definition.result_type with
  | Sst.Int | Sst.Mathematical_int | Sst.Bool | Sst.Bit_vector _ | Sst.Parameter _ -> Ok None
  | Sst.Application _ as typ when Parametric_type.is_spec_function typ ->
      Ok None
  | Sst.Unit | Sst.Tuple _ ->
      Error "recursive finite preservation requires an aggregate result"
  | (Sst.Aggregate _ | Sst.Application _) as result_type ->
      let is_frozen = frozen_helper program definition in
      (match rank_domain_for_result program rank_domains result_type with
      | None ->
          if Option.is_some
            (Spec_definition.classify_logical_type program.Sst.types result_type)
             || authenticated_logical_application program result_type
          then Ok None
          else Error "recursive aggregate result does not resolve to one authenticated rank profile"
      | Some rank_domain ->
      let strict_edge_spans =
        List.filter_map
          (fun (obligation : Vir.obligation) ->
            match obligation.Vir.kind with
            | Vir.Recursive_call_strict_descent { callee; call_span; _ }
              when
                callee.function_index = definition.function_id.function_index
                && String.equal callee.function_name
                     definition.function_id.function_name ->
                Some call_span
            | _ -> None)
          termination_obligations
      in
      let strict_count = List.length strict_edge_spans in
      let* counts =
        if is_frozen then
          (* Frozen-spine recursion remains on its sealed legacy adapter.  Its
             body preservation is authenticated by the frozen termination
             prerequisite and never invokes immutable finite induction. *)
          if strict_count = 0 then
            Error "frozen-spine preservation has no sealed strict edge"
          else
            Ok
              { base = 1; recursive = strict_count; construction = 1 }
        else
          let* routes =
            Finite_expression.check_immutable_recursive_spec
              ~definition ~expanded_body ~strict_edge_spans
          in
          Ok
            {
              base = routes.Finite_expression.base;
              recursive = routes.recursive;
              construction = routes.construction;
            }
      in
      if counts.base = 0 then
        Error "recursive finite preservation has no finite base/pass-through path"
      else if counts.recursive = 0 then
        Error "recursive finite preservation has no recursive result path"
      else if counts.recursive <> strict_count then
        Error
          "recursive finite preservation is not bound to every sealed strict edge"
      else
        let required_aggregate_ordinals =
          if is_frozen then []
          else
            definition.parameters
            |> List.mapi (fun ordinal parameter ->
                   let parameter = Sst.require_value_parameter parameter in
                   match parameter.Sst.pattern.pattern_desc with
                   | Sst.Bind
                       { typ = (Sst.Aggregate _ | Sst.Application _); _ } ->
                       Some ordinal
                   | _ -> None)
            |> List.filter_map Fun.id
        in
        Ok
          (Some
             {
               issuer = private_issuer;
               token = ref ();
               program;
               definition;
               function_id = definition.function_id;
               body_snapshot =
                 Digest.to_hex
                   (Digest.string
                      (Marshal.to_string expanded_body [ Marshal.No_sharing ]));
               helper_closure_snapshot;
               result_type;
               rank_domain_id = Vir.rank_domain_id rank_domain;
               rank_domain_version = Vir.rank_domain_version rank_domain;
               rank_domain_digest = Vir.rank_domain_digest rank_domain;
               required_aggregate_ordinals;
               base_route_count = counts.base;
               recursive_route_count = counts.recursive;
               construction_route_count = counts.construction;
             }))

let authenticate capability ~program ~(definition : Sst.function_definition)
    ~result_type ~rank_domain_id ~rank_domain_version ~rank_domain_digest =
  capability.issuer == private_issuer
  && capability.token != private_issuer
  && capability.program == program
  && capability.definition == definition
  && same_function_id capability.function_id definition.function_id
  && capability.result_type = result_type
  && String.equal capability.rank_domain_id rank_domain_id
  && String.equal capability.rank_domain_version rank_domain_version
  && String.equal capability.rank_domain_digest rank_domain_digest
  && capability.helper_closure_snapshot <> ""
  && capability.body_snapshot <> ""
  && capability.base_route_count > 0
  && capability.recursive_route_count > 0

let function_id capability = capability.function_id
let body_snapshot capability = capability.body_snapshot
let helper_closure_snapshot capability = capability.helper_closure_snapshot
let result_type capability = capability.result_type
let rank_domain_id capability = capability.rank_domain_id
let rank_domain_digest capability = capability.rank_domain_digest
let required_aggregate_ordinals capability =
  capability.required_aggregate_ordinals
let base_route_count capability = capability.base_route_count
let recursive_route_count capability = capability.recursive_route_count
let construction_route_count capability = capability.construction_route_count

let pending = ref []

let clear_pending program =
  pending := List.filter (fun (candidate, _) -> candidate != program) !pending

let retain_pending program capabilities =
  clear_pending program;
  pending := (program, capabilities) :: !pending

let take_pending program =
  let capabilities =
    List.find_map
      (fun (candidate, capabilities) ->
        if candidate == program then Some capabilities else None)
      !pending
    |> Option.value ~default:[]
  in
  clear_pending program;
  capabilities

module For_testing = struct
  let forged capability = { capability with issuer = ref (); token = ref () }
end
