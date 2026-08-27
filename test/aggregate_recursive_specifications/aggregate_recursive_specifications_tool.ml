let () = ignore Aggregate_recursive_specifications_prerequisites.ready
(* VERO-077 reconstruction is observed through the recursive query consumer. *)

let fail message = prerr_endline message; exit 3

let load filename =
  match Cmt_input.load filename with
  | Ok implementation -> implementation
  | Error diagnostic -> fail (diagnostic.Diagnostic.code ^ ": " ^ diagnostic.message)

let load_program filename =
  match Typedtree_lowering.lower (load filename) with
  | Ok program -> program
  | Error diagnostic ->
      fail (diagnostic.Diagnostic.code ^ ": " ^ diagnostic.message)

let status = function
  | Verification_pipeline.Verified -> "verified"
  | Counterexample -> "counterexample"
  | Inconclusive -> "inconclusive"
  | Incomplete_source -> "incomplete"

let report filename =
  match Verification_driver_private.run ~timeout_ms:60_000
          ~allow_imported_opens:false (load filename) with
  | Error (Verification_driver_private.Pipeline_error
      (Verification_pipeline.Engine_error error)) ->
      fail (Symbolic_executor_private.error_to_string error)
  | Error _ -> fail "verification failed before reporting"
  | Ok report -> report

let run filename =
  let report = report filename in
  let counters = Verification_driver_private.counters report in
  Printf.printf
    "status=%s functions=%d obligations=%d recursive-spec=%d/%d lifecycle=%d/%d/%d\n"
    (status (Verification_driver_private.status report))
    (Verification_driver_private.functions report)
    (Verification_driver_private.obligations report)
    counters.Verification_session.recursive_spec_result_issuances
    counters.recursive_spec_result_consumptions
    counters.finite_result_manifests counters.finite_result_completions
    counters.finite_result_finalizations

let resource_retry filename =
  Recursive_spec_encoding.For_testing.reset_ground_counterexample_counters ();
  Recursive_spec_encoding.For_testing.reset_nullary_branch_retry_counters ();
  Verification_solver_private.For_testing.force_recursive_local_inconclusive
    true;
  Verification_solver_private.For_testing.recursive_retry_rlimit (Some 1);
  let report =
    Fun.protect
      ~finally:(fun () ->
        Verification_solver_private.For_testing
        .force_recursive_local_inconclusive false;
        Verification_solver_private.For_testing.recursive_retry_rlimit None)
      (fun () -> report filename)
  in
  let result =
    Verification_driver_private.results report
    |> List.find_opt (fun result ->
           match result.Solver_backend.outcome with
           | Inconclusive { reason = Resource_exhausted; _ } -> true
           | Verified | Counterexample _
           | Inconclusive { reason = Timed_out | Backend_unknown _; _ } ->
               false)
  in
  let retry =
    Recursive_spec_encoding.For_testing.nullary_branch_retry_counters ()
  in
  let ground =
    Recursive_spec_encoding.For_testing.ground_counterexample_counters ()
  in
  (match result with
  | Some
      {
        Solver_backend.outcome =
          Inconclusive { reason = Resource_exhausted; _ };
        _;
      } ->
      ()
  | Some _ | None ->
      fail
        (Printf.sprintf
           "resource-exhausted retry did not commit resource reason \
            retry=%d/%d/%d ground=%d"
           retry.attempts retry.queries retry.inconclusives ground.attempts));
  if
    not
      (retry.attempts = 2 && retry.queries = 1 && retry.inconclusives = 1)
  then
    fail
      (Printf.sprintf
         "resource-exhausted retry counters changed retry=%d/%d/%d ground=%d"
         retry.attempts retry.queries retry.inconclusives ground.attempts);
  if ground.attempts <> retry.attempts - retry.inconclusives then
    fail "resource-exhausted retry entered ground classification";
  Printf.printf
    "resource-retry outcome=resource-exhausted retry=%d/%d/%d ground=%d\n"
    retry.attempts retry.queries retry.inconclusives ground.attempts

let reconstruction filename =
  let report = report filename in
  let local function_name =
    Verification_driver_private.results report
    |> List.find_opt (fun result ->
           String.equal
             result.Solver_backend.obligation.function_ref.function_name
             function_name
           &&
           match result.obligation.kind with
           | Vir.Local_assertion _ -> true
           | _ -> false)
  in
  let outcome function_name =
    match local function_name with
    | Some result -> result.Solver_backend.outcome
    | None -> fail ("missing recursive reconstruction VC: " ^ function_name)
  in
  if outcome "recursive_reconstruction" <> Solver_backend.Verified then
    fail "recursive reconstruction positive did not verify";
  if outcome "recursive_wrong_payload" = Solver_backend.Verified then
    fail "recursive wrong payload unexpectedly verified";
  Printf.printf
    "recursive-reconstruction positive=verified wrong-payload=nonverified \
     functions=%d obligations=%d\n"
    (Verification_driver_private.functions report)
    (Verification_driver_private.obligations report)

let check label condition = if not condition then fail label

let contains text fragment =
  let text = String.lowercase_ascii text
  and fragment = String.lowercase_ascii fragment in
  let text_length = String.length text
  and fragment_length = String.length fragment in
  let rec search offset =
    offset + fragment_length <= text_length
    &&
    (String.equal (String.sub text offset fragment_length) fragment
    || search (offset + 1))
  in
  fragment_length = 0 || search 0

let prepare program =
  match Recursive_spec_encoding.prepare program with
  | Ok prepared -> prepared
  | Error error -> fail (Recursive_spec_encoding.error_to_string error)

let verify prepared =
  match Recursive_spec_encoding.verify ~timeout_ms:60_000 prepared with
  | Ok verified -> verified
  | Error error -> fail (Recursive_spec_encoding.error_to_string error)

let base_query verified function_id =
  match Recursive_spec_encoding.base_query verified function_id with
  | Ok query -> query
  | Error error -> fail (Recursive_spec_encoding.error_to_string error)

let recursive_definition name program =
  List.find
    (fun definition ->
      String.equal definition.Sst.function_id.function_name name
      &&
      match definition.body with
      | Sst.Recursive_spec_definition _ -> true
      | _ -> false)
    program.Sst.functions

let rec count_ites term =
  let recurse = count_ites in
  match Logic_ir.View.term_node term with
  | Logic_ir.View.Ite (condition, then_, else_) ->
      1 + recurse condition + recurse then_ + recurse else_
  | Apply (_, arguments) | And arguments | Or arguments ->
      List.fold_left (fun count term -> count + recurse term) 0 arguments
  | Rank_project (_, _, _, value) | Negate value | Not value
  | Scale (_, value) ->
      recurse value
  | Add (left, right) | Subtract (left, right)
  | Less_than (left, right) | Less_or_equal (left, right)
  | Greater_than (left, right) | Greater_or_equal (left, right)
  | Equal (left, right) | Distinct (left, right)
  | Implies (left, right) ->
      recurse left + recurse right
  | Forall_term quantifier | Exists_term quantifier ->
      recurse (Logic_ir.View.user_quantifier_body quantifier)
      + Option.fold ~none:0 ~some:recurse
          (Logic_ir.View.user_quantifier_trigger quantifier)
  | Integer _ | Boolean _ | Bound _ -> 0

type native_constructor = {
  constructor_index : int;
  constructor_name : string;
  constructor_uid : string;
}

let native_recognizers program query =
  let constructor_uids constructors =
    List.map
      (fun constructor -> constructor.Parametric_adt.constructor_uid)
      constructors
    |> List.sort String.compare
  in
  let add_datatype table datatype =
    let datatype_constructors =
      Logic_ir.View.datatype_constructors datatype
    in
    let datatype_uids =
      List.map Logic_ir.View.datatype_constructor_id datatype_constructors
      |> List.sort String.compare
    in
    check "native datatype has duplicate constructor identities"
      (datatype_uids = List.sort_uniq String.compare datatype_uids);
    let descriptor_constructors =
      List.filter_map
        (fun descriptor ->
          match Parametric_adt.kind descriptor with
          | Parametric_adt.Variant constructors
            when constructor_uids constructors = datatype_uids ->
              Some constructors
          | Parametric_adt.Variant _ | Parametric_adt.Record _ -> None)
        program.Sst.parametric_adts
    in
    let descriptor_constructors =
      match descriptor_constructors with
      | [ constructors ] -> constructors
      | [] ->
          fail "native datatype lacks its exact authenticated variant mapping"
      | _ :: _ :: _ ->
          fail "native datatype has duplicate authenticated variant mappings"
    in
    List.fold_left
      (fun table datatype_constructor ->
        let uid =
          Logic_ir.View.datatype_constructor_id datatype_constructor
        in
        let constructor =
          match
            List.filter
              (fun constructor ->
                String.equal constructor.Parametric_adt.constructor_uid uid)
              descriptor_constructors
          with
          | [ constructor ] -> constructor
          | [] ->
              fail "native recognizer lacks its authenticated constructor"
          | _ :: _ :: _ ->
              fail "native recognizer has duplicate constructor mappings"
        in
        let function_index =
          Logic_ir.View.datatype_recognizer_symbol datatype_constructor
          |> Logic_ir.View.function_index
        in
        check "native recognizer function identity is duplicated"
          (not (List.mem_assoc function_index table));
        ( function_index,
          {
            constructor_index = constructor.constructor_index;
            constructor_name = constructor.constructor_name;
            constructor_uid = constructor.constructor_uid;
          } )
        :: table)
      table datatype_constructors
  in
  let table =
    List.fold_left add_datatype [] (Logic_ir.View.datatypes query)
  in
  check "recursive tuple query has no native datatype recognizers" (table <> []);
  table

let native_condition table expected_binder term =
  let term =
    match Logic_ir.View.term_node term with
    | Logic_ir.View.And (condition :: trivial)
      when List.for_all
             (fun term ->
               match Logic_ir.View.term_node term with
               | Logic_ir.View.Boolean true -> true
               | _ -> false)
             trivial ->
        condition
    | _ -> term
  in
  match Logic_ir.View.term_node term with
  | Logic_ir.View.Apply (recognizer, [ argument ]) -> (
      match Logic_ir.View.term_node argument with
      | Logic_ir.View.Bound binder
        when Logic_ir.View.binder_index binder
             = Logic_ir.View.binder_index expected_binder -> (
          match
            List.assoc_opt (Logic_ir.View.function_index recognizer) table
          with
          | Some constructor -> constructor
          | None -> fail "tuple condition used an unknown Boolean recognizer")
      | Logic_ir.View.Bound _ ->
          fail "tuple recognizer used a repeated, swapped, or unrelated binder"
      | _ -> fail "tuple recognizer argument is not an exact A2 binder")
  | Logic_ir.View.Apply _ ->
      fail "tuple recognizer has the wrong application arity"
  | _ -> fail "tuple condition is not a native recognizer application"

let ordered_match_conditions table first_parameter second_parameter expanded =
  let rec collect conditions term =
    match Logic_ir.View.term_node term with
    | Logic_ir.View.Ite (condition, _then, else_) -> (
        match Logic_ir.View.term_node condition with
        | Logic_ir.View.And [ first; second ] ->
            let pair =
              ( native_condition table first_parameter first,
                native_condition table second_parameter second )
            in
            collect (pair :: conditions) else_
        | Logic_ir.View.And _ ->
            fail "tuple match condition has the wrong ordered arity"
        | _ -> fail "tuple match condition is not an exact conjunction")
    | _ -> List.rev conditions
  in
  collect [] expanded

let legacy_tag_prefix = "verocaml_tag_"

let legacy_tag_function function_ =
  String.starts_with ~prefix:legacy_tag_prefix
    (Logic_ir.View.function_name function_)

let rec term_has_legacy_tag term =
  let recurse = term_has_legacy_tag in
  match Logic_ir.View.term_node term with
  | Logic_ir.View.Apply (function_, arguments) ->
      legacy_tag_function function_ || List.exists recurse arguments
  | Rank_project (_, _, function_, value) ->
      legacy_tag_function function_ || recurse value
  | And terms | Or terms -> List.exists recurse terms
  | Negate value | Not value | Scale (_, value) -> recurse value
  | Add (left, right) | Subtract (left, right)
  | Less_than (left, right) | Less_or_equal (left, right)
  | Greater_than (left, right) | Greater_or_equal (left, right)
  | Equal (left, right) | Distinct (left, right)
  | Implies (left, right) ->
      recurse left || recurse right
  | Ite (condition, then_, else_) ->
      recurse condition || recurse then_ || recurse else_
  | Forall_term quantifier | Exists_term quantifier ->
      recurse (Logic_ir.View.user_quantifier_body quantifier)
      ||
      (match Logic_ir.View.user_quantifier_trigger quantifier with
      | None -> false
      | Some trigger -> recurse trigger)
  | Integer _ | Boolean _ | Bound _ -> false

let query_has_legacy_tag query =
  List.exists
    (function
      | Logic_ir.View.Sort_declaration _ -> false
      | Function_declaration function_ -> legacy_tag_function function_)
    (Logic_ir.View.declarations query)
  || List.exists
       (fun axiom ->
         term_has_legacy_tag (Logic_ir.View.axiom_body axiom)
         || List.exists
              (List.exists term_has_legacy_tag)
              (Logic_ir.View.axiom_patterns axiom))
       (Logic_ir.View.axioms query)
  || List.exists term_has_legacy_tag (Logic_ir.View.assertions query)

let oracle filename =
  let program = load_program filename in
  let all_prepared = prepare program in
  check "tuple matrix termination total changed"
    (Recursive_spec_encoding.termination_obligation_count all_prepared = 9);
  let all_verified = verify all_prepared in
  check "tuple matrix recursive definition count changed"
    (List.length (Recursive_spec_encoding.definition_ids all_verified) = 3);
  let definition = recursive_definition "spec_node_eq_alt" program in
  check "exact tuple match termination obligations changed"
    (Recursive_spec_encoding.For_testing.termination_obligations all_prepared
    |> List.filter (fun (obligation : Vir.obligation) ->
           String.equal obligation.Vir.function_ref.function_name
             "spec_node_eq_alt")
    |> List.length
    = 3);
  let query = base_query all_verified definition.function_id in
  let declarations = Logic_ir.View.declarations query in
  let declaration_names =
    List.map
      (function
        | Logic_ir.View.Sort_declaration sort ->
            Logic_ir.View.named_sort_name sort
        | Function_declaration function_ ->
            Logic_ir.View.function_name function_)
      declarations
  in
  check "tuple-named recursive declaration escaped"
    (not (List.exists (fun name -> contains name "tuple") declaration_names));
  List.iter
    (function
      | Logic_ir.View.Sort_declaration _ -> ()
      | Function_declaration function_ ->
          check "recursive callable ABI gained a tuple component"
            (List.for_all
               (fun sort -> not (contains (Logic_ir.sort_to_string sort) "tuple"))
               (Logic_ir.View.function_range function_
               :: Logic_ir.View.function_domain function_)))
    declarations;
  check "legacy recursive tag declaration or term escaped native encoding"
    (not (query_has_legacy_tag query));
  let expected_a2_qid =
    Printf.sprintf "spec.%d.%s.fuel-body"
      definition.function_id.function_index
      definition.function_id.function_name
  in
  let a2 =
    match
      List.filter
        (fun axiom ->
          String.equal (Logic_ir.View.axiom_qid axiom) expected_a2_qid)
        (Logic_ir.View.axioms query)
    with
    | [ a2 ] -> a2
    | [] -> fail "exact recursive fuel-body A2 axiom is absent"
    | _ :: _ :: _ -> fail "exact recursive fuel-body A2 axiom is duplicated"
  in
  let first_parameter, second_parameter =
    match Logic_ir.View.axiom_binders a2 with
    | [ first_parameter; second_parameter; _fuel ] ->
        (first_parameter, second_parameter)
    | _ -> fail "exact recursive fuel-body A2 binder vector changed"
  in
  let expanded =
    match Logic_ir.View.term_node (Logic_ir.View.axiom_body a2) with
    | Logic_ir.View.Equal (_, expanded) -> expanded
    | _ -> fail "A2 body is not an equality"
  in
  check "tuple match did not produce exactly two ordered fallthrough nodes"
    (count_ites expanded = 2);
  let recognizers = native_recognizers program query in
  let same_constructor left right =
    left.constructor_index = right.constructor_index
    && String.equal left.constructor_name right.constructor_name
    && String.equal left.constructor_uid right.constructor_uid
  in
  check "tuple match synthesized cross-constructor pairs"
    (match
       ordered_match_conditions recognizers first_parameter second_parameter
         expanded
     with
    | [ (empty_left, empty_right); (node_left, node_right) ] ->
        empty_left.constructor_index = 0
        && String.equal empty_left.constructor_name "Empty"
        && same_constructor empty_left empty_right
        && node_left.constructor_index = 1
        && String.equal node_left.constructor_name "Node"
        && same_constructor node_left node_right
    | [] | [ _ ] | _ :: _ :: _ -> false);
  Printf.printf
    "tuple-oracle termination=3 matrix-termination=9 recursive=3 ites=2 \
     pairs=Empty/Empty,Node/Node cross=0 tuple-ir=0 abi=2\n"

let ground filename =
  Recursive_spec_encoding.For_testing.reset_ground_counterexample_counters ();
  run filename;
  let counters =
    Recursive_spec_encoding.For_testing.ground_counterexample_counters ()
  in
  Printf.printf
    "ground attempts=%d complete=%d abstentions=%d antecedents=%d\n"
    counters.attempts counters.complete_violations counters.abstentions
    counters.antecedents_checked

let tuple_abstentions filename =
  let program = load_program filename in
  let prepared = prepare program in
  let verified = verify prepared in
  let report = report filename in
  let result =
    Verification_driver_private.results report
    |> List.find (fun result ->
           let obligation = result.Solver_backend.obligation in
           String.equal obligation.Vir.function_ref.function_name
             "check_partial_pair"
           &&
           match obligation.kind with
           | Vir.Local_assertion _ ->
               Vir.obligation_has_recursive_specification obligation
           | _ -> false)
  in
  let obligation = result.Solver_backend.obligation in
  let proof =
    List.find
      (fun definition ->
        String.equal definition.Sst.function_id.function_name
          "check_partial_pair")
      program.Sst.functions
  in
  let input =
    List.find
      (fun (symbol : Vir.symbol) ->
        symbol.role = Vir.Input
        && String.equal symbol.source_name "a"
        &&
        match symbol.sort with
        | Vir.Aggregate _ -> true
        | Vir.Integer | Vir.Boolean | Vir.Parametric _ -> false)
      obligation.projection_symbols
  in
  let empty =
    List.find_map
      (fun (definition : Sst.type_definition) ->
        match definition.type_kind with
        | Sst.Variant_definition constructors ->
            List.find_map
              (fun (constructor : Sst.constructor_definition) ->
                if
                  String.equal constructor.constructor_id.constructor_name
                    "Empty"
                  && constructor.constructor_fields = []
                then Some constructor.constructor_id
                else None)
              constructors
        | Sst.Record_definition _ -> None)
      program.types
    |> Option.get
  in
  let activations =
    Recursive_spec_encoding.proof_entry_activations verified proof.function_id
  in
  Recursive_spec_encoding.For_testing.reset_ground_counterexample_counters ();
  Recursive_spec_encoding.For_testing.reset_nullary_branch_retry_counters ();
  Recursive_spec_encoding.For_testing.reset_proof_query_construction_count ();
  Solver_backend.For_testing.reset_solver_creation_count ();
  Z3_bridge.reset_counters ();
  let retry =
    match
      Recursive_spec_encoding.retry_nullary_branch ~timeout_ms:60_000 verified
        ~activations ~ground_constructors:[ (input, empty) ] obligation
    with
    | Ok retry -> retry
    | Error error -> fail (Recursive_spec_encoding.error_to_string error)
  in
  check "tuple recursive call entered the nullary retry"
    (retry = Recursive_spec_encoding.Nullary_branch_abstain);
  let ground =
    Recursive_spec_encoding.ground_counterexample verified ~activations
      ~ground_constructors:[ (input, empty) ] obligation
  in
  check "partial tuple ground shape did not abstain"
    (ground = Recursive_spec_encoding.Ground_abstain);
  let retry =
    Recursive_spec_encoding.For_testing.nullary_branch_retry_counters ()
  in
  let ground =
    Recursive_spec_encoding.For_testing.ground_counterexample_counters ()
  in
  let direct = Z3_bridge.counters () in
  check "tuple abstentions created retry facts or queries"
    (retry.attempts = 1 && retry.queries = 0 && retry.facts = 0
    && retry.verified = 0 && retry.counterexamples = 0
    && retry.inconclusives = 0 && retry.abstentions = 1);
  check "partial tuple ground shape changed fallback outcome"
    (ground.attempts = 1 && ground.complete_violations = 0
    && ground.abstentions = 1);
  check "tuple abstentions created solver work"
    (Recursive_spec_encoding.For_testing.proof_query_construction_count () = 0
    && Solver_backend.For_testing.solver_creation_count () = 0
    && direct.contexts_created = 0 && direct.solvers_created = 0);
  Printf.printf
    "tuple-abstain ground=%d/%d/%d nullary=%d/%d/%d solver-delta=0\n"
    ground.attempts ground.complete_violations ground.abstentions retry.attempts
    retry.queries retry.abstentions

let reject ~admission filename =
  Solver_backend.For_testing.reset_solver_creation_count ();
  Z3_bridge.reset_counters ();
  Recursive_spec_encoding.For_testing.reset_helper_expansion_count ();
  Recursive_spec_encoding.For_testing.reset_recursive_lowering_count ();
  Recursive_spec_encoding.For_testing.reset_a2_builder_construction_count ();
  let program = load_program filename in
  (match Recursive_spec_encoding.prepare program with
  | Error _ -> ()
  | Ok _ -> fail "negative tuple fixture reached recursive preparation");
  let direct = Z3_bridge.counters () in
  check "negative tuple fixture created a solver"
    (Solver_backend.For_testing.solver_creation_count () = 0
    && direct.contexts_created = 0
    && direct.solvers_created = 0);
  check "negative tuple fixture reached helper expansion"
    (Recursive_spec_encoding.For_testing.helper_expansion_count () = 0);
  if admission then
    check "negative tuple fixture escaped first-order admission"
      (Recursive_spec_encoding.For_testing.recursive_lowering_count () = 0);
  check "negative tuple fixture reached A2 construction"
    (Recursive_spec_encoding.For_testing.a2_builder_construction_count () = 0);
  print_endline
    (if admission then
       "tuple-rejected admission=true solvers=0 helper-expansion=0 \
        recursive-lowering=0 a2=0"
     else
       "tuple-rejected solvers=0 helper-expansion=0 a2=0")

let () =
  match Array.to_list Sys.argv with
  | [ _; filename ] -> run filename
  | [ _; "resource-retry"; filename ] -> resource_retry filename
  | [ _; "oracle"; filename ] -> oracle filename
  | [ _; "ground"; filename ] -> ground filename
  | [ _; "tuple-abstain"; filename ] -> tuple_abstentions filename
  | [ _; "reconstruction"; filename ] -> reconstruction filename
  | [ _; "reject"; filename ] -> reject ~admission:false filename
  | [ _; "reject-admission"; filename ] -> reject ~admission:true filename
  | _ ->
      fail "usage: aggregate_recursive_specifications_tool [oracle] FILE"
