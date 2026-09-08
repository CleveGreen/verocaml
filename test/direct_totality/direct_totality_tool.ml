let fail format =
  Printf.ksprintf
    (fun message ->
      prerr_endline message;
      exit 3)
    format

let check label condition = if not condition then fail "%s" label

let span line =
  let position column = Diagnostic.{ line; column } in
  Diagnostic.
    {
      file = "totality.ml";
      start_pos = position 0;
      end_pos = position 12;
    }

let expression line typ expression_desc =
  Sst.{ expression_desc; typ; span = span line }

let int line value = expression line Sst.Int (Sst.Int_constant (Z.of_int value))
let bool line value = expression line Sst.Bool (Sst.Bool_constant value)

let binding id name typ line =
  Sst.
    {
      id;
      name;
      typ;
      uniqueness = Definitely_aliased;
      span = span line;
    }

let variable (binding : Sst.binding) =
  expression binding.span.start_pos.line binding.typ
    (Sst.Variable
       { binding; use_uniqueness = Sst.Definitely_aliased })

let bind_pattern binding =
  Sst.{ pattern_desc = Bind binding; typ = binding.typ; span = binding.span }

let parameter pattern = Sst.{ label = None; pattern; optional_default = None }

let compare line comparison left right =
  expression line Sst.Bool (Sst.Compare (comparison, left, right))

let boolean_binary line operation left right =
  expression line Sst.Bool (Sst.Boolean_binary (operation, left, right))

let arithmetic line operation arguments =
  expression line Sst.Int (Sst.Checked_arithmetic (operation, arguments))

type clause_kind = Requires | Ensures | Decreases

type clause = {
  kind : clause_kind;
  binder : Sst.pattern option;
  payload : Sst.expression;
  span : Sst.span;
}

let pending_contracts : (Sst.expression * clause list) list ref = ref []

let ghost line kind ?binder payload =
  { kind; binder; payload; span = span line }

let prefix clauses body =
  pending_contracts := (body, clauses) :: !pending_contracts;
  body

let contracts_for body =
  let rec take before = function
    | [] -> ([], List.rev before)
    | (candidate, clauses) :: rest when candidate == body ->
        (clauses, List.rev_append before rest)
    | entry :: rest -> take (entry :: before) rest
  in
  let clauses, remaining = take [] !pending_contracts in
  pending_contracts := remaining;
  let requires = ref [] and ensures = ref [] and decreases = ref []
  and assertions = ref [] in
  List.iter
    (fun clause ->
      let predicate = Sst.{ stage = Logical; expression = clause.payload } in
      match clause.kind with
      | Requires ->
          requires :=
            Sst.
              {
                clause_index = List.length !requires;
                predicate;
                span = clause.span;
              }
            :: !requires
      | Ensures ->
          ensures :=
            Sst.
              {
                clause_index = List.length !ensures;
                binder = clause.binder;
                predicate;
                span = clause.span;
              }
            :: !ensures
      | Decreases ->
          decreases :=
            Sst.
              {
                clause_index = List.length !decreases;
                predicate;
                span = clause.span;
              }
            :: !decreases
      )
    clauses;
  Sst.
    {
      requires = List.rev !requires;
      ensures = List.rev !ensures;
      decreases = List.rev !decreases;
      assertions = List.rev !assertions;
    }

let direct_call ?(recursive = true) line typ callee arguments =
  expression line typ
    (Sst.Direct_call
       { call_form = Sst.Exec_call; callee; type_arguments = []; arguments; recursive })

let definition ?(syntactically_recursive = true) index name parameters body =
  Sst_normalize.checked_exec_raw
    ~function_id:Sst.{ function_index = index; function_name = name }
    ~recursive:syntactically_recursive ~parameters
    ~contracts:(contracts_for body) ~body ~result_type:body.typ
    ~returns_unique_parameter:None ~span:(span (index + 1))

let lower definition =
  match Symbolic_executor.lower_function definition with
  | Ok execution -> execution
  | Error error -> fail "%s" (Symbolic_executor.error_to_string error)

let solver_config () =
  match Solver_backend.config ~timeout_ms:5000 with
  | Ok config -> config
  | Error error -> fail "%s" (Solver_backend.error_to_string error)

let outcome obligations =
  match Solver_backend.solve_in_order (solver_config ()) obligations with
  | Error error -> fail "%s" (Solver_backend.error_to_string error)
  | Ok [] -> `Verified
  | Ok results -> (
      match (List.hd (List.rev results)).Solver_backend.outcome with
      | Solver_backend.Verified -> `Verified
      | Solver_backend.Counterexample _ -> `Counterexample
      | Solver_backend.Inconclusive _ -> `Inconclusive)

let kind_name (obligation : Vir.obligation) =
  match obligation.kind with
  | Vir.Arithmetic_safety { violated_bound = Vir.Lower_bound; _ } ->
      "arithmetic-lower"
  | Vir.Arithmetic_safety { violated_bound = Vir.Upper_bound; _ } ->
      "arithmetic-upper"
  | Vir.Assertion _ -> "assertion"
  | Vir.Local_assertion _ -> "local-assertion"
  | Vir.Postcondition _ -> "postcondition"
  | Vir.Call_precondition _ -> "call-precondition"
  | Vir.Callback_precondition _ -> "callback-precondition"
  | Vir.Invariant_validity _ -> "invariant-validity"
  | Vir.Entry_measure_nonnegative _ -> "entry-measure-nonnegative"
  | Vir.Recursive_call_measure_nonnegative _ ->
      "recursive-call-measure-nonnegative"
  | Vir.Recursive_call_strict_descent _ -> "recursive-call-strict-descent"

let expect_error label expected definition =
  match Symbolic_executor.lower_function definition with
  | Error error when expected error.Symbolic_executor.unsupported -> ()
  | Error error ->
      fail "%s produced the wrong error: %s" label
        (Symbolic_executor.error_to_string error)
  | Ok _ -> fail "%s was accepted" label

let validated_program definitions =
  let program =
    Sst.
      {
        policy = Default_linear_z3;
        parametric_adts = [];
        types = [];
        logical_constants = [];
        functions = definitions;
      }
  in
  match Sst_validation.validate program with
  | Ok validated -> (program, validated)
  | Error error ->
      fail "termination fixture failed semantic admission: %s"
        (Sst_validation.error_to_string error)

let expect_termination_error label expected analysis =
  match Termination.prepare analysis with
  | Error error when expected error.Termination.kind -> ()
  | Error error ->
      fail "%s produced the wrong termination error: %s" label
        (Termination.error_to_string error)
  | Ok _ -> fail "%s was accepted" label

let rec contains_contiguous expected actual =
  match (expected, actual) with
  | [], _ -> true
  | _, [] -> false
  | expected, actual ->
      let rec prefix expected actual =
        match (expected, actual) with
        | [], _ -> true
        | _, [] -> false
        | expected :: expected_rest, actual :: actual_rest ->
            String.equal expected actual && prefix expected_rest actual_rest
      in
      prefix expected actual || contains_contiguous expected (List.tl actual)

let recursive_fixture ?(requires = true) ?(measure = `N)
    ?(call_argument = `Predecessor) ?(guard = `Nonzero) index name =
  let n = binding (index * 10) "n" Sst.Int (index + 10) in
  let result = binding ((index * 10) + 1) "result" Sst.Int (index + 11) in
  let function_id = Sst.{ function_index = index; function_name = name } in
  let requirement =
    ghost (index + 12) Requires
      (compare (index + 12) Sst.Greater_or_equal (variable n) (int 0 0))
  in
  let measure_expression =
    match measure with
    | `N -> variable n
    | `N_plus_one -> arithmetic (index + 13) Sst.Add [ variable n; int 0 1 ]
    | `Bool -> bool (index + 13) true
  in
  let measure_clause = ghost (index + 13) Decreases measure_expression in
  let actual =
    match call_argument with
    | `N -> variable n
    | `Predecessor ->
        arithmetic (index + 15) Sst.Subtract [ variable n; int 0 1 ]
  in
  let call = direct_call (index + 15) Sst.Int function_id
    [ Sst.Value_argument { label = None; value = actual } ] in
  let condition =
    match guard with
    | `Nonzero -> compare (index + 14) Sst.Equal (variable n) (int 0 0)
    | `Nonnegative ->
        compare (index + 14) Sst.Greater_or_equal (variable n) (int 0 0)
  in
  let consequent, alternative =
    match guard with
    | `Nonzero -> (int (index + 14) 0, call)
    | `Nonnegative -> (call, int (index + 14) 0)
  in
  let body =
    expression (index + 14) Sst.Int
      (Sst.If (condition, consequent, Some alternative))
  in
  let post =
    ghost (index + 11) Ensures ~binder:(bind_pattern result)
      (compare (index + 11) Sst.Equal (variable result) (int 0 0))
  in
  let clauses =
    (if requires then [ requirement ] else []) @ [ post; measure_clause ]
  in
  definition index name
    [ Sst.Value_parameter (parameter (bind_pattern n)) ]
    (prefix clauses body)

let sole_parameter_binding definition =
  match definition.Sst.parameters with
  | [ Sst.Value_parameter
        { pattern = { pattern_desc = Sst.Bind binding; _ }; _ } ] -> binding
  | _ -> fail "fixture does not have one bound parameter"

let run_unit_checks () =
  let countdown = recursive_fixture 0 "countdown" in
  let _, countdown_validated = validated_program [ countdown ] in
  let countdown_analysis = Termination.analyze countdown_validated in
  let countdown_graph = Termination.graph countdown_analysis in
  check "termination graph did not retain the recursive body edge"
    (match Termination.graph_edges countdown_graph with
    | [ edge ] ->
        Termination.call_edge_region edge = Sst_validation.Body_region
        && Termination.call_edge_recursive edge
    | _ -> false);
  let countdown_component =
    match Termination.sccs countdown_analysis with
    | [ component ] -> component
    | _ -> fail "countdown did not produce exactly one SCC"
  in
  check "direct self SCC classification changed"
    (Termination.scc_is_recursive countdown_component
    && Termination.scc_recursion_kind countdown_component
       = Some Termination.Direct_self
    && Termination.scc_modes countdown_component = [ Sst.Exec ]);
  let countdown_plan =
    match Termination.prepare countdown_analysis with
    | Ok plan -> plan
    | Error error -> fail "%s" (Termination.error_to_string error)
  in
  let pending, edge =
    match Termination.pending_summaries countdown_plan with
    | [ pending ] -> (
        match
          Termination.find_edge_intent countdown_plan
            ~caller:countdown.function_id ~callee:countdown.function_id
            ~span:(span 15)
        with
        | Some edge -> (pending, edge)
        | None -> fail "direct self edge has no termination intent")
    | _ -> fail "countdown did not issue one opaque pending summary"
  in
  check "integer termination domain changed"
    (Termination.pending_domain pending = Termination.Integer_height);
  check "edge intent lost its exact integer plan identity"
    (Termination.edge_domain edge = Termination.Integer_height
    &&
    let descriptor = Termination.edge_descriptor edge in
    Termination.callable_id (Termination.call_edge_caller descriptor)
    = countdown.function_id
    && Termination.callable_id (Termination.call_edge_callee descriptor)
       = countdown.function_id
    && Termination.call_edge_span descriptor = span 15);
  let countdown_execution = lower countdown in
  check "countdown did not verify"
    (outcome countdown_execution.obligations = `Verified);
  let expected_order =
    [
      "entry-measure-nonnegative";
      "arithmetic-lower";
      "arithmetic-upper";
      "call-precondition";
      "recursive-call-measure-nonnegative";
      "recursive-call-strict-descent";
      "postcondition";
      "postcondition";
    ]
  in
  check "countdown VC order changed"
    (List.map kind_name countdown_execution.obligations = expected_order);

  let old_n = binding 100 "n" Sst.Int 100 in
  let old_result = binding 101 "result" Sst.Int 101 in
  let old_id =
    Sst.{ function_index = 10; function_name = "recursive_old_entry" }
  in
  let old_requires =
    ghost 102 Requires
      (boolean_binary 102 Sst.And
         (compare 102 Sst.Greater_or_equal (variable old_n) (int 102 0))
         (compare 102 Sst.Less_than (variable old_n)
            (expression 102 Sst.Int (Sst.Int_constant Int_bounds.maximum))))
  in
  let old_entry =
    expression 103 Sst.Int
      (Sst.Old (variable old_n))
  in
  let old_post =
    ghost 103 Ensures ~binder:(bind_pattern old_result)
      (compare 103 Sst.Equal (variable old_result) old_entry)
  in
  let old_recursive_call =
    direct_call 105 Sst.Int old_id
      [
        Sst.Value_argument
          { label = None;
            value = arithmetic 105 Sst.Subtract [ variable old_n; int 105 1 ] };
      ]
  in
  let old_body =
    expression 104 Sst.Int
      (Sst.If
         ( compare 104 Sst.Equal (variable old_n) (int 104 0),
           int 104 0,
           Some
             (arithmetic 105 Sst.Add [ old_recursive_call; int 105 1 ]) ))
  in
  let recursive_old =
    definition 10 "recursive_old_entry"
      [ Sst.Value_parameter (parameter (bind_pattern old_n)) ]
      (prefix
         [ old_requires; old_post; ghost 104 Decreases (variable old_n) ]
         old_body)
  in
  check "recursive summary old did not use the actual call entry"
    (outcome (lower recursive_old).obligations = `Verified);

  let unchanged =
    recursive_fixture ~call_argument:`N 1 "unchanged_measure"
  in
  let _, unchanged_validated = validated_program [ unchanged ] in
  let unchanged_plan =
    match
      Termination.prepare (Termination.analyze unchanged_validated)
    with
    | Ok plan -> plan
    | Error error -> fail "%s" (Termination.error_to_string error)
  in
  check "unchanged measure did not retain a pending descriptor"
    (match Termination.pending_summaries unchanged_plan with
    | [ pending ] ->
        Termination.callable_id (Termination.pending_callable pending)
        = unchanged.function_id
    | _ -> false);
  let unchanged_execution = lower unchanged in
  check "unchanged measure did not fail"
    (outcome unchanged_execution.obligations = `Counterexample);
  let strict =
    List.find
      (fun obligation ->
        match obligation.Vir.kind with
        | Vir.Recursive_call_strict_descent _ -> true
        | _ -> false)
      unchanged_execution.obligations
  in
  check "unchanged measure strict VC was not independently false"
    (outcome [ strict ] = `Counterexample);

  let possibly_negative =
    recursive_fixture ~requires:false ~guard:`Nonnegative 2
      "possibly_negative_measure"
  in
  let negative_execution = lower possibly_negative in
  check "possibly negative entry measure did not fail"
    (outcome negative_execution.obligations = `Counterexample);
  let call_nonnegative =
    List.find
      (fun obligation ->
        match obligation.Vir.kind with
        | Vir.Recursive_call_measure_nonnegative _ -> true
        | _ -> false)
      negative_execution.obligations
  in
  check "possibly negative call measure did not fail its distinct VC"
    (outcome [ call_nonnegative ] = `Counterexample);

  let unsafe_measure =
    recursive_fixture ~measure:`N_plus_one 3 "unsafe_measure"
  in
  let unsafe_execution = lower unsafe_measure in
  check "overflowing entry measure was accepted"
    (outcome unsafe_execution.obligations = `Counterexample);
  check "measure arithmetic safety was not emitted before entry nonnegativity"
    (match List.map kind_name unsafe_execution.obligations with
    | "arithmetic-lower" :: "arithmetic-upper"
      :: "entry-measure-nonnegative" :: _ -> true
    | _ -> false);
  check
    "recursive call did not order argument safety, preconditions, call-measure safety, and descent"
    (contains_contiguous
       [
         "arithmetic-lower";
         "arithmetic-upper";
         "call-precondition";
         "arithmetic-lower";
         "arithmetic-upper";
         "recursive-call-measure-nonnegative";
         "recursive-call-strict-descent";
       ]
       (List.map kind_name unsafe_execution.obligations));

  let missing = recursive_fixture 4 "missing" in
  let missing =
    {
      missing with
      contracts = { missing.contracts with decreases = [] };
    }
  in
  expect_error "missing decreases"
    (function Symbolic_executor.Missing_decreases -> true | _ -> false)
    missing;

  let duplicate = recursive_fixture 5 "duplicate" in
  let duplicate_n = sole_parameter_binding duplicate in
  let duplicate_clause : Sst.predicate_clause =
    {
      clause_index = 1;
      predicate = { stage = Logical; expression = variable duplicate_n };
      span = span 99;
    }
  in
  let duplicate =
    {
      duplicate with
      contracts =
        {
          duplicate.contracts with
          decreases = duplicate.contracts.decreases @ [ duplicate_clause ];
        };
    }
  in
  expect_error "duplicate decreases"
    (function Symbolic_executor.Duplicate_decreases -> true | _ -> false)
    duplicate;
  (match Symbolic_executor.lower_function duplicate with
  | Error error ->
      check "duplicate error did not use the second declaration span"
        (error.span.start_pos.line = 99)
  | Ok _ -> assert false);

  let nonrecursive_n = binding 70 "n" Sst.Int 70 in
  let nonrecursive =
    definition ~syntactically_recursive:false 7 "nonrecursive"
      [ Sst.Value_parameter (parameter (bind_pattern nonrecursive_n)) ]
      (prefix
         [ ghost 71 Decreases (variable nonrecursive_n) ]
         (variable nonrecursive_n))
  in
  expect_error "nonrecursive decreases"
    (function Symbolic_executor.Inapplicable_decreases -> true | _ -> false)
    nonrecursive;

  let noninteger = recursive_fixture ~measure:`Bool 8 "noninteger" in
  expect_error "noninteger decreases"
    (function Symbolic_executor.Malformed_sst _ -> true | _ -> false)
    noninteger;

  let result_bound = recursive_fixture 84 "result_bound_measure" in
  let result_binding = binding 842 "result" Sst.Int 84 in
  let result_bound_clause : Sst.predicate_clause =
    {
      clause_index = 0;
      predicate = { stage = Logical; expression = variable result_binding };
      span = span 84;
    }
  in
  let result_bound =
    {
      result_bound with
      contracts =
        {
          result_bound.contracts with
          decreases = [ result_bound_clause ];
        };
    }
  in
  expect_error "result-bound decreases"
    (function Symbolic_executor.Malformed_sst _ -> true | _ -> false)
    result_bound;

  let ghost_n = binding 85 "n" Sst.Int 85 in
  let ghost_id =
    Sst.{ function_index = 85; function_name = "ghost_only_self_call" }
  in
  let ghost_self_call = direct_call 86 Sst.Int ghost_id
    [ Sst.Value_argument { label = None; value = variable ghost_n } ] in
  let ghost_requires =
    ghost 86 Requires
      (compare 86 Sst.Greater_or_equal ghost_self_call (int 86 0))
  in
  let ghost_only =
    definition 85 "ghost_only_self_call"
      [ Sst.Value_parameter (parameter (bind_pattern ghost_n)) ]
      (prefix
         [ ghost_requires; ghost 87 Decreases (variable ghost_n) ]
         (variable ghost_n))
  in
  expect_error "ghost-only self-call applicability"
    (function Symbolic_executor.Malformed_sst _ -> true | _ -> false)
    ghost_only;

  let false_marker_n = binding 860 "n" Sst.Int 86 in
  let false_marker_id =
    Sst.{ function_index = 86; function_name = "false_marker" }
  in
  let false_marker_call =
    direct_call ~recursive:false 88 Sst.Int false_marker_id
      [ Sst.Value_argument { label = None; value = variable false_marker_n } ]
  in
  let false_marker =
    definition 86 "false_marker"
      [ Sst.Value_parameter (parameter (bind_pattern false_marker_n)) ]
      (prefix
         [ ghost 87 Decreases (variable false_marker_n) ]
         false_marker_call)
  in
  expect_error "false recursive marker"
    (function Symbolic_executor.Malformed_sst _ -> true | _ -> false)
    false_marker;

  let recursive_measure_n = binding 870 "n" Sst.Int 87 in
  let recursive_measure_id =
    Sst.{ function_index = 87; function_name = "recursive_measure" }
  in
  let recursive_measure_call line =
    direct_call line Sst.Int recursive_measure_id
      [ Sst.Value_argument
          { label = None; value = variable recursive_measure_n } ]
  in
  let recursive_measure =
    definition 87 "recursive_measure"
      [ Sst.Value_parameter (parameter (bind_pattern recursive_measure_n)) ]
      (prefix
         [ ghost 88 Decreases (recursive_measure_call 88) ]
         (recursive_measure_call 89))
  in
  expect_error "recursive decreases expression"
    (function Symbolic_executor.Malformed_sst _ -> true | _ -> false)
    recursive_measure;

  let left_id = Sst.{ function_index = 91; function_name = "left" } in
  let right_id = Sst.{ function_index = 92; function_name = "right" } in
  let left =
    definition ~syntactically_recursive:true 91 "left" []
      (direct_call ~recursive:false 91 Sst.Int right_id [])
  in
  let right =
    definition ~syntactically_recursive:true 92 "right" []
      (direct_call ~recursive:false 92 Sst.Int left_id [])
  in
  let mutual_program, mutual_validated = validated_program [ left; right ] in
  let mutual_analysis = Termination.analyze mutual_validated in
  let mutual_component =
    match
      List.filter Termination.scc_is_recursive
        (Termination.sccs mutual_analysis)
    with
    | [ component ] -> component
    | _ -> fail "hidden cross edges were not retained as one recursive SCC"
  in
  check "hidden cross edges were not classified as mutual"
    (Termination.scc_recursion_kind mutual_component
       = Some Termination.Mutual
    && List.map Termination.callable_id
         (Termination.scc_members mutual_component)
       = [ left_id; right_id ]
    && List.length (Termination.scc_edges mutual_component) = 2);
  expect_termination_error "unsupported mutual SCC"
    (function Termination.Unsupported_mutual_scc _ -> true | _ -> false)
    mutual_analysis;
  (match Symbolic_executor.lower_program mutual_program with
  | Error { unsupported = Symbolic_executor.Malformed_sst detail; _ } ->
      check "mutual SCC diagnostic changed"
        (String.starts_with ~prefix:"unsupported mutual recursion SCC:" detail)
  | Error error ->
      fail "mutual SCC produced the wrong lowering error: %s"
        (Symbolic_executor.error_to_string error)
  | Ok _ -> fail "mutual SCC reached VIR lowering");

  let harmless_n = binding 90 "n" Sst.Int 90 in
  let harmless =
    definition 9 "harmless_let_rec"
      [ Sst.Value_parameter (parameter (bind_pattern harmless_n)) ]
      (variable harmless_n)
  in
  let harmless_execution = lower harmless in
  check "syntactic let rec without a resolved self-call required a measure"
    (harmless_execution.obligations = []);
  print_endline
    "totality: countdown, VC order, strict descent, nonnegative calls, and measure safety";
  print_endline
    "termination descriptors: complete SCCs, integer intents, and opaque pending summaries without a public seal issuer";
  print_endline
    "applicability: markers, hidden cross edges, mutual SCCs, and malformed measures fail before VIR"

let load filename =
  match Typedtree_lowering.lower_file filename with
  | Error diagnostic ->
      fail "%s at %s:%d:%d-%d:%d" diagnostic.Diagnostic.code
        (Filename.basename diagnostic.span.file)
        diagnostic.span.start_pos.line diagnostic.span.start_pos.column
        diagnostic.span.end_pos.line diagnostic.span.end_pos.column
  | Ok sst -> (
      match Symbolic_executor.lower_program sst with
      | Ok vir -> vir
      | Error error -> fail "%s" (Symbolic_executor.error_to_string error))

let solve_program program =
  List.iter
    (fun execution ->
      let result =
        match outcome execution.Vir.obligations with
        | `Verified -> "verified"
        | `Counterexample -> "counterexample"
        | `Inconclusive -> "inconclusive"
      in
      Printf.printf "%s: %s (%d obligations, %d exits)\n"
        execution.Vir.function_ref.function_name result
        (List.length execution.obligations)
        (List.length execution.exits))
    program.Vir.functions

let classify filename =
  match Typedtree_lowering.lower_file filename with
  | Ok _ -> fail "%s was accepted" filename
  | Error diagnostic ->
      Printf.printf "%s @ %s:%d:%d-%d:%d\n" diagnostic.Diagnostic.code
        (Filename.basename diagnostic.span.file)
        diagnostic.span.start_pos.line diagnostic.span.start_pos.column
        diagnostic.span.end_pos.line diagnostic.span.end_pos.column

let () =
  match Array.to_list Sys.argv with
  | [ _; "unit" ] -> run_unit_checks ()
  | [ _; "dump"; filename ] -> print_string (Vir.to_string (load filename))
  | [ _; "solve"; filename ] -> solve_program (load filename)
  | [ _; "classify"; filename ] -> classify filename
  | _ ->
      fail
        "usage: direct_totality_tool (unit|dump FILE.cmt|solve FILE.cmt|classify FILE.cmt)"
