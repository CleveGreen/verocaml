let fail format =
  Printf.ksprintf (fun message -> prerr_endline message; exit 3) format

let check label condition = if not condition then fail "%s" label

let span line =
  let position column = Diagnostic.{ line; column } in
  Diagnostic.
    {
      file = "contracts.ml";
      start_pos = position 0;
      end_pos = position 8;
    }

let expression line typ expression_desc =
  Sst.{ expression_desc; typ; span = span line }

let int line value = expression line Sst.Int (Sst.Int_constant (Z.of_int value))
let bool line value = expression line Sst.Bool (Sst.Bool_constant value)
let variable (binding : Sst.binding) =
  expression binding.span.start_pos.line binding.typ
    (Sst.Variable
       { binding; use_uniqueness = Sst.Definitely_aliased })

let compare line comparison left right =
  expression line Sst.Bool (Sst.Compare (comparison, left, right))

let arithmetic line operation arguments =
  expression line Sst.Int (Sst.Checked_arithmetic (operation, arguments))

let binding id name typ line =
  Sst.
    {
      id;
      name;
      typ;
      uniqueness = Definitely_aliased;
      span = span line;
    }

let bind_pattern binding =
  Sst.
    {
      pattern_desc = Bind binding;
      typ = binding.typ;
      span = binding.span;
    }

let wildcard typ line =
  Sst.{ pattern_desc = Wildcard; typ; span = span line }

type clause_kind = Requires | Ensures | Assert

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
      | Assert ->
          assertions :=
            Sst.
              {
                clause_index = List.length !assertions;
                predicate;
                span = clause.span;
              }
            :: !assertions)
    clauses;
  Sst.
    {
      requires = List.rev !requires;
      ensures = List.rev !ensures;
      decreases = List.rev !decreases;
      assertions = List.rev !assertions;
    }

let definition index name parameters result_type body =
  Sst_normalize.checked_exec_raw
    ~function_id:Sst.{ function_index = index; function_name = name }
    ~recursive:false ~parameters ~contracts:(contracts_for body) ~body ~result_type
    ~returns_unique_parameter:None ~span:(span (index + 1))

let parameter pattern = Sst.Value_parameter
  Sst.{ label = None; pattern; optional_default = None }

let direct_call line typ callee arguments =
  expression line typ
    (Sst.Direct_call
       { call_form = Sst.Exec_call; callee; type_arguments = []; arguments; recursive = false })

let lower functions =
  match
    Symbolic_executor.lower_program
      Sst.{ policy = Default_linear_z3; parametric_adts = []; types = []; functions }
  with
  | Ok program -> program
  | Error error -> fail "%s" (Symbolic_executor.error_to_string error)

let execution name program =
  List.find
    (fun execution ->
      String.equal execution.Vir.function_ref.function_name name)
    program.Vir.functions

let outcome execution =
  let config =
    match Solver_backend.config ~timeout_ms:5000 with
    | Ok config -> config
    | Error error -> fail "%s" (Solver_backend.error_to_string error)
  in
  match
    Solver_backend.solve_in_order config execution.Vir.obligations
  with
  | Error error -> fail "%s" (Solver_backend.error_to_string error)
  | Ok [] -> `No_obligations
  | Ok results -> (
      match (List.hd (List.rev results)).Solver_backend.outcome with
      | Solver_backend.Verified -> `Verified
      | Solver_backend.Counterexample _ -> `Counterexample
      | Solver_backend.Inconclusive _ -> `Inconclusive)

let has_kind predicate execution =
  List.exists (fun obligation -> predicate obligation.Vir.kind)
    execution.Vir.obligations

let counterexample_projects_result execution =
  let config =
    match Solver_backend.config ~timeout_ms:5000 with
    | Ok config -> config
    | Error error -> fail "%s" (Solver_backend.error_to_string error)
  in
  match Solver_backend.solve_in_order config execution.Vir.obligations with
  | Ok results -> (
      match (List.hd (List.rev results)).Solver_backend.outcome with
      | Solver_backend.Counterexample bindings ->
          List.exists
            (fun binding -> binding.Solver_backend.symbol.Vir.role = Vir.Result)
            bindings
      | Solver_backend.Verified | Solver_backend.Inconclusive _ -> false)
  | Error error -> fail "%s" (Solver_backend.error_to_string error)

let run_contract_checks () =
  let x = binding 0 "x" Sst.Int 1 in
  let result = binding 1 "result" Sst.Int 2 in
  let result_pattern = bind_pattern result in
  let requires =
    ghost 2 Requires
      (compare 2 Sst.Less_than (variable x)
         (expression 2 Sst.Int (Sst.Int_constant Int_bounds.maximum)))
  in
  let logical_add =
    arithmetic 3 Sst.Add [ variable x; int 3 1 ]
  in
  let valid_post =
    ghost 3 Ensures ~binder:result_pattern
      (compare 3 Sst.Equal (variable result) logical_add)
  in
  let valid_assert =
    ghost 4 Assert
      (compare 4 Sst.Less_than (variable x)
         (expression 4 Sst.Int (Sst.Int_constant Int_bounds.maximum)))
  in
  let body = arithmetic 5 Sst.Add [ variable x; int 5 1 ] in
  let valid =
    definition 0 "valid"
      [ parameter (bind_pattern x) ] Sst.Int
      (prefix [ requires; valid_post; valid_assert ] body)
  in
  let false_result = binding 1 "result" Sst.Int 12 in
  let false_post =
    ghost 12 Ensures ~binder:(bind_pattern false_result)
      (compare 12 Sst.Equal (variable false_result) (variable x))
  in
  let false_post_definition =
    definition 1 "false_post"
      [ parameter (bind_pattern x) ] Sst.Int
      (prefix [ requires; false_post ] body)
  in
  let false_assert =
    ghost 20 Assert
      (compare 20 Sst.Less_than (variable x) (variable x))
  in
  let false_assert_definition =
    definition 2 "false_assert"
      [ parameter (bind_pattern x) ] Sst.Int
      (prefix [ false_assert ] (variable x))
  in
  let shadow = binding 4 "current_x" Sst.Int 30 in
  let saved = binding 3 "saved_entry_x" Sst.Int 30 in
  let old_result = binding 2 "result" Sst.Int 31 in
  let old_x =
    expression 31 Sst.Int
      (Sst.Old (variable x))
  in
  let old_post =
    ghost 31 Ensures ~binder:(bind_pattern old_result)
      (compare 31 Sst.Equal (variable old_result) old_x)
  in
  let shadow_body =
    expression 32 Sst.Int
      (Sst.Let
         ( [ (bind_pattern saved, variable x) ],
           expression 32 Sst.Int
             (Sst.Let
                ([ (bind_pattern shadow, int 32 9) ], variable saved)) ))
  in
  let old_definition =
    definition 3 "entry_old"
      [ parameter (bind_pattern x) ] Sst.Int
      (prefix [ old_post ] shadow_body)
  in
  let program =
    lower
      [
        valid;
        false_post_definition;
        false_assert_definition;
        old_definition;
      ]
  in
  let valid = execution "valid" program in
  check "valid contract did not verify" (outcome valid = `Verified);
  check "logical specification arithmetic emitted a safety VC"
    (List.length
       (List.filter
          (fun obligation ->
            match obligation.Vir.kind with
            | Vir.Arithmetic_safety _ -> true
            | _ -> false)
          valid.obligations)
    = 2);
  check "assertion provenance missing"
    (has_kind
       (function Vir.Assertion { assertion_ordinal = 0 } -> true | _ -> false)
       valid);
  let assertion_goal =
    (List.find
       (fun obligation ->
         match obligation.Vir.kind with Vir.Assertion _ -> true | _ -> false)
       valid.obligations)
      .Vir.goal
  in
  check "later runtime safety does not depend on the ordered assertion"
    (List.exists
       (fun obligation ->
         match obligation.Vir.kind with
         | Vir.Arithmetic_safety _ ->
             List.mem assertion_goal obligation.assumptions
         | _ -> false)
       valid.obligations);
  check "postcondition provenance missing"
    (has_kind
       (function
         | Vir.Postcondition { postcondition_ordinal = 0; _ } -> true
         | _ -> false)
       valid);
  check "false postcondition did not produce a counterexample"
    (outcome (execution "false_post" program) = `Counterexample);
  check "false postcondition model omitted its materialized result"
    (counterexample_projects_result (execution "false_post" program));
  check "false assertion did not stop with a counterexample"
    (outcome (execution "false_assert" program) = `Counterexample);
  check "old did not select the deliberately distinct entry binding"
    (outcome (execution "entry_old" program) = `Verified);
  print_endline
    "contracts: valid and false assertions/postconditions, logical arithmetic, entry-old"

let run_call_checks () =
  let callee_x = binding 10 "callee_x" Sst.Int 40 in
  let callee_result = binding 11 "callee_result" Sst.Int 41 in
  let callee_requires =
    ghost 40 Requires
      (compare 40 Sst.Greater_or_equal (variable callee_x) (int 40 0))
  in
  let callee_post =
    ghost 41 Ensures ~binder:(bind_pattern callee_result)
      (compare 41 Sst.Equal (variable callee_result) (variable callee_x))
  in
  let callee =
    definition 10 "callee"
      [ parameter (bind_pattern callee_x) ] Sst.Int
      (prefix [ callee_requires; callee_post ] (variable callee_x))
  in
  let caller_x = binding 20 "caller_x" Sst.Int 50 in
  let caller_result = binding 21 "caller_result" Sst.Int 51 in
  let caller_requires =
    ghost 50 Requires
      (compare 50 Sst.Greater_or_equal (variable caller_x) (int 50 0))
  in
  let call =
    direct_call 52 Sst.Int callee.function_id
      [ Sst.Value_argument { label = None; value = variable caller_x } ]
  in
  let caller_post =
    ghost 51 Ensures ~binder:(bind_pattern caller_result)
      (compare 51 Sst.Equal (variable caller_result) (variable caller_x))
  in
  let caller =
    definition 11 "caller"
      [ parameter (bind_pattern caller_x) ] Sst.Int
      (prefix [ caller_requires; caller_post ] call)
  in
  let bad_caller =
    definition 12 "bad_caller"
      [ parameter (bind_pattern caller_x) ] Sst.Int call
  in
  let overflow_call =
    direct_call 60 Sst.Int callee.function_id
      [ Sst.Value_argument
          { label = None;
            value =
              arithmetic 60 Sst.Add
                [ expression 60 Sst.Int
                    (Sst.Int_constant Int_bounds.maximum);
                  int 60 1 ] } ]
  in
  let overflow_caller =
    definition 13 "overflow_call" [] Sst.Int overflow_call
  in
  let contractless_x = binding 30 "contractless_x" Sst.Int 70 in
  let contractless =
    definition 20 "contractless"
      [ parameter (bind_pattern contractless_x) ] Sst.Int
      (variable contractless_x)
  in
  let nondet_result = binding 31 "nondet_result" Sst.Int 71 in
  let nondet_post =
    ghost 71 Ensures ~binder:(bind_pattern nondet_result)
      (compare 71 Sst.Equal (variable nondet_result) (variable caller_x))
  in
  let nondet =
    definition 21 "contractless_caller"
      [ parameter (bind_pattern caller_x) ] Sst.Int
      (prefix
         [ nondet_post ]
         (direct_call 72 Sst.Int contractless.function_id
            [ Sst.Value_argument { label = None; value = variable caller_x } ]))
  in
  let tuple_type = Sst.Tuple [ (None, Sst.Int); (None, Sst.Int) ] in
  let tuple_source =
    definition 22 "tuple_source"
      [ parameter (bind_pattern contractless_x) ] tuple_type
      (expression 73 tuple_type
         (Sst.Tuple_value
            [
              (None, variable contractless_x);
              (None, variable contractless_x);
            ]))
  in
  let tuple_post =
    ghost 74 Ensures ~binder:(wildcard tuple_type 74) (bool 74 true)
  in
  let tuple_caller =
    definition 23 "tuple_caller"
      [ parameter (bind_pattern caller_x) ] tuple_type
      (prefix
         [ tuple_post ]
         (direct_call 75 tuple_type tuple_source.function_id
            [ Sst.Value_argument { label = None; value = variable caller_x } ]))
  in
  let program =
    lower
      [
        callee;
        caller;
        bad_caller;
        overflow_caller;
        contractless;
        nondet;
        tuple_source;
        tuple_caller;
      ]
  in
  let caller = execution "caller" program in
  check "same-unit summary did not verify caller" (outcome caller = `Verified);
  check "call precondition provenance missing"
    (has_kind
       (function
         | Vir.Call_precondition
             {
               callee = { function_name = "callee"; _ };
               precondition_ordinal = 0;
               _;
             } ->
             true
         | _ -> false)
       caller);
  check "failed callee precondition did not produce a counterexample"
    (outcome (execution "bad_caller" program) = `Counterexample);
  let overflow = execution "overflow_call" program in
  check "call argument overflow was not ordered before the precondition"
    (match overflow.obligations with
    | { Vir.kind = Arithmetic_safety _; _ } :: _ -> true
    | _ -> false);
  check "contractless body semantics leaked into its call summary"
    (outcome (execution "contractless_caller" program) = `Counterexample);
  let contractless_call = execution "contractless_caller" program in
  check "fresh call result is not model-relevant"
    (List.exists
       (fun (obligation : Vir.obligation) ->
         List.exists
           (fun symbol ->
             symbol.Vir.role = Vir.Result
             && String.equal symbol.source_name "contractless.result")
           obligation.Vir.projection_symbols)
       contractless_call.obligations);
  let tuple_caller = execution "tuple_caller" program in
  let tuple_post =
    List.find
      (fun obligation ->
        match obligation.Vir.kind with Vir.Postcondition _ -> true | _ -> false)
      tuple_caller.obligations
  in
  List.iter
    (fun name ->
      let symbol =
        List.find
          (fun symbol -> String.equal symbol.Vir.source_name name)
          tuple_post.projection_symbols
      in
      List.iter
        (fun range ->
          check ("fresh tuple component lacks a range: " ^ name)
            (List.mem range tuple_post.assumptions))
        (Vir.integer_range (Vir.Integer_symbol symbol)))
    [ "tuple_source.result.0"; "tuple_source.result.1" ];
  print_endline
    "calls: preconditions before summaries, fresh ranges, argument safety, contractless nondeterminism"

let run_match_checks () =
  let x = binding 40 "x" Sst.Int 80 in
  let zero_pattern =
    Sst.
      {
        pattern_desc = Int_pattern Z.zero;
        typ = Int;
        span = span 81;
      }
  in
  let positive = binding 41 "positive" Sst.Int 82 in
  let positive_pattern = bind_pattern positive in
  let guarded_case =
    Sst.
      {
        case_pattern = positive_pattern;
        case_guard =
          Some
            (compare 82 Sst.Greater_than
               (arithmetic 82 Sst.Add [ variable positive; int 82 1 ])
               (int 82 1));
        case_body = int 82 1;
        case_span = span 82;
      }
  in
  let fallback =
    Sst.
      {
        case_pattern = wildcard Sst.Int 83;
        case_guard = None;
        case_body = int 83 2;
        case_span = span 83;
      }
  in
  let match_body =
    expression 80 Sst.Int
      (Sst.Match
         ( variable x,
           [
             {
               Sst.case_pattern = zero_pattern;
               case_guard = None;
               case_body = int 81 0;
               case_span = span 81;
             };
             guarded_case;
             fallback;
           ] ))
  in
  let match_definition =
    definition 30 "ordered_match"
      [ parameter (bind_pattern x) ] Sst.Int match_body
  in
  let flag = binding 50 "flag" Sst.Bool 84 in
  let bool_match =
    expression 84 Sst.Int
      (Sst.Match
         ( variable flag,
           [
             {
               Sst.case_pattern =
                 {
                   pattern_desc = Bool_pattern true;
                   typ = Bool;
                   span = span 84;
                 };
               case_guard = None;
               case_body = int 84 1;
               case_span = span 84;
             };
             {
               Sst.case_pattern =
                 {
                   pattern_desc = Bool_pattern false;
                   typ = Bool;
                   span = span 85;
                 };
               case_guard = None;
               case_body = int 85 0;
               case_span = span 85;
             };
           ] ))
  in
  let bool_definition =
    definition 31 "bool_match"
      [ parameter (bind_pattern flag) ] Sst.Int bool_match
  in
  let tuple_type = Sst.Tuple [ (None, Sst.Int); (None, Sst.Bool) ] in
  let pair = binding 60 "pair" tuple_type 86 in
  let tuple_pattern =
    Sst.
      {
        pattern_desc =
          Tuple_pattern
            [
              ( None,
                {
                  pattern_desc = Int_pattern Z.zero;
                  typ = Int;
                  span = span 86;
                } );
              ( None,
                {
                  pattern_desc = Bool_pattern true;
                  typ = Bool;
                  span = span 86;
                } );
            ];
        typ = tuple_type;
        span = span 86;
      }
  in
  let tuple_match =
    expression 86 Sst.Int
      (Sst.Match
         ( variable pair,
           [
             {
               Sst.case_pattern = tuple_pattern;
               case_guard = None;
               case_body = int 86 1;
               case_span = span 86;
             };
             {
               Sst.case_pattern = wildcard tuple_type 87;
               case_guard = Some (bool 87 true);
               case_body = int 87 0;
               case_span = span 87;
             };
           ] ))
  in
  let tuple_definition =
    definition 32 "tuple_match"
      [ parameter (bind_pattern pair) ] Sst.Int tuple_match
  in
  let program =
    lower [ match_definition; bool_definition; tuple_definition ]
  in
  let ordered = execution "ordered_match" program in
  if
    not
      (has_kind (function Vir.Arithmetic_safety _ -> true | _ -> false) ordered)
  then fail "guard runtime arithmetic did not emit safety obligations:\n%s"
      (Vir.to_string program);
  check "ordered scalar match did not retain all reachable cases"
    (List.length ordered.exits = 3);
  check "later match cases lack first-match exclusion"
    (List.exists
       (fun exit ->
         List.exists
           (fun term ->
             String.contains (Vir.boolean_term_to_string term) '0')
           exit.Vir.path_condition)
       ordered.exits);
  check "Boolean match did not retain two ordered cases"
    (List.length (execution "bool_match" program).exits = 2);
  check "tuple match did not retain two ordered cases"
    (List.length (execution "tuple_match" program).exits = 2);
  print_endline
    "matches: ordered integer/Boolean/tuple cases, scoped guards, and runtime guard safety"

let run_error_checks () =
  let missing =
    definition 40 "missing" [] Sst.Int
      (direct_call 90 Sst.Int
         { Sst.function_index = 999; function_name = "absent" }
         [])
  in
  (match Symbolic_executor.lower_function missing with
  | Error { unsupported = Symbolic_executor.Malformed_sst _; _ } ->
      ()
  | Error error -> fail "wrong missing-summary error: %s"
                     (Symbolic_executor.error_to_string error)
  | Ok _ -> fail "missing summary was accepted");
  let recursive_id =
    Sst.{ function_index = 41; function_name = "recursive" }
  in
  let recursive_call =
    expression 91 Sst.Int
      (Sst.Direct_call
         {
           call_form = Sst.Exec_call;
           callee = recursive_id;
           arguments = [];
           recursive = true;
           type_arguments = [];
         })
  in
  let recursive =
    Sst_normalize.checked_exec_raw ~function_id:recursive_id ~recursive:true
      ~parameters:[] ~contracts:Sst.empty_contracts ~body:recursive_call
      ~result_type:Sst.Int ~returns_unique_parameter:None ~span:(span 91)
  in
  (match
     Symbolic_executor.lower_program
       Sst.
         {
           policy = Default_linear_z3;
           parametric_adts = [];
           types = [];
           functions = [ recursive ];
         }
   with
  | Error { unsupported = Symbolic_executor.Missing_decreases; _ } ->
      ()
  | Error error -> fail "wrong recursion error: %s"
                     (Symbolic_executor.error_to_string error)
  | Ok _ -> fail "recursive function without decreases was accepted");
  print_endline "errors: missing summaries and missing decreases are explicit"

let load filename =
  match Typedtree_lowering.lower_file filename with
  | Error diagnostic ->
      fail "%s at %s" diagnostic.Diagnostic.code diagnostic.span.file
  | Ok sst -> (
      match Symbolic_executor.lower_program sst with
      | Ok vir -> vir
      | Error error -> fail "%s" (Symbolic_executor.error_to_string error))

let solve_program program =
  List.iter
    (fun execution ->
      let result =
        match outcome execution with
        | `Verified | `No_obligations -> "verified"
        | `Counterexample -> "counterexample"
        | `Inconclusive -> "inconclusive"
      in
      Printf.printf "%s: %s (%d obligations, %d exits)\n"
        execution.Vir.function_ref.function_name result
        (List.length execution.obligations)
        (List.length execution.exits))
    program.Vir.functions

let run_unit_checks () =
  run_contract_checks ();
  run_call_checks ();
  run_match_checks ();
  run_error_checks ()

let () =
  match Array.to_list Sys.argv with
  | [ _; "unit" ] -> run_unit_checks ()
  | [ _; "dump"; filename ] -> print_string (Vir.to_string (load filename))
  | [ _; "solve"; filename ] -> solve_program (load filename)
  | _ -> fail "usage: contracts_and_calls_tool (unit|dump FILE.cmt|solve FILE.cmt)"
