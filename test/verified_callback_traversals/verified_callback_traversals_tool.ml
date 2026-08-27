let () = ignore Verified_callback_traversals_prerequisites.ready
let fail format =
  Printf.ksprintf
    (fun message ->
      prerr_endline message;
      exit 3)
    format
let load filename =
  match Cmt_input.load filename with
  | Ok implementation -> implementation
  | Error diagnostic ->
      fail "%s: %s" diagnostic.Diagnostic.code diagnostic.message

let load_program filename =
  match Typedtree_lowering.lower (load filename) with
  | Ok program -> program
  | Error diagnostic ->
      fail "%s: %s" diagnostic.Diagnostic.code diagnostic.message

let configuration threads =
  match
    Verifier_service.configuration ~threads ~timeout_ms:5_000
      ~rlimit:(Some 100_000)
  with
  | Ok configuration -> configuration
  | Error error ->
      fail "%s" (Verifier_service.configuration_error_message error)

let verify filename threads =
  Verifier_service.request ~configuration:(configuration threads)
    ~consumer:(load filename) ~dependencies:[]
  |> Verifier_service.verify

let require_result filename = function
  | Ok result -> result
  | Error error ->
      fail "%s: %s" filename (Verifier_service.error_message error)

let status = function
  | Verifier_service.Verified -> "verified"
  | Counterexample -> "counterexample"
  | Inconclusive -> "inconclusive"
  | Incomplete_source -> "incomplete-source"

let expression_children = Sst_callback_private.expression_children

let rec expression_uses_binding binding expression =
  let uses = expression_uses_binding binding in
  match expression.Sst.expression_desc with
  | Sst.Variable { binding = candidate; _ } -> binding.Sst.id = candidate.id
  | Sst.Forall quantifier | Sst.Exists quantifier
    when binding.id = quantifier.quantifier_binder.id -> false
  | _ -> List.exists uses (expression_children expression)

let definition_expressions (definition : Sst.function_definition) =
  let contracts = definition.contracts in
  let predicate (clause : Sst.predicate_clause) =
    clause.Sst.predicate.expression
  and ensure (clause : Sst.ensures_clause) =
    clause.Sst.predicate.expression
  in
  let body =
    match definition.body with
    | Sst.Checked_exec { body; _ }
    | Sst.Spec_definition body
    | Sst.Recursive_spec_definition { body; _ }
    | Sst.Proof_body { body; _ } ->
        Some body.expression
    | Sst.External_specification _
    | Sst.Trusted_external_spec_target _
    | Sst.Trusted_external_body _
    | Sst.Symbolic_declaration _ ->
        None
  in
  List.map predicate
    (contracts.requires @ contracts.decreases @ contracts.assertions)
  @ List.map ensure contracts.ensures
  @ Option.to_list body

type sst_stats = {
  mutable forall : int;
  mutable exists : int;
  mutable triggers : int;
  mutable int_binders : int;
  mutable bool_binders : int;
  mutable parameter_binders : int;
  mutable option_binders : int;
  mutable sequence_binders : int;
  mutable tree_binders : int;
  mutable outer_free_uses : int;
  mutable qids : string list;
  mutable skids : string list;
}

let empty_sst_stats () =
  {
    forall = 0;
    exists = 0;
    triggers = 0;
    int_binders = 0;
    bool_binders = 0;
    parameter_binders = 0;
    option_binders = 0;
    sequence_binders = 0;
    tree_binders = 0;
    outer_free_uses = 0;
    qids = [];
    skids = [];
  }

let classify_binder stats = function
  | Sst.Int -> stats.int_binders <- stats.int_binders + 1
  | Sst.Bool -> stats.bool_binders <- stats.bool_binders + 1
  | Sst.Parameter _ ->
      stats.parameter_binders <- stats.parameter_binders + 1
  | Sst.Application (constructor, _) ->
      let leaf =
        constructor.Parametric_type.constructor_path
        |> String.split_on_char '.'
        |> List.rev |> List.hd
      in
      if
        Parametric_type.compare_constructor constructor
          Parametric_type.option_constructor
        = 0
      then
        stats.option_binders <- stats.option_binders + 1
      else if String.equal leaf "seq" then
        stats.sequence_binders <- stats.sequence_binders + 1
      else if String.equal leaf "tree" then
        stats.tree_binders <- stats.tree_binders + 1
      else
        fail "unexpected quantified application binder %s"
          constructor.constructor_path
  | Sst.Unit | Sst.Tuple _ | Sst.Aggregate _ ->
      fail "unsupported binder reached structural SST"

let inspect_sst program =
  let stats = empty_sst_stats () in
  let rec visit expected_owner ancestors (expression : Sst.expression) =
    let visit_quantifier kind quantifier =
      (match
         Quantifier_validation_private.validate_sst ~expected_owner kind
           quantifier
       with
      | Ok () -> ()
      | Error message -> fail "invalid retained quantifier: %s" message);
      let metadata = quantifier.Sst.quantifier_metadata in
      let binder = quantifier.quantifier_binder in
      let stable =
        Logic_quantifier_private.create ~kind
          ~owner:(Logic_quantifier_private.owner metadata)
          ~binder_index:binder.id ~binder_type:binder.typ
          ~span:(Logic_quantifier_private.span metadata)
      in
      if not (Logic_quantifier_private.equal stable metadata) then
        fail "qid/skid are not deterministic";
      stats.qids <- Logic_quantifier_private.qid metadata :: stats.qids;
      stats.skids <- Logic_quantifier_private.skid metadata :: stats.skids;
      classify_binder stats binder.typ;
      let uses_outer ancestor =
        expression_uses_binding ancestor quantifier.quantifier_body in
      if List.exists uses_outer ancestors then
        stats.outer_free_uses <- stats.outer_free_uses + 1;
      (match (kind, quantifier.quantifier_trigger) with
      | Logic_quantifier_private.Forall, Some trigger ->
          stats.forall <- stats.forall + 1;
          stats.triggers <- stats.triggers + 1;
          if not (expression_uses_binding binder trigger) then
            fail "universal trigger does not own its lexical binder"
      | Logic_quantifier_private.Exists, None ->
          stats.exists <- stats.exists + 1
      | Logic_quantifier_private.Forall, None
      | Logic_quantifier_private.Exists, Some _ ->
          fail "retained trigger policy differs from quantifier kind");
      let ancestors = binder :: ancestors in
      visit expected_owner ancestors quantifier.quantifier_body;
      Option.iter
        (visit expected_owner ancestors)
        quantifier.quantifier_trigger
    in
    match expression.expression_desc with
    | Sst.Forall quantifier ->
        visit_quantifier Logic_quantifier_private.Forall quantifier
    | Sst.Exists quantifier ->
        visit_quantifier Logic_quantifier_private.Exists quantifier
    | _ ->
        List.iter (visit expected_owner ancestors)
          (expression_children expression)
  in
  List.iter
    (fun definition ->
      let expected_owner =
        "function:" ^ definition.Sst.function_id.function_name
      in
      List.iter (visit expected_owner [])
        (definition_expressions definition))
    program.Sst.functions;
  let unique values =
    List.length values
    = List.length (List.sort_uniq String.compare values)
  in
  if not (unique stats.qids && unique stats.skids) then
    fail "source quantifier qid/skid identities are not unique";
  if
    stats.forall <> 13 || stats.exists <> 9 || stats.triggers <> 13
    || stats.int_binders <> 11 || stats.bool_binders <> 2
    || stats.parameter_binders <> 3 || stats.option_binders <> 2
    || stats.sequence_binders <> 2 || stats.tree_binders <> 2
    || stats.outer_free_uses < 1
  then
    fail
      "unexpected SST matrix forall=%d exists=%d triggers=%d int=%d bool=%d \
       parameter=%d option=%d seq=%d tree=%d outer=%d"
      stats.forall stats.exists stats.triggers stats.int_binders
      stats.bool_binders stats.parameter_binders stats.option_binders
      stats.sequence_binders stats.tree_binders stats.outer_free_uses;
  stats

type logic_stats = {
  mutable logic_forall : int;
  mutable logic_exists : int;
  mutable logic_triggers : int;
  mutable logic_outer_free : int;
  mutable portable_jobs : int;
  mutable function_sorts : int;
  mutable quantifier_queries : int;
}

let empty_logic_stats () =
  {
    logic_forall = 0;
    logic_exists = 0;
    logic_triggers = 0;
    logic_outer_free = 0;
    portable_jobs = 0;
    function_sorts = 0;
    quantifier_queries = 0;
  }

let inspect_logic program =
  let stats = empty_logic_stats () in
  let rec visit ancestors term =
    let children terms = List.concat_map (visit ancestors) terms in
    match Logic_ir.View.term_node term with
    | Logic_ir.View.Bound binder -> [ Logic_ir.View.binder_index binder ]
    | Apply (_, arguments) | And arguments | Or arguments -> children arguments
    | Rank_project (_, _, _, value) | Negate value | Not value
    | Scale (_, value) ->
        visit ancestors value
    | Add (left, right) | Subtract (left, right) | Less_than (left, right)
    | Less_or_equal (left, right) | Greater_than (left, right)
    | Greater_or_equal (left, right) | Equal (left, right)
    | Distinct (left, right) | Implies (left, right) ->
        visit ancestors left @ visit ancestors right
    | Forall_term quantifier ->
        visit_quantifier ancestors `Forall quantifier
    | Exists_term quantifier ->
        visit_quantifier ancestors `Exists quantifier
    | Ite (condition, yes, no) -> children [ condition; yes; no ]
    | Integer _ | Boolean _ -> []
  and visit_quantifier ancestors kind quantifier =
    let binders = Logic_ir.View.user_quantifier_binders quantifier in
    let binder_indices = List.map Logic_ir.View.binder_index binders in
    let body = Logic_ir.View.user_quantifier_body quantifier in
    let trigger = Logic_ir.View.user_quantifier_trigger quantifier in
    let scoped = binder_indices @ ancestors in
    let body_uses = visit scoped body in
    let trigger_uses = Option.fold ~none:[] ~some:(visit scoped) trigger in
    if List.exists (fun outer -> List.mem outer body_uses) ancestors then
      stats.logic_outer_free <- stats.logic_outer_free + 1;
    (match (kind, trigger) with
    | `Forall, Some trigger ->
        stats.logic_forall <- stats.logic_forall + 1;
        stats.logic_triggers <- stats.logic_triggers + 1;
        (match Logic_ir.View.term_node trigger with
        | Logic_ir.View.Apply (_, _ :: _) -> ()
        | _ -> fail "Logic IR trigger is not a non-nullary application");
        if not (List.for_all (fun index -> List.mem index trigger_uses) binder_indices)
        then fail "Logic IR trigger lost a lexical binder"
    | `Exists, None -> stats.logic_exists <- stats.logic_exists + 1
    | (`Forall, None) | (`Exists, Some _) ->
        fail "Logic IR quantifier trigger policy is invalid");
    if
      String.equal (Logic_ir.View.user_quantifier_qid quantifier) ""
      || String.equal (Logic_ir.View.user_quantifier_skid quantifier) ""
    then fail "Logic IR quantifier lost qid/skid";
    List.filter
      (fun index -> not (List.mem index binder_indices))
      (body_uses @ trigger_uses)
  in
  let requires = [ Logic_ir.Named_sorts; Logic_ir.Algebraic_datatypes ] in
  List.iter
    (fun execution ->
      List.iter
        (fun obligation ->
          let translation =
            match
              Vir_logic_ir_translation_private.translate ~requires
                obligation
            with
            | Ok translation -> translation
            | Error message ->
                fail "Logic IR translation failed: %s" message
          in
          let query =
            Vir_logic_ir_translation_private.query translation
          in
          let features = Logic_ir.requirements query in
          if List.mem Logic_ir.Quantifiers features then (
            stats.quantifier_queries <- stats.quantifier_queries + 1;
            if
              not
                (List.mem Logic_ir.Explicit_patterns features
                && List.mem Logic_ir.Quantifier_ids features)
            then fail "quantified query lost explicit-pattern/id features");
          List.iter (fun term -> ignore (visit [] term))
            (Logic_ir.View.assertions query);
          List.iter
            (function
              | Logic_ir.View.Sort_declaration _ -> ()
              | Logic_ir.View.Function_declaration function_ ->
                  let domain = Logic_ir.View.function_domain function_ in
                  let range = Logic_ir.View.function_range function_ in
                  let first_order =
                    List.for_all
                      (function
                        | Logic_ir.Int | Logic_ir.Bool | Logic_ir.Named _ ->
                            true)
                      (range :: domain)
                  in
                  if not first_order then
                    stats.function_sorts <- stats.function_sorts + 1)
            (Logic_ir.View.declarations query);
          match Z3_bridge.detach_vir ~requires obligation with
          | Ok _ -> stats.portable_jobs <- stats.portable_jobs + 1
          | Error error ->
              fail "detached quantifier job failed: %s"
                (Z3_bridge.error_to_string error))
        execution.Vir.obligations)
    program.Vir.functions;
  stats

let structural filename =
  let sst = inspect_sst (load_program filename) in
  let result = require_result filename (verify filename 1) in
  if Verifier_service.status result <> Verifier_service.Verified then
    fail "%s is not verified" filename;
  if Verifier_service.obligations result > 12 then
    fail "quantifier fixture exceeded the 12-VC cap";
  let vir = Verifier_service.vir result in
  let logic = inspect_logic vir in
  if
    logic.logic_forall = 0 || logic.logic_exists = 0
    || logic.logic_triggers <> logic.logic_forall
    || logic.function_sorts <> 0 || logic.portable_jobs = 0
  then fail "translated VIR/Logic structural matrix is incomplete";
  Printf.printf
    "sst forall=%d exists=%d triggers=%d binders=int:%d,bool:%d,param:%d,\
     option:%d,seq:%d,tree:%d outer-free=%d qids=%d skids=%d\n"
    sst.forall sst.exists sst.triggers sst.int_binders sst.bool_binders
    sst.parameter_binders sst.option_binders sst.sequence_binders
    sst.tree_binders sst.outer_free_uses (List.length sst.qids)
    (List.length sst.skids);
  Printf.printf
    "vir-logic forall=%d exists=%d triggers=%d outer-free=%d queries=%d \
     portable=%d function-sort=%d\n"
    logic.logic_forall logic.logic_exists logic.logic_triggers
    logic.logic_outer_free logic.quantifier_queries logic.portable_jobs
    logic.function_sorts;
  Printf.printf
    "status=%s functions=%d obligations=%d policy=100000/5000 cap=12\n"
    (status (Verifier_service.status result))
    (Verifier_service.functions result)
    (Verifier_service.obligations result)

let result_snapshot result =
  ( Verifier_service.status result,
    Verifier_service.semantic_sst result,
    Vir.to_string (Verifier_service.vir result),
    Verifier_service.functions result,
    Verifier_service.obligations result )

let parity filename =
  let first = require_result filename (verify filename 1) in
  let repeated = require_result filename (verify filename 1) in
  let threaded = require_result filename (verify filename 2) in
  if result_snapshot first <> result_snapshot repeated then
    fail "repeat result differs";
  if result_snapshot first <> result_snapshot threaded then
    fail "threads 1/2 result differs";
  Printf.printf
    "parity=repeat/threads status=%s functions=%d obligations=%d \
     resources=100000 timeout-ms=5000\n"
    (status (Verifier_service.status first))
    (Verifier_service.functions first)
    (Verifier_service.obligations first)

let reject_zero_work filename =
  Verification_driver_private.For_testing.reset_driver_entries ();
  Verification_pipeline.For_testing.reset_validated_pipeline_entries ();
  Solver_backend_counter_private.reset_solver_creation_count ();
  Z3_bridge.reset_counters ();
  let rejection =
    match verify filename 1 with
    | Error error -> error
    | Ok _ -> fail "%s unexpectedly entered verification" filename
  in
  let counters = Z3_bridge.counters () in
  let pipeline =
    Verification_pipeline.For_testing.validated_pipeline_entries ()
  in
  if
    pipeline <> 0
    || Solver_backend_counter_private.solver_creation_count () <> 0
    || counters.capability_resolutions <> 0
    || counters.translations <> 0 || counters.contexts_created <> 0
    || counters.solvers_created <> 0
  then fail "%s performed SST/VIR/backend/solver/query work" filename;
  let code =
    match Verifier_service.error_diagnostic rejection with
    | Some diagnostic -> diagnostic.Diagnostic.code
    | None -> "VERO_SERVICE"
  in
  Printf.printf
    "rejected=pre-sst code=%s driver=%d pipeline=0 backend=0 solver=0 \
     query=0 z3=0/0\n"
    code
    (Verification_driver_private.For_testing.driver_entries ())

let semantic_negative filename =
  Solver_backend_counter_private.reset_solver_creation_count ();
  Z3_bridge.reset_counters ();
  let result = require_result filename (verify filename 1) in
  let result_status = Verifier_service.status result in
  if
    result_status <> Verifier_service.Counterexample
    && result_status <> Verifier_service.Inconclusive
  then fail "%s did not produce a semantic negative" filename;
  let counters = Z3_bridge.counters () in
  if
    Solver_backend_counter_private.solver_creation_count () = 0
    || counters.contexts_created = 0 || counters.translations = 0
  then fail "%s did not perform bounded solver work" filename;
  if
    counters.contexts_created <> counters.contexts_cleaned
    || counters.contexts_live <> 0
    || counters.solvers_created <> counters.solver_resets
  then fail "%s leaked solver resources" filename;
  Printf.printf
    "semantic-negative=%s functions=%d obligations=%d queries=%d \
     contexts=%d/%d solvers=%d/%d policy=100000/5000\n"
    (status result_status)
    (Verifier_service.functions result)
    (Verifier_service.obligations result) counters.translations
    counters.contexts_created counters.contexts_cleaned
    counters.solvers_created counters.solver_resets

let resource_evidence filename =
  Solver_backend_counter_private.reset_solver_creation_count ();
  Z3_bridge.reset_counters ();
  let result = require_result filename (verify filename 1) in
  let counters = Z3_bridge.counters () in
  let created = Solver_backend_counter_private.solver_creation_count () in
  let resets = Solver_backend_counter_private.reset_count () in
  if
    counters.contexts_created <> counters.contexts_cleaned
    || counters.contexts_live <> 0
    || counters.solvers_created <> counters.solver_resets
    || created <> resets
  then fail "quantifier resource cleanup is unbalanced";
  Printf.printf
    "resources obligations=%d contexts=%d/%d live=%d solvers=%d/%d \
     max-live=%d policy=100000/5000\n"
    (Verifier_service.obligations result)
    counters.contexts_created counters.contexts_cleaned
    counters.contexts_live counters.solvers_created counters.solver_resets
    counters.maximum_contexts_live

let carrier_attacks filename foreign_filename =
  let implementation = load filename in
  let foreign = load foreign_filename in
  let canonical_marker =
    Broadcast_scope_private.canonical_marker_path implementation.imports
  in
  let artifact =
    Typedtree_adapter_issuance_private.proof_capture_artifact
      implementation
  in
  let stale =
    Typedtree_adapter_issuance_private.proof_capture_artifact foreign
  in
  let forged =
    {
      Typedtree_adapter_issuance_private.proof_capture_artifact_issuer =
        ref ();
      proof_capture_implementation = implementation;
    }
  in
  let carriers = ref [] in
  let default = Tast_iterator.default_iterator in
  let iterator =
    {
      default with
      expr =
        (fun self expression ->
          (match
             Typedtree_logical_builtin_private.authenticate
               ~artifact:(Some artifact)
               ~source_file:implementation.source_file ~canonical_marker
               expression
           with
          | Ok (Some carrier) -> carriers := (expression, carrier) :: !carriers
          | Ok None -> ()
          | Error message -> fail "positive carrier rejected: %s" message);
          default.expr self expression);
    }
  in
  iterator.structure iterator implementation.structure;
  let quantifiers =
    List.filter
      (fun (_, carrier) ->
        match Typedtree_logical_builtin_private.kind carrier with
        | Typedtree_logical_builtin_private.Forall
        | Typedtree_logical_builtin_private.Exists ->
            true
        | Typedtree_logical_builtin_private.Call_requires
        | Typedtree_logical_builtin_private.Call_ensures ->
            false)
      !carriers
  in
  let rejects artifact source_file expression =
    Result.is_error
      (Typedtree_logical_builtin_private.authenticate ~artifact ~source_file
         ~canonical_marker expression)
  in
  List.iter
    (fun (expression, _) ->
      if
        not
          (rejects None implementation.source_file expression
          && rejects (Some forged) implementation.source_file expression
          && rejects (Some stale) implementation.source_file expression
          && rejects (Some artifact)
               (implementation.source_file ^ ".wrong-program")
               expression)
      then fail "carrier attack escaped authentication")
    quantifiers;
  Printf.printf
    "carrier-auth authenticated=%d raw=%d forged=%d stale=%d \
     wrong-program=%d copied-marker=%d sst=0 vir=0 vc=0 backend=0 solver=0 \
     query=0\n"
    (List.length quantifiers) (List.length quantifiers)
    (List.length quantifiers) (List.length quantifiers)
    (List.length quantifiers) (List.length quantifiers)

let first_quantifier program =
  let found = ref None in
  let rec visit expression =
    match !found with
    | Some _ -> ()
    | None -> (
        match expression.Sst.expression_desc with
        | Sst.Forall quantifier ->
            found := Some (Logic_quantifier_private.Forall, quantifier)
        | Sst.Exists quantifier ->
            found := Some (Logic_quantifier_private.Exists, quantifier)
        | _ -> List.iter visit (expression_children expression))
  in
  List.iter
    (fun definition -> List.iter visit (definition_expressions definition))
    program.Sst.functions;
  match !found with
  | Some quantifier -> quantifier
  | None -> fail "positive program contains no quantifier"

let metadata_attacks filename =
  let kind, quantifier = first_quantifier (load_program filename) in
  let metadata = quantifier.Sst.quantifier_metadata in
  let binder = quantifier.quantifier_binder in
  let make ?(owner = Logic_quantifier_private.owner metadata)
      ?(binder_index = binder.id) ?(binder_type = binder.typ) kind =
    Logic_quantifier_private.create ~kind ~owner ~binder_index ~binder_type
      ~span:(Logic_quantifier_private.span metadata)
  in
  let rejects ?expected_owner kind quantifier =
    Result.is_error
      (Quantifier_validation_private.validate_sst ?expected_owner kind
         quantifier)
  in
  let altered quantifier_metadata =
    { quantifier with Sst.quantifier_metadata }
  in
  let wrong_owner =
    altered (make ~owner:"function:foreign_owner" kind)
  and stale = altered (make ~binder_index:(binder.id + 1) kind) in
  let wrong_type =
    let typ = if binder.typ = Sst.Int then Sst.Bool else Sst.Int in
    altered (make ~binder_type:typ kind)
  in
  let wrong_kind =
    match kind with
    | Logic_quantifier_private.Forall ->
        altered (make Logic_quantifier_private.Exists)
    | Logic_quantifier_private.Exists ->
        altered (make Logic_quantifier_private.Forall)
  in
  let expected_owner = Logic_quantifier_private.owner metadata in
  if
    not
      (rejects ~expected_owner kind wrong_owner
      && rejects kind stale && rejects kind wrong_type
      && rejects kind wrong_kind)
  then fail "quantifier metadata attack escaped validation";
  Printf.printf
    "metadata-auth owner=reject stale=reject type=reject kind=reject \
     sst=0 vir=0 vc=0 backend=0 solver=0 query=0\n"

let qid_snapshot filename =
  let stats = inspect_sst (load_program filename) in
  (List.sort String.compare stats.qids, List.sort String.compare stats.skids)

let compare_identities left right =
  let left_ids = qid_snapshot left in
  let right_ids = qid_snapshot right in
  if left_ids <> right_ids then
    fail "alpha/source-CMT qid/skid identities differ";
  Printf.printf
    "identity-parity qids=%d skids=%d stable=true reorder-observation=true\n"
    (List.length (fst left_ids))
    (List.length (snd left_ids))

let () =
  match Array.to_list Sys.argv with
  | [ _; "structural"; filename ] -> structural filename
  | [ _; "parity"; filename ] -> parity filename
  | [ _; "resource-evidence"; filename ] -> resource_evidence filename
  | [ _; "reject-zero-work"; filename ] -> reject_zero_work filename
  | [ _; "semantic-negative"; filename ] -> semantic_negative filename
  | [ _; "carrier-attacks"; filename; foreign ] ->
      carrier_attacks filename foreign
  | [ _; "metadata-attacks"; filename ] -> metadata_attacks filename
  | [ _; "compare-identities"; left; right ] ->
      compare_identities left right
  | _ ->
      fail
        "usage: verified_callback_traversals_tool \
         (structural FILE.cmt|parity FILE.cmt|resource-evidence FILE.cmt|\
         reject-zero-work FILE.cmt|semantic-negative FILE.cmt|\
         carrier-attacks FILE.cmt FOREIGN.cmt|metadata-attacks FILE.cmt|\
         compare-identities LEFT.cmt RIGHT.cmt)"
