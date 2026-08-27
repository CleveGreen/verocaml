let () = ignore Scoped_broadcasts_prerequisites.ready

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

type input = {
  implementation : Cmt_input.implementation;
  program : Sst.program;
  validated : Sst_validation.validated_program;
  invariants : Type_invariant.environment;
}

let input filename =
  let implementation = load filename in
  let program =
    match Typedtree_lowering.lower implementation with
    | Ok program -> program
    | Error diagnostic ->
        fail "%s: %s" diagnostic.Diagnostic.code diagnostic.message
  in
  let validated =
    match Sst_validation.validate program with
    | Ok validated -> validated
    | Error error -> fail "%s" (Sst_validation.error_to_string error)
  in
  let invariants =
    match Type_invariant.authenticate validated with
    | Ok invariants -> invariants
    | Error error -> fail "%s" (Type_invariant.error_to_string error)
  in
  { implementation; program; validated; invariants }

let policy () =
  match Solver_policy_private.create ~timeout_ms:5_000 ~rlimit:100_000 with
  | Ok policy -> policy
  | Error error -> fail "%s" (Solver_policy_private.error_to_string error)

let status_name = function
  | Verification_pipeline.Verified -> "verified"
  | Counterexample -> "counterexample"
  | Inconclusive -> "inconclusive"
  | Incomplete_source -> "incomplete"

let pipeline_error = function
  | Verification_pipeline.Engine_error error ->
      Symbolic_executor_private.error_to_string error
  | Solve_error message -> message
  | Setup_error (Internal_setup_error message) -> message
  | Setup_error (Solver_configuration_error error) ->
      Solver_backend.error_to_string error

let span_name span =
  Printf.sprintf "%s:%d:%d-%d:%d" (Filename.basename span.Diagnostic.file)
    span.start_pos.line span.start_pos.column span.end_pos.line
    span.end_pos.column

let inspect_logic obligation =
  let requires = [ Logic_ir.Named_sorts; Logic_ir.Algebraic_datatypes ] in
  let translation =
    match Vir_logic_ir_translation_private.translate ~requires obligation with
    | Ok translation -> translation
    | Error message -> fail "Logic translation failed: %s" message
  in
  let query = Vir_logic_ir_translation_private.query translation in
  let quantified =
    Logic_ir.View.assertions query
    |> List.fold_left
         (fun count term ->
           match Logic_ir.View.term_node term with
           | Logic_ir.View.Forall_term quantifier ->
               let binders =
                 Logic_ir.View.user_quantifier_binders quantifier
               in
               if
                 binders = []
                 || Option.is_none
                      (Logic_ir.View.user_quantifier_trigger quantifier)
               then fail "Logic IR broadcast quantifier lost binders/trigger";
               count + 1
           | _ -> count)
         0
  in
  let portable =
    match Z3_bridge.detach_vir ~requires obligation with
    | Ok _ -> 1
    | Error error ->
        fail "detached broadcast job failed: %s"
          (Z3_bridge.error_to_string error)
  in
  (quantified, portable)

type observation = {
  function_name : string;
  obligation : Vir.obligation;
  report : Broadcast_vc_private.report option;
}

let inspect_request observations
    (request : Verification_pipeline.solve_request) =
  List.iter
    (fun obligation ->
      observations :=
        {
          function_name =
            request.definition.Sst.function_id.function_name;
          obligation;
          report = Broadcast_vc_private.report obligation;
        }
        :: !observations)
    request.execution.Vir.obligations

let run filename =
  let input = input filename in
  let solver_policy = policy () in
  let preflight = ref None in
  let observations = ref [] in
  let run_preflight () =
    match
      Verification_solver_private.preflight ~solver_policy input.program
    with
    | Error error -> Error error
    | Ok prepared ->
        preflight := Some prepared;
        Ok (Verification_solver_private.termination_obligations prepared)
  in
  let configure_solver () =
    match !preflight with
    | None ->
        Error
          (Verification_pipeline.Internal_setup_error
             "broadcast test lost solver preflight")
    | Some prepared -> (
        match Verification_solver_private.configure ~solver_policy prepared with
        | Error _ as error -> error
        | Ok solve ->
            Ok
              (fun request ->
                inspect_request observations request;
                solve request))
  in
  let proof_entry_activations () =
    match !preflight with
    | None -> Fun.const []
    | Some prepared ->
        Verification_solver_private.proof_entry_activations prepared
  in
  let report =
    match
      Verification_pipeline.run_validated ~imports:None
        ~implementation:input.implementation ~program:input.program
        ~validated:input.validated ~invariants:input.invariants
        ~preflight:run_preflight ~proof_entry_activations ~configure_solver
        ~on_result:ignore
    with
    | Ok report -> report
    | Error message -> fail "%s" message
  in
  (input, report, List.rev !observations)

let structural filename =
  Z3_bridge.reset_counters ();
  Solver_backend.For_testing.reset_solver_creation_count ();
  let _input, report, observations = run filename in
  let completion =
    match report.Verification_pipeline.outcome with
    | Ok completion -> completion
    | Error error ->
        fail "focused positive failed in the production pipeline: %s"
          (pipeline_error error)
  in
  Printf.printf "status=%s functions=%d obligations=%d destroyed=%b\n"
    (status_name completion.status) completion.functions completion.obligations
    report.session_destroyed;
  List.iter
    (fun observation ->
      match observation.report with
      | None ->
          Printf.printf "vc function=%s index=%d broadcasts=absent\n"
            observation.function_name
            observation.obligation.Vir.obligation_index
      | Some report ->
          Printf.printf
            "vc function=%s index=%d active=%d trusted-declarations=%d \
             trusted-uses=%d inserted=%d\n"
            observation.function_name
            observation.obligation.Vir.obligation_index
            report.active_declarations report.trusted_broadcast_declarations
            report.trusted_broadcast_uses (List.length report.inserted);
          List.iter
            (fun (inserted : Broadcast_vc_private.inserted) ->
              let quantifier =
                match
                  List.find_map
                    (function
                      | Vir.Forall_term quantifier
                        when
                          String.equal inserted.qid
                            (Logic_quantifier_private.vector_qid
                               quantifier.boolean_quantifier_schema) ->
                          Some quantifier
                      | _ -> None)
                    observation.obligation.Vir.assumptions
                with
                | Some quantifier -> quantifier
                | None -> fail "reported broadcast quantifier is absent"
              in
              let binders =
                List.length quantifier.Vir.boolean_quantifier_binders
              in
              let logic, portable = inspect_logic observation.obligation in
              Printf.printf
                "insert id=%s vector=[%s] binders=%d qid=%s skid=%s ordinal=%d \
                 trusted=%b paths=%s witness=%s logic=%d portable=%d\n"
                inserted.broadcast_id
                (String.concat ","
                   (List.map Parametric_type.to_string inserted.type_vector))
                binders inserted.qid inserted.skid inserted.insertion_ordinal
                inserted.trusted
                (String.concat ","
                   (List.map (String.concat "/") inserted.selecting_paths))
                (Option.fold ~none:"none" ~some:span_name
                   inserted.witness_span)
                logic portable)
            report.inserted)
    observations;
  let counters = Z3_bridge.counters () in
  Printf.printf
    "resources backend=%d contexts=%d solvers=%d resets=%d cleaned=%d live=%d\n"
    (Solver_backend.For_testing.solver_creation_count ())
    counters.contexts_created counters.solvers_created counters.solver_resets
    counters.contexts_cleaned counters.contexts_live

let semantic filename =
  let _, report, _ = run filename in
  match report.Verification_pipeline.outcome with
  | Ok completion ->
      Printf.printf "status=%s functions=%d obligations=%d\n"
        (status_name completion.status) completion.functions
        completion.obligations
  | Error _ -> fail "semantic fixture failed before solver completion"

let diagnostic filename =
  Solver_backend.For_testing.reset_solver_creation_count ();
  Z3_bridge.reset_counters ();
  match Cmt_input.load filename with
  | Error diagnostic ->
      Printf.printf "rejected=loader code=%s solver=0 z3=0/0\n"
        diagnostic.Diagnostic.code
  | Ok implementation -> (
      match Typedtree_lowering.lower implementation with
      | Ok _ -> fail "authentication fixture unexpectedly lowered"
      | Error diagnostic ->
          let counters = Z3_bridge.counters () in
          Printf.printf "rejected=adapter code=%s solver=%d z3=%d/%d detail=%s\n"
            diagnostic.Diagnostic.code
            (Solver_backend.For_testing.solver_creation_count ())
            counters.contexts_created counters.solvers_created
            diagnostic.message)

let engine_negative filename =
  Solver_backend.For_testing.reset_solver_creation_count ();
  Z3_bridge.reset_counters ();
  let _, report, _ = run filename in
  let disposition =
    match report.Verification_pipeline.outcome with
    | Error (Verification_pipeline.Engine_error error) ->
        "engine:" ^ Symbolic_executor_private.error_to_string error
    | Error error -> "other:" ^ pipeline_error error
    | Ok completion -> "status:" ^ status_name completion.status
  in
  let counters = Z3_bridge.counters () in
  Printf.printf "result=%s solver=%d z3=%d/%d live=%d\n" disposition
    (Solver_backend.For_testing.solver_creation_count ())
    counters.contexts_created counters.solvers_created counters.contexts_live

let wrong_artifact target donor =
  Solver_backend.For_testing.reset_solver_creation_count ();
  Z3_bridge.reset_counters ();
  let target = load target and donor = load donor in
  let proof_capture_artifact =
    Typedtree_adapter_private.Public.proof_capture_artifact donor
  in
  let result =
    Typedtree_adapter_private.Public.lower_with_capture_artifact
      ~proof_capture_artifact
      ~compilation_identity:
        (Callback_certificate_private.cmt_compilation_identity target)
      ~source_file:target.Cmt_input.source_file ~imports:target.imports
      target.structure
  in
  (match result with
  | Ok _ -> fail "foreign broadcast artifact unexpectedly authenticated"
  | Error diagnostic ->
      let counters = Z3_bridge.counters () in
      Printf.printf "wrong-artifact code=%s solver=%d z3=%d/%d\n"
        diagnostic.Diagnostic.code
        (Solver_backend.For_testing.solver_creation_count ())
        counters.contexts_created counters.solvers_created)

let vector_unit () =
  let span = Diagnostic.file_span "vector-unit.ml" in
  let metadata owner kind index typ =
    Logic_quantifier_private.create ~kind ~owner ~binder_index:index
      ~binder_type:typ ~span
  in
  let first = metadata "broadcast:test" Forall (-1) Parametric_type.Int
  and second = metadata "broadcast:test" Forall (-2) Parametric_type.Bool in
  let rejected = ref 0 in
  let reject = function
    | Error _ -> incr rejected
    | Ok _ -> fail "invalid quantifier vector unexpectedly passed"
  in
  reject (Logic_quantifier_private.vector []);
  reject (Logic_quantifier_private.vector [ first; first ]);
  reject
    (Logic_quantifier_private.vector
       [ first; metadata "broadcast:other" Forall (-2) Bool ]);
  reject
    (Logic_quantifier_private.vector
       [ first; metadata "broadcast:test" Exists (-2) Bool ]);
  let schema =
    match Logic_quantifier_private.vector [ first; second ] with
    | Ok schema -> schema
    | Error message -> fail "%s" message
  in
  reject
    (Logic_quantifier_private.validate_vector ~expected_owner:"broadcast:other"
       ~kind:Forall ~binder_indices:[ -1; -2 ] ~binder_types:[ Int; Bool ]
       schema);
  reject
    (Logic_quantifier_private.validate_vector ~kind:Forall
       ~binder_indices:[ -2; -1 ] ~binder_types:[ Bool; Int ] schema);
  reject
    (Logic_quantifier_private.validate_vector ~kind:Forall
       ~binder_indices:[ -1; -2 ] ~binder_types:[ Bool; Int ] schema);
  let symbol index name sort =
    Vir.
      {
        symbol_id = index;
        source_name = name;
        sort;
        role = Local;
        span;
      }
  in
  let left = symbol (-1) "left" Vir.Integer
  and right = symbol (-2) "right" Vir.Boolean in
  let callee = Sst.{ function_index = 90; function_name = "trigger" } in
  let argument value = Vir.Recursive_integer_argument value in
  let trigger arguments =
    Vir.Boolean_specification_application
      { callee; type_arguments = []; arguments; span }
  in
  let body = Vir.Boolean_constant true in
  reject
    (Vir.make_boolean_quantifier
       ~sort_of_type:(function
         | Parametric_type.Int -> Ok Vir.Integer
         | Bool -> Ok Vir.Boolean
         | _ -> Error "unsupported")
       ~schema ~binders:[ left; right ] ~body
       ~trigger:
         (Some
            (Vir.Boolean_application
               (trigger [ argument (Vir.Integer_symbol left) ]))));
  let complete_trigger =
    Vir.Boolean_specification_application
      {
        callee;
        type_arguments = [];
        arguments =
          [
            Vir.Recursive_integer_argument (Vir.Integer_symbol left);
            Vir.Recursive_boolean_argument (Vir.Boolean_symbol right);
          ];
        span;
      }
  in
  let accepted =
    Vir.make_boolean_quantifier
      ~sort_of_type:(function
        | Parametric_type.Int -> Ok Vir.Integer
        | Bool -> Ok Vir.Boolean
        | _ -> Error "unsupported")
      ~schema ~binders:[ left; right ] ~body
      ~trigger:(Some (Vir.Boolean_application complete_trigger))
  in
  (match accepted with
  | Ok quantifier ->
      if List.length quantifier.Vir.boolean_quantifier_binders <> 2 then
        fail "multi-binder VIR quantifier lost a binder"
  | Error message -> fail "%s" message);
  Printf.printf "vector rejects=%d accepted=1 binders=2 trigger=1\n" !rejected

let logic_result = function
  | Ok value -> value
  | Error error -> fail "%s" (Logic_ir.error_to_string error)

let find_substring text needle offset =
  let rec search index =
    if index + String.length needle > String.length text then None
    else if String.sub text index (String.length needle) = needle then Some index
    else search (index + 1)
  in
  search offset

let count_substring text needle =
  let rec loop count offset =
    match find_substring text needle offset with
    | None -> count
    | Some index -> loop (count + 1) (index + String.length needle)
  in
  loop 0 0

let balanced (counters : Z3_bridge.counters) =
  counters.capability_resolutions = 1
  && counters.translations = 1
  && counters.contexts_created = 1
  && counters.solvers_created = 1
  && counters.solver_resets = 1
  && counters.contexts_cleaned = 1
  && counters.contexts_live = 0
  && counters.maximum_contexts_live = 1
  && counters.selected_logics = [ "AUFLIA" ]

let shape_query arity =
  let span = Diagnostic.file_span (Printf.sprintf "native-shape-%d.ml" arity) in
  let builder = Logic_ir.create () in
  let specifications =
    if arity = 2 then [ ("left", Logic_ir.Int); ("flag", Bool) ]
    else [ ("left", Logic_ir.Int); ("flag", Bool); ("right", Int) ]
  in
  let binders =
    List.map
      (fun (name, sort) -> logic_result (Logic_ir.bind builder ~name ~sort ~span))
      specifications
  in
  let trigger_head =
    logic_result
      (Logic_ir.declare_function builder
         ~name:(Printf.sprintf "broadcast_shape_%d" arity)
         ~domain:(List.map snd specifications) ~range:Bool ~span)
  in
  let trigger =
    logic_result
      (Logic_ir.apply ~span trigger_head (List.map Logic_ir.bound binders))
  in
  let quantified =
    logic_result
      (Logic_ir.forall_term builder ~binders ~body:trigger ~trigger
         ~qid:(Printf.sprintf "vero.broadcast.shape.%d" arity)
         ~skid:(Printf.sprintf "vero.broadcast.shape.%d.skolem" arity)
         ~span)
  in
  let query =
    logic_result
      (Logic_ir.query builder ~axioms:[] ~assertions:[ quantified ]
         ~requires:
           [
             Logic_ir.Uninterpreted_functions;
             Quantifiers;
             Explicit_patterns;
             Quantifier_ids;
           ]
         ~span)
  in
  (query, binders)

let backend_shape () =
  List.iter
    (fun arity ->
      let query, binders = shape_query arity in
      let config : Z3_bridge.config = { timeout_ms = 5_000; model = false } in
      Z3_bridge.reset_counters ();
      let _, rendered =
        match Z3_bridge.render_query config query with
        | Ok rendered -> rendered
        | Error error -> fail "%s" (Z3_bridge.error_to_string error)
      in
      let render_counters = Z3_bridge.counters () in
      let expected_names =
        List.map
          (fun binder ->
            Printf.sprintf "verocaml_b%d_%s"
              (Logic_ir.View.binder_index binder)
              (Logic_ir.View.binder_name binder))
          binders
      in
      let rec ordered offset = function
        | [] -> true
        | name :: rest -> (
            match find_substring rendered name offset with
            | None -> false
            | Some index -> ordered (index + String.length name) rest)
      in
      if
        count_substring rendered "(forall" <> 1
        || count_substring rendered ":pattern" <> 1
        || not (ordered 0 expected_names)
        || not (balanced render_counters)
      then fail "direct native quantifier shape changed for arity %d" arity;
      Z3_bridge.reset_counters ();
      let direct =
        Z3_bridge.solve_query_local ~controlled:Z3_bridge.Force_unknown
          ~rlimit:100_000 config query
      in
      let detached =
        Z3_bridge.solve_detached_query_local
          ~controlled:Z3_bridge.Force_unknown ~timeout_ms:5_000 ~rlimit:100_000
          ~model:false (Z3_bridge.detach_query query)
      in
      let direct_unknown =
        match direct.result with
        | Ok (Z3_bridge.Inconclusive (Backend_unknown "controlled unknown")) ->
            true
        | Ok _ | Error _ -> false
      and detached_unknown =
        match detached.detached_result with
        | Ok
            (Z3_bridge.Detached_inconclusive
              (Backend_unknown "controlled unknown")) ->
            true
        | Ok _ | Error _ -> false
      in
      let global = Z3_bridge.counters () in
      if
        (not direct_unknown)
        || not detached_unknown
        || not (balanced direct.telemetry)
        || not (balanced detached.detached_telemetry)
        || global.contexts_created <> 0
        || global.contexts_live <> 0
      then fail "direct/detached quantifier reconstruction changed";
      Printf.printf
        "native arity=%d quantifiers=1 binders=[%s] patterns=1 terms=1 \
         trigger=application direct=true detached=true parity=true \
         lifecycle=1/1/1/1/0\n"
        arity (String.concat "," expected_names))
    [ 2; 3 ]

let () =
  (* Keep the focused command surface bounded: migration-specific comparisons
     are assembled from structural and SST output by the Cram oracle rather
     than by adding a second semantic observer here.
     Structural output remains the sole focused observer. *)
  match Array.to_list Sys.argv with
  | [ _; "structural"; filename ] -> structural filename
  | [ _; "semantic"; filename ] -> semantic filename
  | [ _; "diagnostic"; filename ] -> diagnostic filename
  | [ _; "engine-negative"; filename ] -> engine_negative filename
  | [ _; "wrong-artifact"; target; donor ] -> wrong_artifact target donor
  | [ _; "vector-unit" ] -> vector_unit ()
  | [ _; "backend-shape" ] -> backend_shape ()
  | _ ->
      fail
        "usage: scoped_broadcasts_tool \
         (structural|semantic|diagnostic|engine-negative|wrong-artifact|vector-unit|backend-shape) \
         [CMT]"
