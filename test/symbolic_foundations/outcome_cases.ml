open Outcome_test_support

let ( let* ) = Result.bind
let suite_path = "test/symbolic_foundations/outcome_cases.ml"

let mismatch format =
  Printf.ksprintf
    (fun message -> Error (Failure.make Failure.Expectation_mismatch message))
    format

let verified = Outcome.observation ~status:Outcome.Verified () |> Outcome.project

let require condition format =
  if condition then Ok () else mismatch "%s" format

let contains text fragment =
  let rec loop index =
    if String.length fragment = 0 then true
    else if index + String.length fragment > String.length text then false
    else if String.sub text index (String.length fragment) = fragment then true
    else loop (index + 1)
  in
  loop 0

let rec mkdir_p path =
  if path = "" || path = "." || Sys.file_exists path then ()
  else (
    mkdir_p (Filename.dirname path);
    Unix.mkdir path 0o755)

let write_file path contents =
  mkdir_p (Filename.dirname path);
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let rec files_below root =
  if not (Sys.file_exists root) then []
  else if Sys.is_directory root then
    Sys.readdir root |> Array.to_list
    |> List.concat_map (fun name -> files_below (Filename.concat root name))
  else [ root ]

let discover_artifact project_root unit_name extension =
  let expected = String.uncapitalize_ascii unit_name ^ extension in
  match
    files_below (Filename.concat project_root "_build")
    |> List.filter (fun path -> String.equal (Filename.basename path) expected)
  with
  | [ path ] -> Ok path
  | [] -> mismatch "artifact unavailable for %s%s" unit_name extension
  | _ -> mismatch "artifact is ambiguous for %s%s" unit_name extension

let load_implementation cmt cmi =
  match Cmt_input.load_with_interface ~cmt ~cmi () with
  | Ok implementation -> Ok implementation
  | Error diagnostic ->
      mismatch "artifact load failed with %s" diagnostic.Diagnostic.code

let verified_result ~threads ~unit_name ~dependencies implementation =
  let* configuration =
    match
      Verifier_service.configuration ~threads ~timeout_ms:60_000 ~rlimit:None
    with
    | Ok configuration -> Ok configuration
    | Error error ->
        mismatch "%s" (Verifier_service.configuration_error_message error)
  in
  match
    Verifier_service.verify
      (Verifier_service.request ~configuration ~consumer:implementation
         ~dependencies)
  with
  | Ok result when Verifier_service.status result = Verifier_service.Verified ->
      Ok result
  | Ok _ -> mismatch "%s did not verify" unit_name
  | Error error -> mismatch "%s" (Verifier_service.error_message error)

let verification_result ~threads ~dependencies implementation =
  let* configuration =
    match
      Verifier_service.configuration ~threads ~timeout_ms:60_000 ~rlimit:None
    with
    | Ok configuration -> Ok configuration
    | Error error ->
        mismatch "%s" (Verifier_service.configuration_error_message error)
  in
  match
    Verifier_service.verify
      (Verifier_service.request ~configuration ~consumer:implementation
         ~dependencies)
  with
  | Ok result -> Ok result
  | Error error -> mismatch "%s" (Verifier_service.error_message error)

let check_lifecycle () =
  let counters = Z3_bridge.counters () in
  let schedulers_created =
    Verification_pipeline.For_testing.scheduler_creations ()
  and schedulers_stopped = Verification_pipeline.For_testing.scheduler_stops () in
  let* () = require (counters.contexts_live = 0) "solver context remained live" in
  let* () =
    require (counters.contexts_created = counters.contexts_cleaned)
      "solver context lifecycle did not balance"
  in
  let* () =
    require (schedulers_created = schedulers_stopped)
      "thread scheduler lifecycle did not balance"
  in
  require (Function_vc_worker_private.For_testing.cleanup_failures () = 0)
    "worker cleanup reported a failure"

let reset_lifecycle () =
  Z3_bridge.reset_counters ();
  Verification_pipeline.For_testing.reset_scheduler_counts ();
  Function_vc_worker_private.For_testing.reset ()

type captured_kind = Event of string | Span of string

type captured_field_class = String_field | Int_field | Bool_field

type captured_field_value =
  | String_value of string
  | Int_value of int
  | Bool_value of bool

type captured_field = {
  name : string;
  value : captured_field_value;
}

type captured_record = {
  kind : captured_kind;
  level : Delator.Level.t;
  target : string;
  fields : captured_field list;
}

let capture_records level action =
  let records = ref [] in
  let lock = Mutex.create () in
  let record item =
    Mutex.lock lock;
    records := item :: !records;
    Mutex.unlock lock
  in
  let fields fields =
    List.map
      (fun (name, value) ->
        let value =
          match Delator.Field.view value with
          | Delator.Field.View.String value -> String_value value
          | Int value -> Int_value value
          | Bool value -> Bool_value value
          | Null | Int64 _ | Float _ | Seq _ | Map _ ->
              String_value (Delator.Field.render value)
        in
        { name; value })
      fields
    |> List.sort (fun left right -> String.compare left.name right.name)
  in
  let module Renderer = struct
    let on_new_span ~id:_ ~parent:_ ~name ~target ~level ~fields:values =
      record { kind = Span name; level; target; fields = fields values }

    let on_exit ~id:_ ~duration_ns:_ = ()

    let on_event ~span:_ ~target ~level ~msg ~fields:values =
      record { kind = Event msg; level; target; fields = fields values }
  end in
  Delator.init ();
  Delator.set_default_level level;
  Delator.Renderer.set_current (module Renderer);
  Fun.protect
    ~finally:(fun () ->
      Delator.set_default_level Delator.Info;
      Delator.Renderer.set_current Delator.Renderer.tree)
    (fun () ->
      let result = action () in
      (result, List.rev !records))

let field_names record = List.map (fun field -> field.name) record.fields

let has_fields required record =
  List.for_all (fun name -> List.mem name (field_names record)) required

let field name record =
  List.find_opt (fun field -> String.equal field.name name) record.fields

let field_string name record =
  match field name record with
  | Some { value = String_value value; _ } -> Some value
  | Some { value = Int_value _ | Bool_value _; _ } | None -> None

let field_int name record =
  match field name record with
  | Some { value = Int_value value; _ } -> Some value
  | Some { value = String_value _ | Bool_value _; _ } | None -> None

let field_bool name record =
  match field name record with
  | Some { value = Bool_value value; _ } -> Some value
  | Some { value = String_value _ | Int_value _; _ } | None -> None

let field_has_classification name classification record =
  match field name record with
  | Some { value = String_value _; _ } -> classification = String_field
  | Some { value = Int_value _; _ } -> classification = Int_field
  | Some { value = Bool_value _; _ } -> classification = Bool_field
  | None -> false

let forbidden_private_fields =
  [
    "uid";
    "raw_uid";
    "symbolic_uid";
    "value_uid";
    "compiler_uid";
    "marker";
    "marker_encoding";
    "typed_abi";
    "full_abi";
    "abi_material";
    "structural_key";
    "identity_material";
    "sst";
    "vir";
    "term";
    "private_term";
  ]

let record_has_private_field record =
  List.exists (fun name -> List.mem name forbidden_private_fields)
    (field_names record)

let records_redact secrets records =
  List.for_all
    (fun record ->
      (not (record_has_private_field record))
      && List.for_all
           (fun field ->
             match field.value with
             | String_value value ->
                 List.for_all
                   (fun secret ->
                     secret = "" || not (contains value secret))
                   secrets
             | Int_value _ | Bool_value _ -> true)
           record.fields)
    records

let text_avoids_private_verifier_jargon text =
  let text = String.lowercase_ascii text in
  List.for_all
    (fun fragment -> not (contains text fragment))
    [
      "sst";
      "vir";
      "owner";
      "ppx";
      "raw uid";
      "structural key";
      "private term";
      "injected";
      "malformed";
      "ordinal";
    ]

let rec collect_argument identities = function
  | Vir.Recursive_integer_argument term -> collect_integer identities term
  | Recursive_boolean_argument term -> collect_boolean identities term
  | Recursive_aggregate_argument term -> collect_aggregate identities term
  | Recursive_parametric_argument term -> collect_parametric identities term

and collect_arguments identities arguments =
  List.fold_left collect_argument identities arguments

and collect_application identities application =
  Symbolic_application_private.identity_digest application
  :: collect_arguments identities
       (Symbolic_application_private.arguments application)

and collect_parametric identities term =
  match term.Vir.parametric_desc with
  | Vir.Parametric_symbol _ -> identities
  | Parametric_selector (_, aggregate) -> collect_aggregate identities aggregate
  | Parametric_conditional (condition, consequent, alternative) ->
      collect_parametric
        (collect_parametric (collect_boolean identities condition) consequent)
        alternative
  | Parametric_symbolic_application application ->
      collect_application identities application

and collect_integer identities = function
  | Vir.Integer_constant _ | Integer_symbol _ -> identities
  | Integer_add (left, right)
  | Integer_subtract (left, right)
  | Integer_multiply (left, right) ->
      collect_integer (collect_integer identities left) right
  | Integer_negate value
  | Integer_multiply_constant (_, value)
  | Integer_absolute_value value ->
      collect_integer identities value
  | Integer_conditional (condition, consequent, alternative) ->
      collect_integer
        (collect_integer (collect_boolean identities condition) consequent)
        alternative
  | Integer_rank_project (_, aggregate)
  | Aggregate_tag (_, aggregate)
  | Integer_selector (_, aggregate) ->
      collect_aggregate identities aggregate
  | Integer_recursive_spec_application { arguments; _ } ->
      collect_arguments identities arguments
  | Integer_symbolic_application application ->
      collect_application identities application

and collect_aggregate identities aggregate =
  match aggregate.Vir.aggregate_desc with
  | Vir.Aggregate_symbol _ -> identities
  | Aggregate_imported_model_application { arguments; _ }
  | Aggregate_constructor { arguments; _ }
  | Aggregate_recursive_spec_application { arguments; _ } ->
      collect_arguments identities arguments
  | Aggregate_selector (_, source) -> collect_aggregate identities source
  | Aggregate_record { fields; _ } ->
      List.fold_left
        (fun identities (_, argument) -> collect_argument identities argument)
        identities fields
  | Aggregate_conditional (condition, consequent, alternative) ->
      collect_aggregate
        (collect_aggregate (collect_boolean identities condition) consequent)
        alternative
  | Aggregate_symbolic_application application ->
      collect_application identities application

and collect_boolean identities = function
  | Vir.Logical_adt_schema _ | Boolean_constant _ | Boolean_symbol _ -> identities
  | Boolean_not value -> collect_boolean identities value
  | Boolean_and (left, right)
  | Boolean_or (left, right)
  | Boolean_equal (left, right)
  | Boolean_not_equal (left, right) ->
      collect_boolean (collect_boolean identities left) right
  | Forall_term quantifier | Exists_term quantifier ->
      collect_boolean identities quantifier.boolean_quantifier_body
  | Integer_compare (_, left, right) ->
      collect_integer (collect_integer identities left) right
  | Boolean_selector (_, aggregate) -> collect_aggregate identities aggregate
  | Aggregate_equal (left, right) ->
      collect_aggregate (collect_aggregate identities left) right
  | Parametric_equal (left, right) ->
      collect_parametric (collect_parametric identities left) right
  | Boolean_invariant_application { value; _ } ->
      collect_aggregate identities value
  | Boolean_recursive_spec_application { arguments; _ }
  | Boolean_specification_application { arguments; _ } ->
      collect_arguments identities arguments
  | Boolean_symbolic_application application ->
      collect_application identities application
  | Callback_requires application ->
      collect_arguments identities application.arguments
  | Callback_ensures { application; result } ->
      collect_argument (collect_arguments identities application.arguments) result

let symbolic_identities result =
  (Verifier_service.vir result).Vir.functions
  |> List.fold_left
       (fun identities execution ->
         List.fold_left
           (fun identities (obligation : Vir.obligation) ->
             let identities =
               List.fold_left collect_boolean identities obligation.Vir.assumptions
             in
             let identities =
               List.fold_left collect_boolean identities
                 obligation.required_preceding_safety
             in
             let identities =
               List.fold_left collect_boolean identities obligation.path_condition
             in
             collect_boolean identities obligation.goal)
           identities execution.Vir.obligations)
       []
  |> List.sort_uniq String.compare

let obligation_symbolic_identities (obligation : Vir.obligation) =
  let identities =
    List.fold_left collect_boolean [] obligation.Vir.assumptions
  in
  let identities =
    List.fold_left collect_boolean identities
      obligation.required_preceding_safety
  in
  let identities =
    List.fold_left collect_boolean identities obligation.path_condition
  in
  collect_boolean identities obligation.goal |> List.sort_uniq String.compare

type pipeline_observation = {
  function_name : string;
  obligation : Vir.obligation;
  broadcast : Broadcast_vc_private.report option;
}

type inspected_pipeline = {
  program : Sst.program;
  report : Verification_pipeline.report;
  observations : pipeline_observation list;
}

let pipeline_error = function
  | Verification_pipeline.Engine_error error ->
      Symbolic_executor_private.error_to_string error
  | Solve_error message -> message
  | Setup_error (Internal_setup_error message) -> message
  | Setup_error (Solver_configuration_error error) ->
      Solver_backend.error_to_string error

let inspect_pipeline ?(dependencies = []) implementation =
  let* solver_policy =
    Solver_policy_private.create_default ~timeout_ms:60_000
    |> Result.map_error (fun error ->
           Failure.make Failure.Expectation_mismatch
             (Solver_policy_private.error_to_string error))
  in
  let diagnostic_failure diagnostic =
    Failure.make Failure.Expectation_mismatch
      (diagnostic.Diagnostic.code ^ ": " ^ diagnostic.message)
  in
  let* program, imports =
    if dependencies = [] then
      Typedtree_lowering.lower ~allow_imported_opens:true implementation
      |> Result.map (fun program -> (program, None))
      |> Result.map_error diagnostic_failure
    else
      let* environment, implementation =
        Interface_specification_loaded_private.authenticate ~external_targets:[]
          ~solver_policy ~dependencies ~consumer:implementation
        |> Result.map_error (fun error ->
               Failure.make Failure.Expectation_mismatch
                 (Interface_specification_loaded_private.error_message error))
      in
      let* imported =
        Interface_specification_environment_private
        .imported_environment_authenticated environment
        |> Result.map_error (fun error ->
               Failure.make Failure.Expectation_mismatch
                 (Interface_specification_environment_private.error_to_string
                    error))
      in
      Typedtree_lowering_private.lower ~imported implementation
      |> Result.map
           (fun lowered ->
             ( Typedtree_lowering_private.program lowered,
               Some (Typedtree_lowering_private.registration lowered) ))
      |> Result.map_error diagnostic_failure
  in
  let* validated =
    (match imports with
    | None -> Sst_validation.validate program
    | Some registration ->
        Sst_validation_private.Public.validate_with_imports registration program)
    |> Result.map_error (fun error ->
           Failure.make Failure.Expectation_mismatch
             (Sst_validation.error_to_string error))
  in
  let* invariants =
    Type_invariant.authenticate validated
    |> Result.map_error (fun error ->
           Failure.make Failure.Expectation_mismatch
             (Type_invariant.error_to_string error))
  in
  let preflight = ref None in
  let observations = ref [] in
  let run_preflight () =
    match Verification_solver_private.preflight ~solver_policy program with
    | Error _ as error -> error
    | Ok prepared ->
        preflight := Some prepared;
        Ok (Verification_solver_private.termination_obligations prepared)
  in
  let configure_solver () =
    match !preflight with
    | None ->
        Error
          (Verification_pipeline.Internal_setup_error
             "outcome inspection lost solver preflight")
    | Some prepared -> (
        match Verification_solver_private.configure ~solver_policy prepared with
        | Error _ as error -> error
        | Ok solve ->
            Ok
              (fun (request : Verification_pipeline.solve_request) ->
                List.iter
                  (fun obligation ->
                    observations :=
                      {
                        function_name =
                          request.definition.Sst.function_id.function_name;
                        obligation;
                        broadcast = Broadcast_vc_private.report obligation;
                      }
                      :: !observations)
                  request.execution.Vir.obligations;
                solve request))
  in
  let proof_entry_activations () =
    Option.fold ~none:(Fun.const [])
      ~some:Verification_solver_private.proof_entry_activations !preflight
  in
  let* report =
    Verification_pipeline.run_validated ~imports ~implementation ~program
      ~validated ~invariants ~preflight:run_preflight ~proof_entry_activations
      ~configure_solver ~on_result:ignore
    |> Result.map_error (fun message ->
           Failure.make Failure.Expectation_mismatch message)
  in
  Ok { program; report; observations = List.rev !observations }

let completion inspected =
  inspected.report.Verification_pipeline.outcome
  |> Result.map_error (fun error ->
         Failure.make Failure.Expectation_mismatch (pipeline_error error))

let program_function_facts program names =
  let definitions =
    List.map
      (fun definition -> definition.Sst.function_id.function_name)
      program.Sst.functions
  in
  let rec required facts = function
    | [] -> Ok (List.rev facts)
    | name :: rest ->
        if List.mem name definitions then
          required
            (("function:" ^ name, Outcome.Function_exists name) :: facts)
            rest
        else mismatch "lowered program omitted required function %s" name
  in
  required [] names

let balanced_telemetry (telemetry : Z3_bridge.counters) =
  telemetry.contexts_created > 0
  && telemetry.contexts_created = telemetry.contexts_cleaned
  && telemetry.contexts_live = 0
  && telemetry.solvers_created = telemetry.solver_resets

let direct_detached_parity observation =
  let requires = [ Logic_ir.Named_sorts; Logic_ir.Algebraic_datatypes ] in
  let* translation =
    Vir_logic_ir_translation_private.translate ~requires observation.obligation
    |> Result.map_error (fun message ->
           Failure.make Failure.Expectation_mismatch message)
  in
  let query = Vir_logic_ir_translation_private.query translation in
  let* detached_query =
    Z3_bridge.detach_vir ~requires observation.obligation
    |> Result.map fst
    |> Result.map_error (fun error ->
           Failure.make Failure.Expectation_mismatch
             (Z3_bridge.error_to_string error))
  in
  let config : Z3_bridge.config = { timeout_ms = 60_000; model = false } in
  let (direct, detached), cleanup_records =
    capture_records Delator.Trace (fun () ->
        ( Z3_bridge.solve_query_local ~controlled:Z3_bridge.Force_unknown
            ~rlimit:Solver_policy_private.default_rlimit config query,
          Z3_bridge.solve_detached_query_local
            ~controlled:Z3_bridge.Force_unknown ~timeout_ms:60_000
            ~rlimit:Solver_policy_private.default_rlimit ~model:false
            detached_query ))
  in
  let direct_unknown =
    match direct.Z3_bridge.result with
    | Ok (Z3_bridge.Inconclusive (Backend_unknown _)) -> true
    | Ok _ | Error _ -> false
  and detached_unknown =
    match detached.Z3_bridge.detached_result with
    | Ok (Z3_bridge.Detached_inconclusive (Backend_unknown _)) -> true
    | Ok _ | Error _ -> false
  in
  let cleanup_routes =
    cleanup_records
    |> List.filter (fun record ->
           record.level = Delator.Trace
           && String.equal record.target "Z3_bridge"
           && has_fields
                [ "stage"; "resource"; "route"; "decision"; "contexts_live" ]
                record)
    |> List.filter_map (field_string "route")
  in
  let* () = require direct_unknown "direct symbolic query was not controlled-unknown" in
  let* () =
    require detached_unknown "detached symbolic query was not controlled-unknown"
  in
  let* () =
    require (balanced_telemetry direct.telemetry)
      "direct query-local telemetry did not balance"
  in
  let* () =
    require (balanced_telemetry detached.detached_telemetry)
      "detached query-local telemetry did not balance"
  in
  let* () =
    require
      (List.mem "query-local" cleanup_routes
      && List.mem "detached-query-local" cleanup_routes)
      "direct/detached cleanup decisions lack structured route fields"
  in
  require (records_redact [] cleanup_records)
    "resource cleanup instrumentation exposed a private field"

let direct_detached_counterexample observation =
  let requires = [ Logic_ir.Named_sorts; Logic_ir.Algebraic_datatypes ] in
  let* translation =
    Vir_logic_ir_translation_private.translate ~requires observation.obligation
    |> Result.map_error (fun message ->
           Failure.make Failure.Expectation_mismatch message)
  in
  let query = Vir_logic_ir_translation_private.query translation in
  let* detached_query =
    Z3_bridge.detach_vir ~requires observation.obligation
    |> Result.map fst
    |> Result.map_error (fun error ->
           Failure.make Failure.Expectation_mismatch
             (Z3_bridge.error_to_string error))
  in
  let config : Z3_bridge.config = { timeout_ms = 60_000; model = false } in
  let (direct, detached), cleanup_records =
    capture_records Delator.Trace (fun () ->
        ( Z3_bridge.solve_query_local ~controlled:Z3_bridge.Real
            ~rlimit:Solver_policy_private.default_rlimit config query,
          Z3_bridge.solve_detached_query_local ~controlled:Z3_bridge.Real
            ~timeout_ms:60_000 ~rlimit:Solver_policy_private.default_rlimit
            ~model:false detached_query ))
  in
  let direct_counterexample =
    match direct.Z3_bridge.result with
    | Ok (Z3_bridge.Counterexample _) -> true
    | Ok (Z3_bridge.Verified | Z3_bridge.Inconclusive _) | Error _ -> false
  and detached_counterexample =
    match detached.Z3_bridge.detached_result with
    | Ok (Z3_bridge.Detached_counterexample _) -> true
    | Ok (Z3_bridge.Detached_verified | Z3_bridge.Detached_inconclusive _)
    | Error _ -> false
  in
  let cleanup_routes =
    cleanup_records
    |> List.filter (fun record ->
           record.level = Delator.Trace
           && String.equal record.target "Z3_bridge"
           && has_fields
                [ "stage"; "resource"; "route"; "decision"; "contexts_live" ]
                record)
    |> List.filter_map (field_string "route")
  in
  let* () =
    require direct_counterexample
      "direct symbolic trust-isolation query was not a counterexample"
  in
  let* () =
    require detached_counterexample
      "detached symbolic trust-isolation query was not a counterexample"
  in
  let* () =
    require (balanced_telemetry direct.telemetry)
      "direct trust-isolation telemetry did not balance"
  in
  let* () =
    require (balanced_telemetry detached.detached_telemetry)
      "detached trust-isolation telemetry did not balance"
  in
  let* () =
    require
      (List.mem "query-local" cleanup_routes
      && List.mem "detached-query-local" cleanup_routes)
      "direct/detached trust-isolation cleanup lacks structured route fields"
  in
  require (records_redact [] cleanup_records)
    "trust-isolation cleanup instrumentation exposed a private field"

let semantic_source =
  {|open Vstd

[@@@verocaml.verify]

type 'a option_catalog = 'a option
[@@verocaml.external_type_specification]

type ('a, 'error) result_catalog = ('a, 'error) result
[@@verocaml.external_type_specification]

type 'a box = Box of 'a
type 'a envelope = Envelope of ('a option, 'a box) result

[%%verocaml.symbolic val arbitrary : 'a]
[%%verocaml.symbolic val choose : 'a -> 'a -> 'a]
[%%verocaml.symbolic val nested : ('a option, 'a box) result -> ('a option, 'a box) result]
[%%verocaml.symbolic val deep : ('a envelope option, 'a envelope box) result -> ('a envelope option, 'a envelope box) result]

let direct_nullary (value : 'a) : 'a = arbitrary
[@@verocaml.spec]

let direct_result (value : 'a) : 'a = choose value value
[@@verocaml.spec]

let equality (value : int) : int =
  [%verocaml.ensures fun _ ->
    choose value value = choose value value
    && (arbitrary : int) = (arbitrary : int)];
  value

let conditional (flag : bool) (left : int) (right : int) : int =
  [%verocaml.ensures fun _ ->
    (if flag then choose left left else (arbitrary : int)) =
    (if flag then choose left left else (arbitrary : int))
    && (if flag then (arbitrary : int) else choose right right) =
       (if flag then (arbitrary : int) else choose right right)];
  left

let nested_conditional (outer : bool) (inner : bool) (value : int) : int =
  [%verocaml.ensures fun _ ->
    (if outer then
       (if inner then choose value value else (arbitrary : int))
     else if inner then (arbitrary : int) else choose value value) =
    (if outer then
       (if inner then choose value value else (arbitrary : int))
     else if inner then (arbitrary : int) else choose value value)];
  value

let match_option (value : int option) : int =
  [%verocaml.ensures fun _ ->
    (match value with None -> (arbitrary : int) | Some item -> choose item item) =
    (match value with None -> (arbitrary : int) | Some item -> choose item item)];
  0

let nested_integer (value : (int option, int box) result) : int =
  [%verocaml.ensures fun _ -> nested value = nested value];
  0

let nested_boolean (value : (bool option, bool box) result) : bool =
  [%verocaml.ensures fun _ -> nested value = nested value];
  false

let deep_integer (value : (int envelope option, int envelope box) result) : int =
  [%verocaml.ensures fun _ -> deep value = deep value];
  0

let deep_boolean (value : (bool envelope option, bool envelope box) result) : bool =
  [%verocaml.ensures fun _ -> deep value = deep value];
  false

let symbolic_proof (value : int) : unit =
  [%verocaml.assert
    choose value value = choose value value
    && (arbitrary : int) = (arbitrary : int)]
[@@verocaml.proof]

let proved_broadcast (value : int) : unit =
  [%verocaml.ensures fun _ ->
    ((choose value value) [@trigger]) = choose value value];
  ()
[@@verocaml.proof] [@@verocaml.broadcast]

let lifted_runtime_int_broadcast (value : Int.t) : unit =
  [%verocaml.ensures fun _ ->
    ((choose value value) [@trigger]) = choose value value];
  ()
[@@verocaml.proof] [@@verocaml.broadcast]

let trusted_symbolic_axiom (value : int) : unit =
  [%verocaml.ensures fun _ ->
    ((choose value value) [@trigger]) = choose value value
    && (arbitrary : int) = (arbitrary : int)];
  ()
[@@verocaml.axiom] [@@verocaml.broadcast]

let activated_axiom (value : int) : unit =
  [%verocaml.activate [proved_broadcast; trusted_symbolic_axiom]
    ([%verocaml.assert
       choose value value = choose value value
       && (arbitrary : int) = (arbitrary : int)]; ())]
[@@verocaml.proof]
|}

let single_input module_name source =
  Fixture.single_source ~module_name ~source
    ~libraries:[ "verocaml.vstd"; "verocaml.ghost" ]

let imported_dependencies implementation providers =
  let imported =
    implementation.Cmt_input.imports |> Array.to_list
    |> List.map (fun (import : Cmt_input.import) -> import.unit_name)
  in
  List.filter
    (fun (provider : Cmt_input.implementation) ->
      List.mem provider.unit_name imported)
    providers

let semantic_matrix_case =
  Suite.case ~name:"semantic-source-cmt-thread-lifecycle"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_unit "Symbolic_matrix" Outcome.Unit_verified
      |> Expectation.require_named_fact "function:direct_nullary"
           (Outcome.Function_exists "direct_nullary")
      |> Expectation.require_named_fact "function:direct_result"
           (Outcome.Function_exists "direct_result")
      |> Expectation.require_named_fact "function:equality"
           (Outcome.Function_exists "equality")
      |> Expectation.require_named_fact "function:conditional"
           (Outcome.Function_exists "conditional")
      |> Expectation.require_named_fact "function:nested_conditional"
           (Outcome.Function_exists "nested_conditional")
      |> Expectation.require_named_fact "function:match_option"
           (Outcome.Function_exists "match_option")
      |> Expectation.require_named_fact "function:nested_integer"
           (Outcome.Function_exists "nested_integer")
      |> Expectation.require_named_fact "function:nested_boolean"
           (Outcome.Function_exists "nested_boolean")
      |> Expectation.require_named_fact "function:deep_integer"
           (Outcome.Function_exists "deep_integer")
      |> Expectation.require_named_fact "function:deep_boolean"
           (Outcome.Function_exists "deep_boolean")
      |> Expectation.require_named_fact "function:symbolic_proof"
           (Outcome.Function_exists "symbolic_proof")
      |> Expectation.require_named_fact "function:proved_broadcast"
           (Outcome.Function_exists "proved_broadcast")
      |> Expectation.require_named_fact "function:lifted_runtime_int_broadcast"
           (Outcome.Function_exists "lifted_runtime_int_broadcast")
      |> Expectation.require_named_fact "function:trusted_symbolic_axiom"
           (Outcome.Function_exists "trusted_symbolic_axiom")
      |> Expectation.require_named_fact "function:activated_axiom"
           (Outcome.Function_exists "activated_axiom"))
    (fun ~environment ~workspace ->
      reset_lifecycle ();
      let* providers =
        Fixture.retained_providers ~environment
          ~libraries:[ "verocaml.vstd" ]
      in
      let source_workspace = Filename.concat workspace "source" in
      let* source =
        Fixture.run ~environment ~workspace:source_workspace
          (single_input "Symbolic_matrix" semantic_source)
      in
      let project_root = Filename.concat source_workspace "project" in
      let* cmt = discover_artifact project_root "Symbolic_matrix" ".cmt" in
      let* cmi = discover_artifact project_root "Symbolic_matrix" ".cmi" in
      let* implementation = load_implementation cmt cmi in
      let* serial_result =
        verified_result ~threads:1 ~unit_name:"Symbolic_matrix"
          ~dependencies:(imported_dependencies implementation providers)
          implementation
      in
      let threaded_result, cleanup_records =
        capture_records Delator.Debug (fun () ->
            verified_result ~threads:2 ~unit_name:"Symbolic_matrix"
              ~dependencies:(imported_dependencies implementation providers)
              implementation)
      in
      let* threaded_result = threaded_result in
      let serial =
        Outcome.of_verifier_result serial_result
        |> Outcome.with_unit "Symbolic_matrix" Outcome.Unit_verified
      in
      let threaded =
        Outcome.of_verifier_result threaded_result
        |> Outcome.with_unit "Symbolic_matrix" Outcome.Unit_verified
      in
      let* () =
        match Outcome.semantic_parity ~except:[] source serial with
        | Ok () -> Ok ()
        | Error message -> mismatch "%s" message
      in
      let* () =
        match Outcome.semantic_parity ~except:[] serial threaded with
        | Ok () -> Ok ()
        | Error message -> mismatch "serial/threaded %s" message
      in
      let* () =
        require
          (symbolic_identities serial_result <> []
          && symbolic_identities serial_result
             = symbolic_identities threaded_result)
          "serial/threaded symbolic identities differ"
      in
      let cleanup_present =
        List.exists
          (fun record ->
            record.level = Delator.Debug
            && String.equal record.target "Broadcast_vc_private"
            && has_fields [ "stage"; "resource"; "route"; "decision" ]
                 record
            && field_string "stage" record = Some "lifecycle-cleanup"
            && field_string "resource" record = Some "weak-vc-report"
            && field_string "route" record = Some "broadcast-vc"
            && field_string "decision" record = Some "released")
          cleanup_records
      in
      let* () =
        require cleanup_present
          "threaded verification cleanup lacks structured debug fields"
      in
      let* () =
        require (records_redact [] cleanup_records)
          "verification cleanup instrumentation exposed a private field"
      in
      let* inspected =
        inspect_pipeline
          ~dependencies:(imported_dependencies implementation providers)
          implementation
      in
      let* inspected_completion = completion inspected in
      let* () =
        require
          (inspected_completion.status = Verification_pipeline.Verified
          && inspected.report.session_destroyed)
          "inspected symbolic pipeline did not verify and release its session"
      in
      let required_functions =
        [
          "direct_nullary";
          "direct_result";
          "equality";
          "conditional";
          "nested_conditional";
          "match_option";
          "nested_integer";
          "nested_boolean";
          "deep_integer";
          "deep_boolean";
          "symbolic_proof";
          "proved_broadcast";
          "lifted_runtime_int_broadcast";
          "trusted_symbolic_axiom";
          "activated_axiom";
        ]
      in
      let* lowered_function_facts =
        program_function_facts inspected.program required_functions
      in
      let function_projection =
        Outcome.observation ~status:Outcome.Verified
          ~named_facts:lowered_function_facts ()
        |> Outcome.project
      in
      let broadcast_reports =
        List.filter_map (fun observation -> observation.broadcast)
          inspected.observations
      in
      let inserted =
        List.concat_map
          (fun (report : Broadcast_vc_private.report) -> report.inserted)
          broadcast_reports
      in
      let proved_visible =
        List.exists
          (fun (insertion : Broadcast_vc_private.inserted) ->
            (not insertion.trusted) && Option.is_none insertion.witness_span)
          inserted
      and trusted_visible =
        List.exists
          (fun (insertion : Broadcast_vc_private.inserted) ->
            insertion.trusted && Option.is_some insertion.witness_span)
          inserted
      and trust_accounted =
        List.exists
          (fun observation ->
            String.equal observation.function_name "activated_axiom"
            &&
            match observation.broadcast with
            | Some report ->
                report.active_declarations > 0
                && report.trusted_broadcast_declarations > 0
                && report.trusted_broadcast_uses > 0
            | None -> false)
          inspected.observations
      in
      let* () =
        require proved_visible
          "proved broadcast insertion was not structurally untrusted"
      in
      let* () =
        require trusted_visible
          "trusted axiom insertion was not structurally marked trusted"
      in
      let* () =
        require trust_accounted
          "broadcast report omitted active/trusted accounting"
      in
      let* symbolic_observation =
        match
          List.find_opt
            (fun observation ->
              obligation_symbolic_identities observation.obligation <> [])
            inspected.observations
        with
        | Some observation -> Ok observation
        | None -> mismatch "inspected pipeline has no symbolic obligation"
      in
      let* () = direct_detached_parity symbolic_observation in
      let* trust_observation =
        match
          List.find_opt
            (fun observation ->
              match observation.broadcast with
              | Some report ->
                  List.exists
                    (fun (insertion : Broadcast_vc_private.inserted) ->
                      insertion.trusted)
                    report.inserted
                  && List.exists
                       (fun (insertion : Broadcast_vc_private.inserted) ->
                         not insertion.trusted)
                       report.inserted
              | None -> false)
            inspected.observations
        with
        | Some observation -> Ok observation
        | None -> mismatch "no obligation carries both proved and trusted broadcasts"
      in
      let* () = direct_detached_parity trust_observation in
      let* () = check_lifecycle () in
      Ok
        (Outcome.merge
           [ source; serial; threaded; function_projection ]))

let symbolic_counterexample_case =
  Suite.case ~name:"translated-symbolic-head-preserves-counterexample"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Counterexample
      |> Expectation.require_unit "Symbolic_false_claim"
           Outcome.Unit_counterexample
      |> Expectation.require_named_fact "function:false_symbolic_claim"
           (Outcome.Function_exists "false_symbolic_claim"))
    (fun ~environment ~workspace ->
      reset_lifecycle ();
      let* outcome =
        Fixture.run ~environment ~workspace
          (single_input "Symbolic_false_claim"
             {|[%%verocaml.symbolic val choose : 'a -> 'a -> 'a]

let false_symbolic_claim (value : int) : unit =
  [%verocaml.ensures fun _ -> choose value value <> choose value value];
  ()
[@@verocaml.proof]
|})
      in
      let* () = check_lifecycle () in
      Ok outcome)

let recursive_symbolic_source =
  {|open Vstd

[%%verocaml.symbolic val arbitrary : 'a]
[%%verocaml.symbolic val choose : 'a -> 'a -> 'a]

let ordinary_symbolic (value : Int.t) : Int.t =
  [%verocaml.ensures fun _ ->
    choose value value = choose value value
    && (arbitrary : Int.t) = (arbitrary : Int.t)];
  value
[@@verocaml.proof]

let rec recursive_symbolic (value : Int.t) (remaining : Int.t) : bool =
  [%verocaml.decreases remaining];
  if remaining <= 0 then
    choose value value = choose value value
    && (arbitrary : Int.t) = (arbitrary : Int.t)
  else
    choose value value = choose value value
    && recursive_symbolic (choose value value) (remaining - 1)
[@@verocaml.spec] [@@verocaml.opaque]

let recursive_symbolic_proof (value : Int.t) : unit =
  [%verocaml.ensures fun _ -> recursive_symbolic value 1];
  [%verocaml.reveal_with_fuel (recursive_symbolic, 2)]
[@@verocaml.proof]
|}

let recursive_symbolic_source_case =
  Suite.case ~name:"recursive-specification-translates-parametric-symbolics"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_unit "Recursive_symbolic" Outcome.Unit_verified
      |> Expectation.require_named_fact "function:ordinary_symbolic"
           (Outcome.Function_exists "ordinary_symbolic")
      |> Expectation.require_named_fact "function:recursive_symbolic_proof"
           (Outcome.Function_exists "recursive_symbolic_proof"))
    (fun ~environment ~workspace ->
      reset_lifecycle ();
      let* providers =
        Fixture.retained_providers ~environment
          ~libraries:[ "verocaml.vstd" ]
      in
      let* outcome =
        Fixture.run ~environment ~workspace
          (single_input "Recursive_symbolic" recursive_symbolic_source)
      in
      let root = Filename.concat workspace "project" in
      let* cmt = discover_artifact root "Recursive_symbolic" ".cmt" in
      let* cmi = discover_artifact root "Recursive_symbolic" ".cmi" in
      let* implementation = load_implementation cmt cmi in
      let result, translation_records =
        capture_records Delator.Debug (fun () ->
            verified_result ~threads:1 ~unit_name:"Recursive_symbolic"
              ~dependencies:(imported_dependencies implementation providers)
              implementation)
      in
      let* result = result in
      let recursive_obligation =
        (Verifier_service.vir result).Vir.functions
        |> List.concat_map (fun execution -> execution.Vir.obligations)
        |> List.exists Vir.obligation_has_recursive_specification
      and recursive_symbolic_translation =
        List.exists
          (fun record ->
            record.level = Delator.Debug
            && String.equal record.target "Recursive_spec_encoding"
            && field_string "stage" record
               = Some "recursive-totality-translation"
            && has_fields
                 [ "route"; "correlation"; "type_arity"; "term_arity" ]
                 record)
          translation_records
      in
      let ordinary_correlations =
        translation_records
        |> List.filter (fun record ->
               record.level = Delator.Debug
               && String.equal record.target
                    "Vir_logic_ir_translation_private"
               && field_string "route" record = Some "ordinary")
        |> List.filter_map (field_string "correlation")
        |> List.sort_uniq String.compare
      and recursive_correlations =
        translation_records
        |> List.filter (fun record ->
               record.level = Delator.Debug
               && String.equal record.target "Recursive_spec_encoding"
               && field_string "stage" record
                  = Some "recursive-totality-translation")
        |> List.filter_map (field_string "correlation")
        |> List.sort_uniq String.compare
      in
      let* () =
        require recursive_obligation
          "recursive source route lost recursive specification semantics"
      in
      let* () =
        require recursive_symbolic_translation
          "recursive source route did not translate authenticated symbolic applications"
      in
      let* () =
        require
          (ordinary_correlations <> []
          && recursive_correlations <> []
          && List.exists
               (fun identity -> List.mem identity ordinary_correlations)
               recursive_correlations)
          "ordinary/recursive routes did not preserve one symbolic identity"
      in
      let* () =
        require (records_redact [] translation_records)
          "recursive translation instrumentation exposed a private field"
      in
      let* () = check_lifecycle () in
      Ok outcome)

type ppx_route = Standalone | Ppxlib
type ppx_mode = Ordinary | Retained

let ppx_configuration route mode =
  match (route, mode) with
  | Standalone, Ordinary ->
      "(flags (:standard -ppx \"verocaml-ppx\"))"
  | Standalone, Retained ->
      "(flags (:standard -ppx \"verocaml-ppx --keep-ghost\"))"
  | Ppxlib, Ordinary -> "(preprocess (pps verocaml.ppx))"
  | Ppxlib, Retained ->
      "(preprocess (pps verocaml.ppx -- --verocaml-retained))"

let project_file path contents = { Fixture.path; contents }

let retained_project route =
  let dune =
    Printf.sprintf
      {|(library
 (name symbolic_provider_fixture)
 (wrapped false)
 (modules Provider)
 (libraries verocaml.vstd verocaml.ghost)
 %s)

(library
 (name symbolic_consumer_fixture)
 (wrapped false)
 (modules Consumer)
 (libraries symbolic_provider_fixture verocaml.ghost)
 %s)
|}
      (ppx_configuration route Retained)
      (ppx_configuration route Retained)
  in
  Fixture.dune_project
    {
      files =
        [
          project_file "dune-project"
            "(lang dune 3.17)\n(name symbolic_provider_fixture)\n";
          project_file "dune" dune;
          project_file "provider.mli"
            {|type 'a option_catalog = 'a option
[@@verocaml.external_type_specification]

[%%verocaml.symbolic val choose : 'a -> 'a option -> 'a]
|};
          project_file "provider.ml"
            {|[@@@verocaml.verify]
type 'a option_catalog = 'a option
[@@verocaml.external_type_specification]

[%%verocaml.symbolic val choose : 'a -> 'a option -> 'a]

let local_integer (value : int) : int =
  [%verocaml.ensures fun _ ->
    choose value (Some value) = choose value (Some value)];
  value

let local_boolean (value : bool) : bool =
  [%verocaml.ensures fun _ ->
    choose value (Some value) = choose value (Some value)];
  value
|};
          project_file "consumer.ml"
            {|[@@@verocaml.verify]
let imported_integer (value : int) : int =
  [%verocaml.ensures fun _ ->
    Provider.choose value (Some value) =
    Provider.choose value (Some value)];
  value


let imported_boolean (value : bool) : bool =
  [%verocaml.ensures fun _ ->
    Provider.choose value (Some value) =
    Provider.choose value (Some value)];
  value
|};
        ];
      libraries = [ "verocaml.ghost" ];
      targets = [ "@all" ];
      selected_units = [ "Provider"; "Consumer" ];
    }

let ordinary_project route =
  let dune =
    Printf.sprintf
      {|(library
 (name symbolic_ordinary_fixture)
 (wrapped false)
 (modules Provider Anchor Probe)
 (modules_without_implementation Provider)
 (libraries verocaml.vstd verocaml.ghost)
 %s)
|}
      (ppx_configuration route Ordinary)
  in
  Fixture.dune_project
    {
      files =
        [
          project_file "dune-project"
            "(lang dune 3.17)\n(name symbolic_ordinary_fixture)\n";
          project_file "dune" dune;
          project_file "provider.mli"
            "[%%verocaml.symbolic val choose : 'a -> 'a]\n";
          project_file "anchor.ml" "module Provider_receipt = Provider\n";
          project_file "probe.ml"
            "[@@@verocaml.verify]\nlet identity (value : int) = value\n";
        ];
      libraries = [ "verocaml.ghost" ];
      targets = [ "@all" ];
      selected_units = [ "Probe" ];
    }

let signature_has_value cmi name =
  let info = Cmi_format.read_cmi_lazy cmi in
  Subst.Lazy.force_signature info.Cmi_format.cmi_sign
  |> List.exists (function
       | Types.Sig_value (ident, _, Types.Exported) ->
           String.equal (Ident.name ident) name
       | _ -> false)

let run_retained_route route ~environment ~workspace =
  let* outcome =
    Fixture.run ~environment ~workspace (retained_project route)
  in
  let root = Filename.concat workspace "project" in
  let* cmt = discover_artifact root "Provider" ".cmt" in
  let* cmi = discover_artifact root "Provider" ".cmi" in
  let* implementation = load_implementation cmt cmi in
  let* marker =
    match implementation.Cmt_input.interface_symbolic_declarations with
    | [ marker ] -> Ok marker
    | [] | _ :: _ :: _ -> mismatch "retained interface marker is not unique"
  in
  let* () = require (signature_has_value cmi "choose") "retained value was erased" in
  Ok (outcome, implementation, marker, cmt, cmi)

let run_ordinary_route route ~environment ~workspace =
  let* outcome = Fixture.run ~environment ~workspace (ordinary_project route) in
  let root = Filename.concat workspace "project" in
  let* cmi = discover_artifact root "Provider" ".cmi" in
  let* () =
    require (not (signature_has_value cmi "choose"))
      "ordinary interface retained the symbolic value"
  in
  let* probe_cmt = discover_artifact root "Probe" ".cmt" in
  let* probe_cmi = discover_artifact root "Probe" ".cmi" in
  let* implementation = load_implementation probe_cmt probe_cmi in
  let* () =
    require (Cmt_input.ordinary_ppx_artifact implementation)
      "ordinary artifact lacks authenticated official mode receipt"
  in
  Ok (outcome, implementation)

let identity_parity_case =
  Suite.case ~name:"standalone-ppxlib-retained-ordinary-identity"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_unit "Provider" Outcome.Unit_verified
      |> Expectation.require_unit "Consumer" Outcome.Unit_verified
      |> Expectation.require_unit "Probe" Outcome.Unit_verified)
    (fun ~environment ~workspace ->
      reset_lifecycle ();
      let* standalone, standalone_implementation, standalone_marker, _, _ =
        run_retained_route Standalone ~environment
          ~workspace:(Filename.concat workspace "standalone-retained")
      in
      let ppxlib_result, ppxlib_records =
        capture_records Delator.Debug (fun () ->
            run_retained_route Ppxlib ~environment
              ~workspace:(Filename.concat workspace "ppxlib-retained"))
      in
      let* ppxlib, ppxlib_implementation, ppxlib_marker, ppxlib_cmt,
          ppxlib_cmi =
        ppxlib_result
      in
      let* standalone_ordinary, standalone_ordinary_implementation =
        run_ordinary_route Standalone ~environment
          ~workspace:(Filename.concat workspace "standalone-ordinary")
      in
      let* ppxlib_ordinary, ppxlib_ordinary_implementation =
        run_ordinary_route Ppxlib ~environment
          ~workspace:(Filename.concat workspace "ppxlib-ordinary")
      in
      let* standalone_local =
        verified_result ~threads:1 ~unit_name:"Provider" ~dependencies:[]
          standalone_implementation
      in
      let* standalone_consumer =
        let root = Filename.concat workspace "standalone-retained/project" in
        let* cmt = discover_artifact root "Consumer" ".cmt" in
        let* cmi = discover_artifact root "Consumer" ".cmi" in
        let* consumer = load_implementation cmt cmi in
        verified_result ~threads:2 ~unit_name:"Consumer"
          ~dependencies:[ standalone_implementation ] consumer
      in
      let* ppxlib_local =
        verified_result ~threads:1 ~unit_name:"Provider" ~dependencies:[]
          ppxlib_implementation
      in
      let* ppxlib_consumer =
        let root = Filename.concat workspace "ppxlib-retained/project" in
        let* cmt = discover_artifact root "Consumer" ".cmt" in
        let* cmi = discover_artifact root "Consumer" ".cmi" in
        let* consumer = load_implementation cmt cmi in
        verified_result ~threads:2 ~unit_name:"Consumer"
          ~dependencies:[ ppxlib_implementation ] consumer
      in
      let pairs =
        [
          (symbolic_identities standalone_local, symbolic_identities standalone_consumer);
          (symbolic_identities ppxlib_local, symbolic_identities ppxlib_consumer);
        ]
      in
      let* () =
        List.fold_left
          (fun result (local, imported) ->
            let* () = result in
            let distinct =
              List.exists
                (fun left -> List.exists (fun right -> left <> right) local)
                local
            in
            require (local <> [] && distinct && local = imported)
              "same-unit/imported identity or concrete-vector diversity differs")
          (Ok ()) pairs
      in
      let* () =
        require
          (String.equal standalone_marker.symbolic_type_digest
             ppxlib_marker.symbolic_type_digest)
          "standalone and Ppxlib marker type receipts disagree"
      in
      let* () =
        require
          (standalone_implementation.implementation_family_issuers
             = [ "standalone-v1" ]
          && standalone_implementation.interface_family_issuers
             = [ "standalone-v1" ]
          && not standalone_implementation.ppxlib_context
          && ppxlib_implementation.implementation_family_issuers
             = [ "ppxlib-v1" ]
          && ppxlib_implementation.interface_family_issuers = [ "ppxlib-v1" ]
          && ppxlib_implementation.ppxlib_context
          && standalone_ordinary_implementation.implementation_family_issuers
             = [ "standalone-v1" ]
          && ppxlib_ordinary_implementation.implementation_family_issuers
             = [ "ppxlib-v1" ]
          && ppxlib_ordinary_implementation.ppxlib_context)
          "official route/mode issuer receipts are not exact"
      in
      let ppxlib_authentication =
        List.exists
          (fun record ->
            record.level = Delator.Debug
            && String.equal record.target "Cmt_input"
            && field_string "stage" record = Some "artifact-authentication"
            && field_string "route" record = Some "ppxlib-v1"
            && field_string "family" record = Some "retained-v1"
            && field_string "decision" record = Some "accepted"
            && has_fields [ "provider"; "ppx_context"; "receipt_count" ] record
            && field_has_classification "provider" String_field record
            && field_has_classification "ppx_context" Bool_field record
            && field_has_classification "receipt_count" Int_field record
            && field_bool "ppx_context" record = Some true
            &&
            match field_int "receipt_count" record with
            | Some count -> count > 0
            | None -> false)
          ppxlib_records
      and ppxlib_candidate =
        List.exists
          (fun record ->
            record.level = Delator.Debug
            && String.equal record.target
                 "Interface_specification_candidate_private"
            && field_string "stage" record = Some "retained-candidate"
            && field_string "route" record = Some "ppxlib-v1"
            && field_string "decision" record = Some "accepted"
            && has_fields
                 [ "provider"; "family"; "interface_receipt_count" ]
                 record)
          ppxlib_records
      in
      let _, ppxlib_info_records =
        capture_records Delator.Info (fun () ->
            ignore
              (Cmt_input.load_with_interface ~cmt:ppxlib_cmt ~cmi:ppxlib_cmi
                 ()))
      in
      let ppxlib_authentication_leaked =
        List.exists
          (fun record ->
            String.equal record.target "Cmt_input"
            && field_string "stage" record = Some "artifact-authentication")
          ppxlib_info_records
      in
      let* () =
        require ppxlib_authentication
          "live Ppxlib retained authentication lacks structured fields"
      in
      let* () =
        require ppxlib_candidate
          "live Ppxlib candidate admission lacks structured fields"
      in
      let* () =
        require (not ppxlib_authentication_leaked)
          "debug Ppxlib authentication leaked at info"
      in
      let* () =
        require
          (records_redact
             [
               ppxlib_marker.symbolic_uid;
               ppxlib_marker.symbolic_marker;
               ppxlib_marker.symbolic_typed_abi;
             ]
             ppxlib_records)
          "Ppxlib authentication instrumentation exposed private receipt material"
      in
      let* () = check_lifecycle () in
      Ok
        (Outcome.merge
           [ standalone; ppxlib; standalone_ordinary; ppxlib_ordinary ]))

let dependency_mismatch_project =
  Fixture.dune_project
    {
      files =
        [
          project_file "dune-project"
            "(lang dune 3.17)\n(name symbolic_dependency_mismatch)\n";
          project_file "dune"
            {|(library
 (name symbolic_dependency_mismatch)
 (wrapped false)
 (modules Provider)
 (libraries verocaml.ghost)
 (flags (:standard -ppx "verocaml-ppx --keep-ghost")))

(library
 (name symbolic_dependency_consumer)
 (wrapped false)
 (modules Consumer)
 (libraries symbolic_dependency_mismatch verocaml.ghost)
 (flags (:standard -ppx "verocaml-ppx --keep-ghost")))
|};
          project_file "provider.mli"
            "[%%verocaml.symbolic val choose : 'a -> 'a]\n";
          project_file "provider.ml"
            "[@@@verocaml.verify]\nlet choose value = value\n";
          project_file "consumer.ml"
            {|[@@@verocaml.verify]
let use (value : int) : int =
  [%verocaml.ensures fun _ ->
    Provider.choose value = Provider.choose value];
  value
|};
        ];
      libraries = [ "verocaml.ghost" ];
      targets = [ "@all" ];
      selected_units = [ "Provider"; "Consumer" ];
    }

let dependency_mismatch_case =
  Suite.case ~name:"completed-descriptor-mismatch-is-dependency"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Frontend_rejected
      |> Expectation.require_frontend_code "VERO_DEPENDENCY")
    (fun ~environment ~workspace ->
      reset_lifecycle ();
      let solver_baseline =
        Solver_backend_counter_private.solver_creation_count ()
      in
      let outcome, debug_records =
        capture_records Delator.Debug (fun () ->
            Fixture.run ~environment
              ~workspace:(Filename.concat workspace "debug")
              dependency_mismatch_project)
      in
      let* outcome = outcome in
      let info_outcome, info_records =
        capture_records Delator.Info (fun () ->
            Fixture.run ~environment ~workspace:(Filename.concat workspace "info")
              dependency_mismatch_project)
      in
      let* info_outcome = info_outcome in
      let* () =
        require (Outcome.status info_outcome = Outcome.Frontend_rejected)
          "info-level descriptor mismatch changed semantic rejection"
      in
      let rejection_event =
        List.exists
          (fun record ->
            record.level = Delator.Debug
            && String.equal record.target "Imported_callable"
            && has_fields
                 [
                   "provider";
                   "stage";
                   "failure_class";
                   "remedy_class";
                   "correlation";
                 ]
                 record
            && field_string "stage" record
               = Some "provider-seal-diagnostic")
          debug_records
      and typed_abi_rejection =
        List.exists
          (fun record ->
            record.level = Delator.Debug
            && String.equal record.target "Imported_callable"
            && field_string "stage" record = Some "typed-symbolic-abi"
            && field_string "decision" record = Some "rejected"
            && has_fields [ "failure_class"; "provider"; "correlation" ] record)
          debug_records
      and rejection_leaked =
        List.exists
          (fun record ->
            String.equal record.target "Imported_callable"
            &&
            match field_string "stage" record with
            | Some ("provider-seal-diagnostic" | "typed-symbolic-abi") -> true
            | Some _ | None -> false)
          info_records
      in
      let root = Filename.concat workspace "debug/project" in
      let* provider_cmt = discover_artifact root "Provider" ".cmt" in
      let* provider_cmi = discover_artifact root "Provider" ".cmi" in
      let* provider = load_implementation provider_cmt provider_cmi in
      let private_values =
        List.concat_map
          (fun marker ->
            [
              marker.Cmt_input.symbolic_uid;
              marker.symbolic_marker;
              marker.symbolic_typed_abi;
            ])
          provider.interface_symbolic_declarations
      in
      let* () =
        require rejection_event
          "typed dependency rejection lacks structured routing fields"
      in
      let* () =
        require typed_abi_rejection
          "typed ABI rejection lacks structured decision fields"
      in
      let* () = require (not rejection_leaked) "debug rejection leaked at info" in
      let* () =
        require (records_redact private_values debug_records)
          "dependency instrumentation exposed private receipt material"
      in
      let* () =
        require
          (Solver_backend_counter_private.solver_creation_count ()
          = solver_baseline)
          "descriptor mismatch reached solver creation"
      in
      let* () = check_lifecycle () in
      Ok outcome)

let copied_cmi ~source ~destination ~signature =
  let info = Cmi_format.read_cmi source in
  let lazy_info =
    {
      Cmi_format.cmi_name = info.cmi_name;
      cmi_kind = info.cmi_kind;
      cmi_globals = info.cmi_globals;
      cmi_sign = Subst.Lazy.of_signature signature;
      cmi_params = info.cmi_params;
      cmi_crcs = info.cmi_crcs;
      cmi_flags = info.cmi_flags;
    }
  in
  let channel = open_out_bin destination in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> ignore (Cmi_format.output_cmi destination channel lazy_info))

let malformed_symbolic_attribute attribute =
  if
    String.equal attribute.Parsetree.attr_name.txt
      "verocaml.internal.symbolic.interface.v1"
  then { attribute with attr_payload = Parsetree.PStr [] }
  else attribute

let malformed_symbolic_signature cmi =
  let info = Cmi_format.read_cmi cmi in
  info.Cmi_format.cmi_sign
  |> List.map (function
       | Types.Sig_value (ident, description, visibility) ->
           Types.Sig_value
             ( ident,
               {
                 description with
                 Types.val_attributes =
                   List.map malformed_symbolic_attribute
                     description.Types.val_attributes;
               },
               visibility )
       | item -> item)

let malformed_family_attribute attribute =
  if
    String.starts_with
      ~prefix:"verocaml.internal.artifact_family."
      attribute.Parsetree.attr_name.txt
  then { attribute with attr_payload = Parsetree.PStr [] }
  else attribute

let malformed_family_signature cmi =
  let info = Cmi_format.read_cmi cmi in
  info.Cmi_format.cmi_sign
  |> List.map (function
       | Types.Sig_value (ident, description, visibility) ->
           Types.Sig_value
             ( ident,
               {
                 description with
                 Types.val_attributes =
                   List.map malformed_family_attribute
                     description.Types.val_attributes;
               },
               visibility )
       | item -> item)

let receipt_and_artifact_case =
  Suite.case ~name:"typed-abi-and-artifact-diagnostic"
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
    (fun ~environment ~workspace ->
      reset_lifecycle ();
      let retained, route_records =
        capture_records Delator.Trace (fun () ->
            run_retained_route Standalone ~environment
              ~workspace:(Filename.concat workspace "provider"))
      in
      let* _, _, marker, cmt, cmi = retained in
      let _, issuance_debug =
        capture_records Delator.Debug (fun () ->
            ignore
              (Vero_ppx_rewriter.make
                 [ "--verocaml-internal-ppxlib-v1"; "--keep-ghost" ]))
      and _, issuance_info =
        capture_records Delator.Info (fun () ->
            ignore
              (Vero_ppx_rewriter.make
                 [ "--verocaml-internal-ppxlib-v1"; "--keep-ghost" ]))
      in
      let family_trace =
        List.exists
          (fun record ->
            record.level = Delator.Trace
            && String.equal record.target "Cmt_input"
            && has_fields [ "route"; "family"; "stage"; "decision" ] record
            && field_string "stage" record = Some "artifact-family-receipt"
            && field_string "decision" record = Some "accepted")
          route_records
      and route_debug =
        List.exists
          (fun record ->
            record.level = Delator.Debug
            && String.equal record.target "Cmt_input"
            && has_fields
                 [
                   "provider";
                   "route";
                   "family";
                   "stage";
                   "decision";
                   "receipt_count";
                 ]
                 record
            && field_has_classification "provider" String_field record
            && field_has_classification "receipt_count" Int_field record
            && field_string "stage" record = Some "artifact-authentication"
            &&
            match field_int "receipt_count" record with
            | Some count -> count > 0
            | None -> false)
          route_records
      and seal_debug =
        List.exists
          (fun record ->
            record.level = Delator.Debug
            && String.equal record.target "Imported_callable"
            && has_fields
                 [
                   "provider";
                   "route";
                   "stage";
                   "correlation";
                   "callable_count";
                   "direct_dependency_count";
                 ]
                 record
            && field_has_classification "correlation" String_field record
            && field_has_classification "callable_count" Int_field record
            && field_has_classification "direct_dependency_count" Int_field
                 record
            && field_string "stage" record = Some "provider-seal"
            &&
            match
              ( field_int "callable_count" record,
                field_int "direct_dependency_count" record )
            with
            | Some callables, Some dependencies ->
                callables > 0 && dependencies >= 0
            | None, _ | _, None -> false)
          route_records
      and seal_span =
        List.exists
          (fun record ->
            record.level = Delator.Debug
            && String.equal record.target "Imported_callable"
            &&
            match record.kind with
            | Span "seal_provider_with_diagnostic" ->
                has_fields [ "provider"; "direct_dependencies" ] record
            | Event _ | Span _ -> false)
          route_records
      and typed_abi_debug =
        List.exists
          (fun record ->
            record.level = Delator.Debug
            && String.equal record.target "Imported_callable"
            && field_string "stage" record = Some "typed-symbolic-abi"
            && field_string "decision" record = Some "accepted"
            && has_fields
                 [ "provider"; "route"; "correlation"; "descriptor_count" ]
                 record)
          route_records
      and issuance_present =
        List.exists
          (fun record ->
            record.level = Delator.Debug
            && String.equal record.target "Vero_ppx_rewriter"
            && has_fields [ "route"; "family"; "stage"; "decision" ] record
            && field_string "route" record = Some "ppxlib-v1"
            && field_string "family" record = Some "retained-v1")
          issuance_debug
      and issuance_observed =
        List.exists
          (fun record ->
            String.equal record.target "Vero_ppx_rewriter"
            && field_string "stage" record = Some "ppx-issuance")
          issuance_debug
      and issuance_leaked =
        List.exists
          (fun record -> String.equal record.target "Vero_ppx_rewriter")
          issuance_info
      in
      let* () = require family_trace "family receipt trace fields are absent" in
      let* () = require route_debug "retained-route debug fields are absent" in
      let* () = require seal_debug "provider-seal debug fields are absent" in
      let* () = require seal_span "provider-seal debug span is absent" in
      let* () = require typed_abi_debug "typed ABI success fields are absent" in
      let* () =
        require (not issuance_observed || issuance_present)
          "available Ppxlib issuance instrumentation lacks structured fields"
      in
      let* () = require (not issuance_leaked) "debug PPX issuance leaked at info" in
      let owner = Parametric_type.owner ~index:0 ~name:"probe" in
      let binder = Parametric_type.binder owner ~ordinal:0 in
      let parameter = Parametric_type.Parameter binder in
      let* wrong =
        Symbolic_application_private.declaration_abi_material
          ~canonical_path:marker.symbolic_path ~value_uid:marker.symbolic_uid
          ~type_binders:[ binder ] ~parameter_labels:[ "" ]
          ~parameter_types:[ parameter ]
          ~result_type:(Parametric_type.Tuple [ (None, parameter) ])
        |> Result.map_error (fun message ->
               Failure.make Failure.Expectation_mismatch message)
      in
      let marker_matches ~canonical_path ~value_uid material =
        Imported_callable.For_testing.symbolic_marker_matches_material marker
          ~canonical_path ~value_uid ~typed_abi:material
      in
      let matches material =
        marker_matches ~canonical_path:marker.symbolic_path
          ~value_uid:marker.symbolic_uid material
      in
      let* () =
        require (matches marker.symbolic_typed_abi)
          "canonical marker ABI did not match itself"
      in
      let* () = require (not (matches wrong)) "typed ABI mismatch was accepted" in
      let* () =
        require
          (not
             (marker_matches
                ~canonical_path:(marker.symbolic_path ^ ".forged")
                ~value_uid:marker.symbolic_uid marker.symbolic_typed_abi))
          "provider path mismatch was accepted"
      in
      let* () =
        require
          (not
             (marker_matches ~canonical_path:marker.symbolic_path
                ~value_uid:(marker.symbolic_uid ^ "-forged")
                marker.symbolic_typed_abi))
          "compiler UID mismatch was accepted"
      in
      let forged = Filename.concat workspace "forged-provider.cmi" in
      copied_cmi ~source:cmi ~destination:forged
        ~signature:(malformed_symbolic_signature cmi);
      let artifact_solver_baseline =
        Solver_backend_counter_private.solver_creation_count ()
      in
      let artifact_result, artifact_records =
        capture_records Delator.Debug (fun () ->
            Cmt_input.load_with_interface ~cmt ~cmi:forged ())
      in
      let _, artifact_info_records =
        capture_records Delator.Info (fun () ->
            ignore (Cmt_input.load_with_interface ~cmt ~cmi:forged ()))
      in
      let* diagnostic =
        match artifact_result with
        | Error diagnostic -> Ok diagnostic
        | Ok _ -> mismatch "malformed symbolic provider artifact was accepted"
      in
      let* () =
        match diagnostic.Diagnostic.classification with
        | Diagnostic.Invalid_symbolic_dependency { provider; reason; remedy } ->
            let* () = require (String.equal provider "Provider") "unsafe provider identity" in
            let* () = require (contains remedy "rebuild") "dependency remedy is absent" in
            let private_values =
              [ marker.symbolic_uid; marker.symbolic_marker; marker.symbolic_typed_abi ]
            in
            require
              (List.for_all
                 (fun value ->
                   value <> "" && not (contains reason value || contains remedy value))
                 private_values)
              "dependency diagnostic exposed a private receipt value"
        | _ -> mismatch "symbolic artifact failure lost dependency structure"
      in
      let artifact_routed =
        List.exists
          (fun record ->
            record.level = Delator.Debug
            && String.equal record.target "Cmt_input"
            && has_fields
                 [ "provider"; "stage"; "failure_class"; "remedy_class" ]
                 record
            && field_string "stage" record = Some "artifact-diagnostic"
            && field_string "failure_class" record
               = Some "symbolic-artifact-receipt")
          artifact_records
      and artifact_leaked =
        List.exists
          (fun record ->
            String.equal record.target "Cmt_input"
            && field_string "stage" record = Some "artifact-diagnostic")
          artifact_info_records
      in
      let private_values =
        [ marker.symbolic_uid; marker.symbolic_marker; marker.symbolic_typed_abi ]
      in
      let* () =
        require artifact_routed
          "typed artifact rejection lacks structured dependency routing"
      in
      let* () =
        require (not artifact_leaked)
          "debug artifact rejection routing leaked at info"
      in
      let* () =
        require
          (records_redact private_values
             (route_records @ issuance_debug @ artifact_records))
          "authority instrumentation exposed UID, marker, or full ABI"
      in
      let* () =
        require
          (Solver_backend_counter_private.solver_creation_count ()
          = artifact_solver_baseline)
          "artifact rejection reached solver creation"
      in
      let forged_family = Filename.concat workspace "forged-family.cmi" in
      copied_cmi ~source:cmi ~destination:forged_family
        ~signature:(malformed_family_signature cmi);
      let family_result, family_records =
        capture_records Delator.Trace (fun () ->
            Cmt_input.load_with_interface ~cmt ~cmi:forged_family ())
      in
      let* family_diagnostic =
        match family_result with
        | Error diagnostic -> Ok diagnostic
        | Ok _ -> mismatch "malformed artifact-family receipt was accepted"
      in
      let* () =
        match family_diagnostic.Diagnostic.classification with
        | Diagnostic.Invalid_symbolic_dependency { provider; remedy; _ } ->
            require
              (String.equal provider "Provider" && contains remedy "rebuild")
              "family receipt rejection lost provider/remedy structure"
        | _ -> mismatch "family receipt rejection lost dependency structure"
      in
      let family_rejected =
        List.exists
          (fun record ->
            record.level = Delator.Trace
            && String.equal record.target "Cmt_input"
            && field_string "stage" record
               = Some "artifact-family-receipt"
            && field_string "decision" record = Some "rejected"
            && has_fields [ "route"; "family" ] record)
          family_records
      and family_routed =
        List.exists
          (fun record ->
            record.level = Delator.Debug
            && String.equal record.target "Cmt_input"
            && field_string "stage" record = Some "artifact-diagnostic"
            && has_fields
                 [ "provider"; "failure_class"; "remedy_class" ]
                 record)
          family_records
      in
      let* () =
        require family_rejected
          "malformed family receipt lacks a structured trace rejection"
      in
      let* () =
        require family_routed
          "malformed family receipt lacks dependency routing fields"
      in
      let* () =
        require (records_redact private_values family_records)
          "family receipt instrumentation exposed private receipt material"
      in
      let* () =
        require
          (Solver_backend_counter_private.solver_creation_count ()
          = artifact_solver_baseline)
          "family receipt rejection reached solver creation"
      in
      let* () = check_lifecycle () in
      Ok verified)

let symbolic_probe marker uid =
  let span = Diagnostic.file_span "symbolic-probe.ml" in
  let owner = Parametric_type.owner ~index:1 ~name:"probe" in
  let binder = Parametric_type.binder owner ~ordinal:0 in
  let declaration =
    match
      Symbolic_application_private.declare ~marker_id:marker
        ~declaration_index:1 ~declaration_name:"image"
        ~canonical_path:"Provider.image" ~value_uid:uid
        ~source_file:"symbolic-probe.ml" ~compilation_identity:("artifact-" ^ uid)
        ~declaration_span:span ~type_binders:[ binder ] ~parameter_types:[]
        ~result_type:(Parametric_type.Parameter binder)
    with
    | Ok declaration -> declaration
    | Error message -> failwith message
  in
  match
    Symbolic_application_private.create declaration
      ~type_arguments:[ Parametric_type.Int ] ~arguments:[] ~argument_types:[]
      ~result_type:Parametric_type.Int ~span
  with
  | Ok application -> application
  | Error message -> failwith message

let collision_delator_case =
  Suite.case ~name:"full-key-collision-order-and-delator-levels"
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
    (fun ~environment:_ ~workspace:_ ->
      let left =
        symbolic_probe "symbolic.00000000000000000000000000000000" "uid-left"
      and right =
        symbolic_probe "symbolic.11111111111111111111111111111111" "uid-right"
      in
      let left_name = Symbolic_application_private.symbol_name left
      and right_name = Symbolic_application_private.symbol_name right in
      let left_head = Symbolic_application_private.backend_head left
      and right_head = Symbolic_application_private.backend_head right in
      let reversed =
        [ right; left ]
        |> List.map (fun application ->
               ( Symbolic_application_private.identity_digest application,
                 Symbolic_application_private.backend_head application ))
        |> List.sort compare
      and original =
        [ left; right ]
        |> List.map (fun application ->
               ( Symbolic_application_private.identity_digest application,
                 Symbolic_application_private.backend_head application ))
        |> List.sort compare
      in
      let* () = require (String.equal left_name right_name) "collision was not forced" in
      let* () = require (not (String.equal left_head right_head)) "backend identity collided" in
      let* () = require (original = reversed) "backend identity depends on order" in
      let owner = Parametric_type.owner ~index:1 ~name:"probe" in
      let binder = Parametric_type.binder owner ~ordinal:0 in
      let material uid =
        Symbolic_application_private.declaration_abi_material
          ~canonical_path:"Provider.image" ~value_uid:uid
          ~type_binders:[ binder ] ~parameter_labels:[] ~parameter_types:[]
          ~result_type:(Parametric_type.Parameter binder)
      in
      let* left_material =
        material "uid-left"
        |> Result.map_error (fun message ->
               Failure.make Failure.Expectation_mismatch message)
      in
      let* right_material =
        material "uid-right"
        |> Result.map_error (fun message ->
               Failure.make Failure.Expectation_mismatch message)
      in
      let marker uid material =
        {
          Cmt_input.symbolic_path = "Provider.image";
          symbolic_uid = uid;
          symbolic_marker = left_name;
          symbolic_type_digest = "forced-presentation-collision";
          symbolic_typed_abi = material;
        }
      in
      let left_marker = marker "uid-left" left_material
      and right_marker = marker "uid-right" right_material in
      let matches marker uid material =
        Imported_callable.For_testing.symbolic_marker_matches_material marker
          ~canonical_path:"Provider.image" ~value_uid:uid ~typed_abi:material
      in
      let authority_order =
        [
          matches left_marker "uid-left" left_material;
          matches left_marker "uid-right" right_material;
          matches right_marker "uid-right" right_material;
          matches right_marker "uid-left" left_material;
        ]
      and reversed_authority_order =
        [ right_marker; left_marker ]
        |> List.concat_map (fun marker ->
               [
                 matches marker "uid-left" left_material;
                 matches marker "uid-right" right_material;
               ])
      in
      let* () =
        require
          (authority_order = [ true; false; true; false ]
          && reversed_authority_order = [ false; true; true; false ])
          "collision shared a typed authority/dependency fact"
      in
      let _, trace_events =
        capture_records Delator.Trace (fun () ->
            ignore (Symbolic_application_private.backend_head left))
      and _, debug_events =
        capture_records Delator.Debug (fun () ->
            ignore (Symbolic_application_private.backend_head left))
      and _, info_events =
        capture_records Delator.Info (fun () ->
            ignore (Symbolic_application_private.backend_head left))
      in
      let expected_fields = [ "correlation"; "term_arity"; "type_arity" ] in
      let trace_present =
        List.exists
          (fun event ->
            event.level = Delator.Trace
            && String.equal event.target "Symbolic_application_private"
            && has_fields expected_fields event
            && field_has_classification "correlation" String_field event
            && field_has_classification "term_arity" Int_field event
            && field_has_classification "type_arity" Int_field event
            && field_int "term_arity" event = Some 0
            && field_int "type_arity" event = Some 1
            && field_string "correlation" event
               = Some (Symbolic_application_private.identity_digest left))
          trace_events
      in
      let lower_level_present events =
        List.exists
          (fun event -> String.equal event.target "Symbolic_application_private")
          events
      in
      let* () = require trace_present "trace event lacks structured safe fields" in
      let* () =
        require
          (records_redact
             [
               "uid-left";
               "uid-right";
               "symbolic.00000000000000000000000000000000";
               "symbolic.11111111111111111111111111111111";
               left_material;
               right_material;
             ]
             trace_events)
          "symbolic backend instrumentation exposed private identity material"
      in
      let* () = require (not (lower_level_present debug_events)) "trace leaked at debug" in
      let* () = require (not (lower_level_present info_events)) "trace leaked at info" in
      Ok verified)

let collision_presentation_name = "vero_symbolic_forced_collision"

let collision_project =
  Fixture.dune_project
    {
      files =
        [
          project_file "dune-project"
            "(lang dune 3.17)\n(name symbolic_collision_matrix)\n";
          project_file "dune"
            {|(library
 (name left_collision_provider)
 (wrapped false)
 (modules Left_collision_provider)
 (libraries verocaml.vstd verocaml.ghost)
 (flags (:standard -ppx "verocaml-ppx --keep-ghost")))

(library
 (name right_collision_provider)
 (wrapped false)
 (modules Right_collision_provider)
 (libraries verocaml.vstd verocaml.ghost)
 (flags (:standard -ppx "verocaml-ppx --keep-ghost")))

(library
 (name collision_consumer_fixture)
 (wrapped false)
 (modules Collision_consumer)
 (libraries left_collision_provider right_collision_provider verocaml.vstd verocaml.ghost)
 (flags (:standard -ppx "verocaml-ppx --keep-ghost")))
|};
          project_file "left_collision_provider.mli"
            "[%%verocaml.symbolic val image : 'a -> 'a]\n";
          project_file "left_collision_provider.ml"
            {|[@@@verocaml.verify]
[%%verocaml.symbolic val image : 'a -> 'a]

let local_left (value : int) : int =
  [%verocaml.ensures fun _ -> image value = image value];
  value

let rec recursive_left (value : int) (remaining : Vstd.Int.t) : bool =
  [%verocaml.decreases remaining];
  if remaining <= 0 then image value = image value
  else
    image value = image value
    && recursive_left (image value) (remaining - 1)
[@@verocaml.spec] [@@verocaml.opaque]

let recursive_left_proof (value : int) : unit =
  [%verocaml.ensures fun _ -> recursive_left value 1];
  [%verocaml.reveal_with_fuel (recursive_left, 2)]
[@@verocaml.proof]
|};
          project_file "right_collision_provider.mli"
            "[%%verocaml.symbolic val image : 'a -> 'a]\n";
          project_file "right_collision_provider.ml"
            {|[@@@verocaml.verify]
[%%verocaml.symbolic val image : 'a -> 'a]

let local_right (value : int) : int =
  [%verocaml.ensures fun _ -> image value = image value];
  value

let rec recursive_right (value : int) (remaining : Vstd.Int.t) : bool =
  [%verocaml.decreases remaining];
  if remaining <= 0 then image value = image value
  else
    image value = image value
    && recursive_right (image value) (remaining - 1)
[@@verocaml.spec] [@@verocaml.opaque]

let recursive_right_proof (value : int) : unit =
  [%verocaml.ensures fun _ -> recursive_right value 1];
  [%verocaml.reveal_with_fuel (recursive_right, 2)]
[@@verocaml.proof]
|};
          project_file "collision_consumer.ml"
            {|[@@@verocaml.verify]

let collision_logical_identity (value : Vstd.Int.t) : Vstd.Int.t = value
[@@verocaml.spec]

let ordinary_collision (value : int) : int =
  [%verocaml.ensures fun _ ->
    Left_collision_provider.image value =
      Left_collision_provider.image value
    && Right_collision_provider.image value =
       Right_collision_provider.image value];
  value

|};
        ];
      libraries = [ "verocaml.vstd"; "verocaml.ghost" ];
      targets = [ "@all" ];
      selected_units =
        [
          "Left_collision_provider";
          "Right_collision_provider";
          "Collision_consumer";
        ];
    }

let with_collision_scope action =
  Symbolic_application_collision_testing_private.with_forced_presentation_name
    ~declaration_names:
      [
        "image";
        "Left_collision_provider.image";
        "Right_collision_provider.image";
      ]
    ~presentation_name:collision_presentation_name action

let collision_facts observations =
  observations
  |> List.map
       (fun
         (observation :
           Symbolic_application_collision_testing_private.observation) ->
         ( observation.identity_digest,
           observation.presentation_name,
           observation.backend_head ))
  |> List.sort_uniq compare

let collision_facts_are_injective facts =
  List.exists
    (fun (left_identity, left_presentation, left_head) ->
      List.exists
        (fun (right_identity, right_presentation, right_head) ->
          not (String.equal left_identity right_identity)
          && String.equal left_presentation right_presentation
          && not (String.equal left_head right_head))
        facts)
    facts
  && List.for_all
       (fun (identity, _, head) ->
         List.for_all
           (fun (candidate_identity, _, candidate_head) ->
             (not (String.equal identity candidate_identity))
             || String.equal head candidate_head)
           facts)
       facts

let collision_identity_set facts =
  List.map (fun (identity, _, _) -> identity) facts
  |> List.sort_uniq String.compare

let collision_correlations ~target ~stage records =
  records
  |> List.filter (fun record ->
         record.level = Delator.Debug && String.equal record.target target
         && field_string "stage" record = Some stage)
  |> List.filter_map (field_string "correlation")
  |> List.sort_uniq String.compare

let collision_has_no_trust result =
  (Verifier_service.vir result).Vir.functions
  |> List.for_all (fun execution ->
         execution.Vir.trusted_summary_uses = []
         && List.for_all
              (fun obligation ->
                match Broadcast_vc_private.report obligation with
                | None -> true
                | Some report ->
                    List.for_all
                         (fun (insertion : Broadcast_vc_private.inserted) ->
                           not insertion.trusted)
                         report.inserted)
              execution.obligations)

let collision_route_matrix_case =
  Suite.case ~name:"forced-collision-full-route-matrix"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_unit "Left_collision_provider"
           Outcome.Unit_verified
      |> Expectation.require_unit "Right_collision_provider"
           Outcome.Unit_verified
      |> Expectation.require_unit "Collision_consumer" Outcome.Unit_verified)
    (fun ~environment ~workspace ->
      reset_lifecycle ();
      let* vstd_providers =
        Fixture.retained_providers ~environment
          ~libraries:[ "verocaml.vstd" ]
      in
      let source_workspace = Filename.concat workspace "source" in
      let (source_result, source_observations), source_records =
        capture_records Delator.Debug (fun () ->
            with_collision_scope (fun () ->
                Fixture.run ~environment ~workspace:source_workspace
                  collision_project))
      in
      let* source = source_result in
      let root = Filename.concat source_workspace "project" in
      let* left_cmt =
        discover_artifact root "Left_collision_provider" ".cmt"
      in
      let* left_cmi =
        discover_artifact root "Left_collision_provider" ".cmi"
      in
      let* right_cmt =
        discover_artifact root "Right_collision_provider" ".cmt"
      in
      let* right_cmi =
        discover_artifact root "Right_collision_provider" ".cmi"
      in
      let* consumer_cmt = discover_artifact root "Collision_consumer" ".cmt" in
      let* consumer_cmi = discover_artifact root "Collision_consumer" ".cmi" in
      let* left = load_implementation left_cmt left_cmi in
      let* right = load_implementation right_cmt right_cmi in
      let* consumer = load_implementation consumer_cmt consumer_cmi in
      let consumer_vstd = imported_dependencies consumer vstd_providers in
      let provider_results, provider_observations =
        with_collision_scope (fun () ->
            let* left_result =
              verified_result ~threads:1
                ~unit_name:"Left_collision_provider"
                ~dependencies:(imported_dependencies left vstd_providers)
                left
            in
            let* right_result =
              verified_result ~threads:1
                ~unit_name:"Right_collision_provider"
                ~dependencies:(imported_dependencies right vstd_providers)
                right
            in
            Ok (left_result, right_result))
      in
      let* left_result, right_result = provider_results in
      let run ~threads dependencies =
        capture_records Delator.Debug (fun () ->
            with_collision_scope (fun () ->
                verified_result ~threads ~unit_name:"Collision_consumer"
                  ~dependencies consumer))
      in
      let (serial_result, serial_observations), serial_records =
        run ~threads:1 (left :: right :: consumer_vstd)
      in
      let* serial_result = serial_result in
      let (reversed_result, reversed_observations), reversed_records =
        run ~threads:1 (right :: left :: consumer_vstd)
      in
      let* reversed_result = reversed_result in
      let (threaded_result, threaded_observations), threaded_records =
        run ~threads:2 (right :: left :: consumer_vstd)
      in
      let* threaded_result = threaded_result in
      let source_facts = collision_facts source_observations
      and provider_facts = collision_facts provider_observations
      and serial_facts = collision_facts serial_observations
      and reversed_facts = collision_facts reversed_observations
      and threaded_facts = collision_facts threaded_observations in
      let* () =
        require
          (collision_facts_are_injective source_facts
          && source_facts = provider_facts
          && source_facts = serial_facts
          && serial_facts = reversed_facts
          && reversed_facts = threaded_facts)
          "forced collision identity changed across source/CMT, order, or threads"
      in
      let collision_identities = collision_identity_set serial_facts in
      let ordinary_correlations =
        collision_correlations ~target:"Vir_logic_ir_translation_private"
          ~stage:"symbolic-translation" serial_records
      and recursive_correlations =
        collision_correlations ~target:"Recursive_spec_encoding"
          ~stage:"recursive-totality-translation" serial_records
      in
      let* () =
        require
          (List.for_all
             (fun identity -> List.mem identity ordinary_correlations)
             collision_identities
          && List.for_all
               (fun identity -> List.mem identity recursive_correlations)
               collision_identities)
          "forced collision did not preserve identity in ordinary/recursive translation"
      in
      let* () =
        require
          (collision_has_no_trust serial_result
          && collision_has_no_trust left_result
          && collision_has_no_trust right_result
          && collision_has_no_trust reversed_result
          && collision_has_no_trust threaded_result)
          "forced collision acquired or shared a trust fact"
      in
      let* left_marker =
        match left.interface_symbolic_declarations with
        | [ marker ] -> Ok marker
        | [] | _ :: _ :: _ -> mismatch "left collision marker is not unique"
      in
      let* right_marker =
        match right.interface_symbolic_declarations with
        | [ marker ] -> Ok marker
        | [] | _ :: _ :: _ -> mismatch "right collision marker is not unique"
      in
      let marker_matches marker candidate =
        Imported_callable.For_testing.symbolic_marker_matches_material marker
          ~canonical_path:candidate.Cmt_input.symbolic_path
          ~value_uid:candidate.symbolic_uid
          ~typed_abi:candidate.symbolic_typed_abi
      in
      let* () =
        require
          (marker_matches left_marker left_marker
          && marker_matches right_marker right_marker
          && not (marker_matches left_marker right_marker)
          && not (marker_matches right_marker left_marker))
          "forced collision shared provider authority/dependency material"
      in
      let* observation =
        (Verifier_service.vir serial_result).Vir.functions
        |> List.concat_map (fun execution ->
               List.map
                 (fun obligation ->
                   {
                     function_name =
                       execution.Vir.function_ref.function_name;
                     obligation;
                     broadcast = Broadcast_vc_private.report obligation;
                   })
                 execution.obligations)
        |> List.find_opt (fun observation ->
               let identities =
                 obligation_symbolic_identities observation.obligation
               in
               List.for_all
                 (fun identity -> List.mem identity identities)
                 collision_identities)
        |> function
        | Some observation -> Ok observation
        | None -> mismatch "no obligation carries both forced collision identities"
      in
      let (detached_parity, detached_observations) =
        with_collision_scope (fun () -> direct_detached_parity observation)
      in
      let* () = detached_parity in
      let detached_facts = collision_facts detached_observations in
      let* () =
        require
          (List.for_all
             (fun fact -> List.mem fact serial_facts)
             detached_facts
          && collision_facts_are_injective detached_facts)
          "direct/detached collision identities or heads diverged"
      in
      let restored =
        try
          ignore
            (with_collision_scope (fun () ->
                 raise (Failure "collision scope restoration probe")));
          false
        with Failure _ -> true
      in
      let _, restored_observations = with_collision_scope (fun () -> ()) in
      let* () =
        require (restored && restored_observations = [])
          "forced collision scope did not restore after an exception"
      in
      let all_records =
        source_records @ serial_records @ reversed_records @ threaded_records
      in
      let* () =
        require
          (records_redact
             [
               left_marker.symbolic_uid;
               left_marker.symbolic_marker;
               left_marker.symbolic_typed_abi;
               right_marker.symbolic_uid;
               right_marker.symbolic_marker;
               right_marker.symbolic_typed_abi;
             ]
             all_records)
          "forced collision route instrumentation exposed private identity material"
      in
      let* () = check_lifecycle () in
      Ok source)

let collision_trust_source =
  {|[@@@verocaml.verify]

[%%verocaml.symbolic val left_head : 'a -> 'a]
[%%verocaml.symbolic val right_head : 'a -> 'a]

let trusted_left (value : int) : unit =
  [%verocaml.ensures fun _ -> ((left_head value) [@trigger]) = value];
  ()
[@@verocaml.axiom] [@@verocaml.broadcast]

let right_claim (value : int) : unit =
  [%verocaml.requires left_head value = value];
  [%verocaml.activate [trusted_left]
    ([%verocaml.assert right_head value = value]; ())]
[@@verocaml.proof]
|}

let with_trust_collision_scope action =
  Symbolic_application_collision_testing_private.with_forced_presentation_name
    ~declaration_names:[ "left_head"; "right_head" ]
    ~presentation_name:collision_presentation_name action

let collision_trust_asymmetry_case =
  Suite.case ~name:"forced-collision-trust-asymmetry"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Counterexample
      |> Expectation.require_unit "Collision_trust_asymmetry"
           Outcome.Unit_counterexample
      |> Expectation.require_named_fact "function:right_claim"
           (Outcome.Function_exists "right_claim"))
    (fun ~environment ~workspace ->
      reset_lifecycle ();
      let source_workspace = Filename.concat workspace "source" in
      let (source_result, source_observations), source_records =
        capture_records Delator.Debug (fun () ->
            with_trust_collision_scope (fun () ->
                Fixture.run ~environment ~workspace:source_workspace
                  (single_input "Collision_trust_asymmetry"
                     collision_trust_source)))
      in
      let* source = source_result in
      let root = Filename.concat source_workspace "project" in
      let* cmt =
        discover_artifact root "Collision_trust_asymmetry" ".cmt"
      in
      let* cmi =
        discover_artifact root "Collision_trust_asymmetry" ".cmi"
      in
      let* implementation = load_implementation cmt cmi in
      let run threads =
        capture_records Delator.Debug (fun () ->
            with_trust_collision_scope (fun () ->
                verification_result ~threads ~dependencies:[] implementation))
      in
      let (serial_result, serial_observations), serial_records = run 1 in
      let* serial_result = serial_result in
      let (threaded_result, threaded_observations), threaded_records = run 2 in
      let* threaded_result = threaded_result in
      let* () =
        require
          (Verifier_service.status serial_result
             = Verifier_service.Counterexample
          && Verifier_service.status threaded_result
             = Verifier_service.Counterexample)
          "trusted left collision head masked the right-head counterexample"
      in
      let serial =
        Outcome.of_verifier_result serial_result
        |> Outcome.with_unit "Collision_trust_asymmetry"
             Outcome.Unit_counterexample
      and threaded =
        Outcome.of_verifier_result threaded_result
        |> Outcome.with_unit "Collision_trust_asymmetry"
             Outcome.Unit_counterexample
      in
      let* () =
        match Outcome.semantic_parity ~except:[] source serial with
        | Ok () -> Ok ()
        | Error message -> mismatch "source/CMT trust asymmetry %s" message
      in
      let* () =
        match Outcome.semantic_parity ~except:[] serial threaded with
        | Ok () -> Ok ()
        | Error message -> mismatch "serial/threaded trust asymmetry %s" message
      in
      let inspected_result, inspected_observations =
        with_trust_collision_scope (fun () -> inspect_pipeline implementation)
      in
      let* inspected = inspected_result in
      let* inspected_completion = completion inspected in
      let* () =
        require
          (inspected_completion.status = Verification_pipeline.Counterexample
          && inspected.report.session_destroyed)
          "inspected trust-asymmetry pipeline did not preserve the counterexample and cleanup"
      in
      let source_facts = collision_facts source_observations
      and serial_facts = collision_facts serial_observations
      and threaded_facts = collision_facts threaded_observations
      and inspected_facts = collision_facts inspected_observations in
      let* () =
        require
          (collision_facts_are_injective source_facts
          && source_facts = serial_facts
          && serial_facts = threaded_facts
          && threaded_facts = inspected_facts)
          "trusted collision identity changed across source/CMT or threads"
      in
      let collision_identities = collision_identity_set serial_facts in
      let* right_observation =
        inspected.observations
        |> List.find_opt (fun observation ->
               String.equal observation.function_name "right_claim"
               &&
               match observation.broadcast with
               | Some report ->
                   List.exists
                     (fun (insertion : Broadcast_vc_private.inserted) ->
                       insertion.trusted)
                     report.inserted
               | None -> false)
        |> function
        | Some observation -> Ok observation
        | None ->
            mismatch
              "right collision claim lacks a structured trusted broadcast insertion"
      in
      let premise_identities =
        List.fold_left collect_boolean []
          (right_observation.obligation.assumptions
          @ right_observation.obligation.required_preceding_safety
          @ right_observation.obligation.path_condition)
        |> List.sort_uniq String.compare
      and goal_identities =
        collect_boolean [] right_observation.obligation.goal
        |> List.sort_uniq String.compare
      in
      let* () =
        require
          (premise_identities <> [] && goal_identities <> []
          && List.for_all
               (fun identity -> List.mem identity collision_identities)
               (premise_identities @ goal_identities)
          && List.for_all
               (fun identity -> not (List.mem identity goal_identities))
               premise_identities)
          "trusted broadcast premise was not isolated from the right collision goal"
      in
      let* () =
        match right_observation.broadcast with
        | Some report ->
            require
              (report.trusted_broadcast_uses > 0
              && List.exists
                   (fun (insertion : Broadcast_vc_private.inserted) ->
                     insertion.trusted && Option.is_some insertion.witness_span)
                   report.inserted)
              "right claim omitted structured trusted-use accounting"
        | None -> mismatch "right claim omitted its broadcast report"
      in
      let (detached_result, detached_observations) =
        with_trust_collision_scope (fun () ->
            direct_detached_counterexample right_observation)
      in
      let* () = detached_result in
      let detached_facts = collision_facts detached_observations in
      let* () =
        require
          (collision_facts_are_injective detached_facts
          && List.for_all
               (fun fact -> List.mem fact serial_facts)
               detached_facts)
          "direct/detached trust isolation merged collision identities"
      in
      let* () =
        require
          (records_redact []
             (source_records @ serial_records @ threaded_records))
          "trust-asymmetry instrumentation exposed private identity material"
      in
      let* () = check_lifecycle () in
      Ok source)

let nullary_analysis_case =
  Suite.case ~name:"recursive-nullary-rejects-parametric-symbolic"
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
    (fun ~environment:_ ~workspace:_ ->
      let span = Diagnostic.file_span "recursive-nullary.ml" in
      let declaration_owner = Parametric_type.owner ~index:2 ~name:"declaration" in
      let declaration_binder =
        Parametric_type.binder declaration_owner ~ordinal:0
      in
      let use_owner = Parametric_type.owner ~index:3 ~name:"recursive-use" in
      let use_binder = Parametric_type.binder use_owner ~ordinal:0 in
      let declaration =
        match
          Symbolic_application_private.declare
            ~marker_id:"symbolic.22222222222222222222222222222222"
            ~declaration_index:2 ~declaration_name:"nullary"
            ~canonical_path:"Recursive.nullary" ~value_uid:"uid-nullary"
            ~source_file:"recursive-nullary.ml" ~compilation_identity:"artifact"
            ~declaration_span:span ~type_binders:[ declaration_binder ]
            ~parameter_types:[]
            ~result_type:(Parametric_type.Parameter declaration_binder)
        with
        | Ok declaration -> declaration
        | Error message -> failwith message
      in
      let application =
        match
          Symbolic_application_private.create declaration
            ~type_arguments:[ Parametric_type.Parameter use_binder ] ~arguments:[]
            ~argument_types:[] ~result_type:(Parametric_type.Parameter use_binder)
            ~span
        with
        | Ok application -> application
        | Error message -> failwith message
      in
      let applied =
        {
          Vir.parametric_sort = use_binder;
          parametric_desc = Vir.Parametric_symbolic_application application;
        }
      in
      let condition = Vir.Boolean_constant true in
      let nested =
        {
          Vir.parametric_sort = use_binder;
          parametric_desc = Vir.Parametric_conditional (condition, applied, applied);
        }
      in
      let unsupported term =
        Vir.Recursive_boolean_argument (Vir.Parametric_equal (term, term))
        |> Recursive_spec_encoding.For_testing.nullary_branch_argument_supported
        |> not
      in
      let (direct_unsupported, nested_unsupported), debug_records =
        capture_records Delator.Debug (fun () ->
            (unsupported applied, unsupported nested))
      in
      let _, info_records =
        capture_records Delator.Info (fun () -> ignore (unsupported applied))
      in
      let abstention =
        List.exists
          (fun record ->
            record.level = Delator.Debug
            && String.equal record.target "Recursive_spec_encoding"
            && has_fields
                 [
                   "stage";
                   "failure_class";
                   "decision";
                   "correlation";
                   "type_arity";
                   "term_arity";
                 ]
                 record
            && field_string "failure_class" record
               = Some "parametric-symbolic-application"
            && field_string "decision" record = Some "abstain"
            && field_has_classification "correlation" String_field record
            && field_has_classification "type_arity" Int_field record
            && field_has_classification "term_arity" Int_field record)
          debug_records
      in
      let leaked_at_info =
        List.exists
          (fun record ->
            String.equal record.target "Recursive_spec_encoding"
            && field_string "stage" record
               = Some "recursive-nullary-analysis")
          info_records
      in
      let* () =
        require direct_unsupported "direct symbolic term became nullary"
      in
      let* () =
        require nested_unsupported "nested symbolic term became nullary"
      in
      let* () =
        require abstention
          "recursive nullary abstention lacks structured debug fields"
      in
      let* () =
        require (records_redact [ "uid-nullary" ] debug_records)
          "recursive abstention instrumentation exposed private identity"
      in
      let* () = require (not leaked_at_info) "debug abstention leaked at info" in
      Ok verified)

let parse_signature source =
  let lexbuf = Lexing.from_string source in
  Location.init lexbuf "symbolic-signature.mli";
  Parse.interface lexbuf

let rewriter_rejects mode source =
  let mapper =
    Vero_ppx_rewriter.make
      (match mode with Ordinary -> [] | Retained -> [ "--keep-ghost" ])
  in
  try
    ignore (mapper.Ast_mapper.signature mapper (parse_signature source));
    false
  with Location.Error _ -> true

let materialize_rejection_project root route mode source =
  mkdir_p root;
  write_file (Filename.concat root "dune-project")
    "(lang dune 3.17)\n(name symbolic_signature_rejection)\n";
  write_file (Filename.concat root "bad.mli") source;
  write_file (Filename.concat root "dune")
    (Printf.sprintf
       {|(library
 (name symbolic_signature_rejection)
 (wrapped false)
 (modules Bad)
 (modules_without_implementation Bad)
 (libraries verocaml.ghost)
 %s)
|}
       (ppx_configuration route mode))

let run_live_rejection ~environment ~workspace ~route ~mode ~name source =
  let root = Filename.concat workspace name in
  materialize_rejection_project root route mode source;
  let* outcome =
    Process_adapter.run ~cwd:root
      {
        program = Project_environment.dune_path environment;
        arguments =
          [
            "build";
            "--root";
            root;
            "--build-dir";
            Filename.concat root "_build";
            "@all";
          ];
        forwarded =
          [
            ("PATH", Project_environment.tool_path environment);
            ("OCAMLPATH", Project_environment.package_root environment);
            ("DUNE_CACHE", "disabled");
            ("HOME", root);
            ("TMPDIR", root);
          ];
        cleanup_paths = [];
        adjacency = [];
      }
  in
  let rejected =
    Outcome.process_facts outcome
    |> List.exists (function
         | Outcome.Exit_class (Outcome.Exited code) -> code <> 0
         | Exit_class Signaled | Exit_class Stopped -> true
         | Stable_code _ | Forwarded _ | Cleaned _ | Adjacent _ -> false)
  in
  if rejected then Ok outcome else mismatch "live PPX route accepted %s" name

let signature_rejection_case =
  let duplicate =
    "[%%verocaml.symbolic val choose : int]\n[%%verocaml.symbolic val choose : int]\n"
  and malformed = "[%%verocaml.symbolic: int]\n"
  and raw =
    "val choose : int\n[@@verocaml.internal.symbolic.interface.v1 \"forged\"]\n"
  in
  Suite.case ~name:"all-entrypoints-validate-before-erasure"
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
    (fun ~environment ~workspace ->
      let sources =
        [ ("duplicate", duplicate); ("malformed", malformed); ("raw", raw) ]
      in
      let modes = [ Ordinary; Retained ] and routes = [ Standalone; Ppxlib ] in
      let* () =
        List.fold_left
          (fun result mode ->
            let* () = result in
            List.fold_left
              (fun result (name, source) ->
                let* () = result in
                require (rewriter_rejects mode source)
                  ("rewriter accepted " ^ name))
              (Ok ()) sources)
          (Ok ()) modes
      in
      let* outcomes =
        List.fold_left
          (fun result route ->
            let* outcomes = result in
            List.fold_left
              (fun result mode ->
                let* outcomes = result in
                List.fold_left
                  (fun result (name, source) ->
                    let* outcomes = result in
                    let route_name =
                      match route with Standalone -> "standalone" | Ppxlib -> "ppxlib"
                    and mode_name =
                      match mode with Ordinary -> "ordinary" | Retained -> "retained"
                    in
                    let case_name = route_name ^ "-" ^ mode_name ^ "-" ^ name in
                    let* outcome =
                      run_live_rejection ~environment ~workspace ~route ~mode
                        ~name:case_name source
                    in
                    Ok (outcome :: outcomes))
                  (Ok outcomes) sources)
              (Ok outcomes) modes)
          (Ok []) routes
      in
      Ok (Outcome.merge (verified :: outcomes)))

let source_diagnostic_case =
  Suite.case ~name:"source-symbolic-stage-diagnostic"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Frontend_rejected
      |> Expectation.require_frontend_code "VERO_SYMBOLIC_APPLICATION"
      |> Expectation.require_unit "Symbolic_exec_rejection"
           Outcome.Unit_frontend_rejected)
    (fun ~environment ~workspace ->
      reset_lifecycle ();
      let solver_baseline =
        Solver_backend_counter_private.solver_creation_count ()
      in
      let* outcome =
        Fixture.run ~environment ~workspace
          (single_input "Symbolic_exec_rejection"
             {|[%%verocaml.symbolic val choose : 'a -> 'a]
let bad (value : int) : int = choose value
|})
      in
      let root = Filename.concat workspace "project" in
      let* cmt = discover_artifact root "Symbolic_exec_rejection" ".cmt" in
      let* cmi = discover_artifact root "Symbolic_exec_rejection" ".cmi" in
      let* implementation = load_implementation cmt cmi in
      let* configuration =
        Verifier_service.configuration ~threads:1 ~timeout_ms:60_000 ~rlimit:None
        |> Result.map_error (fun error ->
               Failure.make Failure.Expectation_mismatch
                 (Verifier_service.configuration_error_message error))
      in
      let* diagnostic =
        match
          Verifier_service.verify
            (Verifier_service.request ~configuration ~consumer:implementation
               ~dependencies:[])
        with
        | Error error
          when Verifier_service.error_classification error
               = Verifier_service.Source_error -> (
            match Verifier_service.error_diagnostic error with
            | Some diagnostic -> Ok diagnostic
            | None -> mismatch "source rejection lost typed diagnostic")
        | Error _ -> mismatch "source rejection was not source-classified"
        | Ok _ -> mismatch "executable symbolic use unexpectedly verified"
      in
      let* () =
        match diagnostic.Diagnostic.classification with
        | Diagnostic.Invalid_symbolic_application reason ->
            let* () =
              require (diagnostic.span.start_pos.line > 0)
                "source diagnostic lost its source span"
            in
            let* () =
              require
                (contains diagnostic.message "specification"
                || contains diagnostic.message "proof")
                "source diagnostic lacks an actionable stage remedy"
            in
            require
              (List.for_all
                 (fun marker ->
                   not
                     (contains (reason ^ diagnostic.message)
                        marker.Cmt_input.symbolic_uid
                     || contains (reason ^ diagnostic.message)
                          marker.symbolic_marker
                     || contains (reason ^ diagnostic.message)
                          marker.symbolic_typed_abi))
                 implementation.interface_symbolic_declarations)
              "source diagnostic exposed private symbolic receipt material"
        | _ -> mismatch "source rejection lost symbolic classification"
      in
      let* () =
        require
          (Solver_backend_counter_private.solver_creation_count ()
          = solver_baseline)
          "source symbolic rejection reached solver creation"
      in
      let* () = check_lifecycle () in
      Ok outcome)

let executable_symbolic_lambda_case =
  Suite.case ~name:"reject-executable-symbolic-lambda"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Frontend_rejected
      |> Expectation.require_frontend_code "VERO_SYMBOLIC_APPLICATION"
      |> Expectation.require_unit "Symbolic_lambda_exec_rejection"
           Outcome.Unit_frontend_rejected)
    (fun ~environment ~workspace ->
      Fixture.run ~environment ~workspace
        (single_input "Symbolic_lambda_exec_rejection"
           {|[%%verocaml.symbolic val observe : (int -> int) -> int]
let bad () : int = observe (fun value -> value)
|}))

let internal_diagnostic_case =
  Suite.case ~name:"post-validation-failure-is-internal-and-redacted"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_unit "Symbolic_internal" Outcome.Unit_verified)
    (fun ~environment ~workspace ->
      reset_lifecycle ();
      let* outcome =
        Fixture.run ~environment ~workspace
          (single_input "Symbolic_internal"
             {|[%%verocaml.symbolic val choose : 'a -> 'a]
let checked (value : int) : int =
  [%verocaml.ensures fun _ -> choose value = choose value];
  value
|})
      in
      let root = Filename.concat workspace "project" in
      let* cmt = discover_artifact root "Symbolic_internal" ".cmt" in
      let* cmi = discover_artifact root "Symbolic_internal" ".cmi" in
      let* implementation = load_implementation cmt cmi in
      let* configuration =
        Verifier_service.configuration ~threads:2 ~timeout_ms:60_000 ~rlimit:None
        |> Result.map_error (fun error ->
               Failure.make Failure.Expectation_mismatch
                 (Verifier_service.configuration_error_message error))
      in
      let injected, records =
        capture_records Delator.Info (fun () ->
            Verification_pipeline.For_testing.inject_materialization_error
              (Some 0);
            Fun.protect
              ~finally:(fun () ->
                Verification_pipeline.For_testing.inject_materialization_error
                  None)
              (fun () ->
                Verifier_service.verify
                  (Verifier_service.request ~configuration
                     ~consumer:implementation ~dependencies:[])))
      in
      let* error =
        match injected with
        | Error error -> Ok error
        | Ok _ -> mismatch "injected post-validation failure was not surfaced"
      in
      let* () =
        require
          (Verifier_service.error_classification error
             = Verifier_service.Internal_error
          && Option.is_none (Verifier_service.error_diagnostic error))
          "post-validation failure was not typed as internal"
      in
      let* () =
        require
          (Verifier_service.error_unit_name error = Some "Symbolic_internal")
          "internal diagnostic lost its structured unit identity"
      in
      let private_values =
        List.concat_map
          (fun marker ->
            [
              marker.Cmt_input.symbolic_uid;
              marker.symbolic_marker;
              marker.symbolic_typed_abi;
            ])
          implementation.interface_symbolic_declarations
      in
      let message = Verifier_service.error_message error in
      let* () =
        require
          ((not (contains message "Symbolic_internal"))
          && List.for_all
             (fun value -> value = "" || not (contains message value))
             private_values)
          "internal diagnostic exposed private receipt material"
      in
      let internal_event =
        List.exists
          (fun record ->
            record.level = Delator.Error
            && String.equal record.target
                 "Interface_specification_loaded_private"
            && has_fields
                 [
                   "provider";
                   "stage";
                   "failure_class";
                   "remedy_class";
                   "candidate_authenticated";
                 ]
                 record
            && field_string "stage" record = Some "internal-diagnostic"
            && field_bool "candidate_authenticated" record = Some true
            && field_string "remedy_class" record
               = Some "report-verifier-defect")
          records
      in
      let* () =
        require internal_event
          "internal diagnostic routing lacks structured error fields"
      in
      let* () =
        require (records_redact private_values records)
          "internal instrumentation exposed private receipt material"
      in
      let* () = check_lifecycle () in
      Ok outcome)

let classification_ordinary_project =
  Fixture.dune_project
    {
      files =
        [
          project_file "dune-project"
            "(lang dune 3.17)\n(name verification_classification_ordinary)\n";
          project_file "dune"
            {|(library
 (name verification_classification_ordinary)
 (wrapped false)
 (modules Classification_ordinary)
 (libraries verocaml.ghost)
 (flags (:standard -ppx "verocaml-ppx")))
|};
          project_file "classification_ordinary.ml"
            {|[@@@verocaml.verify]
let checked (value : int) : int =
  [%verocaml.ensures fun result -> result = value];
  value
|};
        ];
      libraries = [ "verocaml.ghost" ];
      targets = [ "@all" ];
      selected_units = [ "Classification_ordinary" ];
    }

let classification_provider_project =
  Fixture.dune_project
    {
      files =
        [
          project_file "dune-project"
            "(lang dune 3.17)\n(name verification_classification_provider)\n";
          project_file "dune"
            {|(library
 (name classification_provider)
 (wrapped false)
 (modules Classification_provider)
 (libraries verocaml.ghost)
 (flags (:standard -ppx "verocaml-ppx --keep-ghost")))

(library
 (name classification_consumer)
 (wrapped false)
 (modules Classification_consumer)
 (libraries classification_provider verocaml.ghost)
 (flags (:standard -ppx "verocaml-ppx --keep-ghost")))
|};
          project_file "classification_provider.mli"
            "val identity : int -> int\n";
          project_file "classification_provider.ml"
            {|[@@@verocaml.verify]
let identity (value : int) : int =
  [%verocaml.ensures fun result -> result = value];
  value
|};
          project_file "classification_consumer.mli" "val use : int -> int\n";
          project_file "classification_consumer.ml"
            {|[@@@verocaml.verify]
let use (value : int) : int =
  [%verocaml.ensures fun result -> result = value];
  Classification_provider.identity value
|};
        ];
      libraries = [ "verocaml.ghost" ];
      targets = [ "@all" ];
      selected_units = [ "Classification_provider"; "Classification_consumer" ];
    }

let non_internal_classification_case =
  Suite.case ~name:"non-internal-verification-failure-matrix"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_unit "Classification_ordinary"
           Outcome.Unit_verified
      |> Expectation.require_unit "Classification_retained"
           Outcome.Unit_verified
      |> Expectation.require_unit "Classification_provider"
           Outcome.Unit_verified
      |> Expectation.require_unit "Classification_consumer"
           Outcome.Unit_verified)
    (fun ~environment ~workspace ->
      reset_lifecycle ();
      let ordinary_workspace = Filename.concat workspace "ordinary" in
      let* ordinary_outcome =
        Fixture.run ~environment ~workspace:ordinary_workspace
          classification_ordinary_project
      in
      let ordinary_root = Filename.concat ordinary_workspace "project" in
      let* ordinary_cmt =
        discover_artifact ordinary_root "Classification_ordinary" ".cmt"
      in
      let* ordinary_cmi =
        discover_artifact ordinary_root "Classification_ordinary" ".cmi"
      in
      let* ordinary = load_implementation ordinary_cmt ordinary_cmi in
      let retained_workspace = Filename.concat workspace "retained" in
      let* retained_outcome =
        Fixture.run ~environment ~workspace:retained_workspace
          (single_input "Classification_retained"
             {|let checked (value : int) : int =
  [%verocaml.ensures fun result -> result = value];
  value
|})
      in
      let retained_root = Filename.concat retained_workspace "project" in
      let* retained_cmt =
        discover_artifact retained_root "Classification_retained" ".cmt"
      in
      let* retained_cmi =
        discover_artifact retained_root "Classification_retained" ".cmi"
      in
      let* retained = load_implementation retained_cmt retained_cmi in
      let provider_workspace = Filename.concat workspace "provider" in
      let* provider_outcome =
        Fixture.run ~environment ~workspace:provider_workspace
          classification_provider_project
      in
      let provider_root = Filename.concat provider_workspace "project" in
      let* provider_cmt =
        discover_artifact provider_root "Classification_provider" ".cmt"
      in
      let* provider_cmi =
        discover_artifact provider_root "Classification_provider" ".cmi"
      in
      let* consumer_cmt =
        discover_artifact provider_root "Classification_consumer" ".cmt"
      in
      let* consumer_cmi =
        discover_artifact provider_root "Classification_consumer" ".cmi"
      in
      let* provider = load_implementation provider_cmt provider_cmi in
      let* consumer = load_implementation consumer_cmt consumer_cmi in
      let private_receipt_values implementation =
        implementation.Cmt_input.raw_artifact_digest
        :: (List.filter_map snd
               (implementation.interface_mode_signatures
               @ implementation.interface_finite_signatures)
           @ List.concat_map
               (fun marker ->
                 [
                   marker.Cmt_input.symbolic_uid;
                   marker.symbolic_marker;
                   marker.symbolic_typed_abi;
                 ])
               implementation.interface_symbolic_declarations)
      in
      let private_values =
        private_receipt_values ordinary @ private_receipt_values retained
        @ private_receipt_values provider @ private_receipt_values consumer
      in
      let* configuration =
        Verifier_service.configuration ~threads:2 ~timeout_ms:60_000 ~rlimit:None
        |> Result.map_error (fun error ->
               Failure.make Failure.Expectation_mismatch
                 (Verifier_service.configuration_error_message error))
      in
      let* serial_configuration =
        Verifier_service.configuration ~threads:1 ~timeout_ms:60_000 ~rlimit:None
        |> Result.map_error (fun error ->
               Failure.make Failure.Expectation_mismatch
                 (Verifier_service.configuration_error_message error))
      in
      let verify ?(dependencies = []) ?(serial = false) implementation =
        let configuration =
          if serial then serial_configuration else configuration
        in
        Verifier_service.verify
          (Verifier_service.request ~configuration ~consumer:implementation
             ~dependencies)
      in
      let unauthenticated_result, unauthenticated_records =
        capture_records Delator.Debug (fun () ->
            Verification_pipeline.For_testing.inject_materialization_error
              (Some 0);
            Fun.protect
              ~finally:(fun () ->
                Verification_pipeline.For_testing.inject_materialization_error
                  None)
              (fun () -> verify ordinary))
      in
      let* unauthenticated_error =
        match unauthenticated_result with
        | Error error -> Ok error
        | Ok _ -> mismatch "unauthenticated invariant injection did not fail"
      in
      let* () =
        require
          (Verifier_service.error_classification unauthenticated_error
             = Verifier_service.Dependency_error)
          "unauthenticated invariant injection was classified internal"
      in
      let* () =
        require
          (Verifier_service.error_unit_name unauthenticated_error
             = Some "Classification_ordinary")
          "unauthenticated dependency error lost its structured unit identity"
      in
      let unauthenticated_message =
        Verifier_service.error_message unauthenticated_error
      in
      let* () =
        require
          ((not (contains unauthenticated_message "Classification_ordinary"))
          && contains (String.lowercase_ascii unauthenticated_message) "rebuild"
          && contains (String.lowercase_ascii unauthenticated_message) "retry"
          && text_avoids_private_verifier_jargon unauthenticated_message
          && List.for_all
               (fun value ->
                 value = "" || not (contains unauthenticated_message value))
               private_values)
          "unauthenticated dependency message lacks a safe rebuild/retry remedy"
      in
      let unauthenticated_decision =
        List.exists
          (fun record ->
            record.level = Delator.Debug
            && String.equal record.target
                 "Interface_specification_loaded_private"
            && has_fields
                 [
                   "provider";
                   "stage";
                   "failure_class";
                   "candidate_authenticated";
                   "decision";
                   "remedy_class";
                 ]
                 record
            && field_string "stage" record
               = Some "verification-failure-classification"
            && field_string "failure_class" record
               = Some "unauthenticated-post-validation-invariant"
            && field_string "decision" record = Some "dependency"
            && field_string "provider" record
               = Some "Classification_ordinary"
            && field_string "remedy_class" record
               = Some "rebuild-with-current-toolchain"
            && field_bool "candidate_authenticated" record = Some false
            && field_has_classification "remedy_class" String_field record
            && field_has_classification "candidate_authenticated" Bool_field
                 record)
          unauthenticated_records
      and unauthenticated_internal =
        List.exists
          (fun record ->
            record.level = Delator.Error
            && String.equal record.target
                 "Interface_specification_loaded_private"
            && field_string "stage" record = Some "internal-diagnostic")
          unauthenticated_records
      in
      let* () =
        require (unauthenticated_decision && not unauthenticated_internal)
          "unauthenticated invariant routing lacks the dependency decision"
      in
      Verification_pipeline.For_testing.reset_frontier_events ();
      let solve_result, solve_records =
        capture_records Delator.Debug (fun () ->
            Verification_solver_private.For_testing.inject_preparation_error
              (Some 0);
            Fun.protect
              ~finally:(fun () ->
                Verification_solver_private.For_testing.inject_preparation_error
                  None)
              (fun () -> verify ~serial:true retained))
      in
      let* solve_error =
        match solve_result with
        | Error error -> Ok error
        | Ok _ -> mismatch "injected solve failure did not fail"
      in
      let* () =
        require
          (Verifier_service.error_classification solve_error
             = Verifier_service.Dependency_error)
          "ordinary solve failure was classified internal"
      in
      let* () =
        require
          (Verifier_service.error_unit_name solve_error
             = Some "Classification_retained")
          "pipeline dependency error lost its structured unit identity"
      in
      let solve_message = Verifier_service.error_message solve_error in
      let* () =
        require
          ((not (contains solve_message "Classification_retained"))
          && contains (String.lowercase_ascii solve_message) "rebuild"
          && contains (String.lowercase_ascii solve_message) "retry"
          && text_avoids_private_verifier_jargon solve_message
          && List.for_all
               (fun value -> value = "" || not (contains solve_message value))
               private_values)
          "pipeline dependency message lacks a safe rebuild/retry remedy"
      in
      let solve_decision =
        List.exists
          (fun record ->
            record.level = Delator.Debug
            && String.equal record.target
                 "Interface_specification_loaded_private"
            && has_fields
                 [
                   "provider";
                   "stage";
                   "failure_class";
                   "candidate_authenticated";
                   "decision";
                   "remedy_class";
                 ]
                 record
            && field_string "stage" record
               = Some "verification-failure-classification"
            && field_string "failure_class" record
               = Some "verification-pipeline"
            && field_string "decision" record = Some "dependency"
            && field_string "provider" record
               = Some "Classification_retained"
            && field_string "remedy_class" record = Some "rebuild-and-retry"
            && field_bool "candidate_authenticated" record = Some true
            && field_has_classification "remedy_class" String_field record
            && field_has_classification "candidate_authenticated" Bool_field
                 record)
          solve_records
      and solve_internal =
        List.exists
          (fun record ->
            record.level = Delator.Error
            && String.equal record.target
                 "Interface_specification_loaded_private"
            && field_string "stage" record = Some "internal-diagnostic")
          solve_records
      in
      let* () =
        require (solve_decision && not solve_internal)
          "ordinary solve failure lacks the dependency classification decision"
      in
      let* () =
        require
          (Verification_pipeline.For_testing.frontier_events ()
          = [ Verification_pipeline.For_testing.Materialized (0, "checked") ])
          "serial preparation failure did not retain exactly one materialization event"
      in
      let verify_provider () = verify ~dependencies:[ provider ] consumer in
      Verification_pipeline.For_testing.reset_frontier_events ();
      let provider_solve_result, provider_solve_records =
        capture_records Delator.Debug (fun () ->
            Verification_solver_private.For_testing.inject_preparation_error
              (Some 0);
            Fun.protect
              ~finally:(fun () ->
                Verification_solver_private.For_testing.inject_preparation_error
                  None)
              verify_provider)
      in
      let* provider_solve_error =
        match provider_solve_result with
        | Error error -> Ok error
        | Ok _ -> mismatch "injected provider solve failure did not fail"
      in
      let* () =
        require
          (Verifier_service.error_classification provider_solve_error
             = Verifier_service.Dependency_error)
          "authenticated provider solve failure was classified internal"
      in
      let* () =
        require
          (Verifier_service.error_unit_name provider_solve_error
             = Some "Classification_provider")
          "provider dependency error lost its structured unit identity"
      in
      let provider_solve_message =
        Verifier_service.error_message provider_solve_error
      in
      let* () =
        require
          ((not (contains provider_solve_message "Classification_provider"))
          && contains (String.lowercase_ascii provider_solve_message) "rebuild"
          && contains (String.lowercase_ascii provider_solve_message) "retry"
          && text_avoids_private_verifier_jargon provider_solve_message
          && List.for_all
               (fun value ->
                 value = "" || not (contains provider_solve_message value))
               private_values)
          "provider dependency message exposed raw detail or lacked its remedy"
      in
      let provider_solve_decision =
        List.exists
          (fun record ->
            record.level = Delator.Debug
            && String.equal record.target
                 "Interface_specification_loaded_private"
            && has_fields
                 [
                   "provider";
                   "stage";
                   "failure_class";
                   "candidate_authenticated";
                   "decision";
                   "remedy_class";
                 ]
                 record
            && field_string "provider" record
               = Some "Classification_provider"
            && field_string "stage" record
               = Some "verification-failure-classification"
            && field_string "failure_class" record
               = Some "verification-pipeline"
            && field_bool "candidate_authenticated" record = Some true
            && field_string "decision" record = Some "dependency"
            && field_string "remedy_class" record = Some "rebuild-and-retry"
            && field_has_classification "candidate_authenticated" Bool_field
                 record
            && field_has_classification "remedy_class" String_field record)
          provider_solve_records
      and provider_solve_internal =
        List.exists
          (fun record ->
            record.level = Delator.Error
            && String.equal record.target
                 "Interface_specification_loaded_private"
            && field_string "stage" record = Some "internal-diagnostic")
          provider_solve_records
      in
      let* () =
        require (provider_solve_decision && not provider_solve_internal)
          "provider solve failure lacks its dependency-only classification"
      in
      let* provider_source_ordinal =
        Verification_pipeline.For_testing.frontier_events ()
        |> List.find_map (function
             | Verification_pipeline.For_testing.Materialized
                 (source_ordinal, "identity") ->
                 Some source_ordinal
             | Materialized _ | Committed _ | Blocked _ -> None)
        |> function
        | Some source_ordinal -> Ok source_ordinal
        | None -> mismatch "provider identity was not materially scheduled"
      in
      let* () =
        let events = Verification_pipeline.For_testing.frontier_events () in
        let identity_materializations =
          List.filter
            (function
              | Verification_pipeline.For_testing.Materialized
                  (_, "identity") ->
                  true
              | Materialized _ | Committed _ | Blocked _ -> false)
            events
        in
        require
          (List.length identity_materializations = 1
          && not
               (List.exists
                  (function
                    | Verification_pipeline.For_testing.Committed _ -> true
                    | Materialized _ | Blocked _ -> false)
                  events))
          "parallel preparation failure did not retain exactly one pre-prepare materialization event"
      in
      let provider_internal_result, provider_internal_records =
        capture_records Delator.Debug (fun () ->
            Verification_pipeline.For_testing.inject_materialization_error
              (Some provider_source_ordinal);
            Fun.protect
              ~finally:(fun () ->
                Verification_pipeline.For_testing.inject_materialization_error
                  None)
              verify_provider)
      in
      let* provider_internal_error =
        match provider_internal_result with
        | Error error -> Ok error
        | Ok _ -> mismatch "injected provider invariant breach did not fail"
      in
      let* () =
        require
          (Verifier_service.error_classification provider_internal_error
             = Verifier_service.Internal_error
          && Option.is_none
               (Verifier_service.error_diagnostic provider_internal_error))
          "authenticated provider invariant breach was not internal"
      in
      let* () =
        require
          (Verifier_service.error_unit_name provider_internal_error
             = Some "Classification_provider")
          "provider internal error lost its structured unit identity"
      in
      let provider_internal_message =
        Verifier_service.error_message provider_internal_error
      in
      let* () =
        require
          ((not (contains provider_internal_message "Classification_provider"))
          && text_avoids_private_verifier_jargon provider_internal_message
          && List.for_all
               (fun value ->
                 value = "" || not (contains provider_internal_message value))
               private_values)
          "provider internal message exposed unit or private failure detail"
      in
      let provider_internal_event =
        List.exists
          (fun record ->
            record.level = Delator.Error
            && String.equal record.target
                 "Interface_specification_loaded_private"
            && has_fields
                 [
                   "provider";
                   "stage";
                   "failure_class";
                   "candidate_authenticated";
                   "remedy_class";
                 ]
                 record
            && field_string "provider" record
               = Some "Classification_provider"
            && field_string "stage" record = Some "internal-diagnostic"
            && field_string "failure_class" record
               = Some "post-validation-invariant"
            && field_bool "candidate_authenticated" record = Some true
            && field_string "remedy_class" record
               = Some "report-verifier-defect"
            && field_has_classification "candidate_authenticated" Bool_field
                 record
            && field_has_classification "remedy_class" String_field record)
          provider_internal_records
      and provider_internal_dependency =
        List.exists
          (fun record ->
            record.level = Delator.Debug
            && String.equal record.target
                 "Interface_specification_loaded_private"
            && field_string "stage" record
               = Some "verification-failure-classification")
          provider_internal_records
      in
      let* () =
        require (provider_internal_event && not provider_internal_dependency)
          "provider invariant breach did not take the internal-only route"
      in
      let info_result, info_records =
        capture_records Delator.Info (fun () ->
            Verification_solver_private.For_testing.inject_preparation_error
              (Some 0);
            Fun.protect
              ~finally:(fun () ->
                Verification_solver_private.For_testing.inject_preparation_error
                  None)
              (fun () -> verify retained))
      in
      let* info_error =
        match info_result with
        | Error error -> Ok error
        | Ok _ -> mismatch "info-level solve injection did not fail"
      in
      let info_classification_leaked =
        List.exists
          (fun record ->
            String.equal record.target
              "Interface_specification_loaded_private"
            && field_string "stage" record
               = Some "verification-failure-classification")
          info_records
      in
      let* () =
        require
          (Verifier_service.error_classification info_error
             = Verifier_service.Dependency_error
          && not info_classification_leaked)
          "debug dependency classification leaked at the default info level"
      in
      let* () =
        require
          (records_redact private_values
             (unauthenticated_records @ solve_records
            @ provider_solve_records @ provider_internal_records @ info_records))
          "classification instrumentation exposed a private field"
      in
      let* () = check_lifecycle () in
      Ok
        (Outcome.merge
           [ ordinary_outcome; retained_outcome; provider_outcome ]))

let () =
  ignore Symbolic_foundations_prerequisites.ready;
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [
      semantic_matrix_case;
      symbolic_counterexample_case;
      recursive_symbolic_source_case;
      identity_parity_case;
      dependency_mismatch_case;
      receipt_and_artifact_case;
      collision_delator_case;
      collision_route_matrix_case;
      collision_trust_asymmetry_case;
      nullary_analysis_case;
      signature_rejection_case;
      source_diagnostic_case;
      executable_symbolic_lambda_case;
      internal_diagnostic_case;
      non_internal_classification_case;
    ]
