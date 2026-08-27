open Asttypes
open Typedtree

let () = ignore Proof_region_modes_prerequisites.ready

module Capture =
  Typedtree_adapter_private.Public.Proof_region_capture_for_testing

let fail format = Printf.ksprintf failwith format

let mode_name = function
  | Sst.Exec_instance -> "Exec"
  | Sst.Tracked_instance -> "Tracked"
  | Sst.Ghost_instance -> "Ghost"

let uniqueness_name = function
  | Sst.Definitely_unique -> "unique"
  | Sst.Definitely_aliased -> "aliased"

let load filename =
  match Cmt_input.load filename with
  | Ok implementation -> implementation
  | Error diagnostic ->
      fail "input:%s:%s" diagnostic.Diagnostic.code diagnostic.message

let lower implementation =
  match Typedtree_lowering.lower implementation with
  | Ok program -> program
  | Error diagnostic ->
      fail "adapter:%s:%s" diagnostic.Diagnostic.code diagnostic.message

let validate program =
  match Instance_mode.validate program with
  | Ok environment -> environment
  | Error error -> fail "modes:%s" error.Instance_mode.message

let reset () =
  Capture.reset ();
  Sst_validation.For_testing.reset_ghost_formal_flow_count ();
  Recursive_spec_encoding.For_testing.reset_recursive_lowering_count ();
  Solver_backend.For_testing.reset_solver_creation_count ();
  Z3_bridge.reset_counters ()

type retained_carrier = {
  manifest : string;
  marker : expression;
  marker_argument : expression;
  sidecar : expression;
  sidecar_marker : expression;
  sidecar_argument : expression;
  ignored_sidecar : expression;
  sidecar_function : expression;
  shadow_parameters : function_param list;
  proof_application : expression;
  proof_id_argument : expression;
  proof_thunk : expression;
  proof_body : expression;
}

let string_argument = function
  | { exp_desc = Texp_constant (Const_string (value, _, _)); _ } -> Some value
  | _ -> None

let retained_carrier marker sidecar =
  match marker.exp_desc with
  | Texp_apply
      ( _,
        [ (Nolabel, Arg (marker_argument, _)) ],
        _,
        _,
        _ ) -> (
      match (string_argument marker_argument, sidecar.exp_desc) with
      | ( Some manifest,
          Texp_ifthenelse
            ( _,
              ({
                 exp_desc =
                   Texp_apply
                     ( _,
                       [ (Nolabel, Arg (sidecar_function, _)) ],
                       _,
                       _,
                       _ );
                 _;
               } as ignored_sidecar),
              Some _ ) )
        when
          String.starts_with ~prefix:"verocaml:proof-region-capture:"
            manifest -> (
          match sidecar_function.exp_desc with
          | Texp_function
              {
                params = shadow_parameters;
                body =
                  Tfunction_body
                    {
                      exp_desc =
                        Texp_sequence (sidecar_marker, _, proof_application);
                      _;
                    };
                _;
              } -> (
              match (sidecar_marker.exp_desc, proof_application.exp_desc) with
              | ( Texp_apply
                    ( _,
                      [ (Nolabel, Arg (sidecar_argument, _)) ],
                      _,
                      _,
                      _ ),
                  Texp_apply
                    ( _,
                      [
                        (Nolabel, Arg (proof_id_argument, _));
                        (Nolabel, Arg (proof_thunk, _));
                      ],
                      _,
                      _,
                      _ ) ) -> (
                  match proof_thunk.exp_desc with
                  | Texp_function
                      {
                        body = Tfunction_body proof_body;
                        _;
                      }
                    when string_argument sidecar_argument = Some manifest ->
                      Some
                        {
                          manifest;
                          marker;
                          marker_argument;
                          sidecar;
                          sidecar_marker;
                          sidecar_argument;
                          ignored_sidecar;
                          sidecar_function;
                          shadow_parameters;
                          proof_application;
                          proof_id_argument;
                          proof_thunk;
                          proof_body;
                        }
                  | _ -> None)
              | _ -> None)
          | _ -> None)
      | _ -> None)
  | _ -> None

let retained_carriers structure =
  let carriers = ref [] in
  let default = Tast_iterator.default_iterator in
  let iterator =
    {
      default with
      expr =
        (fun self expression ->
          (match expression.exp_desc with
          | Texp_sequence
              ( {
                  exp_desc = Texp_sequence (marker, _, sidecar);
                  _;
                },
                _,
                _ ) -> (
              match retained_carrier marker sidecar with
              | Some carrier -> carriers := carrier :: !carriers
              | None -> ())
          | _ -> ());
          default.expr self expression);
    }
  in
  iterator.structure iterator structure;
  List.rev !carriers

let hex value =
  let buffer = Buffer.create (String.length value * 2) in
  String.iter
    (fun character ->
      Buffer.add_string buffer (Printf.sprintf "%02x" (Char.code character)))
    value;
  Buffer.contents buffer

let carrier_for_callable carriers callable =
  let field = "callable=" ^ hex callable in
  match
    List.filter
      (fun carrier ->
        List.exists (String.equal field)
          (String.split_on_char '|' carrier.manifest))
      carriers
  with
  | [ carrier ] -> carrier
  | matches ->
      fail "expected one %s carrier, found %d" callable (List.length matches)

let pattern_parameter = function
  | { fp_kind = Tparam_pat pattern; _ } -> pattern
  | _ -> fail "expected a pattern parameter"

let first_shadow carrier =
  match carrier.shadow_parameters with
  | parameter :: _ -> (parameter, pattern_parameter parameter)
  | [] -> fail "expected a shadow parameter"

let source_pattern structure name =
  let matches = ref [] in
  let default = Tast_iterator.default_iterator in
  let iterator =
    {
      default with
      pat =
        (fun (type k) self (pattern : k general_pattern) ->
          (match pattern.pat_desc with
          | Tpat_var (ident, source_name, _, _, mode)
            when
              not pattern.pat_loc.loc_ghost
              && String.equal source_name.txt name ->
              matches := (ident, pattern.pat_type, mode) :: !matches
          | _ -> ());
          default.pat self pattern);
    }
  in
  iterator.structure iterator structure;
  match !matches with
  | [ source ] -> source
  | matches ->
      fail "expected one source pattern named %s, found %d" name
        (List.length matches)

let source_occurrence structure ident =
  let matches = ref [] in
  let default = Tast_iterator.default_iterator in
  let iterator =
    {
      default with
      expr =
        (fun self expression ->
          (match expression.exp_desc with
          | Texp_ident (Path.Pident candidate, _, _, _, _)
            when Ident.same ident candidate ->
              matches := expression :: !matches
          | _ -> ());
          default.expr self expression);
    }
  in
  iterator.structure iterator structure;
  match !matches with
  | [ occurrence ] -> occurrence
  | matches ->
      fail "expected one live source occurrence, found %d"
        (List.length matches)

let shadow_occurrence carrier =
  let _, pattern = first_shadow carrier in
  let shadow =
    match pattern.pat_desc with
    | Tpat_var (ident, _, _, _, _) -> ident
    | _ -> fail "expected a variable shadow"
  in
  let matches = ref [] in
  let default = Tast_iterator.default_iterator in
  let iterator =
    {
      default with
      expr =
        (fun self expression ->
          (match expression.exp_desc with
          | Texp_ident (Path.Pident candidate, _, _, _, _)
            when Ident.same shadow candidate ->
              matches := expression :: !matches
          | _ -> ());
          default.expr self expression);
    }
  in
  iterator.expr iterator carrier.proof_body;
  match !matches with
  | [ occurrence ] -> occurrence
  | matches ->
      fail "expected one shadow occurrence, found %d" (List.length matches)

let rewrite_parameter_type parameter source_type =
  match parameter.fp_kind with
  | Tparam_pat pattern ->
      {
        parameter with
        fp_kind = Tparam_pat { pattern with pat_type = source_type };
      }
  | Tparam_optional_default _ -> fail "expected a pattern parameter"

let rewrite_parameter_mode parameter source_mode =
  match parameter.fp_kind with
  | Tparam_pat pattern -> (
      match pattern.pat_desc with
      | Tpat_var (ident, name, uid, sort, _) ->
          {
            parameter with
            fp_kind =
              Tparam_pat
                {
                  pattern with
                  pat_desc = Tpat_var (ident, name, uid, sort, source_mode);
                };
          }
      | _ -> fail "expected a variable shadow")
  | Tparam_optional_default _ -> fail "expected a pattern parameter"

let require_genuine_ghost_carrier carrier =
  let components =
    [
      carrier.marker;
      carrier.marker_argument;
      carrier.sidecar;
      carrier.sidecar_marker;
      carrier.sidecar_argument;
      carrier.sidecar_function;
      carrier.proof_application;
      carrier.proof_id_argument;
      carrier.proof_thunk;
    ]
  in
  if
    not
      (List.for_all
         (fun expression -> expression.exp_loc.Location.loc_ghost)
         components)
  then fail "retained carrier did not originate at genuine ghost locations"

let rewrite_retained_structure attack structure =
  let carriers = retained_carriers structure in
  List.iter require_genuine_ghost_carrier carriers;
  let zero = carrier_for_callable carriers "global_only" in
  let target = carrier_for_callable carriers "unique_return" in
  let type_donor = carrier_for_callable carriers "mode_locals" in
  let target_parameter, _ = first_shadow target in
  let _, donor_pattern = first_shadow type_donor in
  let zero_parameter, _ = first_shadow zero in
  let source_ident, _, source_mode = source_pattern structure "result" in
  let live_source = source_occurrence structure source_ident in
  let live_shadow = shadow_occurrence target in
  let changes = ref 0 in
  let changed expression =
    incr changes;
    expression
  in
  let rewrite_function mapped parameters =
    match mapped.exp_desc with
    | Texp_function function_ ->
        changed
          {
            mapped with
            exp_desc = Texp_function { function_ with params = parameters };
          }
    | _ -> fail "target sidecar function changed shape"
  in
  let mapper =
    {
      Tast_mapper.default with
      expr =
        (fun self expression ->
          let mapped = Tast_mapper.default.expr self expression in
          match attack with
          | "zero-capture-substitution"
            when expression == zero.sidecar_function ->
              rewrite_function mapped [ target_parameter ]
          | "unit-padded-nonzero"
            when expression == target.sidecar_function ->
              rewrite_function mapped
                (target.shadow_parameters @ [ zero_parameter ])
          | "wrong-shadow-type"
            when expression == target.sidecar_function ->
              rewrite_function mapped
                [
                  rewrite_parameter_type target_parameter
                    donor_pattern.pat_type;
                ]
          | "wrong-shadow-mode"
            when expression == target.sidecar_function ->
              rewrite_function mapped
                [ rewrite_parameter_mode target_parameter source_mode ]
          | "live-marker-manifest"
            when expression == target.marker_argument ->
              changed { mapped with exp_desc = live_source.exp_desc }
          | "live-sidecar-manifest"
            when expression == target.sidecar_argument ->
              changed { mapped with exp_desc = live_source.exp_desc }
          | "live-region-id"
            when expression == target.proof_id_argument ->
              changed { mapped with exp_desc = live_source.exp_desc }
          | "live-thunk-argument" when expression == live_shadow ->
              changed { mapped with exp_desc = live_source.exp_desc }
          | ("applied-sidecar" | "escaping-sidecar")
            when expression == target.ignored_sidecar -> (
              match mapped.exp_desc with
              | Texp_apply
                  ( _,
                    [ (Nolabel, Arg (mapped_sidecar, argument_sort)) ],
                    position,
                    locality,
                    zero_alloc ) ->
                  if String.equal attack "applied-sidecar" then
                    changed
                      {
                        mapped with
                        exp_desc =
                          Texp_apply
                            ( mapped_sidecar,
                              [
                                ( Nolabel,
                                  Arg (live_source, argument_sort) );
                              ],
                              position,
                              locality,
                              zero_alloc );
                      }
                  else
                    changed
                      {
                        mapped with
                        exp_desc = mapped_sidecar.exp_desc;
                        exp_type = mapped_sidecar.exp_type;
                      }
              | _ -> fail "ignored sidecar changed shape")
          | _ -> mapped);
    }
  in
  let structure = mapper.structure mapper structure in
  if !changes <> 1 then
    fail "attack %s made %d changes instead of one" attack !changes;
  structure

let rewrite_retained_cmt attack input output =
  match Cmt_format.read input with
  | Some cmi, Some cmt -> (
      match cmt.Cmt_format.cmt_annots with
      | Cmt_format.Implementation structure ->
          let cmt =
            {
              cmt with
              Cmt_format.cmt_annots =
                Implementation (rewrite_retained_structure attack structure);
            }
          in
          let channel = open_out_bin output in
          Fun.protect
            ~finally:(fun () -> close_out channel)
            (fun () ->
              ignore (Cmi_format.output_cmi output channel cmi);
              output_string channel Config.cmt_magic_number;
              Marshal.to_channel channel cmt [])
      | _ -> fail "%s is not an implementation CMT" input)
  | None, _ | _, None -> fail "%s has no embedded CMI/CMT pair" input

let reload_cmi filename =
  let cmi =
    try Cmi_format.read_cmi_lazy filename
    with Cmi_format.Error _ -> fail "%s is not a readable CMI" filename
  in
  Printf.printf "cmi=%s imports=%d\n"
    (Compilation_unit.Name.to_string cmi.Cmi_format.cmi_name)
    (Array.length cmi.cmi_crcs)

let print_counters () =
  let counters =
    Capture.counters ()
  in
  let z3 = Z3_bridge.counters () in
  Printf.printf
    "capture-issued=%d remapped=%d proof-sst=%d forgetting=%d recursive=%d backend=%d z3=%d/%d\n"
    counters.capture_issuances counters.capture_remappings
    counters.proof_region_sst_nodes
    (Sst_validation.For_testing.ghost_formal_flow_count ())
    (Recursive_spec_encoding.For_testing.recursive_lowering_count ())
    (Solver_backend.For_testing.solver_creation_count ())
    z3.contexts_created z3.solvers_created

let observe filename =
  reset ();
  let program = lower (load filename) in
  let modes = validate program in
  let observations =
    Capture.observations program modes
  in
  List.iter
    (fun (observation : Capture.observation) ->
      Printf.printf
        "capture callable=%s#%d region=%d-%d binding=%s#%d mode=%s uniqueness=%s synthetic=ghost:%d/tracked:%d\n"
        observation.callable.function_name observation.callable.function_index
        observation.region_start observation.region_end observation.binding_name
        observation.binding_id
        (mode_name observation.incoming_mode)
        (uniqueness_name observation.binding_uniqueness)
        observation.synthetic_ghost_descriptors
        observation.synthetic_tracked_descriptors)
    observations;
  print_string (Instance_mode.to_string modes);
  print_newline ();
  print_counters ()

let reject ~zero_capture_work filename =
  reset ();
  let disposition =
    match Typedtree_lowering.lower (load filename) with
    | Error diagnostic ->
        Printf.printf "rejected=%s\n" diagnostic.Diagnostic.code;
        "adapter"
    | Ok program -> (
        match Sst_validation.validate program with
        | Error _ ->
            print_endline "rejected=semantic";
            "semantic"
        | Ok _ -> fail "attack accepted")
  in
  if zero_capture_work && String.equal disposition "adapter" then (
    let counters =
      Capture.counters ()
    in
    if
      counters.capture_issuances <> 0 || counters.capture_remappings <> 0
      || counters.proof_region_sst_nodes <> 0
    then fail "adapter rejection crossed capture issuance/remapping/SST");
  print_counters ()

let raw filename =
  reset ();
  let implementation = load filename in
  (match
     Typedtree_adapter.lower ~source_file:implementation.source_file
       ~imports:implementation.imports implementation.structure
   with
  | Error diagnostic ->
      Printf.printf "raw-rejected=%s\n" diagnostic.Diagnostic.code
  | Ok _ -> fail "raw Typedtree accepted a retained proof carrier");
  print_counters ()

let driver_reject filename =
  reset ();
  let implementation = load filename in
  let boundary =
    match
      Verification_driver_private.run ~timeout_ms:5_000
        ~allow_imported_opens:false implementation
    with
    | Error (Verification_driver_private.Frontend_error _) -> "frontend"
    | Error (Verification_driver_private.Validation_error _) -> "validation"
    | Error (Verification_driver_private.Invariant_error _) -> "invariant"
    | Error (Verification_driver_private.Pipeline_error _) -> "pipeline"
    | Error (Verification_driver_private.Internal_error _) -> "internal"
    | Ok _ -> fail "semantic authority attack was accepted"
  in
  Printf.printf "driver-rejected=%s\n" boundary;
  print_counters ()

let driver_control filename =
  reset ();
  match
    Verification_driver_private.run ~timeout_ms:5_000
      ~allow_imported_opens:false (load filename)
  with
  | Ok report ->
      let status =
        match Verification_driver_private.status report with
        | Verification_pipeline.Verified -> "verified"
        | Counterexample -> "counterexample"
        | Inconclusive -> "inconclusive"
        | Incomplete_source -> "incomplete"
      in
      Printf.printf "driver-status=%s functions=%d obligations=%d\n" status
        (Verification_driver_private.functions report)
        (Verification_driver_private.obligations report);
      print_counters ()
  | Error _ -> fail "semantic authority control did not produce a report"

let driver_transition_result filename =
  reset ();
  match
    Verification_driver_private.run ~timeout_ms:5_000
      ~allow_imported_opens:false (load filename)
  with
  | Error _ ->
      fail "completed transition result did not reach the caller proof"
  | Ok report ->
      let counters = Verification_driver_private.counters report in
      let capture = Capture.counters () in
      let z3 = Z3_bridge.counters () in
      let preservation_results =
        Verification_driver_private.results report
        |> List.filter (fun result ->
               match
                 (result.Solver_backend.obligation.Vir.kind, result.outcome)
               with
               | ( Vir.Invariant_validity
                     { boundary = Vir.Transition_preservation _; _ },
                   Solver_backend.Verified ) ->
                   true
               | ( Vir.Invariant_validity _
                 | Vir.Arithmetic_safety _ | Vir.Assertion _
                 | Vir.Local_assertion _ | Vir.Postcondition _
                 | Vir.Call_precondition _ | Vir.Callback_precondition _
                 | Vir.Entry_measure_nonnegative _
                 | Vir.Recursive_call_measure_nonnegative _
                 | Vir.Recursive_call_strict_descent _ ),
                 ( Solver_backend.Verified
                 | Solver_backend.Counterexample _
                 | Solver_backend.Inconclusive _ ) ->
                   false)
      in
      if
        Verification_driver_private.status report
        <> Verification_pipeline.Verified
        || Verification_driver_private.functions report <> 4
        || Verification_driver_private.obligations report <> 7
        || List.length preservation_results <> 1
        || counters.transition_predecessor_transfers <> 1
        || counters.transition_predecessor_consumptions <> 1
        || counters.transition_result_receipts <> 1
        || counters.transition_teardown_removals <> 1
        || counters.transition_preservation_obligations <> 1
        || counters.transition_nested_reconstructions <> 1
        || counters.transition_root_reconstructions <> 0
        || counters.dependent_lowerings <> 2
        || counters.dependent_backend_contexts <> 2
        || counters.dependent_solver_attempts <> 5
        || counters.receipts_issued <> 2 || counters.receipts_consumed <> 2
        || counters.callee_solver_attempts <> 5
        || counters.callee_verified_results <> 5
        || counters.callee_failed_results <> 0
        || capture.capture_issuances <> 1
        || capture.capture_remappings <> 1
        || capture.proof_region_sst_nodes <> 1
        || Sst_validation.For_testing.ghost_formal_flow_count () <> 1
        || Recursive_spec_encoding.For_testing.recursive_lowering_count () <> 0
        || Solver_backend.For_testing.solver_creation_count () <> 7
        || z3.contexts_created <> 7 || z3.solvers_created <> 7
      then fail "completed transition result counter boundary changed";
      Printf.printf
        "driver-transition-result status=%s functions=%d obligations=%d preservation-results=%d\n"
        (match Verification_driver_private.status report with
        | Verification_pipeline.Verified -> "verified"
        | Counterexample -> "counterexample"
        | Inconclusive -> "inconclusive"
        | Incomplete_source -> "incomplete")
        (Verification_driver_private.functions report)
        (Verification_driver_private.obligations report)
        (List.length preservation_results);
      Printf.printf
        "transition lifecycle=%d/%d/%d/%d preservation=%d reconstruction=%d/%d\n"
        counters.transition_predecessor_transfers
        counters.transition_predecessor_consumptions
        counters.transition_result_receipts
        counters.transition_teardown_removals
        counters.transition_preservation_obligations
        counters.transition_nested_reconstructions
        counters.transition_root_reconstructions;
      Printf.printf
        "dependency lowering/backend/solver=%d/%d/%d receipts-issued/consumed=%d/%d callee-verified/failed=%d/%d\n"
        counters.dependent_lowerings counters.dependent_backend_contexts
        counters.dependent_solver_attempts counters.receipts_issued
        counters.receipts_consumed counters.callee_verified_results
        counters.callee_failed_results;
      print_counters ()

let solve filename =
  reset ();
  let program = lower (load filename) in
  match Symbolic_executor.lower_program program with
  | Error error -> fail "%s" (Symbolic_executor.error_to_string error)
  | Ok vir ->
      let config =
        match Solver_backend.config ~timeout_ms:5_000 with
        | Ok config -> config
        | Error error -> fail "%s" (Solver_backend.error_to_string error)
      in
      List.iter
        (fun execution ->
          match
            Solver_backend.solve_in_order config execution.Vir.obligations
          with
          | Ok results
            when
              List.for_all
                (fun result ->
                  match result.Solver_backend.outcome with
                  | Verified -> true
                  | Counterexample _ | Inconclusive _ -> false)
                results ->
              Printf.printf "%s=verified:%d\n"
                execution.function_ref.function_name
                (List.length execution.obligations)
          | Ok _ -> fail "%s was not verified" execution.function_ref.function_name
          | Error error -> fail "%s" (Solver_backend.error_to_string error))
        vir.functions;
      print_counters ()

let direct_solve filename =
  reset ();
  let program = lower (load filename) in
  let vir =
    match Symbolic_executor.lower_program program with
    | Ok vir -> vir
    | Error error -> fail "%s" (Symbolic_executor.error_to_string error)
  in
  let obligation =
    match
      List.find_map
        (fun execution ->
          match execution.Vir.obligations with
          | obligation :: _ -> Some obligation
          | [] -> None)
        vir.functions
    with
    | Some obligation -> obligation
    | None -> fail "direct-Z3 control has no obligation"
  in
  (match
     Z3_bridge.solve_vir { timeout_ms = 5_000; model = true } obligation
   with
  | Ok Z3_bridge.Verified -> print_endline "direct-z3=verified"
  | Ok (Counterexample _) | Ok (Inconclusive _) ->
      fail "direct-Z3 control was not verified"
  | Error error -> fail "%s" (Z3_bridge.error_to_string error));
  print_counters ()

let () =
  match Array.to_list Sys.argv with
  | [ _; "observe"; filename ] -> observe filename
  | [ _; "reject"; filename ] -> reject ~zero_capture_work:true filename
  | [ _; "semantic-reject"; filename ] ->
      reject ~zero_capture_work:false filename
  | [ _; "driver-reject"; filename ] -> driver_reject filename
  | [ _; "driver-control"; filename ] -> driver_control filename
  | [ _; "driver-transition-result"; filename ] ->
      driver_transition_result filename
  | [ _; "raw"; filename ] -> raw filename
  | [ _; "solve"; filename ] -> solve filename
  | [ _; "direct-solve"; filename ] -> direct_solve filename
  | [ _; "rewrite-retained"; attack; input; output ] ->
      rewrite_retained_cmt attack input output
  | [ _; "reload-cmi"; filename ] -> reload_cmi filename
  | _ ->
      fail
        "usage: proof_region_modes_tool \
         (observe|reject|semantic-reject|driver-reject|driver-control|driver-transition-result|raw|solve|direct-solve) \
         FILE | proof_region_modes_tool rewrite-retained ATTACK INPUT OUTPUT | \
         proof_region_modes_tool reload-cmi FILE"
