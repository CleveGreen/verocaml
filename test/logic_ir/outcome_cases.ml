open Outcome_test_support

let suite_path = "test/logic_ir/outcome_cases.ml"

let run_fixture input ~environment ~workspace =
  Fixture.run ~environment ~workspace input

let verified_logic_case =
  let input =
    Fixture.single_source ~module_name:"Logic_identity"
      ~source:"[@@@verocaml.verify]\nlet identity (value : int) = value\n"
      ~libraries:[ "verocaml.ghost" ]
  in
  Suite.case ~name:"typed-identity-verifies"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_unit "Logic_identity" Outcome.Unit_verified
      |> Expectation.require_named_fact "function:identity"
           (Outcome.Function_exists "identity"))
    (run_fixture input)

let assertion_logic_case =
  let input =
    Fixture.single_source ~module_name:"Logic_assertion_failure"
      ~source:
        "[@@@verocaml.verify]\nlet rejected value =\n  \
         [%verocaml.assert false];\n  value\n"
      ~libraries:[ "verocaml.ghost" ]
  in
  Suite.case ~name:"assertion-counterexample"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Counterexample
      |> Expectation.require_unit "Logic_assertion_failure"
           Outcome.Unit_counterexample
      |> Expectation.require_semantic ~function_name:"rejected"
           Outcome.Assertion)
    (run_fixture input)

let span =
  Diagnostic.
    {
      file = "logic_ir_outcome.ml";
      start_pos = { line = 1; column = 0 };
      end_pos = { line = 1; column = 1 };
    }

let ok label = function
  | Ok value -> value
  | Error message -> failwith (label ^ ": " ^ message)

let logic_ok label = function
  | Ok value -> value
  | Error error -> failwith (label ^ ": " ^ Logic_ir.error_to_string error)

let target_and_bound_bv_width ?(physical_width = 32) text =
  let profile_capability = Build_target_profile_private.capability () in
  let profile =
    ok "sealed profile"
      (Build_target_profile_private.authenticate_profile profile_capability)
  and targets =
    ok "sealed targets"
      (Build_target_profile_private.authenticate_instances profile_capability)
  in
  let target =
    List.find
      (fun target ->
        target.Build_target_profile_private.target_claim.width = physical_width)
      targets
  in
  let capability = Bv_backend_capability_receipt_private.capability () in
  let width = ok "BV width" (Bv_width.of_string ~profile capability text) in
  (target,
   ok "target-bound BV width" (Bv_width.for_instance capability target width))

let bound_bv_width text = snd (target_and_bound_bv_width text)

let test_projection_authority ~target ~width ~provenance ~outer_authority
    ~modular_authority ~trusted_dependencies ~source_full_key
    ~occurrence_full_key ~operation_span =
  let operation =
    Numeric_bv_projection_evidence_private.For_source_admission.operation
  in
  let outer_operation =
    operation ~descriptor_origin:"logic-ir-test"
      ~occurrence_identity:(occurrence_full_key ^ ":outer")
      ~call_span:operation_span ~callable_uid:(source_full_key ^ ":outer-uid")
      ~callable_tag:"unsigned-view" ~callable_abi:"test-outer-abi"
      ~law_evidence_full_key:(source_full_key ^ ":outer-law")
      ~authority:outer_authority
  and modular_operation =
    operation ~descriptor_origin:"logic-ir-test"
      ~occurrence_identity:(occurrence_full_key ^ ":modular")
      ~call_span:operation_span
      ~callable_uid:(source_full_key ^ ":modular-uid")
      ~callable_tag:"modular-conversion" ~callable_abi:"test-modular-abi"
      ~law_evidence_full_key:(source_full_key ^ ":modular-law")
      ~authority:modular_authority
  in
  Numeric_bv_projection_evidence_private.For_source_admission.issue ~target
    ~width ~provenance ~trusted_dependencies
    ~occurrence_identity:occurrence_full_key ~outer_operation
    ~modular_operation ~carrier_binding_full_key:"test-carrier-binding"
    ~base_int_full_key:"test-base-int" ~source_witness_full_key:source_full_key
    ~occurrence_full_key

let run_worker_request request =
  let result = Portable.Atomic_array.create ~len:1 None in
  let scheduler =
    Parallel_scheduler.create ~max_domains:(min 2 (Multicore.max_domains ())) ()
  in
  Fun.protect
    ~finally:(fun () -> Parallel_scheduler.stop scheduler)
    (fun () ->
      Parallel_scheduler.parallel scheduler ~f:(fun parallel ->
          Parallel_kernel.for_ parallel ~start:0 ~stop:1 ~f:(fun _ _ ->
              Portable.Atomic_array.set result 0
                (Some (Function_vc_worker_private.run request)))));
  match Portable.Atomic_array.get result 0 with
  | Some result -> result
  | None -> failwith "detached BV worker did not join"

let solve_on_detached_worker detached =
  run_worker_request
    Function_vc_worker_private.
      { source_ordinal = 0;
        vcs =
          [ { canonical_index = 0; timeout_ms = 10_000; rlimit = 1_000_000;
              route = Ordinary detached } ] }

let solve_detached_attempt_on_worker scheduler ?fault ~controlled ~timeout_ms
    ~rlimit ~model query =
  let result = Portable.Atomic_array.create ~len:1 None in
  Parallel_scheduler.parallel scheduler ~f:(fun parallel ->
      Parallel_kernel.for_ parallel ~start:0 ~stop:1 ~f:(fun _ _ ->
          let attempt =
            match fault with
            | None ->
                Z3_bridge.solve_detached_query_local ~controlled ~timeout_ms
                  ~rlimit ~model query
            | Some fault ->
                Z3_bridge.For_testing.solve_detached_query_local_with_model_fault
                  ~fault ~controlled ~timeout_ms ~rlimit ~model query
          in
          Portable.Atomic_array.set result 0 (Some attempt)));
  match Portable.Atomic_array.get result 0 with
  | Some attempt -> attempt
  | None -> failwith "portable detached BV attempt did not join"

let native_bv_kernel_case =
  Suite.case ~name:"closed-native-bv-kernel-direct-and-detached"
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
    (fun ~environment:_ ~workspace:_ ->
      try
        let width = bound_bv_width "8" in
        let literal value =
          Bv_value.of_z ~width (Z.of_int value)
          |> ok "BV literal value"
          |> Logic_ir.bv_literal ~span
          |> logic_ok "BV literal term"
        in
        let int value = Logic_ir.int ~span (Z.of_int value) in
        let equal left right = Logic_ir.equal ~span left right |> logic_ok "equality" in
        let binary constructor left right =
          constructor ~span left right |> logic_ok "BV binary operation"
        in
        let zero = literal 0 and one = literal 1 and four = literal 4 in
        let max = literal 255 and high = literal 128 and signed_max = literal 127 in
        let a = literal 170 and b = literal 204 in
        let other_width = bound_bv_width "16" in
        let other =
          Bv_value.of_z ~width:other_width Z.zero
          |> ok "other-width BV literal value"
          |> Logic_ir.bv_literal ~span
          |> logic_ok "other-width BV literal term"
        in
        let contexts_before_mismatch = (Z3_bridge.counters ()).contexts_created in
        if Result.is_ok (Logic_ir.bv_add_mod ~span zero other) then
          failwith "unequal-width BV operands acquired one native operation";
        if (Z3_bridge.counters ()).contexts_created <> contexts_before_mismatch then
          failwith "unequal-width BV rejection created a solver context";
        let assertions =
          [ binary Logic_ir.bv_eq one one;
            binary Logic_ir.bv_distinct zero one;
            equal (binary Logic_ir.bv_add_mod (literal 250) (literal 10)) four;
            equal (binary Logic_ir.bv_sub_mod zero one) max;
            equal (Logic_ir.bv_not ~span zero |> logic_ok "BV not") max;
            equal (binary Logic_ir.bv_and a b) (literal 136);
            equal (binary Logic_ir.bv_or a b) (literal 238);
            equal (binary Logic_ir.bv_xor a b) (literal 102);
            binary Logic_ir.bv_ult zero one;
            binary Logic_ir.bv_ule one one;
            binary Logic_ir.bv_ugt one zero;
            binary Logic_ir.bv_uge one one;
            binary Logic_ir.bv_slt high signed_max;
            binary Logic_ir.bv_sle high high;
            binary Logic_ir.bv_sgt signed_max high;
            binary Logic_ir.bv_sge signed_max signed_max;
            (Logic_ir.not_ ~span (binary Logic_ir.bv_ult one one)
            |> logic_ok "strict unsigned less-than");
            (Logic_ir.not_ ~span (binary Logic_ir.bv_ugt one one)
            |> logic_ok "strict unsigned greater-than");
            (Logic_ir.not_ ~span (binary Logic_ir.bv_slt high high)
            |> logic_ok "strict signed less-than");
            (Logic_ir.not_ ~span (binary Logic_ir.bv_sgt high high)
            |> logic_ok "strict signed greater-than");
            equal
              (Logic_ir.bv_to_int_unsigned ~span max
              |> logic_ok "unsigned BV view")
              (int 255);
            equal
              (Logic_ir.bv_to_int_signed ~span max
              |> logic_ok "signed BV view")
              (int (-1));
            equal
              (Logic_ir.int_to_bv_mod ~span ~width (int (-1))
              |> logic_ok "Int to BV")
              max ]
        in
        let all = Logic_ir.and_ ~span assertions |> logic_ok "BV conjunction" in
        let contradiction = Logic_ir.not_ ~span all |> logic_ok "BV negation" in
        let builder = Logic_ir.create () in
        let query =
          Logic_ir.query builder ~axioms:[] ~assertions:[ contradiction ]
            ~requires:[] ~span
          |> logic_ok "BV kernel query"
        in
        if
          not
            (List.mem Logic_ir.Bit_vectors (Logic_ir.requirements query)
            && List.mem Logic_ir.Int_bitvector_conversions
                 (Logic_ir.requirements query))
        then failwith "BV kernel omitted structural capability requirements";
        Z3_bridge.reset_counters ();
        (match
           Z3_bridge.solve_query { timeout_ms = 10_000; model = true } query
         with
        | Ok Z3_bridge.Verified -> ()
        | Ok _ -> failwith "direct BV kernel query was not verified"
        | Error error -> failwith (Z3_bridge.error_to_string error));
        let direct = Z3_bridge.counters () in
        if
          direct.contexts_created <> 1 || direct.contexts_cleaned <> 1
          || direct.contexts_live <> 0 || direct.selected_logics <> [ "general" ]
        then failwith "direct BV kernel lifecycle or solver seam changed";
        let detached = Z3_bridge.detach_query query in
        let detached_attempt =
          Z3_bridge.solve_detached_query_local ~controlled:Z3_bridge.Real
            ~timeout_ms:10_000 ~rlimit:1_000_000 ~model:true detached
        in
        (match detached_attempt.detached_result with
        | Ok Z3_bridge.Detached_verified -> ()
        | Ok _ -> failwith "detached BV kernel query was not verified"
        | Error message -> failwith message);
        let telemetry = detached_attempt.detached_telemetry in
        if
          telemetry.contexts_created <> 1 || telemetry.contexts_cleaned <> 1
          || telemetry.contexts_live <> 0
          || telemetry.selected_logics <> [ "general" ]
        then failwith "detached BV kernel lifecycle or solver seam changed";
        List.iter
          (fun defect ->
            let malformed =
              Z3_bridge.For_testing.malformed_detached_bv_query ~width
                ~other_width defect
            in
            let attempt =
              Z3_bridge.solve_detached_query_local ~controlled:Z3_bridge.Real
                ~timeout_ms:10_000 ~rlimit:1_000_000 ~model:true malformed
            in
            if Result.is_ok attempt.detached_result then
              failwith "malformed detached BV plan passed typed preflight";
            if
              attempt.detached_telemetry.contexts_created <> 0
              || attempt.detached_telemetry.contexts_cleaned <> 0
              || attempt.detached_telemetry.contexts_live <> 0
            then failwith "malformed detached BV plan allocated a context")
          [ Z3_bridge.For_testing.Mixed_bv_add_widths; Omitted_bv_capability;
            Wrong_int_to_bv_operand; Bv_projection_width_mismatch ];
        let model_builder = Logic_ir.create () in
        let projected_term = binary Logic_ir.bv_add_mod (literal 250) (literal 10) in
        let projection =
          Logic_ir.project_bv model_builder ~identity:"sum-residue" projected_term
            ~span
          |> logic_ok "BV projection"
        in
        let model_query =
          Logic_ir.query model_builder ~bv_projections:[ projection ] ~axioms:[]
            ~assertions:[ Logic_ir.bool ~span true ] ~requires:[] ~span
          |> logic_ok "BV model query"
        in
        (match
           Z3_bridge.solve_bv_query { timeout_ms = 10_000; model = true }
             model_query
         with
        | Ok (Z3_bridge.Bv_counterexample [ binding ])
          when String.equal binding.projection_identity "sum-residue"
               && Bv_width.equal binding.width width
               && Z.equal binding.value.unsigned_bits (Z.of_int 4) ->
            ()
        | Ok _ -> failwith "direct BV model projection was not structured"
        | Error error -> failwith (Z3_bridge.error_to_string error));
        let detached_model, identities = Z3_bridge.detach_bv_query model_query in
        let detached_model_attempt =
          Z3_bridge.solve_detached_query_local ~controlled:Z3_bridge.Real
            ~timeout_ms:10_000 ~rlimit:1_000_000 ~model:true detached_model
        in
        (match (identities, detached_model_attempt.detached_result) with
        | [ "sum-residue" ],
          Ok
            (Z3_bridge.Detached_counterexample
              [ Some
                  (Z3_bridge.Detached_bit_vector
                    ("sum-residue", width_reference, "4")) ])
          when
            (match Bv_width.decode_reference_for_worker width_reference with
            | Ok observed -> Bv_width.equal observed width
            | Error _ -> false) ->
            ()
        | _ -> failwith "detached BV model projection was not structured");
        let worker_result = solve_on_detached_worker detached_model in
        (match (worker_result.worker_exception, worker_result.vc_results) with
        | None,
          [ { result_outcome =
                Ok
                  (Z3_bridge.Detached_counterexample
                    [ Some
                        (Z3_bridge.Detached_bit_vector
                          ("sum-residue", width_reference, "4")) ]);
              attempt_telemetry = [ telemetry ]; _ } ]
          when identities = [ "sum-residue" ]
               && (match
                     Bv_width.decode_reference_for_worker width_reference
                   with
                  | Ok observed -> Bv_width.equal observed width
                  | Error _ -> false)
               && telemetry.contexts_created = 1
               && telemetry.contexts_cleaned = 1
               && telemetry.contexts_live = 0 ->
            ()
        | _ -> failwith "worker BV projection reassembly was not structured");
        let maximum_width = bound_bv_width "4096" in
        let maximum_residue = Z.pred (Z.shift_left Z.one 4096) in
        let maximum_residue_text = Z.to_string maximum_residue in
        let maximum_term =
          Bv_value.of_z ~width:maximum_width maximum_residue
          |> ok "maximum-width BV literal value"
          |> Logic_ir.bv_literal ~span
          |> logic_ok "maximum-width BV literal term"
        in
        let maximum_builder = Logic_ir.create () in
        let maximum_projection =
          Logic_ir.project_bv maximum_builder ~identity:"maximum-residue"
            maximum_term ~span
          |> logic_ok "maximum-width BV projection"
        in
        let maximum_query =
          Logic_ir.query maximum_builder
            ~bv_projections:[ maximum_projection ] ~axioms:[]
            ~assertions:[ Logic_ir.bool ~span true ] ~requires:[] ~span
          |> logic_ok "maximum-width BV model query"
        in
        let maximum_detached, maximum_identities =
          Z3_bridge.detach_bv_query maximum_query
        in
        let maximum_worker = solve_on_detached_worker maximum_detached in
        (match (maximum_worker.worker_exception, maximum_worker.vc_results) with
        | None,
          [ { result_outcome =
                Ok
                  (Z3_bridge.Detached_counterexample
                    [ Some
                        (Z3_bridge.Detached_bit_vector
                          ( "maximum-residue",
                            observed_width_reference,
                            observed_residue )) ]);
              attempt_telemetry = [ telemetry ]; _ } ]
          when maximum_identities = [ "maximum-residue" ]
               && String.equal observed_residue maximum_residue_text
               && (match
                     Bv_width.decode_reference_for_worker
                       observed_width_reference
                   with
                  | Ok observed -> Bv_width.equal observed maximum_width
                  | Error _ -> false)
               && String.length observed_residue = 1234
               && telemetry.contexts_created = 1
               && telemetry.contexts_cleaned = 1
               && telemetry.contexts_live = 0 ->
            ()
        | _ -> failwith "maximum-width worker BV residue was not preserved");
        Ok (Outcome.observation ~status:Outcome.Verified () |> Outcome.project)
      with Failure message -> Error (Failure.make Failure.Runner_internal message))

let bv_model_fault_lifecycle_case =
  Suite.case ~name:"bv-model-faults-preserve-classification-and-cleanup"
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
    (fun ~environment:_ ~workspace:_ ->
      try
        let target, width = target_and_bound_bv_width "8" in
        let value =
          Bv_value.of_z ~width (Z.of_int 129) |> ok "fault-matrix BV value"
        in
        let term =
          Logic_ir.bv_literal ~span value |> logic_ok "fault-matrix BV literal"
        in
        let builder = Logic_ir.create () in
        let projection =
          Logic_ir.project_bv builder ~identity:"fault-residue" term ~span
          |> logic_ok "fault-matrix BV projection"
        in
        let query_with assertion =
          Logic_ir.query builder ~bv_projections:[ projection ] ~axioms:[]
            ~assertions:[ assertion ] ~requires:[] ~span
          |> logic_ok "fault-matrix BV query"
        in
        let sat_query = query_with (Logic_ir.bool ~span true)
        and unsat_query = query_with (Logic_ir.bool ~span false) in
        let source_authority =
          test_projection_authority
            ~target ~width ~provenance:Imported_registry
            ~outer_authority:Checked_proof
            ~modular_authority:Explicit_axiom ~trusted_dependencies:[]
            ~source_full_key:"bv-model-fault-source"
            ~occurrence_full_key:"bv-model-fault-occurrence"
            ~operation_span:span
        in
        let projected_symbol =
          Vir.
            { symbol_id = 200;
              source_name = "fault_residue";
              sort = Bit_vector width;
              role = Input;
              span }
        in
        let projected_term =
          Vir.bv_symbol projected_symbol |> ok "fault-matrix VIR symbol"
        in
        let expected_term =
          Vir.bv_int_to_bv_mod ~width ~input:(Integer_constant (Z.of_int 129))
            ~source_authority
        in
        let vir_obligation goal =
          Vir.
            { obligation_index = 0;
              function_ref =
                { function_index = 201; function_name = "bv_model_fault" };
              kind = Assertion { assertion_ordinal = 0 };
              span;
              assumptions = [ Bv_equal (projected_term, expected_term) ];
              required_preceding_safety = [];
              path_condition = [];
              goal;
              projection_symbols = [ projected_symbol ];
              logical_constant_instances = [];
              logical_constant_equations = [] }
        in
        let sat_vir = vir_obligation (Vir.Boolean_constant false)
        and unsat_vir = vir_obligation (Vir.Boolean_constant true) in
        let detached, identities = Z3_bridge.detach_bv_query sat_query in
        if identities <> [ "fault-residue" ] then
          failwith "fault-matrix detached projection identity changed";
        let require_balanced label (telemetry : Z3_bridge.counters) =
          if
            telemetry.contexts_created = 0
            || telemetry.contexts_created <> telemetry.contexts_cleaned
            || telemetry.contexts_live <> 0
            || telemetry.solvers_created <> telemetry.solver_resets
          then failwith (label ^ " did not release its solver context")
        in
        let require_global_balanced label =
          require_balanced label (Z3_bridge.counters ())
        in
        Z3_bridge.reset_counters ();
        (match
           Z3_bridge.solve_bv_query { timeout_ms = 10_000; model = true }
             sat_query
         with
        | Ok (Z3_bridge.Bv_counterexample [ binding ])
          when String.equal binding.projection_identity "fault-residue"
               && Bv_value.equal binding.value value ->
            ()
        | Ok _ -> failwith "structured BV positive model control changed"
        | Error error -> failwith (Z3_bridge.error_to_string error));
        require_global_balanced "structured BV positive model control";
        let positive_vir =
          Z3_bridge.solve_vir_local ~controlled:Z3_bridge.Real
            ~rlimit:1_000_000 ~requires:[]
            { timeout_ms = 10_000; model = true }
            sat_vir
        in
        (match positive_vir.result with
        | Ok
            (Z3_bridge.Counterexample
              [ { symbol; value = Some (Z3_bridge.Bit_vector observed) } ])
          when symbol = projected_symbol && Bv_value.equal observed value ->
            ()
        | Ok _ -> failwith "direct VIR positive model control changed"
        | Error error -> failwith (Z3_bridge.error_to_string error));
        require_balanced "direct VIR positive model control"
          positive_vir.telemetry;
        let positive_detached =
          Z3_bridge.solve_detached_query_local ~controlled:Z3_bridge.Real
            ~timeout_ms:10_000 ~rlimit:1_000_000 ~model:true detached
        in
        (match positive_detached.detached_result with
        | Ok
            (Z3_bridge.Detached_counterexample
              [ Some
                  (Z3_bridge.Detached_bit_vector
                    ("fault-residue", _, "129")) ]) ->
            ()
        | Ok _ -> failwith "detached BV positive model control changed"
        | Error message -> failwith message);
        require_balanced "detached BV positive model control"
          positive_detached.detached_telemetry;
        Z3_bridge.reset_counters ();
        (match
           Z3_bridge.solve_bv_query { timeout_ms = 10_000; model = true }
             unsat_query
         with
        | Ok Z3_bridge.Bv_verified -> ()
        | Ok _ -> failwith "structured BV unsat control was not verified"
        | Error error -> failwith (Z3_bridge.error_to_string error));
        require_global_balanced "structured BV unsat control";
        let unsat_direct =
          Z3_bridge.solve_vir_local ~controlled:Z3_bridge.Real
            ~rlimit:1_000_000 ~requires:[]
            { timeout_ms = 10_000; model = true }
            unsat_vir
        in
        (match unsat_direct.result with
        | Ok Z3_bridge.Verified -> ()
        | Ok _ -> failwith "direct VIR unsat control was not verified"
        | Error error -> failwith (Z3_bridge.error_to_string error));
        require_balanced "direct VIR unsat control" unsat_direct.telemetry;
        let detached_unsat, _ = Z3_bridge.detach_bv_query unsat_query in
        let unsat_detached =
          Z3_bridge.solve_detached_query_local ~controlled:Z3_bridge.Real
            ~timeout_ms:10_000 ~rlimit:1_000_000 ~model:true detached_unsat
        in
        (match unsat_detached.detached_result with
        | Ok Z3_bridge.Detached_verified -> ()
        | Ok _ -> failwith "detached BV unsat control was not verified"
        | Error message -> failwith message);
        require_balanced "detached BV unsat control"
          unsat_detached.detached_telemetry;
        let scheduler =
          Parallel_scheduler.create
            ~max_domains:(min 2 (Multicore.max_domains ())) ()
        in
        Fun.protect
          ~finally:(fun () -> Parallel_scheduler.stop scheduler)
          (fun () ->
        let faults =
          Z3_bridge.For_testing.
            [ ("missing", Missing_evaluation); ("wrong-sort", Wrong_sort);
              ("non-numeral", Non_numeral);
              ("width-mismatch", Width_mismatch);
              ("empty", Residue_text "");
              ("leading-zero", Residue_text "00");
              ("plus", Residue_text "+1");
              ("whitespace", Residue_text " 1");
              ("alternate-base", Residue_text "0x1");
              ("malformed", Residue_text "residue");
              ("negative", Residue_text "-1");
              ("oversized", Residue_text (String.make 1235 '9'));
              ("out-of-range", Residue_text "256") ]
        in
        List.iter
          (fun (label, fault) ->
            Z3_bridge.reset_counters ();
            (match
               Z3_bridge.For_testing.solve_bv_query_with_model_fault ~fault
                 { timeout_ms = 10_000; model = true }
                 sat_query
             with
            | Error (Z3_bridge.Backend_failure _) -> ()
            | Error error ->
                failwith
                  (label ^ " structured BV fault had the wrong error: "
                 ^ Z3_bridge.error_to_string error)
            | Ok _ ->
                failwith (label ^ " structured BV fault produced a model"));
            require_global_balanced (label ^ " structured BV fault");
            let direct =
              Z3_bridge.For_testing.solve_vir_local_with_model_fault ~fault
                ~controlled:Z3_bridge.Real ~rlimit:1_000_000 ~requires:[]
                { timeout_ms = 10_000; model = true }
                sat_vir
            in
            (match direct.result with
            | Error (Z3_bridge.Backend_failure _) -> ()
            | Error error ->
                failwith
                  (label ^ " direct VIR fault had the wrong error: "
                 ^ Z3_bridge.error_to_string error)
            | Ok _ -> failwith (label ^ " direct VIR fault produced a model"));
            require_balanced (label ^ " direct VIR fault") direct.telemetry;
            let detached_attempt =
              solve_detached_attempt_on_worker scheduler ~fault
                ~controlled:Z3_bridge.Real ~timeout_ms:10_000 ~rlimit:1_000_000
                ~model:true detached
            in
            (match detached_attempt.detached_result with
            | Error _ -> ()
            | Ok _ -> failwith (label ^ " detached BV fault produced a model"));
            require_balanced (label ^ " detached BV fault")
              detached_attempt.detached_telemetry)
          faults;
        let controlled =
          [ ( Z3_bridge.Force_unknown,
              (function
                | Z3_bridge.Bv_inconclusive (Backend_unknown _) -> true
                | _ -> false),
              (function
                | Z3_bridge.Detached_inconclusive (Backend_unknown _) -> true
                | _ -> false),
              "unknown" );
            ( Force_timeout,
              (function Bv_inconclusive Timed_out -> true | _ -> false),
              (function Detached_inconclusive Timed_out -> true | _ -> false),
              "timeout" ) ]
        in
        List.iter
          (fun (control, direct_expected, detached_expected, label) ->
            Z3_bridge.reset_counters ();
            (match
               Z3_bridge.solve_bv_query ~controlled:control ~rlimit:1_000_000
                 { timeout_ms = 10_000; model = true }
                 sat_query
             with
            | Ok outcome when direct_expected outcome -> ()
            | Ok _ -> failwith (label ^ " structured BV classification changed")
            | Error error -> failwith (Z3_bridge.error_to_string error));
            require_global_balanced (label ^ " structured BV");
            let attempt =
              solve_detached_attempt_on_worker scheduler ~controlled:control
                ~timeout_ms:10_000 ~rlimit:1_000_000 ~model:true detached
            in
            (match attempt.detached_result with
            | Ok outcome when detached_expected outcome -> ()
            | Ok _ -> failwith (label ^ " detached BV classification changed")
            | Error message -> failwith message);
            require_balanced (label ^ " detached BV")
              attempt.detached_telemetry)
          controlled;
        Z3_bridge.reset_counters ();
        (match
           Z3_bridge.solve_bv_query ~controlled:Force_backend_failure
             ~rlimit:1_000_000 { timeout_ms = 10_000; model = true }
             sat_query
         with
        | Error (Z3_bridge.Backend_failure _) -> ()
        | Error error -> failwith (Z3_bridge.error_to_string error)
        | Ok _ -> failwith "controlled structured BV backend failure succeeded");
        require_global_balanced "controlled structured BV backend failure";
        let detached_failure =
          solve_detached_attempt_on_worker scheduler
            ~controlled:Z3_bridge.Force_backend_failure ~timeout_ms:10_000
            ~rlimit:1_000_000 ~model:true detached
        in
        (match detached_failure.detached_result with
        | Error _ -> ()
        | Ok _ -> failwith "controlled detached BV backend failure succeeded");
        require_balanced "controlled detached BV backend failure"
          detached_failure.detached_telemetry;
        let resource_direct =
          Z3_bridge.solve_vir_local ~controlled:Z3_bridge.Real ~rlimit:1
            ~requires:[] { timeout_ms = 10_000; model = true }
            sat_vir
        in
        (match resource_direct.result with
        | Ok (Z3_bridge.Inconclusive Resource_exhausted) -> ()
        | Ok _ -> failwith "direct BV rlimit exhaustion changed classification"
        | Error error -> failwith (Z3_bridge.error_to_string error));
        require_balanced "direct BV rlimit exhaustion" resource_direct.telemetry;
        let resource_detached_query, _ =
          Z3_bridge.detach_vir ~requires:[] sat_vir
          |> (function
               | Ok detached -> detached
               | Error error -> failwith (Z3_bridge.error_to_string error))
        in
        let resource_detached =
          solve_detached_attempt_on_worker scheduler ~controlled:Z3_bridge.Real
            ~timeout_ms:10_000 ~rlimit:1 ~model:true resource_detached_query
        in
        (match resource_detached.detached_result with
        | Ok (Z3_bridge.Detached_inconclusive Resource_exhausted) -> ()
        | Ok _ -> failwith "detached BV rlimit exhaustion changed classification"
        | Error message -> failwith message);
        require_balanced "detached BV rlimit exhaustion"
          resource_detached.detached_telemetry);
        Ok (Outcome.observation ~status:Outcome.Verified () |> Outcome.project)
      with Failure message -> Error (Failure.make Failure.Runner_internal message))

let vir_bv_structural_case =
  Suite.case ~name:"typed-vir-bv-structure-preserves-width-and-authority"
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
    (fun ~environment:_ ~workspace:_ ->
      try
        let target, width = target_and_bound_bv_width "8" in
        let _, other_width = target_and_bound_bv_width "16" in
        let _, same_size_other_target =
          target_and_bound_bv_width ~physical_width:64 "8"
        in
        let evidence width suffix =
          test_projection_authority
            ~target ~width ~provenance:Imported_registry
            ~outer_authority:Checked_proof
            ~modular_authority:Explicit_axiom ~trusted_dependencies:[]
            ~source_full_key:("vir-structural-source-" ^ suffix)
            ~occurrence_full_key:("vir-structural-occurrence-" ^ suffix)
            ~operation_span:span
        in
        let converted width suffix value =
          Vir.bv_int_to_bv_mod ~width
            ~input:(Vir.Integer_constant (Z.of_int value))
            ~source_authority:(evidence width suffix)
        in
        let converted8 = converted width "8" (-1)
        and converted16 = converted other_width "16" 255 in
        let converted_zero = converted width "zero" 0 in
        let symbol id name sort role =
          Vir.{ symbol_id = id; source_name = name; sort; role; span }
        in
        let local_symbol = symbol 10 "bv_local" (Vir.Bit_vector width) Vir.Local in
        let local = ok "typed BV local" (Vir.bv_symbol local_symbol) in
        let joined =
          ok "typed BV join"
            (Vir.bv_conditional (Vir.Boolean_constant true) local converted_zero)
        and joined_alternative =
          ok "typed BV alternative join"
            (Vir.bv_conditional
               (Vir.Boolean_constant false)
               converted_zero local)
        in
        let aggregate_type =
          Vir.
            { aggregate_type_index = 901;
              aggregate_type_name = "internal_bv_box";
              aggregate_type_arguments = [] }
        in
        let aggregate_symbol =
          symbol 11 "box" (Vir.Aggregate aggregate_type) Vir.Input
        in
        let aggregate =
          Vir.
            { aggregate_type;
              aggregate_desc = Aggregate_symbol aggregate_symbol }
        in
        let selector =
          Vir.
            { selector_domain = aggregate_type;
              selector_range = Bit_vector width;
              selector_namespace = "internal_bv_box";
              selector_index = 0;
              selector_name = "bits";
              selector_path = [] }
        in
        let selected = ok "typed BV selector" (Vir.bv_selector selector aggregate) in
        let constructor =
          Sst.
            { constructor_type =
                { type_index = aggregate_type.aggregate_type_index;
                  type_name = aggregate_type.aggregate_type_name };
              constructor_index = 0;
              constructor_name = "Box" }
        in
        let constructed =
          Vir.
            { aggregate_type;
              aggregate_desc =
                Aggregate_constructor
                  { constructor;
                    arguments = [ Recursive_bv_argument converted8 ] } }
        in
        let exact_selector =
          Vir.
            { selector_domain = aggregate_type;
              selector_range = Bit_vector width;
              selector_namespace = "t901_internal_bv_box_c0_Box";
              selector_index = 0;
              selector_name = "$arg0";
              selector_path = [] }
        in
        let exact_selected =
          ok "typed exact BV selector"
            (Vir.bv_selector exact_selector constructed)
        in
        let zero_constructed =
          Vir.
            { aggregate_type;
              aggregate_desc =
                Aggregate_constructor
                  { constructor;
                    arguments = [ Recursive_bv_argument converted_zero ] } }
        in
        let conditional_constructed condition consequent alternative =
          Vir.
            { aggregate_type;
              aggregate_desc =
                Aggregate_conditional (condition, consequent, alternative) }
        in
        let exact_selected_true =
          ok "typed exact conditional BV selector"
            (Vir.bv_selector exact_selector
               (conditional_constructed (Boolean_constant true) constructed
                  zero_constructed))
        and exact_selected_false =
          ok "typed exact alternative BV selector"
            (Vir.bv_selector exact_selector
               (conditional_constructed (Boolean_constant false)
                  zero_constructed constructed))
        in
        let relation callee_index argument =
          Vir.Boolean_specification_application
            { callee =
                { Sst.function_index = callee_index;
                  function_name = "internal_bv_relation" };
              type_arguments = [];
              arguments =
                [ Vir.Recursive_bv_argument argument;
                  Recursive_aggregate_argument aggregate ];
              span }
        in
        let equal left right = ok "typed BV equality" (Vir.bv_equal left right) in
        let reflexive value = equal value value in
        let relation8 = relation 902 joined in
        let local_assumption = equal local converted8 in
        let literal label value =
          Bv_value.of_z ~width (Z.of_int value)
          |> ok ("typed BV " ^ label ^ " literal")
          |> Vir.bv_literal
        in
        let bv_binary label operation left right expected =
          let actual =
            Vir.bv_binary operation left right
            |> ok ("typed BV " ^ label ^ " operation")
          in
          equal actual (literal (label ^ " expected") expected)
        in
        let bv_compare label operation left right =
          Vir.bv_compare operation left right
          |> ok ("typed BV " ^ label ^ " comparison")
        in
        let conjunct predicates =
          List.fold_right
            (fun predicate combined -> Vir.Boolean_and (predicate, combined))
            predicates (Vir.Boolean_constant true)
        in
        let ten = literal "ten" 10
        and fifteen = literal "fifteen" 15
        and two_hundred_fifty = literal "two-hundred-fifty" 250
        and aa = literal "aa" 0xaa
        and low_nibble = literal "low-nibble" 0x0f in
        let operation_goal =
          conjunct
            [ bv_binary "addmod" Bv_add_mod two_hundred_fifty ten 4;
              bv_binary "submod" Bv_sub_mod ten two_hundred_fifty 16;
              equal (Vir.bv_not fifteen) (literal "not expected" 240);
              bv_binary "and" Bv_and aa low_nibble 0x0a;
              bv_binary "or" Bv_or aa low_nibble 0xaf;
              bv_binary "xor" Bv_xor aa low_nibble 0xa5;
              bv_compare "ult" Bv_unsigned_less_than ten two_hundred_fifty;
              bv_compare "ule" Bv_unsigned_less_or_equal ten
                two_hundred_fifty;
              bv_compare "ugt" Bv_unsigned_greater_than two_hundred_fifty ten;
              bv_compare "uge" Bv_unsigned_greater_or_equal
                two_hundred_fifty ten;
              bv_compare "slt" Bv_signed_less_than two_hundred_fifty ten;
              bv_compare "sle" Bv_signed_less_or_equal two_hundred_fifty ten;
              bv_compare "sgt" Bv_signed_greater_than ten two_hundred_fifty;
              bv_compare "sge" Bv_signed_greater_or_equal ten
                two_hundred_fifty;
              Vir.Integer_compare
                (Equal, Vir.bv_to_int_unsigned two_hundred_fifty,
                 Vir.Integer_constant (Z.of_int 250));
              Vir.Integer_compare
                (Equal, Vir.bv_to_int_signed two_hundred_fifty,
                 Vir.Integer_constant (Z.of_int (-6))) ]
        in
        let goal =
          Vir.Boolean_and
            ( operation_goal,
              Boolean_and
                ( equal local converted8,
              Boolean_and
                ( equal joined converted8,
                  Boolean_and
                    ( equal joined_alternative converted8,
                      Boolean_and
                        ( reflexive selected,
                          Boolean_and
                            ( equal exact_selected converted8,
                              Boolean_and
                                ( equal exact_selected_true converted8,
                                  Boolean_and
                                    ( equal exact_selected_false converted8,
                                      Boolean_equal
                                        (relation8, relation 902 converted8) ) ) ) ) ) ) ) )
        in
        let obligation ?(assumptions = []) goal =
          Vir.
            { obligation_index = 0;
              function_ref =
                { function_index = 901; function_name = "vir_bv_structure" };
              kind = Assertion { assertion_ordinal = 0 };
              span;
              assumptions;
              required_preceding_safety = [];
              path_condition = [];
              goal;
              projection_symbols = [];
              logical_constant_instances = [];
              logical_constant_equations = [] }
        in
        Z3_bridge.reset_counters ();
        (match
           Z3_bridge.solve_vir ~requires:[]
             { timeout_ms = 10_000; model = true }
             (obligation ~assumptions:[ local_assumption ] goal)
         with
        | Ok Z3_bridge.Verified -> ()
        | Ok (Z3_bridge.Counterexample _) ->
            failwith "typed VIR BV structural query had a counterexample"
        | Ok (Z3_bridge.Inconclusive _) ->
            failwith "typed VIR BV structural query was inconclusive"
        | Error error -> failwith (Z3_bridge.error_to_string error));
        let direct = Z3_bridge.counters () in
        if
          direct.contexts_created <> 1 || direct.contexts_cleaned <> 1
          || direct.contexts_live <> 0 || direct.selected_logics <> [ "general" ]
        then failwith "typed VIR BV direct lifecycle or solver seam changed";
        let detached, projected =
          match
            Z3_bridge.detach_vir ~requires:[]
              (obligation ~assumptions:[ local_assumption ] goal)
          with
          | Ok detached -> detached
          | Error error -> failwith (Z3_bridge.error_to_string error)
        in
        if projected <> [] then
          failwith "unrequested typed VIR BV values entered model projection";
        let worker = solve_on_detached_worker detached in
        (match (worker.worker_exception, worker.vc_results) with
        | None,
          [ { result_outcome = Ok Z3_bridge.Detached_verified;
              attempt_telemetry = [ telemetry ]; _ } ]
          when telemetry.contexts_created = 1
               && telemetry.contexts_cleaned = 1
               && telemetry.contexts_live = 0
               && telemetry.selected_logics = [ "general" ] ->
            ()
        | _ -> failwith "typed VIR BV detached worker did not verify");
        let false_goal =
          Vir.Bv_not_equal (exact_selected, converted8)
        in
        Z3_bridge.reset_counters ();
        (match
           Z3_bridge.solve_vir ~requires:[]
             { timeout_ms = 10_000; model = true }
             (obligation false_goal)
         with
        | Ok (Z3_bridge.Counterexample _) -> ()
        | Ok Z3_bridge.Verified ->
            failwith "false known-constructor BV selection verified"
        | Ok (Z3_bridge.Inconclusive _) ->
            failwith "false known-constructor BV selection was inconclusive"
        | Error error -> failwith (Z3_bridge.error_to_string error));
        let false_detached, false_projected =
          match Z3_bridge.detach_vir ~requires:[] (obligation false_goal) with
          | Ok detached -> detached
          | Error error -> failwith (Z3_bridge.error_to_string error)
        in
        if false_projected <> [] then
          failwith "false BV selector query unexpectedly projected a model";
        let false_worker = solve_on_detached_worker false_detached in
        (match (false_worker.worker_exception, false_worker.vc_results) with
        | None,
          [ { result_outcome =
                Ok (Z3_bridge.Detached_counterexample []); _ } ] ->
            ()
        | _ -> failwith "false BV selector worker outcome was not a counterexample");
        let recursive_goal =
          Vir.Boolean_and
            ( equal exact_selected converted8,
              Boolean_and
                ( equal exact_selected_true converted8,
                  equal exact_selected_false converted8 ) )
        in
        let recursive_query =
          match
            Recursive_spec_encoding.For_testing.internal_bv_proof_query ~span
              recursive_goal
          with
          | Ok query -> query
          | Error error ->
              failwith (Recursive_spec_encoding.error_to_string error)
        in
        Z3_bridge.reset_counters ();
        (match
           Z3_bridge.solve_query { timeout_ms = 10_000; model = true }
             recursive_query
         with
        | Ok Z3_bridge.Verified -> ()
        | Ok (Z3_bridge.Counterexample _) ->
            failwith "recursive-definition BV selector query had a counterexample"
        | Ok (Z3_bridge.Inconclusive _) ->
            failwith "recursive-definition BV selector query was inconclusive"
        | Error error -> failwith (Z3_bridge.error_to_string error));
        let recursive_worker =
          recursive_query |> Z3_bridge.detach_query
          |> solve_on_detached_worker
        in
        (match (recursive_worker.worker_exception, recursive_worker.vc_results) with
        | None,
          [ { result_outcome = Ok Z3_bridge.Detached_verified; _ } ] ->
            ()
        | _ -> failwith "recursive-definition BV selector worker did not verify");
        if Result.is_ok (Vir.bv_conditional (Vir.Boolean_constant true) converted8 converted16)
        then failwith "unequal-width VIR BV join was accepted";
        if Result.is_ok (Vir.bv_equal converted8 converted16) then
          failwith "unequal-width VIR BV equality was accepted";
        if
          Result.is_ok
            (Vir.bv_binary Bv_add_mod converted8 converted16)
        then failwith "unequal-width VIR BV binary operation was accepted";
        if
          Result.is_ok
            (Vir.bv_compare Bv_unsigned_less_than converted8 converted16)
        then failwith "unequal-width VIR BV comparison was accepted";
        if
          Result.is_ok
            (Vir.bv_symbol
               (symbol 12 "not_bv" Vir.Integer Vir.Local))
        then failwith "integer symbol acquired a VIR BV sort";
        let wrong_selector = { selector with Vir.selector_range = Bit_vector other_width } in
        if Result.is_ok (Vir.bv_selector wrong_selector aggregate) |> not then
          failwith "valid distinct-width selector declaration was rejected";
        let copied_wrong_selector =
          Vir.
            { bit_vector_width = width;
              bit_vector_desc = Bv_selector (wrong_selector, aggregate) }
        in
        let forged_join =
          Vir.
            { bit_vector_width = width;
              bit_vector_desc =
                Bv_conditional
                  (Boolean_constant true, converted8, converted16) }
        in
        let other_width_value =
          Bv_value.of_z ~width:other_width (Z.of_int 7)
          |> ok "malformed wrapped BV16 literal"
        and other_target_value =
          Bv_value.of_z ~width:same_size_other_target (Z.of_int 7)
          |> ok "malformed wrapped other-target BV8 literal"
        in
        let other_target_literal = Vir.bv_literal other_target_value in
        let forged_literal =
          Vir.
            { bit_vector_width = width;
              bit_vector_desc = Bv_literal other_width_value }
        and forged_not =
          Vir.
            { bit_vector_width = width;
              bit_vector_desc = Bv_not other_target_literal }
        and forged_binary =
          Vir.
            { bit_vector_width = width;
              bit_vector_desc =
                Bv_binary (Bv_xor, literal "forged-left" 3,
                  other_target_literal) }
        in
        let expect_pre_context_rejection label goal =
          let before = (Z3_bridge.counters ()).contexts_created in
          (match Z3_bridge.detach_vir ~requires:[] (obligation goal) with
          | Error _ -> ()
          | Ok _ -> failwith (label ^ " passed VIR typed preflight"));
          if (Z3_bridge.counters ()).contexts_created <> before then
            failwith (label ^ " created a solver context")
        in
        expect_pre_context_rejection "copied selector width mismatch"
          (Vir.Bv_equal (copied_wrong_selector, copied_wrong_selector));
        expect_pre_context_rejection "copied conditional width mismatch"
          (Vir.Bv_equal (forged_join, forged_join));
        let expect_width_node_rejection label term =
          let goal = Vir.Bv_equal (term, term) in
          let before = (Z3_bridge.counters ()).contexts_created in
          (match
             Z3_bridge.solve_vir ~requires:[]
               { timeout_ms = 10_000; model = true }
               (obligation goal)
           with
          | Error _ -> ()
          | Ok _ -> failwith (label ^ " passed direct VIR translation"));
          (match Z3_bridge.detach_vir ~requires:[] (obligation goal) with
          | Error _ -> ()
          | Ok _ -> failwith (label ^ " passed detached VIR translation"));
          (match
             Recursive_spec_encoding.For_testing.internal_bv_proof_query ~span
               goal
           with
          | Error _ -> ()
          | Ok _ ->
              failwith (label ^ " passed recursive-definition translation"));
          if (Z3_bridge.counters ()).contexts_created <> before then
            failwith (label ^ " created a solver context")
        in
        expect_width_node_rejection "wrapped BV literal mismatch" forged_literal;
        expect_width_node_rejection "wrapped BV not mismatch" forged_not;
        expect_width_node_rejection "wrapped BV binary mismatch" forged_binary;
        let invalid_selector =
          Vir.
            { exact_selector with
              selector_index = 1;
              selector_name = "$arg1" }
        in
        let invalid_selected =
          ok "typed out-of-range BV selector declaration"
            (Vir.bv_selector invalid_selector constructed)
        in
        expect_pre_context_rejection "known-constructor selector index"
          (Vir.Bv_equal (invalid_selected, invalid_selected));
        (match
           Recursive_spec_encoding.For_testing.internal_bv_proof_query ~span
             (Vir.Bv_equal (invalid_selected, invalid_selected))
         with
        | Error _ -> ()
        | Ok _ ->
            failwith
              "recursive-definition accepted an out-of-range known selector");
        let relation16 = relation 902 converted16 in
        expect_pre_context_rejection "conflicting BV relation declaration"
          (Vir.Boolean_and
             ( Boolean_equal (relation8, relation8),
               Boolean_equal (relation16, relation16) ));
        let binder =
          symbol 13 "claimed_bv_binder" (Vir.Bit_vector width) Vir.Input
        in
        let schema =
          Logic_quantifier_private.create ~kind:Exists
            ~owner:"vir-bv-structural" ~binder_index:0
            ~binder_type:Parametric_type.Mathematical_int ~span
          |> Logic_quantifier_private.singleton
        in
        if
          Result.is_ok
            (Vir.make_boolean_quantifier
               ~sort_of_type:(function
                 | Parametric_type.Mathematical_int -> Ok Vir.Integer
                 | _ -> Error "unexpected binder schema")
               ~schema ~binders:[ binder ] ~body:(Boolean_constant true)
               ~trigger:None)
        then
          failwith "Mathematical_int binder schema was reinterpreted as BV";
        Ok (Outcome.observation ~status:Outcome.Verified () |> Outcome.project)
      with Failure message -> Error (Failure.make Failure.Runner_internal message))

let internal_sst_bv_case =
  Suite.case ~name:"validated-internal-sst-bv-closed-operations"
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
    (fun ~environment:_ ~workspace:_ ->
      try
        let target, width = target_and_bound_bv_width "8" in
        let _, same_size_other_target =
          target_and_bound_bv_width ~physical_width:64 "8"
        in
        let evidence width suffix =
          test_projection_authority
            ~target ~width ~provenance:Imported_registry
            ~outer_authority:Checked_proof
            ~modular_authority:Explicit_axiom ~trusted_dependencies:[]
            ~source_full_key:("internal-sst-source-" ^ suffix)
            ~occurrence_full_key:("internal-sst-occurrence-" ^ suffix)
            ~operation_span:span
        in
        if
          Parametric_type.equal (Bit_vector width)
            (Bit_vector same_size_other_target)
          || String.equal
               (Parametric_type.structural_identity_digest (Bit_vector width))
               (Parametric_type.structural_identity_digest
                  (Bit_vector same_size_other_target))
        then
          failwith "BV type identity collapsed distinct authenticated targets";
        let expr typ expression_desc = Sst.{ typ; expression_desc; span } in
        let literal label value =
          let value =
            Bv_value.of_z ~width (Z.of_int value)
            |> ok ("internal SST " ^ label ^ " literal")
          in
          expr (Sst.Bit_vector width) (Sst.Bv_literal value)
        in
        let bv_equal left right =
          expr Sst.Bool (Sst.Compare (Sst.Equal, left, right))
        in
        let int_equal left value =
          expr Sst.Bool
            (Sst.Compare
               ( Sst.Equal,
                 left,
                 expr Sst.Mathematical_int
                   (Sst.Int_constant (Z.of_int value)) ))
        in
        let binary label operation left right expected =
          let actual =
            expr (Sst.Bit_vector width)
              (Sst.Bv_binary (operation, left, right))
          in
          bv_equal actual (literal (label ^ " expected") expected)
        in
        let compare operation left right =
          expr Sst.Bool (Sst.Bv_compare (operation, left, right))
        in
        let conjunction predicates =
          List.fold_right
            (fun predicate combined ->
              expr Sst.Bool (Sst.Boolean_binary (Sst.And, predicate, combined)))
            predicates (expr Sst.Bool (Sst.Bool_constant true))
        in
        let ten = literal "ten" 10
        and fifteen = literal "fifteen" 15
        and two_hundred_fifty = literal "two-hundred-fifty" 250
        and aa = literal "aa" 0xaa
        and low_nibble = literal "low-nibble" 0x0f in
        let conversion =
          expr (Sst.Bit_vector width)
            (Sst.Bv_int_to_bv_mod
               { width;
                 input =
                   expr Sst.Mathematical_int (Sst.Int_constant (Z.of_int (-1)));
                 source_authority = evidence width "sst-intmod" })
        in
        let predicate =
          conjunction
            [ binary "addmod" Bv_add_mod two_hundred_fifty ten 4;
              binary "submod" Bv_sub_mod ten two_hundred_fifty 16;
              bv_equal
                (expr (Sst.Bit_vector width) (Sst.Bv_not fifteen))
                (literal "not expected" 240);
              binary "and" Bv_and aa low_nibble 0x0a;
              binary "or" Bv_or aa low_nibble 0xaf;
              binary "xor" Bv_xor aa low_nibble 0xa5;
              compare Bv_unsigned_less_than ten two_hundred_fifty;
              compare Bv_unsigned_less_or_equal ten two_hundred_fifty;
              compare Bv_unsigned_greater_than two_hundred_fifty ten;
              compare Bv_unsigned_greater_or_equal two_hundred_fifty ten;
              compare Bv_signed_less_than two_hundred_fifty ten;
              compare Bv_signed_less_or_equal two_hundred_fifty ten;
              compare Bv_signed_greater_than ten two_hundred_fifty;
              compare Bv_signed_greater_or_equal ten two_hundred_fifty;
              int_equal
                (expr Sst.Mathematical_int
                   (Sst.Bv_to_int_unsigned two_hundred_fifty))
                250;
              int_equal
                (expr Sst.Mathematical_int
                   (Sst.Bv_to_int_signed two_hundred_fifty))
                (-6);
              bv_equal conversion (literal "intmod expected" 255) ]
        in
        let definition predicate =
          let body = expr Sst.Unit Sst.Unit_constant in
          let contracts =
            Sst.
              { empty_contracts with
                ensures =
                  [ { clause_index = 0;
                      binder = None;
                      predicate = { stage = Logical; expression = predicate };
                      span } ] }
          in
          Sst.
            { function_id =
                { function_index = 990; function_name = "internal_sst_bv" };
              type_binders = [];
              mode = Proof;
              recursive = false;
              parameters = [];
              contracts;
              body =
                Proof_body
                  { body = { stage = Proof_stage; expression = body };
                    provenance = Raw_semantic_body span };
              policy = Default_linear_z3;
              result_type = Unit;
              returns_unique_parameter = None;
              span }
        in
        let program predicate =
          Sst.
            { policy = Default_linear_z3;
              parametric_adts = [];
              types = [];
              logical_constants = [];
              functions = [ definition predicate ] }
        in
        let valid_program = program predicate in
        (match Sst_validation.validate valid_program with
        | Ok _ -> ()
        | Error error ->
            failwith
              ("internal SST BV validation failed: "
              ^ Sst_validation.error_to_string error));
        (match Parametric_sst_validation_private.validate_program valid_program with
        | Ok () -> ()
        | Error _ ->
            failwith "parametric SST validation rejected closed internal BV");
        let wrong_value =
          Bv_value.of_z ~width:same_size_other_target (Z.of_int 1)
          |> ok "other-target BV literal"
        in
        let malformed_right =
          expr (Sst.Bit_vector same_size_other_target)
            (Sst.Bv_literal wrong_value)
        in
        let malformed =
          expr (Sst.Bit_vector width)
            (Sst.Bv_binary (Bv_add_mod, ten, malformed_right))
        in
        if
          Result.is_ok
            (Sst_validation.validate
               (program (bv_equal malformed (literal "one" 1))))
        then
          failwith
            "SST BV operation accepted same-size distinct-target operands";
        Ok (Outcome.observation ~status:Outcome.Verified () |> Outcome.project)
      with Failure message -> Error (Failure.make Failure.Runner_internal message))

let bv_declaration_schema_case =
  Suite.case ~name:"authenticated-bv-declarations-preserve-exact-vectors"
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
    (fun ~environment:_ ~workspace:_ ->
      try
        let _, width = target_and_bound_bv_width "8" in
        let _, width16 = target_and_bound_bv_width "16" in
        let _, same_size_other_target =
          target_and_bound_bv_width ~physical_width:64 "8"
        in
        let profile_capability = Build_target_profile_private.capability () in
        let profile =
          Build_target_profile_private.authenticate_profile profile_capability
          |> ok "BV declaration profile"
        in
        let capability = Bv_backend_capability_receipt_private.capability () in
        let unbound_width =
          Bv_width.of_string ~profile capability "8"
          |> ok "unbound BV declaration width"
        in
        let phantom_owner =
          Parametric_type.owner ~index:995 ~name:"symbolic_phantom"
        in
        let phantom_binder =
          Parametric_type.binder phantom_owner ~ordinal:0
        in
        let phantom_declaration =
          Symbolic_application_private.declare
            ~marker_id:"symbolic.phantom.declaration"
            ~declaration_index:995 ~declaration_name:"symbolic_phantom"
            ~canonical_path:"Internal.symbolic_phantom"
            ~value_uid:"symbolic-phantom-uid" ~source_file:span.file
            ~compilation_identity:"internal-bv-schema"
            ~declaration_span:span ~type_binders:[ phantom_binder ]
            ~parameter_types:[] ~result_type:Parametric_type.Bool
          |> ok "polymorphic symbolic phantom declaration"
        in
        let instantiate_phantom label type_argument result_type =
          Symbolic_application_private.create phantom_declaration
            ~type_arguments:[ type_argument ] ~arguments:[] ~argument_types:[]
            ~result_type ~span
          |> ok label
        in
        let aggregate_type =
          Parametric_type.Aggregate
            { type_index = 995; type_name = "symbolic_aggregate_argument" }
        in
        let nested_tuple =
          Parametric_type.Tuple
            [ (Some "aggregate", aggregate_type);
              (Some "bits", Parametric_type.Bit_vector width) ]
        in
        ignore
          (instantiate_phantom "aggregate symbolic type argument" aggregate_type
             Parametric_type.Bool);
        ignore
          (instantiate_phantom "nested tuple symbolic type argument" nested_tuple
             Parametric_type.Bool);
        let tuple_constructor =
          Parametric_type.
            { constructor_path = "Internal.symbolic_container";
              constructor_identity = "symbolic-container-v1" }
        in
        let nested_application =
          Parametric_type.application tuple_constructor [ nested_tuple ]
          |> ok "nested symbolic application type"
        in
        ignore
          (instantiate_phantom "nested application symbolic type argument"
             nested_application Parametric_type.Bool);
        let unauthenticated_nested_tuple =
          Parametric_type.Tuple
            [ (None, aggregate_type);
              (None, Parametric_type.Bit_vector unbound_width) ]
        in
        if
          Result.is_ok
            (Symbolic_application_private.create phantom_declaration
               ~type_arguments:[ unauthenticated_nested_tuple ] ~arguments:[]
               ~argument_types:[] ~result_type:Parametric_type.Bool ~span)
        then
          failwith
            "symbolic application accepted a nested unbound bit-vector width";
        if
          Result.is_ok
            (Symbolic_application_private.create phantom_declaration
               ~type_arguments:[ aggregate_type ] ~arguments:[] ~argument_types:[]
               ~result_type:Parametric_type.Int ~span)
        then failwith "symbolic application accepted an inexact result type";
        let declare result_type parameter_types =
          Symbolic_application_private.declare
            ~marker_id:"symbolic.bv.declaration"
            ~declaration_index:992 ~declaration_name:"bv_symbolic"
            ~canonical_path:"Internal.bv_symbolic" ~value_uid:"bv-symbolic-uid"
            ~source_file:span.file ~compilation_identity:"internal-bv-schema"
            ~declaration_span:span ~type_binders:[] ~parameter_types
            ~result_type
        in
        let declaration =
          declare (Parametric_type.Bit_vector width)
            [ Parametric_type.Bit_vector width ]
          |> ok "authenticated BV symbolic declaration"
        in
        if
          Result.is_ok
            (declare (Parametric_type.Bit_vector unbound_width)
               [ Parametric_type.Bit_vector width ])
        then failwith "symbolic declaration accepted an unbound BV result width";
        let literal label width value =
          Bv_value.of_z ~width (Z.of_int value)
          |> ok (label ^ " BV declaration literal")
          |> Vir.bv_literal
        in
        let literal10 = literal "ten" width 10 in
        let equivalent10 =
          Vir.bv_binary Bv_add_mod (literal "two-fifty" width 250)
            (literal "sixteen" width 16)
          |> ok "BV symbolic equivalent argument"
        in
        let apply argument argument_type result_type =
          let application =
            Symbolic_application_private.create declaration ~type_arguments:[]
              ~arguments:[ Vir.Recursive_bv_argument argument ]
              ~argument_types:[ argument_type ] ~result_type ~span
          in
          Result.bind application (fun application ->
              Vir.symbolic_application ~aggregate_type:(fun _ -> None)
                application)
        in
        let applied_literal =
          apply literal10 (Parametric_type.Bit_vector width)
            (Parametric_type.Bit_vector width)
          |> ok "literal BV symbolic application"
        and applied_equivalent =
          apply equivalent10 (Parametric_type.Bit_vector width)
            (Parametric_type.Bit_vector width)
          |> ok "computed BV symbolic application"
        in
        let applied_literal =
          match applied_literal with
          | Vir.Bv_application term -> term
          | Integer_application _ | Boolean_application _
          | Aggregate_application _ | Parametric_application _ ->
              failwith "BV declaration produced a non-BV application"
        and applied_equivalent =
          match applied_equivalent with
          | Vir.Bv_application term -> term
          | Integer_application _ | Boolean_application _
          | Aggregate_application _ | Parametric_application _ ->
              failwith "computed BV declaration produced a non-BV application"
        in
        if
          Result.is_ok
            (apply (literal "other-target" same_size_other_target 10)
               (Parametric_type.Bit_vector same_size_other_target)
               (Parametric_type.Bit_vector width))
        then
          failwith "BV symbolic application accepted a different target argument";
        if
          Result.is_ok
            (apply literal10 (Parametric_type.Bit_vector width)
               (Parametric_type.Bit_vector width16))
        then failwith "BV symbolic application accepted a different result width";
        let goal =
          Vir.bv_equal applied_literal applied_equivalent
          |> ok "BV symbolic declaration congruence"
        in
        let obligation =
          Vir.
            { obligation_index = 0;
              function_ref =
                { function_index = 992; function_name = "bv_declaration" };
              kind = Assertion { assertion_ordinal = 0 };
              span;
              assumptions = [];
              required_preceding_safety = [];
              path_condition = [];
              goal;
              projection_symbols = [];
              logical_constant_instances = [];
              logical_constant_equations = [] }
        in
        (match
           Z3_bridge.solve_vir ~requires:[]
             { timeout_ms = 10_000; model = true }
             obligation
         with
        | Ok Z3_bridge.Verified -> ()
        | Ok _ -> failwith "BV symbolic declaration congruence did not verify"
        | Error error -> failwith (Z3_bridge.error_to_string error));
        let detached, identities =
          match Z3_bridge.detach_vir ~requires:[] obligation with
          | Ok result -> result
          | Error error -> failwith (Z3_bridge.error_to_string error)
        in
        if identities <> [] then
          failwith "BV declaration congruence unexpectedly projected a model";
        (match solve_on_detached_worker detached with
        | { worker_exception = None;
            vc_results = [ { result_outcome = Ok Detached_verified; _ } ];
            _ } ->
            ()
        | _ -> failwith "BV declaration congruence worker did not verify");
        let recursive_goal =
          Vir.Integer_compare
            ( Vir.Greater_or_equal,
              Vir.bv_to_int_unsigned applied_literal,
              Vir.Integer_constant Z.zero )
        in
        let recursive_query =
          Recursive_spec_encoding.For_testing.internal_bv_proof_query ~span
            recursive_goal
          |> Result.map_error Recursive_spec_encoding.error_to_string
          |> ok "recursive BV symbolic declaration translation"
        in
        (match
           Z3_bridge.solve_query { timeout_ms = 10_000; model = true }
             recursive_query
         with
        | Ok Z3_bridge.Verified -> ()
        | Ok _ ->
            failwith "recursive BV symbolic declaration query did not verify"
        | Error error -> failwith (Z3_bridge.error_to_string error));
        let binding =
          Sst.
            { id = 92;
              name = "bv_parameter";
              typ = Bit_vector width;
              uniqueness = Definitely_aliased;
              span }
        in
        let pattern =
          Sst.{ pattern_desc = Bind binding; typ = binding.typ; span }
        in
        let variable =
          Sst.
            { typ = binding.typ;
              expression_desc =
                Variable
                  { binding; use_uniqueness = Definitely_aliased };
              span }
        in
        let spec result_type =
          Sst.
            { function_id =
                { function_index = 993; function_name = "bv_identity_spec" };
              type_binders = [];
              mode = Spec;
              recursive = false;
              parameters =
                [ Value_parameter
                    { label = None; pattern; optional_default = None } ];
              contracts = empty_contracts;
              body =
                Spec_definition { stage = Logical; expression = variable };
              policy = Default_linear_z3;
              result_type;
              returns_unique_parameter = None;
              span }
        in
        let program definition =
          Sst.
            { policy = Default_linear_z3;
              parametric_adts = [];
              types = [];
              logical_constants = [];
              functions = [ definition ] }
        in
        let valid_program = program (spec (Sst.Bit_vector width)) in
        (match Sst_validation.validate valid_program with
        | Ok _ -> ()
        | Error error ->
            failwith
              ("BV specification declaration failed validation: "
              ^ Sst_validation.error_to_string error));
        (match Parametric_sst_validation_private.validate_program valid_program with
        | Ok () -> ()
        | Error _ ->
            failwith "parametric validator rejected BV specification declaration");
        if
          Result.is_ok
            (Sst_validation.validate
               (program (spec (Sst.Bit_vector same_size_other_target))))
        then
          failwith "BV specification declaration accepted a mismatched result target";
        if
          Result.is_ok
            (Sst_validation.validate
               (program (spec (Sst.Bit_vector unbound_width))))
        then
          failwith "BV specification declaration accepted an unbound result width";
        let exec_binding =
          Sst.
            { id = 94;
              name = "bv_exec_parameter";
              typ = Bit_vector width;
              uniqueness = Definitely_aliased;
              span }
        in
        let exec_pattern =
          Sst.
            { pattern_desc = Bind exec_binding;
              typ = exec_binding.typ;
              span }
        in
        let exec_body =
          Sst.
            { typ = exec_binding.typ;
              expression_desc =
                Variable
                  { binding = exec_binding;
                    use_uniqueness = Definitely_aliased };
              span }
        in
        let exec_definition =
          Sst.
            { function_id =
                { function_index = 994; function_name = "bv_identity_exec" };
              type_binders = [];
              mode = Exec;
              recursive = false;
              parameters =
                [ Value_parameter
                    { label = None;
                      pattern = exec_pattern;
                      optional_default = None } ];
              contracts = empty_contracts;
              body =
                Checked_exec
                  { body = { stage = Runtime; expression = exec_body };
                    provenance = Raw_semantic_body span };
              policy = Default_linear_z3;
              result_type = Bit_vector width;
              returns_unique_parameter = None;
              span }
        in
        let lowered_exec =
          match Symbolic_executor.lower_program (program exec_definition) with
          | Ok lowered -> lowered
          | Error error -> failwith (Symbolic_executor.error_to_string error)
        in
        (match lowered_exec.Vir.functions with
        | [ { exits =
                [ { result = Vir.Bv_result result; projection_symbols; _ } ];
              _ } ]
          when result.role = Vir.Result
               && Vir.sort_equal result.sort (Vir.Bit_vector width)
               && List.exists
                    (fun (symbol : Vir.symbol) ->
                      symbol.role = Vir.Input
                      && Vir.sort_equal symbol.sort (Vir.Bit_vector width))
                    projection_symbols ->
            ()
        | _ ->
            failwith
              "BV executable declaration did not retain exact input/result vectors");
        Ok (Outcome.observation ~status:Outcome.Verified () |> Outcome.project)
      with Failure message -> Error (Failure.make Failure.Runner_internal message))

let vir_bv_model_coordinator_case =
  Suite.case ~name:"typed-vir-bv-models-survive-worker-coordinator-reassembly"
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
    (fun ~environment:_ ~workspace:_ ->
      try
        let target, width8 = target_and_bound_bv_width "8" in
        let _, width64 = target_and_bound_bv_width "64" in
        let source_authority width suffix =
          test_projection_authority
            ~target ~width ~provenance:Imported_registry
            ~outer_authority:Checked_proof
            ~modular_authority:Explicit_axiom ~trusted_dependencies:[]
            ~source_full_key:("vir-model-source-" ^ suffix)
            ~occurrence_full_key:("vir-model-occurrence-" ^ suffix)
            ~operation_span:span
        in
        let converted width suffix value =
          Vir.bv_int_to_bv_mod ~width ~input:(Vir.Integer_constant value)
            ~source_authority:(source_authority width suffix)
        in
        let residue8 = Z.of_int 129
        and residue64 = Z.add (Z.shift_left Z.one 63) (Z.of_int 5) in
        let expected8 = ok "expected BV8 model" (Bv_value.of_z ~width:width8 residue8)
        and expected64 =
          ok "expected BV64 model" (Bv_value.of_z ~width:width64 residue64)
        in
        let symbol symbol_id sort =
          let source_span =
            Diagnostic.
              { span with
                start_pos = { line = symbol_id; column = 1 };
                end_pos = { line = symbol_id; column = 2 } }
          in
          Vir.
            { symbol_id;
              source_name = "duplicate_display_name";
              sort;
              role = Input;
              span = source_span }
        in
        let integer_symbol = symbol 20 Vir.Integer
        and bv8_symbol = symbol 21 (Vir.Bit_vector width8)
        and boolean_symbol = symbol 22 Vir.Boolean
        and bv64_symbol = symbol 23 (Vir.Bit_vector width64) in
        let bv8_term = ok "BV8 model symbol" (Vir.bv_symbol bv8_symbol)
        and bv64_term = ok "BV64 model symbol" (Vir.bv_symbol bv64_symbol) in
        let assumptions =
          [ Vir.Integer_compare
              (Equal, Integer_symbol integer_symbol, Integer_constant (Z.of_int 42));
            Vir.Bv_equal (bv8_term, converted width8 "8" residue8);
            Vir.Boolean_equal
              (Boolean_symbol boolean_symbol, Boolean_constant true);
            Vir.Bv_equal (bv64_term, converted width64 "64" residue64) ]
        in
        let function_ref =
          Vir.{ function_index = 975; function_name = "vir_bv_model_owner" }
        in
        let obligation =
          Vir.
            { obligation_index = 0;
              function_ref;
              kind = Assertion { assertion_ordinal = 0 };
              span;
              assumptions;
              required_preceding_safety = [];
              path_condition = [];
              goal = Boolean_constant false;
              projection_symbols =
                [ bv64_symbol; boolean_symbol; integer_symbol; bv8_symbol ];
              logical_constant_instances = [];
              logical_constant_equations = [] }
        in
        let binding_ids bindings =
          List.map
            (fun (binding : Z3_bridge.model_binding) ->
              binding.symbol.Vir.symbol_id)
            bindings
        in
        Z3_bridge.reset_counters ();
        (match
           Z3_bridge.solve_vir ~requires:[]
             { timeout_ms = 10_000; model = true }
             obligation
         with
        | Ok
            (Z3_bridge.Counterexample
              ([ { symbol = direct_integer;
                   value = Some (Z3_bridge.Integer integer) };
                 { symbol = direct_bv8;
                   value = Some (Z3_bridge.Bit_vector bv8) };
                 { symbol = direct_boolean;
                   value = Some (Z3_bridge.Boolean boolean) };
                 { symbol = direct_bv64;
                   value = Some (Z3_bridge.Bit_vector bv64) } ]
              as bindings))
          when binding_ids bindings = [ 20; 21; 22; 23 ]
               && direct_integer.symbol_id = integer_symbol.symbol_id
               && direct_bv8.symbol_id = bv8_symbol.symbol_id
               && direct_boolean.symbol_id = boolean_symbol.symbol_id
               && direct_bv64.symbol_id = bv64_symbol.symbol_id
               && direct_integer.span = integer_symbol.span
               && direct_bv8.span = bv8_symbol.span
               && direct_boolean.span = boolean_symbol.span
               && direct_bv64.span = bv64_symbol.span
               && Z.equal integer (Z.of_int 42)
               && Bv_value.equal bv8 expected8 && boolean
               && Bv_value.equal bv64 expected64 ->
            ()
        | Ok _ -> failwith "direct mixed VIR model lost identity, order, or value"
        | Error error -> failwith (Z3_bridge.error_to_string error));
        let direct = Z3_bridge.counters () in
        if
          direct.contexts_created <> 1 || direct.contexts_cleaned <> 1
          || direct.contexts_live <> 0 || direct.selected_logics <> [ "general" ]
        then failwith "direct mixed VIR model lifecycle changed";
        let body =
          Sst.{ expression_desc = Unit_constant; typ = Unit; span }
        in
        let definition =
          Sst_normalize.checked_exec_raw
            ~function_id:
              Sst.
                { function_index = function_ref.function_index;
                  function_name = function_ref.function_name }
            ~recursive:false ~parameters:[] ~contracts:Sst.empty_contracts
            ~body ~result_type:Sst.Unit ~returns_unique_parameter:None ~span
        in
        let program =
          Sst.
            { policy = Default_linear_z3;
              parametric_adts = [];
              types = [];
              logical_constants = [];
              functions = [ definition ] }
        in
        let solver_policy =
          match Solver_policy_private.create_default ~timeout_ms:10_000 with
          | Ok policy -> policy
          | Error error ->
              failwith (Solver_policy_private.error_to_string error)
        in
        let preflight =
          match
            Verification_solver_private.preflight ~solver_policy program
          with
          | Ok preflight -> preflight
          | Error _ -> failwith "mixed model coordinator preflight failed"
        in
        let threaded =
          match
            Verification_solver_private.configure_threaded ~solver_policy
              preflight
          with
          | Ok threaded -> threaded
          | Error _ -> failwith "mixed model coordinator configuration failed"
        in
        let execution =
          Vir.
            { function_ref;
              mode = Sst.Exec;
              body_provenance = Sst.Raw_semantic_body span;
              policy = Sst.Default_linear_z3;
              trusted_summary_uses = [];
              reached_callback_calls = [];
              owned_tree_transitions = [];
              shared_scalar_heap_reads = [];
              shared_scalar_heap_writes = [];
              obligations = [ obligation ];
              exits = [] }
        in
        let request =
          Verification_pipeline.
            { definition;
              receipt_source = false;
              receipt_dependent = false;
              proof_activation_routes = [];
              execution }
        in
        if not (Vir.function_execution_has_native_bv_projection execution) then
          failwith "native BV model execution lost its assistance classification";
        let prepare_request request =
          Verification_solver_private.prepare_function threaded
            ~source_ordinal:0 request
          |> ok "mixed model coordinator preparation"
        in
        let prepare () = prepare_request request in
        let prepared = prepare () in
        let worker =
          prepared |> Verification_solver_private.worker_request
          |> run_worker_request
        in
        let raw_values, raw_vc =
          match (worker.worker_exception, worker.vc_results) with
          | None,
            [ ({ result_outcome =
                   Ok (Z3_bridge.Detached_counterexample values);
                 attempt_telemetry = [ telemetry ];
                 ordinary_contribution = true;
                 _ } as vc) ]
            when telemetry.contexts_created = 1
                 && telemetry.contexts_cleaned = 1
                 && telemetry.contexts_live = 0
                 && telemetry.selected_logics = [ "general" ] ->
              (values, vc)
          | Some (index, message), _ ->
              failwith
                (Printf.sprintf "mixed BV worker exception at VC %d: %s" index
                   message)
          | None, [ { result_outcome = Error message; _ } ] ->
              failwith ("mixed BV worker backend error: " ^ message)
          | None,
            [ { result_outcome = Ok Z3_bridge.Detached_verified; _ } ] ->
              failwith "mixed BV worker incorrectly verified the false goal"
          | None,
            [ { result_outcome = Ok (Detached_inconclusive _); _ } ] ->
              failwith "mixed BV worker was inconclusive"
          | None,
            [ { result_outcome = Ok (Detached_counterexample values);
                attempt_telemetry;
                ordinary_contribution;
                _ } ] ->
              failwith
                (Printf.sprintf
                   "mixed BV worker returned values=%d telemetry=%d ordinary=%b"
                   (List.length values) (List.length attempt_telemetry)
                   ordinary_contribution)
          | None, results ->
              failwith
                (Printf.sprintf "mixed BV worker returned VC-results=%d"
                   (List.length results))
        in
        let integer_identity, bv8_identity, boolean_identity, bv64_identity,
            width8_reference, width64_reference =
          match raw_values with
          | [ Some (Z3_bridge.Detached_integer (integer_identity, "42"));
              Some
                (Detached_bit_vector
                  (bv8_identity, width8_reference, observed8));
              Some (Detached_boolean (boolean_identity, true));
              Some
                (Detached_bit_vector
                  (bv64_identity, width64_reference, observed64)) ]
            when String.equal observed8 (Z.to_string residue8)
                 && String.equal observed64 (Z.to_string residue64)
                 &&
                 (match
                    ( Bv_width.decode_reference_for_worker width8_reference,
                      Bv_width.decode_reference_for_worker width64_reference )
                  with
                 | Ok observed8, Ok observed64 ->
                     Bv_width.equal observed8 width8
                     && Bv_width.equal observed64 width64
                 | Error _, _ | _, Error _ -> false) ->
              ( integer_identity,
                bv8_identity,
                boolean_identity,
                bv64_identity,
                width8_reference,
                width64_reference )
          | _ -> failwith "worker mixed model transport changed identity or order"
        in
        if
          List.length
            (List.sort_uniq String.compare
               [ integer_identity; bv8_identity; boolean_identity;
                 bv64_identity ])
          <> 4
        then failwith "duplicate display names collapsed projection identities";
        let committed =
          Verification_solver_private.commit_function prepared worker
          |> ok "mixed model coordinator commit"
        in
        (match committed with
        | [ { Solver_backend.outcome =
                Counterexample
                  [ { symbol = committed_integer;
                      value = Some (Integer integer) };
                    { symbol = committed_bv8;
                      value = Some (Bit_vector bv8) };
                    { symbol = committed_boolean;
                      value = Some (Boolean boolean) };
                    { symbol = committed_bv64;
                      value = Some (Bit_vector bv64) } ];
              _ } ]
          when committed_integer.symbol_id = 20
               && committed_bv8.symbol_id = 21
               && committed_boolean.symbol_id = 22
               && committed_bv64.symbol_id = 23
               && committed_integer.span = integer_symbol.span
               && committed_bv8.span = bv8_symbol.span
               && committed_boolean.span = boolean_symbol.span
               && committed_bv64.span = bv64_symbol.span
               && Z.equal integer (Z.of_int 42)
               && Bv_value.equal bv8 expected8 && boolean
               && Bv_value.equal bv64 expected64 ->
            ()
        | _ -> failwith "coordinator did not preserve the mixed structured model");
        let structural_detached, structural_manifest =
          match Z3_bridge.detach_vir ~requires:[] obligation with
          | Ok detached -> detached
          | Error error -> failwith (Z3_bridge.error_to_string error)
        in
        if List.length structural_manifest <> 4 then
          failwith "structural-route model manifest lost mixed projections";
        let structural_worker =
          run_worker_request
            Function_vc_worker_private.
              { source_ordinal = 0;
                vcs =
                  [ { canonical_index = 0;
                      timeout_ms = 10_000;
                      rlimit = 1_000_000;
                      route =
                        Structural_rank
                          { direct_query = structural_detached;
                            deliver_original_model = true } } ] }
        in
        (match (structural_worker.worker_exception, structural_worker.vc_results) with
        | None,
          [ { result_outcome = Ok (Z3_bridge.Detached_counterexample values);
              ordinary_contribution = false;
              _ } ]
          when values = raw_values ->
            ()
        | _ ->
            failwith
              "structural-rank worker route dropped its requested BV model");
        let structural_job =
          match
            Vc_solver_job_private.prepare_direct
              ~route:Vc_solver_job_private.Structural_rank ~canonical_index:0
              ~solver_policy obligation
          with
          | Ok job -> job
          | Error error ->
              failwith (Vc_solver_job_private.error_to_string error)
        in
        let structural_solved =
          Vc_solver_job_private.solve_prepared structural_job
        in
        (match Vc_solver_job_private.outcome structural_solved with
        | Ok
            (Solver_backend.Counterexample
              [ { symbol = structural_integer; value = Some (Integer integer) };
                { symbol = structural_bv8; value = Some (Bit_vector bv8) };
                { symbol = structural_boolean; value = Some (Boolean boolean) };
                { symbol = structural_bv64; value = Some (Bit_vector bv64) } ])
          when structural_integer = integer_symbol
               && structural_bv8 = bv8_symbol
               && structural_boolean = boolean_symbol
               && structural_bv64 = bv64_symbol
               && Z.equal integer (Z.of_int 42)
               && Bv_value.equal bv8 expected8 && boolean
               && Bv_value.equal bv64 expected64 ->
            ()
        | _ ->
            failwith "local structural-rank route dropped its requested BV model");
        if
          Option.is_some
            (Vc_solver_job_private.ordinary_contribution structural_solved)
        then failwith "structural-rank model delivery changed accounting";
        let reject_response label values =
          let prepared = prepare () in
          let altered_vc =
            Function_vc_worker_private.
              { raw_vc with
                result_outcome =
                  Ok (Z3_bridge.Detached_counterexample values);
                attempt_telemetry = [] }
          in
          let altered_worker = { worker with vc_results = [ altered_vc ] } in
          if
            Result.is_ok
              (Verification_solver_private.commit_function prepared
                 altered_worker)
          then failwith (label ^ " worker model response was accepted")
        in
        let nth index = List.nth raw_values index in
        reject_response "missing projection" (List.tl raw_values);
        reject_response "absent mandatory BV"
          [ nth 0; None; nth 2; nth 3 ];
        reject_response "reordered projection"
          [ nth 1; nth 0; nth 2; nth 3 ];
        reject_response "duplicated projection identity"
          [ nth 0; nth 0; nth 2; nth 3 ];
        reject_response "foreign projection identity"
          [ nth 0;
            Some
              (Z3_bridge.Detached_bit_vector
                 ("foreign-projection", width8_reference, Z.to_string residue8));
            nth 2; nth 3 ];
        reject_response "wrong projection sort"
          [ nth 0; Some (Z3_bridge.Detached_integer (bv8_identity, "129"));
            nth 2; nth 3 ];
        let _, target64_width8 =
          target_and_bound_bv_width ~physical_width:64 "8"
        in
        let capability = Bv_backend_capability_receipt_private.capability () in
        let target64_reference =
          Bv_width.encode_reference capability target64_width8
          |> ok "target64 BV8 width reference"
        in
        reject_response "same numeric width from another target"
          [ nth 0;
            Some
              (Z3_bridge.Detached_bit_vector
                 (bv8_identity, target64_reference, Z.to_string residue8));
            nth 2; nth 3 ];
        List.iter
          (fun residue ->
            reject_response "malformed BV residue"
              [ nth 0;
                Some
                  (Z3_bridge.Detached_bit_vector
                     (bv8_identity, width8_reference, residue));
                nth 2; nth 3 ])
          [ ""; "00"; "+1"; "-1"; " 1"; "0x1"; "256";
            String.make 1235 '9' ];
        let private_aggregate_type =
          Vir.
            { aggregate_type_index = 976;
              aggregate_type_name = "private_model_box";
              aggregate_type_arguments = [] }
        in
        let private_constructor =
          Sst.
            { constructor_type =
                { type_index = 976; type_name = "private_model_box" };
              constructor_index = 0;
              constructor_name = "Private_model_box" }
        in
        let private_aggregate =
          Vir.
            { aggregate_type = private_aggregate_type;
              aggregate_desc =
                Aggregate_constructor
                  { constructor = private_constructor;
                    arguments =
                      [ Recursive_bv_argument
                          (converted width8 "private" residue8) ] } }
        in
        let private_selector =
          Vir.
            { selector_domain = private_aggregate_type;
              selector_range = Bit_vector width8;
              selector_namespace =
                "t976_private_model_box_c0_Private_model_box";
              selector_index = 0;
              selector_name = "$arg0";
              selector_path = [] }
        in
        let private_selected =
          ok "private aggregate BV selector"
            (Vir.bv_selector private_selector private_aggregate)
        in
        let private_obligation =
          Vir.
            { obligation with
              obligation_index = 1;
              assumptions =
                [ Bv_equal
                    (bv8_term, converted width8 "private-binding" residue8) ];
              goal =
                Bv_not_equal
                  (private_selected, converted width8 "private-goal" residue8);
              projection_symbols = [ bv8_symbol ] }
        in
        let private_request =
          Verification_pipeline.
            { request with
              execution = { execution with obligations = [ private_obligation ] } }
        in
        let private_prepared = prepare_request private_request in
        let private_worker =
          private_prepared |> Verification_solver_private.worker_request
          |> run_worker_request
        in
        (match (private_worker.worker_exception, private_worker.vc_results) with
        | None,
          [ { result_outcome =
                Ok
                  (Z3_bridge.Detached_counterexample
                    [ Some
                        (Detached_bit_vector
                          (_, private_width_reference, private_residue)) ]);
              ordinary_contribution = false;
              _ } ] ->
            if
              not
                (String.equal private_residue (Z.to_string residue8)
                &&
                match
                  Bv_width.decode_reference_for_worker private_width_reference
                with
                | Ok observed -> Bv_width.equal observed width8
                | Error _ -> false)
            then
              failwith
                "logical-aggregate route changed its requested BV model value"
        | _ ->
            failwith
              "logical-aggregate route dropped its requested BV countermodel");
        (match
           Verification_solver_private.commit_function private_prepared
             private_worker
           |> ok "private aggregate coordinator commit"
         with
        | [ { Solver_backend.outcome =
                Counterexample
                  [ { symbol; value = Some (Bit_vector observed) } ];
              _ } ]
          when symbol = bv8_symbol && Bv_value.equal observed expected8 ->
            ()
        | _ ->
            failwith
              "coordinator dropped aggregate-route BV model metadata");
        let local_aggregate_job =
          match
            Vc_solver_job_private.prepare_direct
              ~route:Vc_solver_job_private.Logical_aggregate
              ~canonical_index:1 ~solver_policy private_obligation
          with
          | Ok job -> job
          | Error error ->
              failwith (Vc_solver_job_private.error_to_string error)
        in
        let local_aggregate_solved =
          Vc_solver_job_private.solve_prepared local_aggregate_job
        in
        (match Vc_solver_job_private.outcome local_aggregate_solved with
        | Ok
            (Solver_backend.Counterexample
              [ { symbol; value = Some (Bit_vector observed) } ])
          when symbol = bv8_symbol && Bv_value.equal observed expected8 ->
            ()
        | _ ->
            failwith
              "local logical-aggregate route dropped its requested BV model");
        if
          Option.is_some
            (Vc_solver_job_private.ordinary_contribution
               local_aggregate_solved)
        then failwith "logical-aggregate model delivery changed accounting";
        let nonbv_private_obligation =
          Vir.
            { private_obligation with
              obligation_index = 2;
              projection_symbols = [ integer_symbol ] }
        in
        let nonbv_private_request =
          Verification_pipeline.
            { request with
              execution =
                { execution with obligations = [ nonbv_private_obligation ] } }
        in
        let nonbv_private_prepared =
          prepare_request nonbv_private_request
        in
        let nonbv_private_worker =
          nonbv_private_prepared
          |> Verification_solver_private.worker_request
          |> run_worker_request
        in
        (match
           ( nonbv_private_worker.worker_exception,
             nonbv_private_worker.vc_results )
         with
        | None,
          [ { result_outcome = Ok (Z3_bridge.Detached_counterexample []);
              ordinary_contribution = false;
              _ } ] ->
            ()
        | _ ->
            failwith
              "non-BV-only logical-aggregate route exposed its private model");
        (match
           Verification_solver_private.commit_function nonbv_private_prepared
             nonbv_private_worker
           |> ok "non-BV private aggregate coordinator commit"
         with
        | [ { Solver_backend.outcome = Counterexample []; _ } ] -> ()
        | _ ->
            failwith
              "coordinator exposed a non-BV-only logical-aggregate model");
        let recursive_worker =
          run_worker_request
            Function_vc_worker_private.
              { source_ordinal = 0;
                vcs =
                  [ { canonical_index = 0;
                      timeout_ms = 10_000;
                      rlimit = 1_000_000;
                      route =
                        Recursive
                          { initial_query = structural_detached;
                            retry_query = None;
                            retry_eligible = false;
                            ground_plan = None;
                            force_initial_inconclusive = false;
                            retry_control = Retry_real;
                            retry_rlimit = 1_000_000 } } ] }
        in
        (match (recursive_worker.worker_exception, recursive_worker.vc_results) with
        | None,
          [ { result_outcome = Ok (Z3_bridge.Detached_counterexample []);
              ordinary_contribution = false;
              _ } ] ->
            ()
        | _ -> failwith "recursive private query exposed its projected BV model");
        let retry_worker =
          run_worker_request
            Function_vc_worker_private.
              { source_ordinal = 0;
                vcs =
                  [ { canonical_index = 0;
                      timeout_ms = 10_000;
                      rlimit = 1_000_000;
                      route =
                        Recursive
                          { initial_query = structural_detached;
                            retry_query = Some structural_detached;
                            retry_eligible = true;
                            ground_plan = None;
                            force_initial_inconclusive = true;
                            retry_control = Retry_real;
                            retry_rlimit = 1_000_000 } } ] }
        in
        (match (retry_worker.worker_exception, retry_worker.vc_results) with
        | None,
          [ { result_outcome = Ok (Z3_bridge.Detached_inconclusive _);
              ordinary_contribution = false;
              retry_observation =
                { retry_used = true; retry_queries = 1; _ };
              _ } ] ->
            ()
        | _ -> failwith "recursive retry exposed an original projected BV model");
        let lifecycle = Z3_bridge.counters () in
        if
          lifecycle.contexts_created <> lifecycle.contexts_cleaned
          || lifecycle.contexts_live <> 0
        then failwith "mixed BV model failure paths leaked a solver context";
        let independent_render width value =
          let width_int = Bv_width.to_int width in
          let modulus = Z.shift_left Z.one width_int in
          let high = Z.shift_left Z.one (width_int - 1) in
          let signed = if Z.geq value high then Z.sub value modulus else value in
          let bits =
            String.init width_int (fun index ->
                if Z.testbit value (width_int - index - 1) then '1' else '0')
          in
          Printf.sprintf "bits[%d]=%s unsigned=%s signed=%s" width_int bits
            (Z.to_string value) (Z.to_string signed)
        in
        List.iter
          (fun width_text ->
            let width = bound_bv_width width_text in
            let width_int = Bv_width.to_int width in
            let modulus = Z.shift_left Z.one width_int in
            let high = Z.shift_left Z.one (width_int - 1) in
            List.iter
              (fun value ->
                let bv = ok "rendered BV value" (Bv_value.of_z ~width value) in
                let expected = independent_render width value in
                if not (String.equal (Bv_value.render bv) expected) then
                  failwith
                    (Printf.sprintf "BV rendering changed at width %d" width_int))
              [ Z.zero; high; Z.pred modulus ])
          [ "1"; "8"; "32"; "64"; "4096" ];
        ignore width64_reference;
        Ok (Outcome.observation ~status:Outcome.Verified () |> Outcome.project)
      with Failure message -> Error (Failure.make Failure.Runner_internal message))

let bv_quantifier_binder_feature_case =
  Suite.case ~name:"unused-bv-quantifier-binder-selects-general-solver"
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
    (fun ~environment:_ ~workspace:_ ->
      try
        let width = bound_bv_width "8" in
        let builder = Logic_ir.create () in
        let binder =
          Logic_ir.bind builder ~name:"unused" ~sort:(Logic_ir.Bv width) ~span
          |> logic_ok "BV existential binder"
        in
        let quantified =
          Logic_ir.exists_term builder ~binders:[ binder ]
            ~body:(Logic_ir.bool ~span true) ~qid:"bv-unused-exists"
            ~skid:"bv-unused-exists-sk" ~span
          |> logic_ok "BV existential"
        in
        let assertion =
          Logic_ir.not_ ~span quantified |> logic_ok "false BV existential"
        in
        let query =
          Logic_ir.query builder ~axioms:[] ~assertions:[ assertion ] ~requires:[]
            ~span
          |> logic_ok "unused BV binder query"
        in
        if not (List.mem Logic_ir.Bit_vectors (Logic_ir.requirements query)) then
          failwith "unused BV quantifier binder omitted the BV feature";
        Z3_bridge.reset_counters ();
        (match
           Z3_bridge.solve_query { timeout_ms = 10_000; model = true } query
         with
        | Ok Z3_bridge.Verified -> ()
        | Ok _ -> failwith "direct unused BV binder query was not verified"
        | Error error -> failwith (Z3_bridge.error_to_string error));
        let direct = Z3_bridge.counters () in
        if direct.selected_logics <> [ "general" ] then
          failwith "direct unused BV binder query did not select general solver";
        let detached = Z3_bridge.detach_query query in
        let attempt =
          Z3_bridge.solve_detached_query_local ~controlled:Z3_bridge.Real
            ~timeout_ms:10_000 ~rlimit:1_000_000 ~model:true detached
        in
        (match attempt.detached_result with
        | Ok Z3_bridge.Detached_verified -> ()
        | Ok _ -> failwith "detached unused BV binder query was not verified"
        | Error message -> failwith message);
        if attempt.detached_telemetry.selected_logics <> [ "general" ] then
          failwith "detached unused BV binder query did not select general solver";
        let nested_builder = Logic_ir.create () in
        let reused =
          Logic_ir.bind nested_builder ~name:"reused" ~sort:(Logic_ir.Bv width)
            ~span
          |> logic_ok "reused BV binder"
        in
        let inner =
          Logic_ir.exists_term nested_builder ~binders:[ reused ]
            ~body:(Logic_ir.bool ~span true) ~qid:"bv-inner-exists"
            ~skid:"bv-inner-exists-sk" ~span
          |> logic_ok "inner reused-binder existential"
        in
        let outer =
          Logic_ir.exists_term nested_builder ~binders:[ reused ] ~body:inner
            ~qid:"bv-outer-exists" ~skid:"bv-outer-exists-sk" ~span
          |> logic_ok "outer reused-binder existential"
        in
        let nested_assertion =
          Logic_ir.not_ ~span outer |> logic_ok "false nested BV existential"
        in
        let nested_query =
          Logic_ir.query nested_builder ~axioms:[]
            ~assertions:[ nested_assertion ] ~requires:[] ~span
          |> logic_ok "nested reused-binder query"
        in
        Z3_bridge.reset_counters ();
        (match
           Z3_bridge.solve_query { timeout_ms = 10_000; model = true }
             nested_query
         with
        | Ok Z3_bridge.Verified -> ()
        | Ok _ -> failwith "direct nested reused-binder query was not verified"
        | Error error -> failwith (Z3_bridge.error_to_string error));
        if (Z3_bridge.counters ()).selected_logics <> [ "general" ] then
          failwith "direct nested reused-binder query did not use general solver";
        let nested_attempt =
          Z3_bridge.solve_detached_query_local ~controlled:Z3_bridge.Real
            ~timeout_ms:10_000 ~rlimit:1_000_000 ~model:true
            (Z3_bridge.detach_query nested_query)
        in
        (match nested_attempt.detached_result with
        | Ok Z3_bridge.Detached_verified -> ()
        | Ok _ -> failwith "detached nested reused-binder query was not verified"
        | Error message -> failwith message);
        if nested_attempt.detached_telemetry.selected_logics <> [ "general" ] then
          failwith
            "detached nested reused-binder query did not use general solver";
        Ok (Outcome.observation ~status:Outcome.Verified () |> Outcome.project)
      with Failure message -> Error (Failure.make Failure.Runner_internal message))

let vir_bv_quantifier_schema_case =
  Suite.case ~name:"vir-bv-quantifiers-revalidate-exact-schema-before-context"
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
    (fun ~environment:_ ~workspace:_ ->
      try
        let _, width = target_and_bound_bv_width "8" in
        let _, width16 = target_and_bound_bv_width "16" in
        let _, other_target =
          target_and_bound_bv_width ~physical_width:64 "8"
        in
        let symbol id name width =
          Vir.
            { symbol_id = id;
              source_name = name;
              sort = Bit_vector width;
              role = Local;
              span }
        in
        let binder = symbol 1201 "shadowed_bv" width in
        let binder_term = Vir.bv_symbol binder |> ok "BV quantifier binder" in
        let zero =
          Bv_value.of_z ~width Z.zero |> ok "BV quantifier zero"
          |> Vir.bv_literal
        in
        let declaration parameter_count =
          Symbolic_application_private.declare
            ~marker_id:("quantifier.trigger." ^ string_of_int parameter_count)
            ~declaration_index:(1200 + parameter_count)
            ~declaration_name:("bv_trigger_" ^ string_of_int parameter_count)
            ~canonical_path:
              ("Internal.bv_trigger_" ^ string_of_int parameter_count)
            ~value_uid:("bv-trigger-uid-" ^ string_of_int parameter_count)
            ~source_file:span.file ~compilation_identity:"bv-quantifier-schema"
            ~declaration_span:span ~type_binders:[]
            ~parameter_types:
              (List.init parameter_count (fun _ ->
                   Parametric_type.Bit_vector width))
            ~result_type:(Parametric_type.Bit_vector width)
          |> ok "BV quantifier trigger declaration"
        in
        let trigger declaration arguments =
          let argument_types =
            List.map (fun _ -> Parametric_type.Bit_vector width) arguments
          in
          let application =
            Symbolic_application_private.create declaration ~type_arguments:[]
              ~arguments:
                (List.map
                   (fun argument -> Vir.Recursive_bv_argument argument)
                   arguments)
              ~argument_types
              ~result_type:(Parametric_type.Bit_vector width) ~span
            |> ok "BV quantifier trigger application"
          in
          Vir.symbolic_application ~aggregate_type:(fun _ -> None) application
          |> ok "BV quantifier trigger term"
        in
        let one_trigger = trigger (declaration 1) [ binder_term ] in
        let metadata kind owner index typ =
          Logic_quantifier_private.create ~kind ~owner ~binder_index:index
            ~binder_type:typ ~span
        in
        let singleton kind owner typ =
          metadata kind owner 0 typ |> Logic_quantifier_private.singleton
        in
        let sort_of_type = function
          | Parametric_type.Bit_vector width -> Ok (Vir.Bit_vector width)
          | _ -> Error "BV quantifier test received a non-BV schema"
        in
        let inner_schema =
          singleton Logic_quantifier_private.Exists "bv-inner-shadow"
            (Parametric_type.Bit_vector width)
        in
        let inner =
          Vir.make_boolean_quantifier ~sort_of_type ~schema:inner_schema
            ~binders:[ binder ]
            ~body:(Vir.bv_equal binder_term zero |> ok "inner BV equality")
            ~trigger:None
          |> ok "inner BV existential"
        in
        let xor_zero =
          Vir.bv_binary Bv_xor binder_term binder_term
          |> ok "BV quantifier xor"
          |> fun term -> Vir.bv_equal term zero |> ok "BV quantifier xor zero"
        in
        let outer_schema =
          singleton Logic_quantifier_private.Forall "bv-outer-shadow"
            (Parametric_type.Bit_vector width)
        in
        let outer =
          Vir.make_boolean_quantifier ~sort_of_type ~schema:outer_schema
            ~binders:[ binder ]
            ~body:(Vir.Boolean_and (xor_zero, Vir.Exists_term inner))
            ~trigger:(Some one_trigger)
          |> ok "outer BV universal"
        in
        let obligation goal =
          Vir.
            { obligation_index = 0;
              function_ref =
                { function_index = 1200; function_name = "bv_quantifier_schema" };
              kind = Assertion { assertion_ordinal = 0 };
              span;
              assumptions = [];
              required_preceding_safety = [];
              path_condition = [];
              goal;
              projection_symbols = [];
              logical_constant_instances = [];
              logical_constant_equations = [] }
        in
        let valid_goal = Vir.Forall_term outer in
        let valid_obligation = obligation valid_goal in
        (match
           Z3_bridge.solve_vir ~requires:[]
             { timeout_ms = 10_000; model = false }
             valid_obligation
         with
        | Ok Z3_bridge.Verified -> ()
        | Ok _ -> failwith "valid VIR BV quantifier did not verify"
        | Error error -> failwith (Z3_bridge.error_to_string error));
        let detached, projections =
          match Z3_bridge.detach_vir ~requires:[] valid_obligation with
          | Ok detached -> detached
          | Error error -> failwith (Z3_bridge.error_to_string error)
        in
        if projections <> [] then
          failwith "BV quantifier unexpectedly projected a model";
        (match solve_on_detached_worker detached with
        | { worker_exception = None;
            vc_results = [ { result_outcome = Ok Detached_verified; _ } ];
            _ } ->
            ()
        | _ -> failwith "detached VIR BV quantifier did not verify");
        let recursive_query =
          Recursive_spec_encoding.For_testing.internal_bv_proof_query ~span
            valid_goal
          |> Result.map_error Recursive_spec_encoding.error_to_string
          |> ok "recursive VIR BV quantifier query"
        in
        (match
           Z3_bridge.solve_query { timeout_ms = 10_000; model = false }
             recursive_query
         with
        | Ok Z3_bridge.Verified -> ()
        | Ok _ -> failwith "recursive VIR BV quantifier did not verify"
        | Error error -> failwith (Z3_bridge.error_to_string error));
        let second_binder = symbol 1202 "second_bv" width in
        let second_term =
          Vir.bv_symbol second_binder |> ok "second BV quantifier binder"
        in
        let pair_schema =
          Logic_quantifier_private.vector
            [ metadata Logic_quantifier_private.Forall "bv-pair" 0
                (Parametric_type.Bit_vector width);
              metadata Logic_quantifier_private.Forall "bv-pair" 1
                (Parametric_type.Bit_vector width) ]
          |> ok "paired BV quantifier schema"
        in
        let pair_trigger =
          trigger (declaration 2) [ binder_term; second_term ]
        in
        let pair =
          Vir.make_boolean_quantifier ~sort_of_type ~schema:pair_schema
            ~binders:[ binder; second_binder ] ~body:(Boolean_constant true)
            ~trigger:(Some pair_trigger)
          |> ok "paired BV quantifier"
        in
        let stale_schema =
          singleton Logic_quantifier_private.Forall "bv-stale-schema"
            (Parametric_type.Bit_vector width16)
        in
        let copied_schema =
          singleton Logic_quantifier_private.Forall "bv-copied-schema"
            (Parametric_type.Bit_vector width)
        in
        let malformed =
          [ ( "BV quantifier numeric width",
              Vir.For_testing.replace_boolean_quantifier_binders outer
                [ symbol 1201 "shadowed_bv" width16 ] );
            ( "BV quantifier target width",
              Vir.For_testing.replace_boolean_quantifier_binders outer
                [ symbol 1201 "shadowed_bv" other_target ] );
            ( "BV quantifier stale schema",
              Vir.For_testing.replace_boolean_quantifier_schema outer stale_schema );
            ( "BV quantifier copied schema identity",
              Vir.For_testing.replace_boolean_quantifier_schema outer copied_schema );
            ( "BV quantifier schema arity",
              Vir.For_testing.replace_boolean_quantifier_schema outer pair_schema );
            ( "BV quantifier duplicate binder",
              Vir.For_testing.replace_boolean_quantifier_binders pair
                [ binder; binder ] );
            ( "BV quantifier trigger coverage",
              Vir.For_testing.replace_boolean_quantifier_trigger pair
                (Some one_trigger) );
            ( "BV existential trigger policy",
              Vir.For_testing.replace_boolean_quantifier_trigger inner
                (Some one_trigger) ) ]
        in
        List.iter
          (fun (label, quantifier) ->
            let goal =
              if
                Logic_quantifier_private.vector_kind
                  quantifier.Vir.boolean_quantifier_schema
                = Logic_quantifier_private.Forall
              then Vir.Forall_term quantifier
              else Vir.Exists_term quantifier
            in
            let before = (Z3_bridge.counters ()).contexts_created in
            (match
               Z3_bridge.solve_vir ~requires:[]
                 { timeout_ms = 10_000; model = false }
                 (obligation goal)
             with
            | Error _ -> ()
            | Ok _ -> failwith (label ^ " passed direct VIR translation"));
            (match Z3_bridge.detach_vir ~requires:[] (obligation goal) with
            | Error _ -> ()
            | Ok _ -> failwith (label ^ " passed detached VIR translation"));
            (match
               Recursive_spec_encoding.For_testing.internal_bv_proof_query
                 ~span goal
             with
            | Error _ -> ()
            | Ok _ -> failwith (label ^ " passed recursive VIR translation"));
            if (Z3_bridge.counters ()).contexts_created <> before then
              failwith (label ^ " created a solver context"))
          malformed;
        Ok (Outcome.observation ~status:Outcome.Verified () |> Outcome.project)
      with Failure message -> Error (Failure.make Failure.Runner_internal message))

let bounded_bv_reference_fields_case =
  Suite.case ~name:"bv-reference-field-bounds-precede-field-copy"
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
    (fun ~environment:_ ~workspace:_ ->
      let frame value = string_of_int (String.length value) ^ ":" ^ value in
      let record width backend =
        String.concat ""
          (List.map frame
             [ "verocaml.bv-width-reference.v1"; width; "profile"; "target";
               backend ])
      in
      let oversized_width = record "12345" "backend"
      and oversized_backend = record "8" (String.make 513 'b') in
      let capability = Bv_backend_capability_receipt_private.capability () in
      let rejected encoded =
        Result.is_error
          (Bv_width.For_testing.decode_reference_fields encoded)
        && Result.is_error (Bv_width.decode_reference capability encoded)
        && Result.is_error (Bv_width.decode_reference_for_worker encoded)
      in
      if rejected oversized_width && rejected oversized_backend then
        Ok (Outcome.observation ~status:Outcome.Verified () |> Outcome.project)
      else
        Error
          (Failure.make Failure.Expectation_mismatch
             "oversized BV reference field reached a copying decoder"))

let nonlinear_query () =
  let ( let* ) = Result.bind in
  let builder = Logic_ir.create () in
  let* left_symbol =
    Logic_ir.declare_function builder ~name:"left" ~domain:[]
      ~range:Logic_ir.Int ~span
  in
  let* right_symbol =
    Logic_ir.declare_function builder ~name:"right" ~domain:[]
      ~range:Logic_ir.Int ~span
  in
  let* left = Logic_ir.apply ~span left_symbol [] in
  let* right = Logic_ir.apply ~span right_symbol [] in
  let* product = Logic_ir.multiply ~span left right in
  let* reverse = Logic_ir.multiply ~span right left in
  let* commutative = Logic_ir.equal ~span product reverse in
  let* counterexample = Logic_ir.not_ ~span commutative in
  Logic_ir.query builder ~axioms:[] ~assertions:[ counterexample ] ~requires:[]
    ~span

let nonlinear_requirement_case =
  Suite.case ~name:"multiplication-propagates-nonlinear-requirement"
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
    (fun ~environment:_ ~workspace:_ ->
      match nonlinear_query () with
      | Ok query
        when List.mem Logic_ir.Nonlinear_integer_arithmetic
               (Logic_ir.requirements query) ->
          Ok
            (Outcome.observation ~status:Outcome.Verified ()
            |> Outcome.project)
      | Ok _ ->
          Error
            (Failure.make Failure.Expectation_mismatch
               "multiplication omitted its nonlinear logic requirement")
      | Error error ->
          Error
            (Failure.make Failure.Runner_internal
               (Logic_ir.error_to_string error)))

let nonlinear_query_case =
  Suite.case ~name:"nonlinear-logic-query-verifies"
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
    (fun ~environment:_ ~workspace:_ ->
      match nonlinear_query () with
      | Error error ->
          Error
            (Failure.make Failure.Runner_internal
               (Logic_ir.error_to_string error))
      | Ok query -> (
          match
            Z3_bridge.solve_query { timeout_ms = 10_000; model = true } query
          with
          | Ok Z3_bridge.Verified ->
              Ok
                (Outcome.observation ~status:Outcome.Verified ()
                |> Outcome.project)
          | Ok _ ->
              Error
                (Failure.make Failure.Expectation_mismatch
                   "nonlinear commutativity query did not verify")
          | Error error ->
              Error
                (Failure.make Failure.Verifier_outcome
                   (Z3_bridge.error_to_string error))))

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [
      verified_logic_case;
      assertion_logic_case;
      nonlinear_requirement_case;
      nonlinear_query_case;
      native_bv_kernel_case;
      bv_model_fault_lifecycle_case;
      vir_bv_structural_case;
      internal_sst_bv_case;
      bv_declaration_schema_case;
      vir_bv_model_coordinator_case;
      bv_quantifier_binder_feature_case;
      vir_bv_quantifier_schema_case;
      bounded_bv_reference_fields_case;
    ]
