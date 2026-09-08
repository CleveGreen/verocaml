open Outcome_test_support

let suite_path = "test/bv_oracle/outcome_cases.ml"

let span =
  Diagnostic.
    { file = "bv_oracle_outcome.ml";
      start_pos = { line = 1; column = 0 };
      end_pos = { line = 1; column = 1 } }

let require condition message = if not condition then failwith message

let ok label = function
  | Ok value -> value
  | Error message -> failwith (label ^ ": " ^ message)

let logic_ok label = function
  | Ok value -> value
  | Error error -> failwith (label ^ ": " ^ Logic_ir.error_to_string error)

type oracle = {
  width : Bv_width.t;
  width_int : int;
  modulus : Z.t;
  mask : Z.t;
  sign_boundary : Z.t;
  literals : (Z.t * Logic_ir.term) list ref;
}

let sealed_environment = lazy (
  let capability = Build_target_profile_private.capability () in
  let profile =
    Build_target_profile_private.authenticate_profile capability
    |> ok "sealed profile"
  in
  let target =
    Build_target_profile_private.authenticate_instances capability
    |> ok "sealed target instances"
    |> List.find (fun target ->
           target.Build_target_profile_private.target_claim.width = 32)
  in
  (profile, target, Bv_backend_capability_receipt_private.capability ()))

let make_oracle width_int =
  let profile, target, backend = Lazy.force sealed_environment in
  let width =
    Bv_width.of_z ~profile backend (Z.of_int width_int)
    |> ok "oracle BV width"
    |> Bv_width.for_instance backend target
    |> ok "target-bound oracle BV width"
  in
  let modulus = Z.shift_left Z.one width_int in
  { width; width_int; modulus; mask = Z.pred modulus;
    sign_boundary = Z.shift_left Z.one (width_int - 1); literals = ref [] }

let normalize oracle value =
  let residue = Z.erem value oracle.modulus in
  if Z.sign residue < 0 then Z.add residue oracle.modulus else residue

let signed oracle value =
  if Z.compare value oracle.sign_boundary >= 0 then
    Z.sub value oracle.modulus
  else value

let int_term value = Logic_ir.int ~span value

let literal oracle value =
  match List.find_opt (fun (cached, _) -> Z.equal cached value) !(oracle.literals) with
  | Some (_, term) -> term
  | None ->
      let term =
        Bv_value.of_z ~width:oracle.width value
        |> ok "oracle input literal"
        |> Logic_ir.bv_literal ~span
        |> logic_ok "oracle literal term"
      in
      oracle.literals := (value, term) :: !(oracle.literals);
      term

let bool_expected actual expected =
  if expected then actual
  else Logic_ir.not_ ~span actual |> logic_ok "negated Boolean expectation"

let bv_expected oracle actual expected =
  Logic_ir.bv_eq ~span actual (literal oracle expected)
  |> logic_ok "BV result expectation"

let int_expected actual expected =
  Logic_ir.equal ~span actual (int_term expected)
  |> logic_ok "Int result expectation"

let binary constructor left right =
  constructor ~span left right |> logic_ok "oracle binary BV operation"

let unary_assertions oracle value term =
  let not_value = Z.sub oracle.mask value in
  let modular_representatives =
    [ value; Z.add value oracle.modulus; Z.sub value oracle.modulus;
      Z.add value (Z.mul (Z.of_int 3) oracle.modulus);
      Z.sub value (Z.mul (Z.of_int 3) oracle.modulus) ]
  in
  let conversions =
    List.map
      (fun representative ->
        let actual =
          Logic_ir.int_to_bv_mod ~span ~width:oracle.width
            (int_term representative)
          |> logic_ok "Int_to_bv_mod oracle term"
        in
        bv_expected oracle actual (normalize oracle representative))
      modular_representatives
  in
  ( bv_expected oracle
      (Logic_ir.bv_not ~span term |> logic_ok "Bv_not oracle term")
      not_value
    :: int_expected
         (Logic_ir.bv_to_int_unsigned ~span term
         |> logic_ok "unsigned BV oracle view")
         value
    :: int_expected
         (Logic_ir.bv_to_int_signed ~span term
         |> logic_ok "signed BV oracle view")
         (signed oracle value)
    :: conversions,
    List.length modular_representatives )

let binary_assertions oracle left_value left right_value right =
  let unsigned relation = relation left_value right_value in
  let left_signed = signed oracle left_value
  and right_signed = signed oracle right_value in
  [ bool_expected (binary Logic_ir.bv_eq left right)
      (Z.equal left_value right_value);
    bool_expected (binary Logic_ir.bv_distinct left right)
      (not (Z.equal left_value right_value));
    bv_expected oracle (binary Logic_ir.bv_add_mod left right)
      (normalize oracle (Z.add left_value right_value));
    bv_expected oracle (binary Logic_ir.bv_sub_mod left right)
      (normalize oracle (Z.sub left_value right_value));
    bv_expected oracle (binary Logic_ir.bv_and left right)
      (Z.logand left_value right_value);
    bv_expected oracle (binary Logic_ir.bv_or left right)
      (Z.logor left_value right_value);
    bv_expected oracle (binary Logic_ir.bv_xor left right)
      (Z.logxor left_value right_value);
    bool_expected (binary Logic_ir.bv_ult left right)
      (unsigned (fun x y -> Z.compare x y < 0));
    bool_expected (binary Logic_ir.bv_ule left right)
      (unsigned (fun x y -> Z.compare x y <= 0));
    bool_expected (binary Logic_ir.bv_ugt left right)
      (unsigned (fun x y -> Z.compare x y > 0));
    bool_expected (binary Logic_ir.bv_uge left right)
      (unsigned (fun x y -> Z.compare x y >= 0));
    bool_expected (binary Logic_ir.bv_slt left right)
      (Z.compare left_signed right_signed < 0);
    bool_expected (binary Logic_ir.bv_sle left right)
      (Z.compare left_signed right_signed <= 0);
    bool_expected (binary Logic_ir.bv_sgt left right)
      (Z.compare left_signed right_signed > 0);
    bool_expected (binary Logic_ir.bv_sge left right)
      (Z.compare left_signed right_signed >= 0) ]

let verified_query assertions =
  let conjunction =
    Logic_ir.and_ ~span assertions |> logic_ok "oracle conjunction"
  in
  let contradiction =
    Logic_ir.not_ ~span conjunction |> logic_ok "oracle contradiction"
  in
  let builder = Logic_ir.create () in
  Logic_ir.query builder ~axioms:[] ~assertions:[ contradiction ] ~requires:[]
    ~span
  |> logic_ok "oracle verification query"

let check_lifecycle label telemetry =
  require
    (telemetry.Z3_bridge.contexts_created = 1
    && telemetry.contexts_cleaned = 1
    && telemetry.contexts_live = 0
    && telemetry.selected_logics = [ "general" ])
    (label ^ " did not preserve bounded general-solver cleanup")

let run_worker scheduler canonical_index ~label detached =
  let request =
    Function_vc_worker_private.
      { source_ordinal = canonical_index;
        vcs =
          [ { canonical_index; timeout_ms = 10_000; rlimit = 1_000_000;
              route = Ordinary detached } ] }
  in
  let slot = Portable.Atomic_array.create ~len:1 None in
  Parallel_scheduler.parallel scheduler ~f:(fun parallel ->
      Parallel_kernel.for_ parallel ~start:0 ~stop:1 ~f:(fun _ _ ->
          Portable.Atomic_array.set slot 0
            (Some (Function_vc_worker_private.run request))));
  match Portable.Atomic_array.get slot 0 with
  | Some result -> result
  | None -> failwith (label ^ ": detached worker did not join")

let solve_verified scheduler canonical_index ~label query =
  Z3_bridge.reset_counters ();
  (match
     Z3_bridge.solve_query { timeout_ms = 10_000; model = true } query
   with
  | Ok Z3_bridge.Verified -> ()
  | Ok _ -> failwith (label ^ ": direct query was not unsatisfiable")
  | Error error ->
      failwith (label ^ ": " ^ Z3_bridge.error_to_string error));
  check_lifecycle (label ^ ": direct query") (Z3_bridge.counters ());
  let worker =
    run_worker scheduler canonical_index ~label (Z3_bridge.detach_query query)
  in
  match (worker.worker_exception, worker.vc_results) with
  | None,
    [ { result_outcome = Ok Z3_bridge.Detached_verified;
        attempt_telemetry = [ telemetry ]; _ } ] ->
      check_lifecycle (label ^ ": detached worker query") telemetry
  | _ -> failwith (label ^ ": detached worker disagreed with direct outcome")

let solve_model_probe scheduler canonical_index oracle =
  let label = Printf.sprintf "width=%d model-probe" oracle.width_int in
  let maximum = literal oracle oracle.mask and one = literal oracle Z.one in
  let residue = binary Logic_ir.bv_add_mod maximum one in
  let wrong = literal oracle Z.one in
  let differs_from_wrong =
    binary Logic_ir.bv_eq residue wrong
    |> Logic_ir.not_ ~span
    |> logic_ok "deliberately false BV equality"
  in
  let maximum_not_below_zero =
    binary Logic_ir.bv_ult maximum (literal oracle Z.zero)
    |> Logic_ir.not_ ~span
    |> logic_ok "deliberately false unsigned comparison"
  in
  let builder = Logic_ir.create () in
  let projection =
    Logic_ir.project_bv builder
      ~identity:("model-residue-" ^ string_of_int oracle.width_int)
      residue ~span
    |> logic_ok "oracle model projection"
  in
  let query =
    Logic_ir.query builder ~bv_projections:[ projection ] ~axioms:[]
      ~assertions:[ differs_from_wrong; maximum_not_below_zero ] ~requires:[]
      ~span
    |> logic_ok "oracle model query"
  in
  Z3_bridge.reset_counters ();
  (match
     Z3_bridge.solve_bv_query { timeout_ms = 10_000; model = true } query
   with
  | Ok (Z3_bridge.Bv_counterexample [ binding ]) ->
      require
        (String.equal binding.projection_identity
           ("model-residue-" ^ string_of_int oracle.width_int))
        "direct oracle model changed projection identity";
      require (Bv_width.equal binding.width oracle.width)
        "direct oracle model changed projection width";
      require (Z.equal binding.value.unsigned_bits Z.zero)
        "direct oracle model changed projected residue"
  | Ok _ -> failwith "direct oracle bad assertion lacked structured model"
  | Error error -> failwith (Z3_bridge.error_to_string error));
  check_lifecycle "direct oracle model query" (Z3_bridge.counters ());
  let detached, identities = Z3_bridge.detach_bv_query query in
  let worker = run_worker scheduler canonical_index ~label detached in
  match (identities, worker.worker_exception, worker.vc_results) with
  | [ identity ], None,
    [ { result_outcome =
          Ok
            (Z3_bridge.Detached_counterexample
              [ Some
                  (Z3_bridge.Detached_bit_vector
                    (observed_identity, width_reference, residue)) ]);
        attempt_telemetry = [ telemetry ]; _ } ] ->
      require
        (String.equal identity
           ("model-residue-" ^ string_of_int oracle.width_int))
        "detached oracle model changed projection identity";
      require (String.equal observed_identity identity)
        "detached oracle response changed projection identity";
      let width =
        Bv_width.decode_reference_for_worker width_reference
        |> ok "detached oracle width reference"
      in
      require (Bv_width.equal width oracle.width && String.equal residue "0")
        "detached oracle model disagreed with direct structured residue";
      check_lifecycle "detached worker oracle model query" telemetry
  | _ -> failwith "detached oracle bad assertion lacked structured model"

let closed_kernel_oracle_case =
  Suite.case ~name:"closed-bv-table-independent-zarith-oracle"
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
    (fun ~environment:_ ~workspace:_ ->
      try
        let scheduler =
          Parallel_scheduler.create
            ~max_domains:(min 2 (Multicore.max_domains ())) ()
        in
        Fun.protect ~finally:(fun () -> Parallel_scheduler.stop scheduler)
          (fun () ->
            let canonical_index = ref 0 in
            let binary_pairs = ref 0
            and pattern_values = ref 0
            and conversion_representatives = ref 0 in
            let solve ~label assertions =
              solve_verified scheduler !canonical_index ~label
                (verified_query assertions);
              incr canonical_index
            in
            List.iter
              (fun width_int ->
                let oracle = make_oracle width_int in
                let cardinality = 1 lsl width_int in
                let terms =
                  Array.init cardinality (fun value ->
                      literal oracle (Z.of_int value))
                in
                let unary = ref [] in
                Array.iteri
                  (fun value term ->
                    let assertions, conversions =
                      unary_assertions oracle (Z.of_int value) term
                    in
                    unary := List.rev_append assertions !unary;
                    incr pattern_values;
                    conversion_representatives :=
                      !conversion_representatives + conversions)
                  terms;
                solve
                  ~label:(Printf.sprintf "width=%d unary-conversion" width_int)
                  (List.rev !unary);
                let chunk = ref [] and chunk_pairs = ref 0 and batch = ref 0 in
                let flush () =
                  if !chunk_pairs > 0 then (
                    solve
                      ~label:
                        (Printf.sprintf "width=%d binary-batch=%d" width_int
                           !batch)
                      (List.rev !chunk);
                    chunk := [];
                    chunk_pairs := 0;
                    incr batch)
                in
                for left = 0 to cardinality - 1 do
                  for right = 0 to cardinality - 1 do
                    let assertions =
                      binary_assertions oracle (Z.of_int left) terms.(left)
                        (Z.of_int right) terms.(right)
                    in
                    chunk := List.rev_append assertions !chunk;
                    incr chunk_pairs;
                    incr binary_pairs;
                    if !chunk_pairs = 256 then flush ()
                  done
                done;
                flush ())
              [ 1; 2; 4; 8 ];
            let larger_widths = [ 32; 64; 128; 256; 512; 1024; 2048; 4096 ] in
            List.iter
              (fun width_int ->
                let oracle = make_oracle width_int in
                let boundary_values =
                  [ Z.zero; Z.one; Z.pred oracle.sign_boundary;
                    oracle.sign_boundary; Z.pred oracle.mask; oracle.mask ]
                  |> List.sort_uniq Z.compare
                in
                let assertions = ref [] in
                List.iter
                  (fun value ->
                    let unary, conversions =
                      unary_assertions oracle value (literal oracle value)
                    in
                    assertions := List.rev_append unary !assertions;
                    incr pattern_values;
                    conversion_representatives :=
                      !conversion_representatives + conversions)
                  boundary_values;
                List.iter
                  (fun left_value ->
                    let left = literal oracle left_value in
                    List.iter
                      (fun right_value ->
                        let right = literal oracle right_value in
                        assertions :=
                          List.rev_append
                            (binary_assertions oracle left_value left
                               right_value right)
                            !assertions;
                        incr binary_pairs)
                      boundary_values)
                  boundary_values;
                solve
                  ~label:(Printf.sprintf "width=%d boundary-vectors" width_int)
                  (List.rev !assertions))
              larger_widths;
            let huge = Z.add (Z.pow (Z.of_int 10) 1300) (Z.of_int 123) in
            require (String.length (Z.to_string huge) > 1234)
              "mathematical Int oracle operand did not exceed BV field cap";
            List.iter
              (fun width_int ->
                let oracle = make_oracle width_int in
                let assertions =
                  List.map
                    (fun representative ->
                      let actual =
                        Logic_ir.int_to_bv_mod ~span ~width:oracle.width
                          (int_term representative)
                        |> logic_ok "huge Int_to_bv_mod oracle term"
                      in
                      bv_expected oracle actual
                        (normalize oracle representative))
                    [ huge; Z.neg huge ]
                in
                conversion_representatives :=
                  !conversion_representatives + List.length assertions;
                solve
                  ~label:(Printf.sprintf "width=%d huge-int-conversion" width_int)
                  assertions)
              [ 8; 4096 ];
            List.iter
              (fun width_int ->
                solve_model_probe scheduler !canonical_index
                  (make_oracle width_int);
                incr canonical_index)
              [ 1; 8; 64; 4096 ];
            require (!binary_pairs = 66_100)
              "oracle did not enumerate every required pair";
            require (!pattern_values = 326)
              "oracle did not enumerate every required bit pattern/vector";
            require (!conversion_representatives = 1_634)
              "oracle did not enumerate every modular Int representative";
            Ok
              (Outcome.observation ~status:Outcome.Verified ()
              |> Outcome.project))
      with Failure message -> Error (Failure.make Failure.Runner_internal message))

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [ closed_kernel_oracle_case ]
