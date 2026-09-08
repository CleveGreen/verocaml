open Outcome_test_support

let require condition message = if not condition then failwith message

let equal_span (left : Diagnostic.span) (right : Diagnostic.span) =
  String.equal left.file right.file
  && left.start_pos.line = right.start_pos.line
  && left.start_pos.column = right.start_pos.column
  && left.end_pos.line = right.end_pos.line
  && left.end_pos.column = right.end_pos.column

let loaded = function
  | Ok value -> value
  | Error diagnostic -> failwith diagnostic.Diagnostic.message

let rec integer_bv_widths = function
  | Vir.Integer_bv_to_int_unsigned term | Integer_bv_to_int_signed term ->
      bit_vector_widths term
  | Integer_add (left, right) | Integer_subtract (left, right)
  | Integer_multiply (left, right) ->
      integer_bv_widths left @ integer_bv_widths right
  | Integer_negate value | Integer_multiply_constant (_, value)
  | Integer_absolute_value value -> integer_bv_widths value
  | Integer_conditional (condition, consequent, alternative) ->
      boolean_bv_widths condition @ integer_bv_widths consequent
      @ integer_bv_widths alternative
  | Integer_constant _ | Integer_symbol _ | Integer_rank_project _
  | Aggregate_tag _ | Integer_selector _
  | Integer_recursive_spec_application _ | Integer_symbolic_application _ -> []

and bit_vector_widths term =
  Bv_width.to_int term.Vir.bit_vector_width
  ::
  (match term.bit_vector_desc with
  | Vir.Bv_int_to_bv_mod { input; _ } -> integer_bv_widths input
  | Bv_not operand -> bit_vector_widths operand
  | Bv_binary (_, left, right) ->
      bit_vector_widths left @ bit_vector_widths right
  | Bv_conditional (condition, consequent, alternative) ->
      boolean_bv_widths condition @ bit_vector_widths consequent
      @ bit_vector_widths alternative
  | Bv_literal _ | Bv_selector _ | Bv_symbol _
  | Bv_symbolic_application _ | Bv_recursive_spec_application _ -> [])

and boolean_bv_widths = function
  | Vir.Boolean_not value -> boolean_bv_widths value
  | Boolean_and (left, right) | Boolean_or (left, right)
  | Boolean_equal (left, right) | Boolean_not_equal (left, right) ->
      boolean_bv_widths left @ boolean_bv_widths right
  | Integer_compare (_, left, right) ->
      integer_bv_widths left @ integer_bv_widths right
  | Bv_equal (left, right) | Bv_not_equal (left, right) ->
      bit_vector_widths left @ bit_vector_widths right
  | Bv_compare (_, left, right) ->
      bit_vector_widths left @ bit_vector_widths right
  | Forall_term quantifier | Exists_term quantifier ->
      boolean_bv_widths quantifier.boolean_quantifier_body
  | Logical_adt_schema _ | Boolean_constant _ | Boolean_symbol _
  | Boolean_selector _ | Aggregate_equal _ | Parametric_equal _
  | Boolean_invariant_application _
  | Boolean_recursive_spec_application _
  | Boolean_specification_application _ | Boolean_symbolic_application _
  | Callback_requires _ | Callback_ensures _ -> []

let report_bv_widths report =
  (Verification_driver_private.vir report).Vir.functions
  |> List.concat_map (fun execution ->
         execution.Vir.obligations
         |> List.concat_map (fun obligation ->
                List.concat_map boolean_bv_widths
                  (obligation.Vir.goal :: obligation.assumptions
                 @ obligation.required_preceding_safety
                 @ obligation.path_condition)))

let rec integer_bv_authorities = function
  | Vir.Integer_bv_to_int_unsigned term | Integer_bv_to_int_signed term ->
      bit_vector_authorities term
  | Integer_add (left, right) | Integer_subtract (left, right)
  | Integer_multiply (left, right) ->
      integer_bv_authorities left @ integer_bv_authorities right
  | Integer_negate value | Integer_multiply_constant (_, value)
  | Integer_absolute_value value -> integer_bv_authorities value
  | Integer_conditional (condition, consequent, alternative) ->
      boolean_bv_authorities condition @ integer_bv_authorities consequent
      @ integer_bv_authorities alternative
  | Integer_constant _ | Integer_symbol _ | Integer_rank_project _
  | Aggregate_tag _ | Integer_selector _
  | Integer_recursive_spec_application _ | Integer_symbolic_application _ -> []

and bit_vector_authorities term =
  match term.Vir.bit_vector_desc with
  | Vir.Bv_int_to_bv_mod { source_authority; input } ->
      Numeric_bv_projection_evidence_private.semantic_authorities
        source_authority
      :: integer_bv_authorities input
  | Bv_not operand -> bit_vector_authorities operand
  | Bv_binary (_, left, right) ->
      bit_vector_authorities left @ bit_vector_authorities right
  | Bv_conditional (condition, consequent, alternative) ->
      boolean_bv_authorities condition @ bit_vector_authorities consequent
      @ bit_vector_authorities alternative
  | Bv_literal _ | Bv_selector _ | Bv_symbol _
  | Bv_symbolic_application _ | Bv_recursive_spec_application _ -> []

and boolean_bv_authorities = function
  | Vir.Boolean_not value -> boolean_bv_authorities value
  | Boolean_and (left, right) | Boolean_or (left, right)
  | Boolean_equal (left, right) | Boolean_not_equal (left, right) ->
      boolean_bv_authorities left @ boolean_bv_authorities right
  | Integer_compare (_, left, right) ->
      integer_bv_authorities left @ integer_bv_authorities right
  | Bv_equal (left, right) | Bv_not_equal (left, right) ->
      bit_vector_authorities left @ bit_vector_authorities right
  | Bv_compare (_, left, right) ->
      bit_vector_authorities left @ bit_vector_authorities right
  | Forall_term quantifier | Exists_term quantifier ->
      boolean_bv_authorities quantifier.boolean_quantifier_body
  | Logical_adt_schema _ | Boolean_constant _ | Boolean_symbol _
  | Boolean_selector _ | Aggregate_equal _ | Parametric_equal _
  | Boolean_invariant_application _
  | Boolean_recursive_spec_application _
  | Boolean_specification_application _ | Boolean_symbolic_application _
  | Callback_requires _ | Callback_ensures _ -> []

let report_bv_authorities report =
  (Verification_driver_private.vir report).Vir.functions
  |> List.concat_map (fun execution ->
         execution.Vir.obligations
         |> List.concat_map (fun obligation ->
                List.concat_map boolean_bv_authorities
                  (obligation.Vir.goal :: obligation.assumptions
                 @ obligation.required_preceding_safety
                 @ obligation.path_condition)))

let rec integer_bv_trusted_dependencies = function
  | Vir.Integer_bv_to_int_unsigned term | Integer_bv_to_int_signed term ->
      bit_vector_trusted_dependencies term
  | Integer_add (left, right) | Integer_subtract (left, right)
  | Integer_multiply (left, right) ->
      integer_bv_trusted_dependencies left
      @ integer_bv_trusted_dependencies right
  | Integer_negate value | Integer_multiply_constant (_, value)
  | Integer_absolute_value value -> integer_bv_trusted_dependencies value
  | Integer_conditional (condition, consequent, alternative) ->
      boolean_bv_trusted_dependencies condition
      @ integer_bv_trusted_dependencies consequent
      @ integer_bv_trusted_dependencies alternative
  | Integer_constant _ | Integer_symbol _ | Integer_rank_project _
  | Aggregate_tag _ | Integer_selector _
  | Integer_recursive_spec_application _ | Integer_symbolic_application _ -> []

and bit_vector_trusted_dependencies term =
  match term.Vir.bit_vector_desc with
  | Vir.Bv_int_to_bv_mod { source_authority; input } ->
      Numeric_bv_projection_evidence_private.trusted_dependencies
        source_authority
      @ integer_bv_trusted_dependencies input
  | Bv_not operand -> bit_vector_trusted_dependencies operand
  | Bv_binary (_, left, right) ->
      bit_vector_trusted_dependencies left
      @ bit_vector_trusted_dependencies right
  | Bv_conditional (condition, consequent, alternative) ->
      boolean_bv_trusted_dependencies condition
      @ bit_vector_trusted_dependencies consequent
      @ bit_vector_trusted_dependencies alternative
  | Bv_literal _ | Bv_selector _ | Bv_symbol _
  | Bv_symbolic_application _ | Bv_recursive_spec_application _ -> []

and boolean_bv_trusted_dependencies = function
  | Vir.Boolean_not value -> boolean_bv_trusted_dependencies value
  | Boolean_and (left, right) | Boolean_or (left, right)
  | Boolean_equal (left, right) | Boolean_not_equal (left, right) ->
      boolean_bv_trusted_dependencies left
      @ boolean_bv_trusted_dependencies right
  | Integer_compare (_, left, right) ->
      integer_bv_trusted_dependencies left
      @ integer_bv_trusted_dependencies right
  | Bv_equal (left, right) | Bv_not_equal (left, right) ->
      bit_vector_trusted_dependencies left
      @ bit_vector_trusted_dependencies right
  | Bv_compare (_, left, right) ->
      bit_vector_trusted_dependencies left
      @ bit_vector_trusted_dependencies right
  | Forall_term quantifier | Exists_term quantifier ->
      boolean_bv_trusted_dependencies quantifier.boolean_quantifier_body
  | Logical_adt_schema _ | Boolean_constant _ | Boolean_symbol _
  | Boolean_selector _ | Aggregate_equal _ | Parametric_equal _
  | Boolean_invariant_application _
  | Boolean_recursive_spec_application _
  | Boolean_specification_application _ | Boolean_symbolic_application _
  | Callback_requires _ | Callback_ensures _ -> []

let report_bv_trusted_dependencies report =
  (Verification_driver_private.vir report).Vir.functions
  |> List.concat_map (fun execution ->
         execution.Vir.obligations
         |> List.concat_map (fun obligation ->
                List.concat_map boolean_bv_trusted_dependencies
                  (obligation.Vir.goal :: obligation.assumptions
                 @ obligation.required_preceding_safety
                 @ obligation.path_condition)))

let report_bv_source_observations report =
  (Verification_driver_private.vir report).Vir.functions
  |> List.concat_map (fun execution ->
         List.concat_map Vir.obligation_native_bv_source_observations
           execution.Vir.obligations)
  |> List.sort_uniq
       (fun
         (left : Numeric_bv_projection_evidence_private.source_observation)
         right ->
         String.compare left.occurrence_identity right.occurrence_identity)

let require_source_observation ~target ~provenance:expected_provenance ~outer_authority
    ~modular_authority observation =
  let open Numeric_bv_projection_evidence_private in
  require (Bv_width.to_int observation.semantic_width = 8)
    "source observation lost its semantic width";
  require (observation.provenance = expected_provenance)
    "source observation lost its descriptor origin class";
  require
    (String.equal observation.target_full_key
       target.Build_target_profile_private.full_key)
    "source observation lost its exact physical target";
  require (String.length observation.profile_full_key > 0)
    "source observation lost its profile identity";
  require (String.length observation.backend_capability_full_key > 0)
    "source observation lost its backend capability receipt";
  require (String.length observation.backend_capability_id = 64)
    "source observation lost its checked backend capability id";
  require (String.length observation.backend_abi_receipt > 0)
    "source observation lost its backend ABI receipt";
  require (observation.runtime_equivalence = Runtime_unknown)
    "ghost projection invented executable runtime equivalence";
  require
    (observation.outer_operation.authority = outer_authority
    && observation.modular_operation.authority = modular_authority)
    "source observation changed its semantic proof or trust axes";
  require
    (String.equal observation.outer_operation.callable_tag "unsigned-view"
    && String.equal observation.modular_operation.callable_tag
         "modular-conversion")
    "source observation lost its exact native operation tags";
  require
    (String.length observation.outer_operation.callable_uid > 0
    && String.length observation.modular_operation.callable_uid > 0
    && String.length observation.outer_operation.callable_abi > 0
    && String.length observation.modular_operation.callable_abi > 0
    && String.length observation.outer_operation.law_evidence_full_key > 0
    && String.length observation.modular_operation.law_evidence_full_key > 0)
    "source observation lost callable or law provenance";
  require
    (not
       (String.equal observation.outer_operation.callable_uid
          observation.modular_operation.callable_uid))
    "outer and nested operations collapsed to one callable identity";
  require
    (not
       (String.equal observation.outer_operation.occurrence_identity
          observation.modular_operation.occurrence_identity))
    "outer and nested modular calls collapsed to one source identity"

let require_service_result_associations report =
  let results = Verification_driver_private.results report in
  let associations = Verifier_service.For_testing.native_bv_results results in
  require (associations <> [])
    "service omitted the native BV obligation/result association";
  List.iter
    (fun association ->
      let identity =
        Verifier_service.native_bv_result_obligation_identity association
      in
      let result =
        List.find
          (fun (result : Solver_backend.obligation_result) ->
            String.equal identity
              (Vir_identity_private.obligation result.obligation))
          results
      in
      let function_ = Verifier_service.native_bv_result_function association in
      require
        (Verifier_service.function_index function_
        = result.obligation.function_ref.function_index)
        "service associated source provenance with another function";
      require
        (equal_span
           (Verifier_service.native_bv_result_obligation_span association)
           result.obligation.span)
        "service associated source provenance with another obligation span";
      require
        (Verifier_service.native_bv_result_source_observations association <> [])
        "service emitted an empty native BV result association";
      match
        ( Verifier_service.native_bv_result_outcome association,
          result.outcome )
      with
      | Native_bv_verified, Solver_backend.Verified
      | Native_bv_counterexample, Counterexample _ -> ()
      | Native_bv_inconclusive observed,
        Inconclusive expected ->
          let same_reason =
            match (observed.reason, expected.reason) with
            | Verifier_service.Resource_exhausted,
              Solver_backend.Resource_exhausted
            | Verifier_service.Timed_out, Solver_backend.Timed_out -> true
            | Verifier_service.Backend_unknown left,
              Solver_backend.Backend_unknown right ->
                String.equal left right
            | _ -> false
          in
          require
            (observed.configured_timeout_ms = expected.configured_timeout_ms
            && observed.configured_rlimit = expected.configured_rlimit
            && same_reason)
            "service changed native BV inconclusive outcome or resource configuration"
      | _ ->
          failwith "service changed native BV obligation outcome classification")
    associations

let require_native_resource_association ~timeout_ms ~rlimit
    (result : Solver_backend.obligation_result) =
  (match result.outcome with
  | Solver_backend.Inconclusive observation ->
      require
        (observation.configured_timeout_ms = timeout_ms
        && observation.configured_rlimit = rlimit
        && observation.reason = Solver_backend.Resource_exhausted)
        "native BV resource solve changed its exact budget or exhaustion reason"
  | Verified | Counterexample _ ->
      failwith "native BV resource fixture did not remain inconclusive");
  let associations = Verifier_service.For_testing.native_bv_results [ result ] in
  require
    (List.exists
       (fun association ->
         let identity =
           Verifier_service.native_bv_result_obligation_identity association
         in
         let function_ = Verifier_service.native_bv_result_function association in
         match Verifier_service.native_bv_result_outcome association with
         | Verifier_service.Native_bv_inconclusive observation ->
             String.equal identity
               (Vir_identity_private.obligation result.obligation)
             && Verifier_service.function_index function_
                = result.obligation.function_ref.function_index
             && equal_span
                  (Verifier_service.native_bv_result_obligation_span association)
                  result.obligation.span
             && observation.configured_timeout_ms = timeout_ms
             && observation.configured_rlimit = rlimit
             && observation.reason = Verifier_service.Resource_exhausted
             && Verifier_service.native_bv_result_source_observations association
                = Vir.obligation_native_bv_source_observations result.obligation
         | Native_bv_verified | Native_bv_counterexample -> false)
       associations)
    "native BV resource result lost its obligation, source provenance, exact budget, or exhaustion reason";
  let counters = Z3_bridge.counters () in
  require
    (counters.contexts_live = 0
    && counters.contexts_created = counters.contexts_cleaned)
    "native BV resource association left an unbalanced solver context"

let cases ~write_file ~run_process =
  [ Suite.case ~name:"imported-source-projection-reaches-native-bv"
      ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
      (fun ~environment:_ ~workspace ->
        try
          let directory = Filename.dirname Sys.executable_name in
          let ppx =
            Unix.realpath
              (Filename.concat directory "../../ppx/vero_ppx.exe")
          and ghost =
            Unix.realpath
              (Filename.concat directory "../../runtime/.vero_ghost.objs/byte")
          in
          let compile stem suffix =
            run_process (Filename.concat Config.bindir "ocamlc")
              [ "-w"; "-A"; "-c"; "-bin-annot"; "-I"; workspace; "-I";
                ghost; "-ppx"; ppx ^ " --keep-ghost"; "-o";
                stem ^ (if suffix = ".mli" then ".cmi" else ".cmo");
                stem ^ suffix ]
          in
          let provider = Filename.concat workspace "bv_provider" in
          let unsigned_marker =
            {|[@@verocaml.numeric_role
  {carrier = carrier; role_schema = "numeric-role.v1"; role = "unsigned-view";
   semantics = u_law; visibility = "opaque"; reveal = false; inline = false}]
|}
          and modular_marker =
            {|[@@verocaml.numeric_role
  {carrier = carrier; role_schema = "numeric-role.v1"; role = "modular-conversion";
   semantics = convert_law; visibility = "opaque"; reveal = false; inline = false}]
|}
          in
          write_file (provider ^ ".mli")
            ({|module Arithmetic : sig
  type t = int [@@verocaml.logical_sort]
  val parse : string -> t [@@verocaml.integer_literal]
end
type carrier = int
[@@verocaml.numeric_carrier
  {base = Arithmetic.t; profile = "request"; representation = "immediate";
   compatibility = []}]
val u_law : carrier -> unit [@@verocaml.proof]
val u : carrier -> Arithmetic.t [@@verocaml.spec]
|}
            ^ unsigned_marker
            ^ {|val convert_law : Arithmetic.t -> unit [@@verocaml.proof]
val convert : Arithmetic.t -> carrier [@@verocaml.spec]
|}
            ^ modular_marker);
          write_file (provider ^ ".ml")
            ({|[@@@verocaml.verify]
module Arithmetic = struct
  type t = int [@@verocaml.logical_sort]
  let parse text = int_of_string text [@@verocaml.integer_literal]
end
type carrier = int
let u (_x : carrier) : Arithmetic.t = 0 [@@verocaml.spec]
let u_law (x : carrier) =
  [%verocaml.ensures fun _ -> 0 <= u x && u x < 256];
  ()
[@@verocaml.proof]
let convert (_x : Arithmetic.t) : carrier = 0 [@@verocaml.spec]
let convert_law (x : Arithmetic.t) =
  [%verocaml.ensures fun _ ->
    exists (fun (q : Arithmetic.t) -> x = u (convert x) + 256 * q)];
  ()
[@@verocaml.axiom]
|});
          List.iter (compile provider) [ ".mli"; ".ml" ];
          loaded
            (Cmt_input.emit_retained_interface_authority
               ~cmt:(provider ^ ".cmt") ~cmi:(provider ^ ".cmi")
               ~cmti:(provider ^ ".cmti") ~output:(provider ^ ".vri")
               ~artifact_directories:[ workspace ] ());
          let provider =
            loaded
              (Cmt_input.load_with_interface ~cmt:(provider ^ ".cmt")
                 ~cmi:(provider ^ ".cmi") ~cmti:(provider ^ ".cmti")
                 ~artifact_directories:[ workspace ] ())
          in
          let consumer = Filename.concat workspace "bv_consumer" in
          write_file (consumer ^ ".ml")
            {|[@@@verocaml.verify]
let projection () =
  [%verocaml.ensures fun _ ->
    Bv_provider.u (Bv_provider.convert (-1)) = 255
    && Bv_provider.u (Bv_provider.convert 257) = 1];
  ()
[@@verocaml.proof]
|};
          compile consumer ".ml";
          let consumer = loaded (Cmt_input.load (consumer ^ ".cmt")) in
          let policy =
            match Solver_policy_private.create_default ~timeout_ms:5000 with
            | Ok policy -> policy
            | Error _ -> failwith "invalid native source projection policy"
          in
          let targets =
            match
              Build_target_profile_private.authenticate_instances
                (Build_target_profile_private.capability ())
            with
            | Ok targets -> targets
            | Error message -> failwith message
          in
          require (List.length targets = 2)
            "build target matrix no longer has two sealed children";
          let stable_imported_occurrences = ref None in
          let resource_obligation = ref None in
          List.iter
            (fun target ->
              List.iter
                (fun threads ->
                  let result =
                    Interface_specification_loaded_private
                    .verify_for_numeric_target ~threads ~solver_policy:policy
                      ~external_specifications:None ~external_targets:[] ~target
                      ~consumer ~dependencies:[ provider ]
                  in
                  let loaded =
                    match result with
                    | Ok loaded -> loaded
                    | Error error ->
                        failwith
                          (Interface_specification_loaded_private.error_message
                             error)
                  in
                  let report =
                    Interface_specification_loaded_private.driver loaded
                  in
                  if Option.is_none !resource_obligation then
                    resource_obligation :=
                      Verification_driver_private.results report
                      |> List.find_opt (fun result ->
                             Vir.obligation_native_bv_source_observations
                               result.Solver_backend.obligation
                             <> [])
                      |> Option.map
                           (fun (result : Solver_backend.obligation_result) ->
                             result.obligation);
                  require
                    (Verification_driver_private.status report = Verified)
                    "native imported projection did not verify";
                  require_service_result_associations report;
                  require
                    (List.mem 8 (report_bv_widths report))
                    "verified source query did not retain its structured BV8 projection";
                  require
                    (List.exists
                       (fun (outer, modular) ->
                         outer
                         = Numeric_bv_projection_evidence_private.Checked_proof
                         && modular
                            = Numeric_bv_projection_evidence_private
                              .Explicit_axiom)
                       (report_bv_authorities report))
                    "imported registry did not preserve checked unsigned and explicit modular authority";
                  let observations = report_bv_source_observations report in
                  require (List.length observations = 2)
                    "two imported same-callee sites did not retain two source observations";
                  List.iter
                    (require_source_observation ~target
                       ~provenance:
                         Numeric_bv_projection_evidence_private
                         .Imported_registry
                       ~outer_authority:Checked_proof
                       ~modular_authority:Explicit_axiom)
                    observations;
                  require
                    (List.for_all
                       (fun observation ->
                         String.equal
                           observation.Numeric_bv_projection_evidence_private
                           .outer_operation.descriptor_origin
                           "Bv_provider"
                         && String.equal
                              observation.modular_operation.descriptor_origin
                              "Bv_provider"
                         && not
                              (equal_span
                                 observation.outer_operation.call_span
                                 observation.modular_operation.call_span))
                       observations)
                    "imported outer and nested calls lost exact structured source attribution";
                  let occurrence_ids =
                    List.map
                      (fun
                        (observation :
                          Numeric_bv_projection_evidence_private
                          .source_observation) ->
                        observation.occurrence_identity)
                      observations
                  in
                  (match !stable_imported_occurrences with
                  | None -> stable_imported_occurrences := Some occurrence_ids
                  | Some expected ->
                      require (occurrence_ids = expected)
                        "imported source occurrence identities changed by target or worker count"))
                [ 1; 2 ])
            targets;
          let resource_timeout_ms = 10_000 and resource_rlimit = 1 in
          let resource_config =
            match
              Solver_backend.config_with_rlimit
                ~timeout_ms:resource_timeout_ms ~rlimit:resource_rlimit
            with
            | Ok config -> config
            | Error error -> failwith (Solver_backend.error_to_string error)
          in
          let obligation =
            match !resource_obligation with
            | Some obligation -> obligation
            | None -> failwith "coordinator omitted its native BV obligation"
          in
          let resource_outcome =
            match Solver_backend.solve_obligation resource_config obligation with
            | Ok outcome -> outcome
            | Error error -> failwith (Solver_backend.error_to_string error)
          in
          require_native_resource_association ~timeout_ms:resource_timeout_ms
            ~rlimit:resource_rlimit
            { Solver_backend.obligation; outcome = resource_outcome };
          Ok (Outcome.observation ~status:Outcome.Verified () |> Outcome.project)
        with Failure message ->
          Error (Failure.make Failure.Runner_internal message));
    Suite.case ~name:"ordinary-source-mathematics-exceeds-native-bv-ceiling"
      ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
      (fun ~environment:_ ~workspace ->
        try
          let directory = Filename.dirname Sys.executable_name in
          let ppx =
            Unix.realpath
              (Filename.concat directory "../../ppx/vero_ppx.exe")
          and ghost =
            Unix.realpath
              (Filename.concat directory "../../runtime/.vero_ghost.objs/byte")
          in
          let compile stem suffix =
            run_process (Filename.concat Config.bindir "ocamlc")
              [ "-w"; "-A"; "-c"; "-bin-annot"; "-I"; workspace; "-I";
                ghost; "-ppx"; ppx ^ " --keep-ghost"; "-o";
                stem ^ (if suffix = ".mli" then ".cmi" else ".cmo");
                stem ^ suffix ]
          in
          let beyond_native = Z.shift_left Z.one 4097 in
          let successor = Z.succ beyond_native in
          require (Z.numbits beyond_native > 4096)
            "ordinary mathematical fixture did not exceed the native BV ceiling";
          let stem = Filename.concat workspace "ordinary_large_math" in
          write_file (stem ^ ".mli")
            {|module Arithmetic : sig
  type t = int [@@verocaml.logical_sort]
  val parse : string -> t [@@verocaml.integer_literal]
end
val beyond_native_ceiling : unit -> unit
|};
          write_file (stem ^ ".ml")
            (Printf.sprintf
               {|[@@@verocaml.verify]
module Arithmetic = struct
  type t = int [@@verocaml.logical_sort]
  let parse value = int_of_string value [@@verocaml.integer_literal]
end

let beyond_native_ceiling () =
  [%%verocaml.ensures fun _ ->
    Arithmetic.parse "%s" + 1 = Arithmetic.parse "%s"];
  ()
[@@verocaml.proof]
|}
               (Z.to_string beyond_native) (Z.to_string successor));
          List.iter (compile stem) [ ".mli"; ".ml" ];
          loaded
            (Cmt_input.emit_retained_interface_authority
               ~cmt:(stem ^ ".cmt") ~cmi:(stem ^ ".cmi")
               ~cmti:(stem ^ ".cmti") ~output:(stem ^ ".vri")
               ~artifact_directories:[ workspace ] ());
          let implementation =
            loaded
              (Cmt_input.load_with_interface ~cmt:(stem ^ ".cmt")
                 ~cmi:(stem ^ ".cmi") ~cmti:(stem ^ ".cmti")
                 ~artifact_directories:[ workspace ] ())
          in
          let report =
            match
              Verification_driver_private.run ~timeout_ms:60_000
                ~allow_imported_opens:true implementation
            with
            | Ok report -> report
            | Error (Verification_driver_private.Frontend_error diagnostic) ->
                failwith diagnostic.Diagnostic.message
            | Error _ ->
                failwith "ordinary mathematical verification failed"
          in
          require (Verification_driver_private.status report = Verified)
            "ordinary mathematical proof above the BV ceiling did not verify";
          require (report_bv_widths report = [])
            "ordinary mathematical proof acquired a native BV width";
          require (report_bv_source_observations report = [])
            "ordinary mathematical proof acquired native source authority";
          require
            (Verifier_service.For_testing.native_bv_results
               (Verification_driver_private.results report)
            = [])
            "ordinary mathematical proof acquired a native result association";
          Ok (Outcome.observation ~status:Outcome.Verified () |> Outcome.project)
        with Failure message ->
          Error (Failure.make Failure.Runner_internal message));
    Suite.case ~name:"same-unit-source-projection-uses-prior-law-closure"
      ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
      (fun ~environment:_ ~workspace ->
        try
          let directory = Filename.dirname Sys.executable_name in
          let ppx =
            Unix.realpath
              (Filename.concat directory "../../ppx/vero_ppx.exe")
          and ghost =
            Unix.realpath
              (Filename.concat directory "../../runtime/.vero_ghost.objs/byte")
          in
          let compile stem suffix =
            run_process (Filename.concat Config.bindir "ocamlc")
              [ "-w"; "-A"; "-c"; "-bin-annot"; "-I"; workspace; "-I";
                ghost; "-ppx"; ppx ^ " --keep-ghost"; "-o";
                stem ^ (if suffix = ".mli" then ".cmi" else ".cmo");
                stem ^ suffix ]
          in
          let load_local ?(different_domain = false) ?(include_false = true)
              ?(failed_modular = false) stem modulus =
            let unsigned_marker =
              {|[@@verocaml.numeric_role
  {carrier = carrier; role_schema = "numeric-role.v1"; role = "unsigned-view";
   semantics = u_law; visibility = "opaque"; reveal = false; inline = false}]
|}
            and modular_marker =
              {|[@@verocaml.numeric_role
  {carrier = carrier; role_schema = "numeric-role.v1"; role = "modular-conversion";
   semantics = convert_law; visibility = "opaque"; reveal = false; inline = false}]
|}
            in
            write_file (stem ^ ".mli")
              ({|module Arithmetic : sig
  type t = int [@@verocaml.logical_sort]
  val parse : string -> t [@@verocaml.integer_literal]
end
type carrier = int
[@@verocaml.numeric_carrier
  {base = Arithmetic.t; profile = "request"; representation = "immediate";
   compatibility = []}]
type other = int
val u_law : |}
              ^ (if different_domain then "other" else "carrier")
              ^ {| -> unit [@@verocaml.proof]
val u : |}
              ^ (if different_domain then "other" else "carrier")
              ^ {| -> Arithmetic.t [@@verocaml.spec]
|}
              ^ unsigned_marker
              ^ {|val convert_law : Arithmetic.t -> unit [@@verocaml.proof]
val convert : Arithmetic.t -> carrier [@@verocaml.spec]
|}
              ^ modular_marker
              ^ {|val projection : unit -> unit [@@verocaml.proof]
|});
            write_file (stem ^ ".ml")
              ({|[@@@verocaml.verify]
module Arithmetic = struct
  type t = int [@@verocaml.logical_sort]
  let parse text = int_of_string text [@@verocaml.integer_literal]
end
type carrier = int
type other = int
[%%verocaml.symbolic val u_sym : |}
              ^ (if different_domain then "other" else "carrier")
              ^ {| -> Arithmetic.t]
[%%verocaml.symbolic val convert_sym : Arithmetic.t -> carrier]
let u (x : |}
              ^ (if different_domain then "other" else "carrier")
              ^ {|) : Arithmetic.t = u_sym x [@@verocaml.spec]
let convert (x : Arithmetic.t) : carrier = convert_sym x [@@verocaml.spec]
let projection () =
  [%verocaml.ensures fun _ ->
    u (convert (-1)) = 255 && u (convert 257) = 1];
  ()
[@@verocaml.proof]
let u_fact (x : |}
              ^ (if different_domain then "other" else "carrier")
              ^ {|) =
  [%verocaml.ensures fun _ -> 0 <= u x && u x < 256];
  ()
[@@verocaml.axiom]
|}
              ^ (if failed_modular then "" else
                   {|let convert_fact (x : Arithmetic.t) =
  [%verocaml.ensures fun _ ->
    exists (fun (q : Arithmetic.t) -> x = u (convert x) + |}
                   ^ modulus
                   ^ {| * q)];
  ()
[@@verocaml.axiom]
|})
              ^ {|let u_law (x : |}
              ^ (if different_domain then "other" else "carrier")
              ^ {|) =
  [%verocaml.ensures fun _ -> 0 <= u x && u x < 256];
  u_fact x
[@@verocaml.proof]
let convert_law (x : Arithmetic.t) =
  [%verocaml.ensures fun _ ->
    exists (fun (q : Arithmetic.t) -> x = u (convert x) + |}
              ^ modulus
              ^ {| * q)];
  |}
              ^ (if failed_modular then "()" else "convert_fact x")
              ^ {|
[@@verocaml.proof]
|}
              ^ (if include_false then
                   {|let later_false () =
  [%verocaml.ensures fun _ -> false];
  ()
[@@verocaml.proof]
|}
                 else ""));
            List.iter (compile stem) [ ".mli"; ".ml" ];
            loaded
              (Cmt_input.emit_retained_interface_authority
                 ~cmt:(stem ^ ".cmt") ~cmi:(stem ^ ".cmi")
                 ~cmti:(stem ^ ".cmti") ~output:(stem ^ ".vri")
                 ~artifact_directories:[ workspace ] ());
            loaded
              (Cmt_input.load_with_interface ~cmt:(stem ^ ".cmt")
                 ~cmi:(stem ^ ".cmi") ~cmti:(stem ^ ".cmti")
                 ~artifact_directories:[ workspace ] ())
          in
          let policy =
            match Solver_policy_private.create_default ~timeout_ms:5000 with
            | Ok policy -> policy
            | Error _ -> failwith "invalid local native projection policy"
          in
          let targets =
            match
              Build_target_profile_private.authenticate_instances
                (Build_target_profile_private.capability ())
            with
            | Ok targets -> targets
            | Error message -> failwith message
          in
          require (List.length targets = 2)
            "build target matrix no longer has two sealed children";
          let good =
            load_local (Filename.concat workspace "bv_local_good") "256"
          in
          let stable_local_occurrences = ref None in
          List.iter
            (fun target ->
              List.iter
                (fun threads ->
                  let loaded =
                    match
                      Interface_specification_loaded_private
                      .verify_for_numeric_target ~threads
                        ~solver_policy:policy ~external_specifications:None
                        ~external_targets:[] ~target ~consumer:good
                        ~dependencies:[]
                    with
                    | Ok loaded -> loaded
                    | Error error ->
                        failwith
                          (Interface_specification_loaded_private.error_message
                             error)
                  in
                  let report =
                    Interface_specification_loaded_private.driver loaded
                  in
                  require_service_result_associations report;
                  let coordinator =
                    match Verification_driver_private.prior_law_evidence report with
                    | Some coordinator -> coordinator
                    | None -> failwith "same-unit verification omitted prior-law coordinator"
                  in
                  let program =
                    Sst_validation.program
                      (Verification_driver_private.validated report)
                  in
                  let definition name =
                    List.find
                      (fun definition ->
                        String.equal definition.Sst.function_id.function_name
                          name)
                      program.functions
                  in
                  let projection_definition = definition "projection" in
                  let rec find_outer expression =
                    match expression.Sst.expression_desc with
                    | Direct_call
                        { callee; arguments = [ Value_argument _ ]; _ }
                      when String.equal callee.function_name "u" ->
                        Some expression
                    | _ ->
                        Sst_callback_private.expression_children expression
                        |> List.find_map find_outer
                  in
                  let original_occurrence =
                    Numeric_proof_dependencies_private.definition_expressions
                      projection_definition
                    |> List.find_map find_outer
                    |> Option.get
                  in
                  let copied_occurrence =
                    match original_occurrence.Sst.expression_desc with
                    | Direct_call
                        { call_form; callee; type_arguments;
                          arguments =
                            [ Value_argument
                                { label; value =
                                    ({ expression_desc = Direct_call inner_call;
                                       _ } as inner) } ];
                          recursive } ->
                        let changed_input =
                          { Sst.expression_desc = Int_constant Z.zero;
                            typ = Sst.Mathematical_int; span = inner.span }
                        in
                        let changed_inner =
                          { inner with
                            expression_desc =
                              Direct_call
                                { call_form = inner_call.call_form;
                                  callee = inner_call.callee;
                                  type_arguments = inner_call.type_arguments;
                                  arguments =
                                    [ Value_argument
                                        { label = None; value = changed_input } ];
                                  recursive = inner_call.recursive } }
                        in
                        { original_occurrence with
                          expression_desc =
                            Direct_call
                              { call_form; callee; type_arguments;
                                arguments =
                                  [ Value_argument
                                      { label; value = changed_inner } ];
                                recursive } }
                    | _ -> failwith "expected nested local projection call"
                  in
                  require
                    (Numeric_bv_source_admission_private.For_testing
                     .original_logical_occurrence
                       ~caller:projection_definition original_occurrence)
                    "actual source occurrence lost its immutable identity";
                  require
                    (not
                       (Numeric_bv_source_admission_private.For_testing
                        .original_logical_occurrence
                          ~caller:projection_definition copied_occurrence))
                    "same-span copied occurrence with altered input retained source authority";
                  let law_closures =
                    List.map
                    (fun name ->
                      let root = definition name in
                      match
                        Numeric_prior_law_closure_private.For_pipeline.complete
                          coordinator ~root
                      with
                      | Ok closure ->
                          require
                            (Numeric_prior_law_closure_private
                             .authorizes_definition closure root)
                            ("prior-law closure omitted " ^ name);
                          closure
                      | Error reason ->
                          failwith
                            ("same-unit prior-law closure rejected " ^ name
                           ^ ": " ^ reason))
                    [ "u_law"; "convert_law" ]
                  in
                  let expected_trusted_dependencies =
                    law_closures
                    |> List.concat_map
                         Numeric_prior_law_closure_private.dependency_evidence
                    |> List.filter_map (fun dependency ->
                           if
                             dependency.Numeric_proof_closure_walk_private
                             .trusted
                           then
                             Some
                               ( dependency.compiler_uid,
                                 dependency.full_key )
                           else None)
                    |> List.sort_uniq compare
                  in
                  (match
                     Numeric_prior_law_closure_private.For_pipeline.complete
                       coordinator ~root:projection_definition
                   with
                  | Error reason ->
                      require
                        (String.length reason >= 9
                        && String.contains reason 'n')
                        "native-assisted scoped-law rejection lost its reason"
                  | Ok _ ->
                      failwith
                        "native projection proof was laundered into prior-law authority");
                  require
                    (List.mem 8 (report_bv_widths report))
                    "same-unit source query did not retain structured BV8";
                  require
                    (List.exists
                       (fun (outer, modular) ->
                         outer
                         = Numeric_bv_projection_evidence_private.Checked_proof
                         && modular
                            = Numeric_bv_projection_evidence_private
                              .Checked_proof)
                       (report_bv_authorities report))
                    "same-unit source evidence lost its semantic trust axes";
                  let observed_trusted_dependencies =
                    report_bv_trusted_dependencies report
                    |> List.filter_map (fun dependency ->
                           if
                             dependency.Numeric_bv_projection_evidence_private
                             .origin
                             = Same_unit_law
                           then
                             Some
                               ( dependency.compiler_identity,
                                 dependency.evidence_full_key )
                           else None)
                    |> List.sort_uniq compare
                  in
                  require (expected_trusted_dependencies <> [])
                    "checked same-unit laws lost their trusted helper closure";
                  require
                    (observed_trusted_dependencies
                    = expected_trusted_dependencies)
                    "native source evidence did not retain exact transitive trusted helpers";
                  let observations = report_bv_source_observations report in
                  require (List.length observations = 2)
                    "two same-unit same-callee sites did not retain two source observations";
                  List.iter
                    (fun observation ->
                      require_source_observation ~target
                        ~provenance:
                          Numeric_bv_projection_evidence_private
                          .Same_unit_prior_closure
                        ~outer_authority:Checked_proof
                        ~modular_authority:Checked_proof observation;
                      require
                        (observation.trusted_dependencies
                        = (report_bv_trusted_dependencies report
                          |> List.sort_uniq compare))
                        "structured same-unit observation lost trusted helper dependencies";
                      require
                        (String.equal
                           observation.outer_operation.descriptor_origin
                           good.Cmt_input.unit_name
                        && String.equal
                             observation.modular_operation.descriptor_origin
                             good.unit_name)
                        "same-unit operation observation lost descriptor origin";
                      require
                        (not
                           (equal_span observation.outer_operation.call_span
                              observation.modular_operation.call_span))
                        "same-unit outer and nested call spans collapsed")
                    observations;
                  let occurrence_ids =
                    List.map
                      (fun
                        (observation :
                          Numeric_bv_projection_evidence_private
                          .source_observation) ->
                        observation.occurrence_identity)
                      observations
                  in
                  (match !stable_local_occurrences with
                  | None -> stable_local_occurrences := Some occurrence_ids
                  | Some expected ->
                      require (occurrence_ids = expected)
                        "same-unit occurrence identities changed by target or worker count");
                  require
                    (Verification_driver_private.status report = Counterexample)
                    "later independent false consumer did not remain a counterexample";
                  require
                    (Option.is_none
                       (Verification_driver_private.verified_completion report))
                    "failed whole program unexpectedly issued completion authority")
                [ 1; 2 ])
            targets;
          let whole_native =
            load_local ~include_false:false
              (Filename.concat workspace "bv_local_whole_native") "256"
          in
          let whole_loaded =
            match
              Interface_specification_loaded_private.verify_for_numeric_target
                ~threads:1 ~solver_policy:policy ~external_specifications:None
                ~external_targets:[] ~target:(List.hd targets)
                ~consumer:whole_native ~dependencies:[]
            with
            | Ok loaded -> loaded
            | Error error ->
                failwith
                  (Interface_specification_loaded_private.error_message error)
          in
          let whole_report =
            Interface_specification_loaded_private.driver whole_loaded
          in
          require
            (Verification_driver_private.status whole_report = Verified)
            "native-assisted whole-completion fixture did not verify";
          let whole_completion =
            Verification_driver_private.verified_completion whole_report
            |> Option.get
          in
          let whole_validated =
            Verification_driver_private.validated whole_report
          in
          let whole_projection =
            (Sst_validation.program whole_validated).functions
            |> List.find (fun definition ->
                   String.equal definition.Sst.function_id.function_name
                     "projection")
          in
          (match
             Numeric_proof_dependencies_private.complete_definition
               ~completion:whole_completion ~implementation:whole_native
               ~validated:whole_validated ~context:Ghost_context
               whole_projection
           with
          | Error (Native_bv_dependency _) -> ()
          | Error _ ->
              failwith
                "native-assisted whole proof was rejected for the wrong dependency reason"
          | Ok _ ->
              failwith
                "native-assisted whole proof was laundered into numeric dependency authority");
          let other_domain =
            load_local ~different_domain:true
              (Filename.concat workspace "bv_local_other_domain") "256"
          in
          let other_loaded =
            match
              Interface_specification_loaded_private.verify_for_numeric_target
                ~threads:1 ~solver_policy:policy ~external_specifications:None
                ~external_targets:[] ~target:(List.hd targets)
                ~consumer:other_domain ~dependencies:[]
            with
            | Ok loaded -> loaded
            | Error error ->
                failwith
                  (Interface_specification_loaded_private.error_message error)
          in
          let other_report =
            Interface_specification_loaded_private.driver other_loaded
          in
          require (report_bv_widths other_report = [])
            "non-exact carrier alias acquired native source authority";
          require
            (Verification_driver_private.status other_report = Counterexample)
            "non-exact carrier alias did not retain ordinary semantics";
          let bad =
            load_local (Filename.concat workspace "bv_local_bad") "17"
          in
          let bad_loaded =
            match
              Interface_specification_loaded_private.verify_for_numeric_target
                ~threads:1 ~solver_policy:policy ~external_specifications:None
                ~external_targets:[] ~target:(List.hd targets) ~consumer:bad
                ~dependencies:[]
            with
            | Ok loaded -> loaded
            | Error error ->
                failwith
                  (Interface_specification_loaded_private.error_message error)
          in
          let bad_report =
            Interface_specification_loaded_private.driver bad_loaded
          in
          require
            (Verification_driver_private.status bad_report = Counterexample)
            "wrong modular statement acquired native source authority";
          require (report_bv_widths bad_report = [])
            "wrong modular statement constructed a native BV term";
          let failed_checked_modular =
            load_local ~include_false:false ~failed_modular:true
              (Filename.concat workspace "bv_local_failed_checked_modular")
              "256"
          in
          List.iter
            (fun target ->
              List.iter
                (fun threads ->
                  let loaded =
                    match
                      Interface_specification_loaded_private
                      .verify_for_numeric_target ~threads
                        ~solver_policy:policy ~external_specifications:None
                        ~external_targets:[] ~target
                        ~consumer:failed_checked_modular ~dependencies:[]
                    with
                    | Ok loaded -> loaded
                    | Error error ->
                        failwith
                          (Interface_specification_loaded_private.error_message
                             error)
                  in
                  let report =
                    Interface_specification_loaded_private.driver loaded
                  in
                  require (report_bv_widths report = [])
                    "failed checked modular law granted native BV authority";
                  require
                    (Verification_driver_private.status report
                    = Counterexample)
                    "failed checked modular law did not retain ordinary failure";
                  require
                    (Option.is_none
                       (Verification_driver_private.verified_completion report))
                    "failed checked modular law issued whole-program completion")
                [ 1; 2 ])
            targets;
          Ok (Outcome.observation ~status:Outcome.Verified () |> Outcome.project)
        with Failure message ->
          Error (Failure.make Failure.Runner_internal message));
    Suite.case ~name:"typed-bv-source-evidence-keeps-original-target-binding"
      ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
      (fun ~environment:_ ~workspace:_ ->
        try
          let contexts_before = (Z3_bridge.counters ()).contexts_created in
          let build_capability = Build_target_profile_private.capability () in
          let profile =
            match
              Build_target_profile_private.authenticate_profile
                build_capability
            with
            | Ok profile -> profile
            | Error message -> failwith message
          in
          let targets =
            match
              Build_target_profile_private.authenticate_instances
                build_capability
            with
            | Ok targets ->
                List.sort
                  (fun left right ->
                    Int.compare left.Build_target_profile_private.target_claim.width
                      right.Build_target_profile_private.target_claim.width)
                  targets
            | Error message -> failwith message
          in
          let target32, target64 =
            match targets with
            | [ target32; target64 ] -> (target32, target64)
            | _ -> failwith "expected exact two-child build target matrix"
          in
          require (target32.target_claim.width = 32)
            "first sealed target is not physical 32";
          require (target64.target_claim.width = 64)
            "second sealed target is not physical 64";
          let backend = Bv_backend_capability_receipt_private.capability () in
          let unbound =
            match Bv_width.of_z ~profile backend (Z.of_int 8) with
            | Ok width -> width
            | Error message -> failwith message
          in
          let bind target =
            match Bv_width.for_instance backend target unbound with
            | Ok width -> width
            | Error message -> failwith message
          in
          let bound32 = bind target32 and bound64 = bind target64 in
          let span =
            Diagnostic.
              { file = "numeric_bv_source_cases.ml";
                start_pos = { line = 1; column = 0 };
                end_pos = { line = 1; column = 1 } }
          in
          let issue target width =
            let operation =
              Numeric_bv_projection_evidence_private.For_source_admission
              .operation
            in
            let outer_operation =
              operation ~descriptor_origin:"test-provider"
                ~occurrence_identity:"test-original-occurrence:outer"
                ~call_span:span ~callable_uid:"test-outer-uid"
                ~callable_tag:"unsigned-view" ~callable_abi:"test-outer-abi"
                ~law_evidence_full_key:"test-outer-law"
                ~authority:Checked_proof
            and modular_operation =
              operation ~descriptor_origin:"test-provider"
                ~occurrence_identity:"test-original-occurrence:modular"
                ~call_span:span ~callable_uid:"test-modular-uid"
                ~callable_tag:"modular-conversion"
                ~callable_abi:"test-modular-abi"
                ~law_evidence_full_key:"test-modular-law"
                ~authority:Explicit_axiom
            in
            Numeric_bv_projection_evidence_private.For_source_admission.issue
              ~target ~width ~provenance:Imported_registry
              ~trusted_dependencies:[] ~outer_operation ~modular_operation
              ~occurrence_identity:"test-original-occurrence"
              ~carrier_binding_full_key:"test-carrier"
              ~base_int_full_key:"test-base"
              ~source_witness_full_key:"test-authenticated-source"
              ~occurrence_full_key:"test-original-occurrence"
          in
          let evidence32 = issue target32 bound32
          and evidence64 = issue target64 bound64 in
          require
            (Result.is_ok
               (Numeric_bv_projection_evidence_private.validate
                  ~width:bound32 evidence32))
            "coherent target32 source evidence was rejected";
          require
            (Result.is_ok
               (Numeric_bv_projection_evidence_private.validate
                  ~width:bound64 evidence64))
            "coherent target64 source evidence was rejected";
          require
            (Result.is_error
               (Numeric_bv_projection_evidence_private.validate
                  ~width:bound64 evidence32))
            "target32 evidence accepted an already target64-bound width";
          require
            (Result.is_error
               (Numeric_bv_projection_evidence_private.validate
                  ~width:unbound evidence32))
            "target32 evidence accepted an unbound source width";
          require
            ((Z3_bridge.counters ()).contexts_created = contexts_before)
            "typed BV source evidence checking created a solver context";
          Ok (Outcome.observation ~status:Outcome.Verified () |> Outcome.project)
        with Failure message ->
          Error (Failure.make Failure.Runner_internal message)) ]
