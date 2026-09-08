let fail format = Printf.ksprintf failwith format

let require condition format =
  Printf.ksprintf (fun message -> if not condition then failwith message) format

let private_driver_report report : Verification_driver_private.report =
  Obj.obj (Obj.field (Obj.repr report) 0)

let status = function
  | Verification_pipeline.Verified -> "verified"
  | Counterexample -> "counterexample"
  | Inconclusive -> "inconclusive"
  | Incomplete_source -> "incomplete"

let verify_positive consumer dependency =
  match
    Interface_specification.verify_consumer_with_threads ~threads:2
      ~timeout_ms:60_000 ~dependency_files:[ dependency ]
      ~consumer_file:consumer
  with
  | Error error -> fail "%s" (Interface_specification.error_to_string error)
  | Ok report ->
      let report = private_driver_report report in
      let counters = Verification_driver_private.counters report in
      require
        (Verification_driver_private.status report
        = Verification_pipeline.Verified)
        "positive retained Exec consumer did not verify";
      require
        (counters.finite_witness_issuances = 0
        && counters.finite_parent_issuances = 0
        && counters.finite_child_derivations = 0
        && counters.finite_result_promotions = 0
        && counters.finite_result_manifests = 0
        && counters.finite_result_completions = 0
        && counters.finite_result_consumptions = 0
        && counters.finite_formal_assumption_issuances = 0
        && counters.finite_formal_transfers = 0
        && counters.recursive_spec_result_issuances = 0
        && counters.recursive_spec_result_consumptions = 0
        && counters.receipts_issued = 0
        && counters.receipts_consumed = 0
        && counters.transition_predecessor_transfers = 0
        && counters.invariant_cell_entry_eligibilities = 0
        && counters.owned_root_scalar_plans_issued = 0)
        "retained Exec result minted forbidden authority";
      let vir = Verification_driver_private.vir report in
      require
        (vir.Vir.rank_domains = [])
        "retained Exec result imported a rank domain";
      Printf.printf
        "positive status=%s authority=finite:0/0 result:0/0 recursive:0/0 \
         invariant:0 ownership:0 rank-domains=0\n"
        (status (Verification_driver_private.status report))

type observations = {
  pipeline : int;
  solver : int;
  z3_contexts : int;
  z3_solvers : int;
}

let reset () =
  Verification_driver_private.For_testing.reset_driver_entries ();
  Verification_pipeline.For_testing.reset_validated_pipeline_entries ();
  Solver_backend_counter_private.reset_solver_creation_count ();
  Z3_bridge.reset_counters ()

let observations () =
  let z3 = Z3_bridge.counters () in
  {
    pipeline = Verification_pipeline.For_testing.validated_pipeline_entries ();
    solver = Solver_backend_counter_private.solver_creation_count ();
    z3_contexts = z3.contexts_created;
    z3_solvers = z3.solvers_created;
  }

let rejected_zero_work label consumer dependencies =
  reset ();
  let authentication =
    Interface_specification.authenticate ~timeout_ms:60_000
      ~dependency_files:dependencies ~consumer_file:consumer
  in
  let provider_boundary = observations () in
  reset ();
  let verification =
    Interface_specification.verify_consumer ~timeout_ms:60_000
      ~dependency_files:dependencies ~consumer_file:consumer
  in
  (match (authentication, verification) with
  | Error _, Error _ | Ok _, Error _ -> ()
  | (Error _ | Ok _), Ok _ -> fail "%s unexpectedly verified" label);
  let after = observations () in
  require
    (after.pipeline = provider_boundary.pipeline
    && after.solver = provider_boundary.solver
    && after.z3_contexts = provider_boundary.z3_contexts
    && after.z3_solvers = provider_boundary.z3_solvers)
    "%s advanced consumer semantic or solver work" label;
  Printf.printf
    "%s boundary=pre-consumer-sst-vir backend-delta=0 solver-delta=0 \
     z3-delta=0/0\n"
    label

let raw_span =
  Diagnostic.
    {
      file = "retained_exec_result_private_test.ml";
      start_pos = { line = 1; column = 0 };
      end_pos = { line = 1; column = 1 };
    }

let type_id index name = Sst.{ type_index = index; type_name = name }

let field ?(uniqueness = Sst.Preserve_uniqueness) type_id =
  Sst.
    {
      field_id =
        {
          field_owner = Record_owner type_id;
          field_index = 0;
          field_name = "value";
        };
      field_type = Int;
      field_mutability = Immutable_field;
      field_modalities =
        {
          uniqueness_modality = uniqueness;
          linearity_modality = Preserve_linearity;
        };
      span = raw_span;
    }

let type_definition type_id field =
  Sst.
    {
      type_id;
      type_kind = Record_definition [ field ];
      representation = Revealed;
      span = raw_span;
    }

let checked_definition index name type_id =
  let binding =
    Sst.
      {
        id = index;
        name = "result";
        typ = Aggregate type_id;
        uniqueness = Definitely_aliased;
        span = raw_span;
      }
  in
  let body =
    Sst.
      {
        stage = Runtime;
        expression =
          {
            expression_desc =
              Variable { binding; use_uniqueness = Definitely_aliased };
            typ = binding.typ;
            span = raw_span;
          };
      }
  in
  Sst.
    {
      function_id = { function_index = index; function_name = name };
      type_binders = [];
      mode = Exec;
      recursive = false;
      parameters = [];
      contracts = empty_contracts;
      body = Checked_exec { body; provenance = Raw_semantic_body raw_span };
      policy = Default_linear_z3;
      result_type = Aggregate type_id;
      returns_unique_parameter = None;
      span = raw_span;
    }

let provider_type definition field_modes =
  Retained_exec_result_private.
    {
      resolved_path = "Provider.result";
      source_path = "Provider.result";
      source_name = definition.Sst.type_id.type_name;
      binding_uid = "Provider.result.uid";
      definition;
      parametric_descriptor = None;
      revealed = true;
      logical = false;
      field_modes;
      rank_profile_digest = None;
    }

let provider =
  Retained_exec_result_private.
    {
      unit_name = "Provider";
      interface_digest = "interface";
      source_digest = "source";
      family_digest = "family";
      import_digest = "imports";
    }

let require_zero_delta label before after =
  require (before = after)
    "%s advanced driver, pipeline, backend, solver, or Z3 work" label

let classify_ineligible label ~result_mode types definition =
  reset ();
  let before = observations () in
  let classification =
    Retained_exec_result_private.classify ~provider ~types ~parametric_adts:[]
      ~resolved_path:definition.Sst.function_id.function_name
      ~binding_uid:"Provider.callable.uid" ~definition ~result_mode ~model:None
  in
  let after = observations () in
  require_zero_delta label before after;
  (match classification with
  | Ok Retained_exec_result_private.Ineligible -> ()
  | Ok _ -> fail "%s classifier unexpectedly admitted closure" label
  | Error error ->
      fail "%s classifier errored instead of ineligible: %s" label error);
  Printf.printf
    "%s classification=ineligible boundary=direct-classifier backend-delta=0 \
     solver-delta=0 z3-delta=0/0\n"
    label

let classify_closure_matrix () =
  let shared_id = type_id 100 "shared_result" in
  let shared_field = field ~uniqueness:Sst.Force_aliased shared_id in
  let shared_definition = type_definition shared_id shared_field in
  classify_ineligible "closure-unsupported-shared"
    ~result_mode:Sst.Exec_instance
    [
      provider_type shared_definition
        [ (shared_field.field_id, Sst.Exec_instance) ];
    ]
    (checked_definition 100 "Provider.make_shared" shared_id);
  let incomplete_id = type_id 101 "incomplete_result" in
  let incomplete_field = field incomplete_id in
  let incomplete_definition = type_definition incomplete_id incomplete_field in
  classify_ineligible "closure-incomplete" ~result_mode:Sst.Exec_instance
    [ provider_type incomplete_definition [] ]
    (checked_definition 101 "Provider.make_incomplete" incomplete_id);
  let foreign_id = type_id 102 "Foreign.result" in
  classify_ineligible "closure-foreign" ~result_mode:Sst.Exec_instance []
    (checked_definition 102 "Provider.make_foreign" foreign_id);
  let malformed_id = type_id 103 "malformed_result" in
  let malformed_field =
    let field = field malformed_id in
    {
      field with
      Sst.field_id =
        {
          field.field_id with
          field_owner = Sst.Record_owner (type_id 104 "wrong_owner");
        };
    }
  in
  let malformed_definition = type_definition malformed_id malformed_field in
  let malformed_callable =
    checked_definition 103 "Provider.make_malformed" malformed_id
  in
  reset ();
  let before = observations () in
  let validation =
    Sst_validation.validate
      Sst.
        {
          policy = Default_linear_z3;
          parametric_adts = [];
          types = [ malformed_definition ];
          logical_constants = [];
          functions = [ malformed_callable ];
        }
  in
  let after = observations () in
  require_zero_delta "closure-malformed" before after;
  (match validation with
  | Error _ -> ()
  | Ok _ -> fail "closure-malformed source gate unexpectedly accepted SST");
  Printf.printf
    "closure-malformed classification=rejected boundary=sst-validation \
     backend-delta=0 solver-delta=0 z3-delta=0/0\n"

let () =
  match Array.to_list Sys.argv with
  | [ _; "positive"; consumer; dependency ] ->
      verify_positive consumer dependency
  | _ :: "reject" :: label :: consumer :: dependencies when dependencies <> []
    ->
      rejected_zero_work label consumer dependencies
  | [ _; "classify-closures" ] -> classify_closure_matrix ()
  | _ ->
      fail
        "usage: retained_exec_results_tool positive CONSUMER DEPENDENCY | \
         reject LABEL CONSUMER DEPENDENCY... | classify-closures"
