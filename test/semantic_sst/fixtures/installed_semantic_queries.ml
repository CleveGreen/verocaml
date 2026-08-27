let span = Diagnostic.file_span "installed-semantic-client.ml"
let function_id = Sst.{ function_index = 0; function_name = "identity" }

let parameter =
  Sst.
    {
      id = 0;
      name = "value";
      typ = Int;
      uniqueness = Definitely_aliased;
      span;
    }

let pattern =
  Sst.{ pattern_desc = Bind parameter; typ = Int; span }

let body =
  Sst.
    {
      expression_desc =
        Variable
          {
            binding = parameter;
            use_uniqueness = Definitely_aliased;
          };
      typ = Int;
      span;
    }

let definition =
  Sst.
    {
      function_id;
      mode = Exec;
      recursive = false;
      type_binders = [];
      parameters =
        [ Value_parameter { label = None; pattern; optional_default = None } ];
      contracts = empty_contracts;
      body =
        Checked_exec
          {
            body = { stage = Runtime; expression = body };
            provenance = Raw_semantic_body span;
          };
      policy = Default_linear_z3;
      result_type = Int;
      returns_unique_parameter = None;
      span;
    }

let () =
  let validated =
    match
      Sst_validation.validate
        Sst.
          {
            policy = Default_linear_z3;
            parametric_adts = [];
            types = [];
            functions = [ definition ];
          }
    with
    | Ok validated -> validated
    | Error error -> failwith (Sst_validation.error_to_string error)
  in
  let descriptors = Sst_validation.callable_descriptors validated in
  let registry = Sst_callable.build validated in
  let termination = Termination.analyze validated in
  let termination_plan = Termination.prepare termination in
  match
    ( descriptors,
      Sst_callable.find registry function_id,
      Termination.graph_callables (Termination.graph termination),
      Termination.sccs termination,
      termination_plan )
  with
  | [ descriptor ], Some wrapper, [ graph_descriptor ], [ component ], Ok plan
    when
      Sst_validation.callable_id descriptor = function_id
      && Termination.callable_id graph_descriptor = function_id
      && wrapper.function_id = function_id
      && not (Termination.scc_is_recursive component)
      && Termination.pending_summaries plan = [] ->
      print_endline
        "installed validated queries, callable wrapper, and termination graph accepted"
  | _ -> failwith "installed validated query mismatch"
