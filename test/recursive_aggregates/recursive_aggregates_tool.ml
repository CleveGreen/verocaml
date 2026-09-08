let fail format = Printf.ksprintf (fun message -> prerr_endline message; exit 3) format

module Pinned_z3 = Smtml.Solver.Batch (Smtml.Z3_mappings)
module Raw = Smtml.Expr_raw

let span_to_string span =
  Printf.sprintf "%s:%d:%d-%d:%d" (Filename.basename span.Diagnostic.file)
    span.start_pos.line span.start_pos.column span.end_pos.line span.end_pos.column

let lower filename =
  match Typedtree_lowering.lower_file filename with
  | Ok program -> program
  | Error diagnostic ->
      fail "%s @ %s" diagnostic.Diagnostic.code
        (span_to_string diagnostic.span)

let lower_vir filename =
  let program = lower filename in
  match Symbolic_executor.lower_program program with
  | Ok vir -> vir
  | Error error -> fail "%s" (Symbolic_executor.error_to_string error)

let solve filename =
  let vir = lower_vir filename in
  let config =
    match Solver_backend.config ~timeout_ms:5000 with
    | Ok config -> config
    | Error error -> fail "%s" (Solver_backend.error_to_string error)
  in
  List.iter
    (fun execution ->
      match Solver_backend.solve_in_order config execution.Vir.obligations with
      | Error error -> fail "%s" (Solver_backend.error_to_string error)
      | Ok results ->
          let verified =
            List.length results = List.length execution.obligations
            && List.for_all
                 (fun result -> result.Solver_backend.outcome = Verified)
                 results
          in
          Printf.printf "%s: %s (%d obligations, %d exits)\n"
            execution.function_ref.function_name
            (if verified then "verified" else "not verified")
            (List.length execution.obligations) (List.length execution.exits);
          if not verified then exit 1)
    vir.functions

let classify filename =
  match Typedtree_lowering.lower_file filename with
  | Ok _ -> print_endline "accepted"
  | Error diagnostic ->
      Printf.printf "%s @ %s\n" diagnostic.Diagnostic.code
        (span_to_string diagnostic.span)

let unit () =
  let aggregate_type : Vir.aggregate_type =
    { aggregate_type_index = 7; aggregate_type_name = "node" ; aggregate_type_arguments = []}
  in
  let aggregate_symbol : Vir.symbol =
    {
      symbol_id = 0;
      source_name = "node";
      sort = Aggregate aggregate_type;
      role = Input;
      span = Diagnostic.file_span "unit.ml";
    }
  in
  let aggregate : Vir.aggregate_term =
    { aggregate_type; aggregate_desc = Aggregate_symbol aggregate_symbol }
  in
  let selector : Vir.selector =
    {
      selector_domain = aggregate_type;
      selector_range = Integer;
      selector_namespace = "t7_node_c1_Node_inline";
      selector_index = 0;
      selector_name = "value";
      selector_path = [];
    }
  in
  let selected = Vir.Integer_selector (selector, aggregate) in
  let obligation : Vir.obligation =
    {
      obligation_index = 0;
      function_ref = { function_index = 0; function_name = "unit" };
      kind = Assertion { assertion_ordinal = 0 };
      span = Diagnostic.file_span "unit.ml";
      assumptions = Vir.integer_range selected;
      required_preceding_safety = [];
      path_condition = [];
      goal = Integer_compare (Equal, selected, selected);
      projection_symbols = [];
      logical_constant_instances = [];
      logical_constant_equations = [];
    }
  in
  let config =
    match Solver_backend.config ~timeout_ms:5000 with
    | Ok config -> config
    | Error error -> fail "%s" (Solver_backend.error_to_string error)
  in
  (match Solver_backend.solve_obligation config obligation with
  | Ok Verified -> ()
  | Ok (Counterexample _ | Inconclusive _) -> fail "UF congruence was not verified"
  | Error error -> fail "%s" (Solver_backend.error_to_string error));
  let malformed =
    {
      obligation with
      goal = Vir.Boolean_selector (selector, aggregate);
    }
  in
  (match Solver_backend.solve_obligation config malformed with
  | Error (Malformed_vir _) -> ()
  | Error error -> fail "wrong cross-sort error: %s" (Solver_backend.error_to_string error)
  | Ok _ -> fail "cross-sort selector was accepted");
  let app_symbol = Smtml.Symbol.make Smtml.Ty.Ty_app "aggregate" in
  let app_term = Raw.symbol app_symbol in
  let app_equality =
    Raw.raw_relop Smtml.Ty.Ty_app Smtml.Ty.Relop.Eq app_term app_term
  in
  let app_solver = Pinned_z3.create ~logic:Smtml.Logic.QF_UFLIA () in
  let app_outcome =
    try
      Fun.protect
        ~finally:(fun () -> Pinned_z3.reset app_solver)
        (fun () ->
          ignore
            (Pinned_z3.get_sat_model ~symbols:[ app_symbol ] app_solver
               (Smtml.Expr.Set.singleton app_equality)));
      `Accepted
    with
    | Failure message
      when String.equal message "Trying to use unsupported theory: app\n" ->
        `Rejected
    | exn -> fail "unexpected Ty_app probe failure: %s" (Printexc.to_string exn)
  in
  (match app_outcome with
  | `Rejected -> ()
  | `Accepted -> fail "Ty_app unexpectedly reached the pinned Z3 mapping");
  print_endline "aggregate carrier: typed VIR identity maps to unconstrained SMT Int";
  print_endline "selector/tag encoding: namespaced unary UFs in QF_UFLIA";
  print_endline "selector typing: cross-sort applications rejected before SMT";
  print_endline "Ty_app probe: pinned Z3 mapping rejects unsupported theory app"

let () =
  match Array.to_list Sys.argv with
  | [ _; "unit" ] -> unit ()
  | [ _; "dump-sst"; filename ] -> print_string (Sst.to_string (lower filename))
  | [ _; "dump-vir"; filename ] -> print_string (Vir.to_string (lower_vir filename))
  | [ _; "solve"; filename ] -> solve filename
  | [ _; "classify"; filename ] -> classify filename
  | _ -> fail "usage: recursive_aggregates_tool (unit|dump-sst|dump-vir|solve|classify) [FILE.cmt]"
