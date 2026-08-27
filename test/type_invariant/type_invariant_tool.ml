let fail format =
  Printf.ksprintf
    (fun message ->
      prerr_endline message;
      exit 3)
    format

let span_to_string span =
  Printf.sprintf "%s:%d:%d-%d:%d" (Filename.basename span.Diagnostic.file)
    span.start_pos.line span.start_pos.column span.end_pos.line
    span.end_pos.column

let load filename =
  match Typedtree_lowering.lower_file filename with
  | Ok program -> program
  | Error diagnostic ->
      fail "%s @ %s" diagnostic.Diagnostic.code
        (span_to_string diagnostic.span)

let validate program =
  match Sst_validation.validate program with
  | Ok validated -> validated
  | Error error -> fail "%s" (Sst_validation.error_to_string error)

let invariants validated =
  match Type_invariant.authenticate validated with
  | Ok invariants -> invariants
  | Error error -> fail "%s" (Type_invariant.error_to_string error)

let lower program =
  match Symbolic_executor.lower_program program with
  | Ok vir -> vir
  | Error error -> fail "%s" (Symbolic_executor.error_to_string error)

let descriptor filename =
  let environment = load filename |> validate |> invariants in
  match Type_invariant.handles environment with
  | [ handle ] ->
      let type_id = Type_invariant.abstract_type handle in
      let model = Type_invariant.model_callable handle in
      let predicate = Type_invariant.predicate_callable handle in
      Printf.printf
        "handle=%s certificate=%s type=%s#%d model=%s#%d snapshot=%s \
         predicate=%s#%d digest=%s operations=%d\n"
        (Type_invariant.invariant_id handle)
        (Type_invariant.certificate_id handle)
        type_id.type_name type_id.type_index model.function_name
        model.function_index
        (Sst.string_of_type (Type_invariant.model_snapshot_type handle))
        predicate.function_name predicate.function_index
        (Type_invariant.predicate_digest handle)
        (List.length (Type_invariant.public_operations handle))
  | handles -> fail "expected one invariant handle, got %d" (List.length handles)

let solve filename =
  let vir = load filename |> lower in
  let config =
    match Solver_backend.config ~timeout_ms:5000 with
    | Ok config -> config
    | Error error -> fail "%s" (Solver_backend.error_to_string error)
  in
  List.iter
    (fun execution ->
      match Solver_backend.solve_in_order config execution.Vir.obligations with
      | Error error -> fail "%s" (Solver_backend.error_to_string error)
      | Ok results
        when
          List.length results = List.length execution.obligations
          && List.for_all
               (fun result ->
                 result.Solver_backend.outcome = Solver_backend.Verified)
               results ->
          Printf.printf "%s: verified (%d obligations)\n"
            execution.function_ref.function_name
            (List.length execution.obligations)
      | Ok _ -> fail "%s did not verify" execution.function_ref.function_name)
    vir.functions

let counterexamples filename =
  let vir = load filename |> lower in
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
          List.iter
            (fun result ->
              match result.Solver_backend.outcome with
              | Solver_backend.Verified -> ()
              | Solver_backend.Counterexample _ ->
                  let kind =
                    match result.obligation.kind with
                    | Vir.Assertion _ -> "assertion"
                    | Vir.Invariant_validity _ -> "invariant-validity"
                    | _ -> "other"
                  in
                  Printf.printf "%s: counterexample %s\n"
                    execution.function_ref.function_name kind
              | Solver_backend.Inconclusive _ ->
                  Printf.printf "%s: inconclusive\n"
                    execution.function_ref.function_name)
            results)
    vir.functions

let reject filename =
  match Typedtree_lowering.lower_file filename with
  | Error diagnostic ->
      Printf.printf "adapter rejected: %s\n" diagnostic.Diagnostic.code
  | Ok program -> (
      match Symbolic_executor.lower_program program with
      | Error { Symbolic_executor.unsupported = Malformed_sst _; _ } ->
          print_endline "semantic invariant gate rejected before VIR"
      | Error error -> fail "%s" (Symbolic_executor.error_to_string error)
      | Ok _ -> fail "negative invariant fixture was accepted")

let map_evidence map (program : Sst.program) =
  let types =
    List.map
      (fun (definition : Sst.type_definition) ->
        match definition.representation with
        | Sst.Abstract_with_evidence
            (Sst.Authenticated_same_cmt_abstraction evidence)
          when
            List.exists
              (fun operation ->
                operation.Sst.public_role = Sst.Abstract_invariant)
              evidence.public_surface ->
            {
              definition with
              representation =
                Sst.Abstract_with_evidence
                  (Sst.Authenticated_same_cmt_abstraction (map evidence));
            }
        | _ -> definition)
      program.types
  in
  { program with types }

let expect_forgery label program =
  let validation =
    match Sst_validation.validate program with
    | Error error -> Sst_validation.error_to_string error
    | Ok _ -> fail "%s: forged invariant snapshot validated" label
  in
  let lowering =
    match Symbolic_executor.lower_program program with
    | Error error -> Symbolic_executor.error_to_string error
    | Ok _ -> fail "%s: forged invariant snapshot reached VIR" label
  in
  ignore validation;
  ignore lowering;
  Printf.printf "%s: rejected before registration and VIR\n" label

let attacks filename =
  let program = load filename in
  let alter_invariant_operation alter
      (evidence : Sst.same_cmt_abstraction_evidence) =
    {
      evidence with
      Sst.public_surface =
        List.map
          (fun operation ->
            if operation.Sst.public_role = Sst.Abstract_invariant then
              alter operation
            else operation)
          evidence.public_surface;
    }
  in
  expect_forgery "predicate-identity"
    (map_evidence
       (alter_invariant_operation (fun operation ->
            {
              operation with
              public_function_name = operation.public_function_name ^ ".copy";
            }))
       program);
  expect_forgery "predicate-role"
    (map_evidence
       (alter_invariant_operation (fun operation ->
            { operation with public_role = Sst.Abstract_model }))
       program);
  expect_forgery "operation-snapshot"
    (map_evidence
       (fun (evidence : Sst.same_cmt_abstraction_evidence) ->
         {
           evidence with
           public_surface = List.rev evidence.public_surface;
         })
       program);
  expect_forgery "model-identity"
    (map_evidence
       (fun (evidence : Sst.same_cmt_abstraction_evidence) ->
         {
           evidence with
           public_surface =
             List.map
               (fun operation ->
                 if operation.Sst.public_role = Sst.Abstract_model then
                   {
                     operation with
                     public_function_index =
                       operation.public_function_index + 100;
                   }
                 else operation)
               evidence.public_surface;
         })
       program);
  expect_forgery "copied-token"
    (map_evidence
       (fun (evidence : Sst.same_cmt_abstraction_evidence) ->
         {
           evidence with
           authentication_token = ref !(evidence.authentication_token);
         })
       program);
  expect_forgery "certificate-type"
    (map_evidence
       (fun (evidence : Sst.same_cmt_abstraction_evidence) ->
         {
           evidence with
           abstract_signature_type =
             {
               evidence.abstract_signature_type with
               type_name = "mismatched";
             };
         })
       program)

let map_transition_expressions alter expression =
  let rec map expression =
    let expression_desc =
      match expression.Sst.expression_desc with
      | Sst.Field_write { provenance; field; value; transition } ->
          Sst.Field_write
            {
              provenance;
              field;
              value = map value;
              transition = Option.map alter transition;
            }
      | Sst.Owned_tree_nested_write { transition; value } ->
          Sst.Owned_tree_nested_write
            { transition = alter transition; value = map value }
      | Sst.Owned_tree_rebase { transition } ->
          Sst.Owned_tree_rebase { transition = alter transition }
      | Sst.Sequence (left, right) -> Sst.Sequence (map left, map right)
      | Sst.Let (bindings, body) ->
          Sst.Let
            ( List.map (fun (pattern, value) -> (pattern, map value)) bindings,
              map body )
      | Sst.If (condition, yes, no) ->
          Sst.If (map condition, map yes, Option.map map no)
      | Sst.Match (scrutinee, cases) ->
          Sst.Match
            ( map scrutinee,
              List.map
                (fun (case : Sst.case) ->
                  {
                    case with
                    Sst.case_guard = Option.map map case.case_guard;
                    case_body = map case.case_body;
                  })
                cases )
      | desc -> desc
    in
    { expression with Sst.expression_desc }
  in
  map expression

let transition_attacks filename =
  let program = load filename in
  let rewrite alter =
    {
      program with
      Sst.functions =
        List.map
          (fun definition ->
            match definition.Sst.body with
            | Sst.Checked_exec { body; provenance } ->
                {
                  definition with
                  body =
                    Sst.Checked_exec
                      {
                        body =
                          {
                            body with
                            expression =
                              map_transition_expressions alter body.expression;
                          };
                        provenance;
                      };
                }
            | _ -> definition)
          program.functions;
    }
  in
  let skipped = ref false in
  expect_forgery "skipped-successor"
    (rewrite (fun transition ->
         if !skipped then transition
         else (
           skipped := true;
           { transition with successor_version = transition.pre_version })));
  expect_forgery "mismatched-successor"
    (rewrite (fun transition ->
         {
           transition with
           successor_version = transition.successor_version + 100;
         }))


let dump_vir filename =
  print_string (Vir.to_string (load filename |> lower))

let dump_sst filename =
  print_string (Sst.to_string (load filename))

let () =
  match Array.to_list Sys.argv with
  | [ _; "descriptor"; filename ] -> descriptor filename
  | [ _; "solve"; filename ] -> solve filename
  | [ _; "counterexamples"; filename ] -> counterexamples filename
  | [ _; "reject"; filename ] -> reject filename
  | [ _; "attacks"; filename ] -> attacks filename
  | [ _; "transition-attacks"; filename ] -> transition_attacks filename
  | [ _; "dump-sst"; filename ] -> dump_sst filename
  | [ _; "dump-vir"; filename ] -> dump_vir filename
  | _ ->
      fail
        "usage: type_invariant_tool \
         (descriptor|solve|counterexamples|reject|attacks|transition-attacks|dump-sst|dump-vir) \
         FILE"
