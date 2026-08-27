let fail format =
  Printf.ksprintf
    (fun message ->
      prerr_endline message;
      exit 3)
    format

let check label condition = if not condition then fail "%s" label

let span line =
  let position column = Diagnostic.{ line; column } in
  Diagnostic.
    {
      file = "semantic.ml";
      start_pos = position 0;
      end_pos = position 8;
    }

let expression ?(line = 1) typ expression_desc =
  Sst.{ expression_desc; typ; span = span line }

let int ?line value = expression ?line Sst.Int (Sst.Int_constant (Z.of_int value))
let bool ?line value = expression ?line Sst.Bool (Sst.Bool_constant value)
let unit ?line () = expression ?line Sst.Unit Sst.Unit_constant

let binding ?(line = 1) ?(typ = Sst.Int) id name =
  Sst.
    {
      id;
      name;
      typ;
      uniqueness = Definitely_aliased;
      span = span line;
    }

let bind binding =
  Sst.
    {
      pattern_desc = Bind binding;
      typ = binding.typ;
      span = binding.span;
    }

let variable (binding : Sst.binding) =
  expression ~line:binding.Sst.span.start_pos.line binding.typ
    (Sst.Variable
       { binding; use_uniqueness = Sst.Definitely_aliased })

let function_id index name = Sst.{ function_index = index; function_name = name }

let raw_exec ?(index = 0) ?(name = "f") ?(recursive = false)
    ?(parameters = []) ?(contracts = Sst.empty_contracts)
    ?(result_type = Sst.Int) ?returns_unique_parameter ?(policy = Sst.Default_linear_z3)
    body =
  Sst.
    {
      function_id = function_id index name;
      type_binders = [];
      mode = Exec;
      recursive;
      parameters;
      contracts;
      body =
        Checked_exec
          {
            body = { stage = Runtime; expression = body };
            provenance = Raw_semantic_body (span (index + 1));
          };
      policy;
      result_type;
      returns_unique_parameter;
      span = span (index + 1);
    }

let program ?(policy = Sst.Default_linear_z3) ?(types = []) functions =
  Sst.{ policy; parametric_adts = []; types; functions }

let expect_error label predicate program =
  match Sst_validation.validate program with
  | Error error when predicate error.Sst_validation.kind -> ()
  | Error error ->
      fail "%s produced the wrong error: %s" label
        (Sst_validation.error_to_string error)
  | Ok _ -> fail "%s was accepted" label

let expect_validation_before_lowering label program =
  match Symbolic_executor.lower_program program with
  | Error { unsupported = Symbolic_executor.Malformed_sst _; _ } -> ()
  | Error error ->
      fail "%s reached the wrong lowering error: %s" label
        (Symbolic_executor.error_to_string error)
  | Ok _ -> fail "%s reached symbolic lowering" label

let invalid_call = function Sst_validation.Invalid_call _ -> true | _ -> false
let invalid_body = function Sst_validation.Invalid_body _ -> true | _ -> false

let identity () =
  let x = binding 0 "x" in
  raw_exec ~name:"identity"
    ~parameters:[ Sst.Value_parameter
      Sst.{ label = None; pattern = bind x; optional_default = None } ]
    (variable x)

let type_id index name = Sst.{ type_index = index; type_name = name }

let record_type ?(representation = Sst.Revealed) (id : Sst.type_id) =
  Sst.
    {
      type_id = id;
      type_kind = Record_definition [];
      representation;
      span = span (id.type_index + 20);
    }

let call ?(line = 1) ?(recursive = false) ~form ~callee ~typ () =
  expression ~line typ
    (Sst.Direct_call
       {
         call_form = form;
         callee;
         arguments = [];
         type_arguments = [];
         recursive;
       })

let mode_definition mode index =
  let id = function_id index (Printf.sprintf "callee_%d" index) in
  match mode with
  | Sst.Exec -> raw_exec ~index ~name:id.function_name (int 0)
  | Sst.Spec ->
      {
        (raw_exec ~index ~name:id.function_name (int 0)) with
        mode = Spec;
        body = Spec_definition { stage = Logical; expression = int 0 };
      }
  | Sst.Proof ->
      {
        (raw_exec ~index ~name:id.function_name ~result_type:Sst.Unit
           (unit ()))
        with
        mode = Proof;
        body =
          Proof_body
            {
              body = { stage = Proof_stage; expression = unit () };
              provenance = Raw_semantic_body (span index);
            };
      }

let staged_caller stage form callee =
  let typ = callee.Sst.result_type in
  let call = call ~form ~callee:callee.function_id ~typ () in
  let body, result_type =
    match (stage, typ) with
    | Sst.Proof_stage, (Sst.Int | Sst.Bool) ->
        ( expression Sst.Unit
            (Sst.Let
               ([ (Sst.{ pattern_desc = Wildcard; typ; span = span 100 }, call) ],
                unit ())),
          Sst.Unit )
    | _ -> (call, typ)
  in
  let base = raw_exec ~index:100 ~name:"caller" ~result_type body in
  match stage with
  | Sst.Runtime -> base
  | Sst.Logical ->
      { base with mode = Spec; body = Spec_definition { stage; expression = body } }
  | Sst.Proof_stage ->
      {
        base with
        mode = Proof;
        body =
          Proof_body
            {
              body = { stage; expression = body };
              provenance = Raw_semantic_body (span 100);
            };
      }

let raw_recursive_mode_definition mode index name callee ~self =
  let id = function_id index name in
  let call_form, result_type, stage =
    match mode with
    | Sst.Spec -> (Sst.Specification_call, Sst.Int, Sst.Logical)
    | Sst.Proof -> (Sst.Proof_call, Sst.Unit, Sst.Proof_stage)
    | Sst.Exec -> fail "raw recursive mode fixture must be Spec or Proof"
  in
  let body =
    call ~line:(index + 1) ~recursive:self ~form:call_form ~callee
      ~typ:result_type ()
  in
  let base =
    raw_exec ~index ~name ~recursive:true ~result_type body
  in
  match mode with
  | Sst.Spec ->
      {
        base with
        function_id = id;
        mode = Spec;
        body = Spec_definition { stage; expression = body };
      }
  | Sst.Proof ->
      {
        base with
        function_id = id;
        mode = Proof;
        body =
          Proof_body
            {
              body = { stage; expression = body };
              provenance = Raw_semantic_body (span index);
            };
      }
  | Sst.Exec -> assert false

let check_raw_recursive_modes () =
  let mode_name = function
    | Sst.Spec -> "specification"
    | Sst.Proof -> "proof"
    | Sst.Exec -> assert false
  in
  let expect_precheck label expected analysis =
    match Termination.precheck analysis with
    | Error error when expected error.Termination.kind -> ()
    | Error error ->
        fail "%s produced the wrong termination error: %s" label
          (Termination.error_to_string error)
    | Ok () -> fail "%s passed raw termination precheck" label
  in
  let expect_legacy_cycle label mode candidate =
    let expected =
      match mode with
      | Sst.Spec -> "cyclic pure specification call graph"
      | Sst.Proof -> "cyclic proof call graph"
      | Sst.Exec -> assert false
    in
    match Sst_validation.validate candidate with
    | Error
        {
          kind = Sst_validation.Invalid_call detail;
          _;
        }
      when String.equal detail expected ->
        ()
    | Error error ->
        fail "%s changed the legacy unavailable-recursion gate: %s" label
          (Sst_validation.error_to_string error)
    | Ok _ -> fail "%s admitted unavailable recursion" label
  in
  List.iteri
    (fun offset mode ->
      let first_index = 200 + (offset * 10) in
      let self_id =
        function_id first_index
          (Printf.sprintf "%s_self" (mode_name mode))
      in
      let self =
        raw_recursive_mode_definition mode first_index self_id.function_name
          self_id ~self:true
      in
      let self_program = program [ self ] in
      let self_analysis = Termination.analyze_raw self_program in
      let self_graph = Termination.graph self_analysis in
      check "raw self callable order/mode changed"
        (match Termination.graph_callables self_graph with
        | [ callable ] ->
            Termination.callable_id callable = self_id
            && Termination.callable_mode callable = mode
        | _ -> false);
      check "raw self edge order/identity changed"
        (match Termination.graph_edges self_graph with
        | [ edge ] ->
            Termination.callable_id (Termination.call_edge_caller edge) = self_id
            && Termination.callable_id (Termination.call_edge_callee edge) = self_id
            && Termination.call_edge_recursive edge
            && Termination.call_edge_region edge = Sst_validation.Body_region
        | _ -> false);
      check "raw self SCC descriptor changed"
        (match Termination.sccs self_analysis with
        | [ component ] ->
            Termination.scc_modes component = [ mode ]
            && List.map Termination.callable_id
                 (Termination.scc_members component)
               = [ self_id ]
            && List.length (Termination.scc_edges component) = 1
            && Termination.scc_recursion_kind component
               = Some Termination.Direct_self
        | _ -> false);
      (match mode with
      | Sst.Spec ->
          (match Termination.precheck self_analysis with
          | Ok () -> ()
          | Error error ->
              fail "raw specification self was not retained for authentication: %s"
                (Termination.error_to_string error));
          (match Termination.prepare self_analysis with
          | Error { kind = Termination.Raw_analysis_only; _ } -> ()
          | Error error ->
              fail "raw specification self produced %s"
                (Termination.error_to_string error)
          | Ok _ -> fail "raw specification analysis issued a pending plan");
          expect_legacy_cycle "raw unsupported specification self mode" mode
            self_program;
          (match Symbolic_executor.lower_program self_program with
          | Error { unsupported = Symbolic_executor.Malformed_sst detail; _ } ->
              check
                "raw specification self did not retain legacy authentication gate"
                (String.starts_with
                   ~prefix:
                     "specification_self: invalid semantic SST: cyclic pure \
                      specification call graph"
                   detail)
          | Error error ->
              fail "raw specification self received the wrong pre-VIR error: %s"
                (Symbolic_executor.error_to_string error)
          | Ok _ -> fail "raw specification self reached VIR")
      | Sst.Proof ->
          (match Termination.precheck self_analysis with
          | Ok () -> ()
          | Error error ->
              fail "raw proof self was not recognized as a supported mode: %s"
                (Termination.error_to_string error));
          (match Termination.prepare self_analysis with
          | Error { kind = Termination.Raw_analysis_only; _ } -> ()
          | Error error ->
              fail "raw proof self produced the wrong preparation error: %s"
                (Termination.error_to_string error)
          | Ok _ -> fail "raw proof self issued a pending plan");
          (match Sst_validation.validate self_program with
          | Error { kind = Sst_validation.Invalid_body detail; _ } ->
              check "raw recursive proof authentication diagnostic changed"
                (String.starts_with
                   ~prefix:
                     "recursive proof declarations require authenticated"
                   detail)
          | Error error ->
              fail "raw proof self produced the wrong semantic error: %s"
                (Sst_validation.error_to_string error)
          | Ok _ -> fail "raw proof self issued validated authority");
          (match Symbolic_executor.lower_program self_program with
          | Error { unsupported = Symbolic_executor.Malformed_sst detail; _ } ->
              check "raw proof self did not fail authentication before VIR"
                (String.starts_with
                   ~prefix:
                     "proof_self: invalid semantic SST: recursive proof \
                      declarations require authenticated"
                   detail)
          | Error error ->
              fail "raw proof self received the wrong pre-VIR error: %s"
                (Symbolic_executor.error_to_string error)
          | Ok _ -> fail "raw proof self reached VIR")
      | Sst.Exec -> assert false);

      let left_id =
        function_id (first_index + 1)
          (Printf.sprintf "%s_left" (mode_name mode))
      in
      let right_id =
        function_id (first_index + 2)
          (Printf.sprintf "%s_right" (mode_name mode))
      in
      let left =
        raw_recursive_mode_definition mode left_id.function_index
          left_id.function_name right_id ~self:false
      in
      let right =
        raw_recursive_mode_definition mode right_id.function_index
          right_id.function_name left_id ~self:false
      in
      let mutual_program = program [ left; right ] in
      let mutual_analysis = Termination.analyze_raw mutual_program in
      let mutual_graph = Termination.graph mutual_analysis in
      check "raw mutual callable order/modes changed"
        (List.map
           (fun callable ->
             (Termination.callable_id callable, Termination.callable_mode callable))
           (Termination.graph_callables mutual_graph)
        = [ (left_id, mode); (right_id, mode) ]);
      check "raw mutual edge order/identity changed"
        (List.map
           (fun edge ->
             ( Termination.callable_id (Termination.call_edge_caller edge),
               Termination.callable_id (Termination.call_edge_callee edge),
               Termination.call_edge_recursive edge,
               Termination.call_edge_region edge ))
           (Termination.graph_edges mutual_graph)
        =
        [
          (left_id, right_id, false, Sst_validation.Body_region);
          (right_id, left_id, false, Sst_validation.Body_region);
        ]);
      check "raw mutual SCC descriptor changed"
        (match Termination.sccs mutual_analysis with
        | [ component ] ->
            Termination.scc_modes component = [ mode; mode ]
            && List.map Termination.callable_id
                 (Termination.scc_members component)
               = [ left_id; right_id ]
            && List.length (Termination.scc_edges component) = 2
            && Termination.scc_recursion_kind component
               = Some Termination.Mutual
        | _ -> false);
      expect_precheck "raw unsupported mutual mode"
        (function
          | Termination.Unsupported_mutual_scc ids ->
              ids = [ left_id; right_id ]
          | _ -> false)
        mutual_analysis;
      expect_legacy_cycle "raw unsupported mutual mode" mode mutual_program;
      (match Symbolic_executor.lower_program mutual_program with
      | Error { unsupported = Symbolic_executor.Malformed_sst detail; _ } ->
          check "raw mutual did not receive a termination-layer diagnostic"
            (String.starts_with
               ~prefix:"unsupported mutual recursion SCC:" detail)
      | Error error ->
          fail "raw mutual received the wrong pre-VIR error: %s"
            (Symbolic_executor.error_to_string error)
      | Ok _ -> fail "raw mutual reached VIR"))
    [ Sst.Spec; Sst.Proof ];
  print_endline
    "raw termination graph: Spec/Proof SCC order is preserved while proof self recursion requires authenticated retained authority"

let check_call_matrix () =
  let expected_form stage mode =
    match (stage, mode) with
    | Sst.Logical, Sst.Spec -> Some Sst.Specification_call
    | Sst.Proof_stage, Sst.Spec -> Some Sst.Specification_call
    | Sst.Proof_stage, Sst.Proof -> Some Sst.Proof_call
    | Sst.Runtime, Sst.Exec -> Some Sst.Exec_call
    | _ -> None
  in
  List.iteri
    (fun index stage ->
      List.iteri
        (fun offset mode ->
          let callee = mode_definition mode ((index * 10) + offset + 1) in
          let form =
            Option.value ~default:Sst.Unclassified_call
              (expected_form stage mode)
          in
          let caller = staged_caller stage form callee in
          let candidate =
            match expected_form stage mode with
            | Some _ -> program [ callee; caller ]
            | None -> program [ caller; callee ]
          in
          match (stage, mode, expected_form stage mode) with
          | _, _, Some _ ->
              check "enabled mode/stage call was rejected"
                (Result.is_ok (Sst_validation.validate candidate))
          | _, _, None ->
              expect_error "forbidden matrix edge" invalid_call candidate)
        [ Sst.Spec; Sst.Proof; Sst.Exec ])
    [ Sst.Logical; Sst.Proof_stage; Sst.Runtime ];
  let exec = mode_definition Sst.Exec 42 in
  expect_error "unclassified call" invalid_call
    (program
       [
         staged_caller Sst.Runtime Sst.Unclassified_call exec;
         exec;
       ])

let check_call_arguments () =
  let x = binding 70 "x" in
  let callee =
    raw_exec ~index:70 ~name:"labelled"
      ~parameters:[ Sst.Value_parameter
        Sst.{ label = Some "x"; pattern = bind x; optional_default = None } ]
      (variable x)
  in
  let make_call label argument =
    expression Sst.Int
      (Sst.Direct_call
         {
           call_form = Sst.Exec_call;
           callee = callee.function_id;
           arguments = [ Sst.Value_argument { label; value = argument } ];
           recursive = false;
           type_arguments = [];
         })
  in
  let mismatched_label =
    program
      [
        raw_exec ~index:71 ~name:"bad_label"
          (make_call None (int 1));
        callee;
      ]
  in
  expect_error "call argument label mismatch" invalid_call mismatched_label;
  expect_validation_before_lowering "call argument label mismatch"
    mismatched_label;
  let mismatched_type =
    program
      [
        raw_exec ~index:72 ~name:"bad_type"
          (make_call (Some "x") (bool true));
        callee;
      ]
  in
  expect_error "call argument type mismatch" invalid_call mismatched_type;
  expect_validation_before_lowering "call argument type mismatch"
    mismatched_type

let check_validated_descriptors () =
  let descriptor_type = record_type (type_id 30 "descriptor_type") in
  let spec = mode_definition Sst.Spec 31 in
  let formal = binding 32 "formal" in
  let requires =
    Sst.
      {
        clause_index = 0;
        predicate = { stage = Logical; expression = bool true };
        span = span 32;
      }
  in
  let callee =
    raw_exec ~index:32 ~name:"descriptor_callee"
      ~parameters:[ Sst.Value_parameter
        Sst.{ label = Some "value"; pattern = bind formal; optional_default = None } ]
      ~contracts:{ Sst.empty_contracts with requires = [ requires ] }
      (variable formal)
  in
  let make_call value =
    expression Sst.Int
      (Sst.Direct_call
         {
           call_form = Sst.Exec_call;
           callee = callee.function_id;
           arguments =
             [ Sst.Value_argument { label = Some "value"; value = int value } ];
           recursive = false;
           type_arguments = [];
         })
  in
  let discard =
    Sst.{ pattern_desc = Wildcard; typ = Int; span = span 33 }
  in
  let caller =
    raw_exec ~index:33 ~name:"descriptor_caller"
      (expression Sst.Int
         (Sst.Let ([ (discard, make_call 1) ], make_call 2)))
  in
  let recursive_id = function_id 34 "descriptor_recursive" in
  let recursive_parameter = binding 34 "n" in
  let recursive_call =
    expression Sst.Int
      (Sst.Direct_call
         {
           call_form = Sst.Exec_call;
           callee = recursive_id;
           arguments = [ Sst.Value_argument { label = None; value = variable recursive_parameter } ];
           recursive = true;
           type_arguments = [];
         })
  in
  let decreases =
    Sst.
      {
        clause_index = 0;
        predicate = { stage = Logical; expression = variable recursive_parameter };
        span = span 34;
      }
  in
  let recursive =
    raw_exec ~index:34 ~name:recursive_id.function_name ~recursive:true
      ~parameters:
        [ Sst.Value_parameter
          Sst.{ label = None; pattern = bind recursive_parameter; optional_default = None } ]
      ~contracts:{ Sst.empty_contracts with decreases = [ decreases ] }
      recursive_call
  in
  let candidate =
    program ~types:[ descriptor_type ] [ spec; callee; caller; recursive ]
  in
  let validated =
    match Sst_validation.validate candidate with
    | Ok validated -> validated
    | Error error -> fail "%s" (Sst_validation.error_to_string error)
  in
  let type_descriptors = Sst_validation.type_descriptors validated in
  check "validated type descriptor order changed"
    (List.map Sst_validation.type_id type_descriptors
    = [ descriptor_type.type_id ]);
  let type_descriptor =
    match Sst_validation.find_type validated descriptor_type.type_id with
    | Some descriptor -> descriptor
    | None -> fail "validated type descriptor lookup failed"
  in
  check "validated visibility descriptor changed"
    (Sst_validation.visibility_representation
       (Sst_validation.type_visibility type_descriptor)
    = Sst.Revealed);
  let callable_descriptors = Sst_validation.callable_descriptors validated in
  check "validated callable descriptor order changed"
    (List.map Sst_validation.callable_id callable_descriptors
    = List.map (fun definition -> definition.Sst.function_id)
        [ spec; callee; caller; recursive ]);
  let recursive_descriptor =
    match Sst_validation.find_callable validated recursive_id with
    | Some descriptor -> descriptor
    | None -> fail "validated recursive callable lookup failed"
  in
  check "validated callable mode changed"
    (Sst_validation.callable_mode recursive_descriptor = Sst.Exec);
  let callee_descriptor =
    match Sst_validation.find_callable validated callee.function_id with
    | Some descriptor -> descriptor
    | None -> fail "validated callee lookup failed"
  in
  let callee_contract =
    Sst_validation.callable_contract callee_descriptor
  in
  check "validated contract descriptor changed"
    (match Sst_validation.contract_requires callee_contract with
    | [ clause ] ->
        Sst_validation.contract_clause_index clause = 0
        && (Sst_validation.contract_clause_expression clause).typ = Sst.Bool
    | _ -> false);
  let decrease =
    match Sst_validation.callable_decrease recursive_descriptor with
    | Sst_validation.Direct_integer_decrease decrease -> decrease
    | _ -> fail "validated direct decrease was not classified"
  in
  check "validated decrease descriptor changed"
    ((Sst_validation.contract_clause_expression
        (Sst_validation.decrease_clause decrease))
       .typ
    = Sst.Int);
  let edges = Sst_validation.call_edge_descriptors validated in
  check "validated call-edge descriptor count changed" (List.length edges = 3);
  check "validated call forms changed"
    (List.for_all
       (fun edge -> Sst_validation.call_edge_form edge = Sst.Exec_call)
       edges);
  check "validated call-edge regions changed"
    (List.for_all
       (fun edge ->
         Sst_validation.call_edge_region edge = Sst_validation.Body_region)
       edges);
  check "validated call-edge descriptor order changed"
    (List.map
       (fun edge ->
         ( (Sst_validation.callable_id
              (Sst_validation.call_edge_caller edge))
             .function_name,
           (Sst_validation.callable_id
              (Sst_validation.call_edge_callee edge))
             .function_name,
           Sst_validation.call_edge_recursive edge ))
       edges
    =
    [
      ("descriptor_caller", "descriptor_callee", false);
      ("descriptor_caller", "descriptor_callee", false);
      ("descriptor_recursive", "descriptor_recursive", true);
    ]);
  let first_actual =
    match Sst_validation.call_edge_actuals (List.hd edges) with
    | [ actual ] -> actual
    | _ -> fail "validated formal/actual mapping changed"
  in
  check "validated formal mapping changed"
    ((Sst.require_value_parameter
       (Sst_validation.formal_parameter first_actual)).label = Some "value");
  check "validated actual label changed"
    (Sst_validation.actual_label first_actual = Some "value");
  check "validated actual expression changed"
    ((Sst_validation.actual_expression first_actual).typ = Sst.Int);
  check "validated feature descriptor order changed"
    (List.map
       (fun feature ->
         ( (Sst_validation.callable_id
              (Sst_validation.feature_callable feature))
             .function_name,
           Sst_validation.feature_requirement feature ))
       (Sst_validation.feature_descriptors validated)
    =
    [
      ("callee_31", Sst_validation.Specification_semantics);
      ("descriptor_recursive", Sst_validation.Direct_recursion);
    ]);
  let termination = Termination.analyze validated in
  check "termination graph did not preserve all validated callables and edges"
    (List.map Termination.callable_id
       (Termination.graph_callables (Termination.graph termination))
     = List.map Sst_validation.callable_id callable_descriptors
    && List.length (Termination.graph_edges (Termination.graph termination))
       = List.length edges);
  check "termination SCC mode order changed"
    (List.map Termination.scc_modes (Termination.sccs termination)
    = [ [ Sst.Spec ]; [ Sst.Exec ]; [ Sst.Exec ]; [ Sst.Exec ] ]);
  let termination_plan =
    match Termination.prepare termination with
    | Ok plan -> plan
    | Error error -> fail "%s" (Termination.error_to_string error)
  in
  check "termination admission did not isolate the direct recursive callable"
    (match Termination.pending_summaries termination_plan with
    | [ pending ] ->
        Termination.callable_id (Termination.pending_callable pending)
        = recursive_id
        && Termination.pending_domain pending = Termination.Integer_height
    | _ -> false);
  let invalid =
    {
      candidate with
      policy = Sst.Nonlinear_z3;
      functions = caller :: candidate.functions;
    }
  in
  expect_error "ordered semantic admission"
    (function Sst_validation.Unsupported_policy _ -> true | _ -> false)
    invalid;
  print_endline
    "validated descriptors: opaque type/callable/contracts/edges/visibility/features preserve declaration and call order";
  print_endline
    "semantic admission: handles issue only after the existing ordered validator succeeds"

let structural () =
  let valid = identity () in
  check "valid checked exec rejected"
    (Result.is_ok (Sst_validation.validate (program [ valid ])));
  expect_error "unsupported program policy"
    (function Sst_validation.Unsupported_policy _ -> true | _ -> false)
    (program ~policy:Sst.Nonlinear_z3 []);
  expect_error "declaration policy conflict"
    (function Sst_validation.Conflicting_identity _ -> true | _ -> false)
    (program [ { valid with policy = Sst.Default_linear_cvc5 } ]);
  expect_error "duplicate function id"
    (function Sst_validation.Duplicate_function_id _ -> true | _ -> false)
    (program [ valid; { valid with span = span 9 } ]);
  let t = record_type (type_id 0 "t") in
  expect_error "duplicate type id"
    (function Sst_validation.Duplicate_type_id _ -> true | _ -> false)
    (program ~types:[ t; { t with span = span 10 } ] []);
  let x = binding 0 "x" and y = binding 0 "y" in
  expect_error "duplicate binder id"
    (function Sst_validation.Duplicate_binding_id 0 -> true | _ -> false)
    (program
       [
         raw_exec
           ~parameters:
             [
               Sst.Value_parameter
                 Sst.{ label = None; pattern = bind x; optional_default = None };
               Sst.Value_parameter
                 Sst.{ label = None; pattern = bind y; optional_default = None };
             ]
           (variable x);
       ]);
  let missing = binding 99 "missing" in
  expect_error "unbound variable"
    (function Sst_validation.Unbound_binding _ -> true | _ -> false)
    (program [ raw_exec (variable missing) ]);
  let bad_requires =
    Sst.
      {
        clause_index = 1;
        predicate = { stage = Runtime; expression = int 1 };
        span = span 11;
      }
  in
  expect_error "malformed contract clause"
    (function Sst_validation.Invalid_clause _ -> true | _ -> false)
    (program
       [
         raw_exec
           ~contracts:
             { Sst.empty_contracts with requires = [ bad_requires ] }
           (int 0);
       ]);
  expect_error "mode/body mismatch" invalid_body
    (program [ { valid with mode = Proof } ]);
  let self =
    function_id 8 "self"
  in
  let false_recursive_call =
    expression Sst.Int
      (Sst.Direct_call
         {
           call_form = Exec_call;
           callee = self;
           arguments = [];
           recursive = false;
           type_arguments = [];
         })
  in
  expect_error "recursive call marker"
    (function Sst_validation.Invalid_recursive_marker _ -> true | _ -> false)
    (program [ raw_exec ~index:8 ~name:"self" ~recursive:true false_recursive_call ]);
  expect_error "unknown call edge"
    (function Sst_validation.Unknown_function_id _ -> true | _ -> false)
    (program
       [
         raw_exec
           (call ~form:Sst.Exec_call ~callee:(function_id 99 "missing")
              ~typ:Sst.Int ());
       ]);
  expect_error "bad unique return index"
    (function Sst_validation.Invalid_unique_return _ -> true | _ -> false)
    (program [ { valid with returns_unique_parameter = Some 2 } ]);
  expect_error "target link before trust support"
    (function Sst_validation.Invalid_target_link _ -> true | _ -> false)
    (program
       [
         {
           valid with
           body =
             Trusted_external_spec_target
               (Same_unit_target
                  {
                    wrapper = function_id 10 "wrapper";
                    target = function_id 9 "target";
                    target_span = span 11;
                    declaration_span = span 12;
                    witness_span = span 13;
                  });
         };
       ]);
  let unknown_type = type_id 77 "unknown" in
  expect_error "unknown semantic type"
    (function Sst_validation.Unknown_type_id _ -> true | _ -> false)
    (program
       [
         raw_exec ~result_type:(Sst.Aggregate unknown_type)
           (expression (Sst.Aggregate unknown_type) Sst.Unit_constant);
       ]);
  check_call_matrix ();
  check_call_arguments ();
  check_validated_descriptors ();
  check_raw_recursive_modes ();
  print_endline
    "semantic SST structural checks: ids, binders, clauses, bodies, recursion, returns, calls, policy, targets, types";
  print_endline
    "semantic SST mode matrix: logical/proof/runtime call forms are exhaustive and closed";
  let abstract_id = type_id 1 "abstract_t" in
  let incomplete =
    Sst.Incomplete_abstraction_evidence
      { evidence_id = "incomplete"; evidence_span = span 20 }
  in
  expect_error "incomplete abstract evidence"
    (function Sst_validation.Forged_abstract_evidence _ -> true | _ -> false)
    (program
       ~types:
         [
           record_type
             ~representation:(Sst.Abstract_with_evidence incomplete)
             abstract_id;
         ]
       []);
  let forged =
    Sst.Proposed_same_cmt_abstraction
      {
        evidence_id = "forged";
        abstract_signature_type = abstract_id;
        hidden_implementation_type = abstract_id;
        signature_module_identity =
          { source_name = "FORGED"; resolved_identifier = "FORGED/1" };
        implementation_module_identity =
          { source_name = "Forged"; resolved_identifier = "Forged/2" };
        abstract_signature_type_identity =
          { source_name = "t"; resolved_identifier = "t/3" };
        hidden_implementation_type_identity =
          { source_name = "t"; resolved_identifier = "t/4" };
        constraint_span = span 21;
        declaration_spans = [ span 22 ];
        public_surface = [];
        owned_tree_prerequisite = None;
        authentication_token = ref ();
      }
  in
  expect_error "forged complete abstract evidence"
    (function Sst_validation.Forged_abstract_evidence _ -> true | _ -> false)
    (program
       ~types:
         [
           record_type
             ~representation:(Sst.Abstract_with_evidence forged)
             abstract_id;
         ]
       []);
  let authenticated =
    match forged with
    | Sst.Proposed_same_cmt_abstraction evidence ->
        Sst.Authenticated_same_cmt_abstraction evidence
    | Sst.Incomplete_abstraction_evidence _
    | Sst.Authenticated_same_cmt_abstraction _ ->
        assert false
  in
  expect_error "pre-VERO-019 authenticated abstract evidence"
    (function Sst_validation.Forged_abstract_evidence _ -> true | _ -> false)
    (program
       ~types:
         [
           record_type
             ~representation:(Sst.Abstract_with_evidence authenticated)
             abstract_id;
         ]
       []);
  expect_error "conflicting abstract identity"
    (function Sst_validation.Duplicate_type_id _ -> true | _ -> false)
    (program
       ~types:
         [
           record_type abstract_id;
           record_type
             ~representation:(Sst.Abstract_with_evidence forged)
             abstract_id;
         ]
       []);
  print_endline
    "semantic type registry: revealed accepted; forged, incomplete, and conflicting abstraction evidence rejected";
  let validated =
    match Sst_validation.validate (program [ valid ]) with
    | Ok validated -> validated
    | Error error -> fail "%s" (Sst_validation.error_to_string error)
  in
  let callable = Sst_callable.build validated in
  check "callable identity missing"
    (Option.is_some (Sst_callable.find callable valid.function_id));
  check "callable accepted conflicting name"
    (Option.is_none
       (Sst_callable.find callable
          { valid.function_id with function_name = "counterfeit" }));
  print_endline
    "callable registry: constructed only after validation and preserves exact semantic identities"

let dumps () =
  let sst = program [ identity () ] in
  let vir =
    match Symbolic_executor.lower_program sst with
    | Ok vir -> vir
    | Error error -> fail "%s" (Symbolic_executor.error_to_string error)
  in
  print_endline "-- SST --";
  print_string (Sst.to_string sst);
  print_endline "-- VIR --";
  print_string (Vir.to_string vir)

let load_program filename =
  match Typedtree_lowering.lower_file filename with
  | Ok program -> program
  | Error diagnostic ->
      fail "%s: %s" diagnostic.Diagnostic.code diagnostic.message

let tuple_match_definition program =
  List.find
    (fun definition ->
      String.equal definition.Sst.function_id.function_name
        "spec_node_eq_alt")
    program.Sst.functions

let replace_tuple_match program transform =
  let target = tuple_match_definition program in
  let replacement =
    match target.Sst.body with
    | Sst.Recursive_spec_definition { body; visibility; provenance } ->
        {
          target with
          Sst.body =
            Sst.Recursive_spec_definition
              {
                body = { body with expression = transform body.expression };
                visibility;
                provenance;
              };
        }
    | _ -> fail "tuple matrix target is not a recursive specification"
  in
  {
    program with
    Sst.functions =
      List.map
        (fun definition ->
          if definition.Sst.function_id = target.function_id then replacement
          else definition)
        program.functions;
  }

let tuple_match_parts expression =
  match expression.Sst.expression_desc with
  | Sst.Match
      ( ({ expression_desc = Sst.Tuple_value values; _ } as scrutinee),
        cases ) ->
      (scrutinee, values, cases)
  | _ -> fail "tuple matrix target body changed shape"

let raw_tuple_boundary_controls malformed =
  let map_first_case transform cases =
    match cases with
    | first :: rest -> transform first :: rest
    | [] -> fail "tuple matrix target has no cases"
  in
  let map_last_case transform cases =
    match List.rev cases with
    | last :: rest -> List.rev (transform last :: rest)
    | [] -> fail "tuple matrix target has no cases"
  in
  malformed "zero-arity tuple" (fun expression ->
      let scrutinee, _values, cases = tuple_match_parts expression in
      let empty_type = Sst.Tuple [] in
      let empty_scrutinee =
        {
          scrutinee with
          Sst.typ = empty_type;
          expression_desc = Sst.Tuple_value [];
        }
      in
      let cases =
        map_first_case
          (fun (case : Sst.case) ->
            {
              case with
              Sst.case_pattern =
                {
                  case.case_pattern with
                  Sst.typ = empty_type;
                  pattern_desc = Sst.Tuple_pattern [];
                };
            })
          cases
      in
      {
        expression with
        Sst.expression_desc = Sst.Match (empty_scrutinee, cases);
      });
  malformed "whole tuple binding" (fun expression ->
      let scrutinee, _values, cases = tuple_match_parts expression in
      let whole = binding ~typ:scrutinee.typ 10_000 "whole_tuple" |> bind in
      let cases =
        map_first_case
          (fun (case : Sst.case) -> { case with Sst.case_pattern = whole })
          cases
      in
      { expression with expression_desc = Sst.Match (scrutinee, cases) });
  malformed "tuple let variable" (fun expression ->
      let scrutinee, _values, cases = tuple_match_parts expression in
      let whole = binding ~typ:scrutinee.typ 10_001 "tuple_value" in
      let carried_match =
        {
          expression with
          Sst.expression_desc = Sst.Match (variable whole, cases);
        }
      in
      {
        expression with
        Sst.expression_desc =
          Sst.Let ([ (bind whole, scrutinee) ], carried_match);
      });
  malformed "partial ordered fallback" (fun expression ->
      let scrutinee, _values, cases = tuple_match_parts expression in
      let cases =
        map_last_case
          (fun (case : Sst.case) ->
            let components =
              match case.Sst.case_pattern.pattern_desc with
              | Sst.Tuple_pattern (_ :: rest) -> rest
              | _ -> fail "tuple matrix fallback changed shape"
            in
            {
              case with
              Sst.case_pattern =
                {
                  case.case_pattern with
                  pattern_desc = Sst.Tuple_pattern components;
                };
            })
          cases
      in
      { expression with expression_desc = Sst.Match (scrutinee, cases) });
  malformed "exhaustive ordered fallback" (fun expression ->
      let scrutinee, _values, cases = tuple_match_parts expression in
      let malformed_case =
        match List.rev cases with
        | (fallback : Sst.case) :: _ ->
            let components =
              match fallback.Sst.case_pattern.pattern_desc with
              | Sst.Tuple_pattern (_ :: rest) -> rest
              | _ -> fail "tuple matrix fallback changed shape"
            in
            {
              fallback with
              Sst.case_pattern =
                {
                  fallback.case_pattern with
                  pattern_desc = Sst.Tuple_pattern components;
                };
            }
        | [] -> fail "tuple matrix target has no cases"
      in
      {
        expression with
        expression_desc = Sst.Match (scrutinee, cases @ [ malformed_case ]);
      })

let raw_tuple_matrix filename =
  let original = load_program filename in
  check "retained tuple source did not validate"
    (Result.is_ok (Sst_validation.validate original));
  let malformed label transform =
    Solver_backend.For_testing.reset_solver_creation_count ();
    let candidate = replace_tuple_match original transform in
    (match Symbolic_executor.lower_program candidate with
    | Error { unsupported = Symbolic_executor.Malformed_sst _; _ } -> ()
    | Error error ->
        fail "%s produced the wrong pre-VIR rejection: %s" label
          (Symbolic_executor.error_to_string error)
    | Ok _ -> fail "%s raw tuple substitution reached VIR" label);
    check (label ^ " created solver work")
      (Solver_backend.For_testing.solver_creation_count () = 0)
  in
  let map_first_case transform cases =
    match cases with
    | first :: rest -> transform first :: rest
    | [] -> fail "tuple matrix target has no cases"
  in
  malformed "tuple labels" (fun expression ->
      let scrutinee, values, cases = tuple_match_parts expression in
      let values =
        match values with
        | (_, first) :: rest -> (Some "forged", first) :: rest
        | [] -> assert false
      in
      {
        expression with
        Sst.expression_desc =
          Sst.Match
            ({ scrutinee with expression_desc = Sst.Tuple_value values }, cases);
      });
  malformed "tuple arity" (fun expression ->
      let scrutinee, values, cases = tuple_match_parts expression in
      let values = match values with _ :: rest -> rest | [] -> [] in
      {
        expression with
        Sst.expression_desc =
          Sst.Match
            ({ scrutinee with expression_desc = Sst.Tuple_value values }, cases);
      });
  malformed "tuple nesting" (fun body_expression ->
      let scrutinee, values, cases = tuple_match_parts body_expression in
      let values =
        match values with
        | (label, first) :: rest ->
            let nested =
              expression
                (Sst.Tuple [ (None, first.Sst.typ) ])
                (Sst.Tuple_value [ (None, first) ])
            in
            (label, nested) :: rest
        | [] -> assert false
      in
      {
        body_expression with
        Sst.expression_desc =
          Sst.Match
            ({ scrutinee with expression_desc = Sst.Tuple_value values }, cases);
      });
  malformed "tuple component type" (fun expression ->
      let scrutinee, values, cases = tuple_match_parts expression in
      let values =
        match values with
        | first :: (_label, second) :: rest ->
            first :: (None, { second with Sst.typ = Sst.Bool }) :: rest
        | _ -> assert false
      in
      {
        expression with
        Sst.expression_desc =
          Sst.Match
            ({ scrutinee with expression_desc = Sst.Tuple_value values }, cases);
      });
  malformed "raw tuple value/pattern" (fun expression ->
      let scrutinee, _values, cases = tuple_match_parts expression in
      let first_value =
        match scrutinee.Sst.expression_desc with
        | Sst.Tuple_value ((_, first) :: _) -> first
        | _ -> assert false
      in
      { expression with expression_desc = Sst.Match (first_value, cases) });
  malformed "whole tuple wildcard" (fun expression ->
      let scrutinee, _values, cases = tuple_match_parts expression in
      let wildcard =
        {
          Sst.pattern_desc = Sst.Wildcard;
          typ = scrutinee.typ;
          span = scrutinee.span;
        }
      in
      let cases =
        map_first_case
          (fun case -> { case with Sst.case_pattern = wildcard })
          cases
      in
      { expression with expression_desc = Sst.Match (scrutinee, cases) });
  malformed "guard with malformed shape" (fun expression ->
      let scrutinee, values, cases = tuple_match_parts expression in
      let values = match values with _ :: rest -> rest | [] -> [] in
      let cases =
        map_first_case
          (fun case -> { case with Sst.case_guard = Some (bool true) })
          cases
      in
      {
        expression with
        Sst.expression_desc =
          Sst.Match
            ({ scrutinee with expression_desc = Sst.Tuple_value values }, cases);
      });
  raw_tuple_boundary_controls malformed;
  print_endline
    "raw recursive tuple matrix: labels/arity/zero/nesting/types/value-pattern/whole-wildcard/whole-binding/let-variable/partial-fallback/exhaustive-fallback/guard rejected solver-delta=0"

let raw_application_matrix filename =
  let original = load_program filename in
  check "retained application source did not validate"
    (Result.is_ok (Sst_validation.validate original));
  let target = tuple_match_definition original in
  let measured_type =
    match target.parameters with
    | parameter :: _ ->
        (Sst.require_value_parameter parameter).pattern.typ
    | [] -> fail "application matrix target has no measured parameter"
  in
  let constructor =
    match measured_type with
    | Sst.Application (constructor, _) -> constructor
    | Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _ | Sst.Aggregate _
    | Sst.Parameter _ ->
        fail "application matrix target is not an exact application"
  in
  let malformed label candidate =
    Solver_backend.For_testing.reset_solver_creation_count ();
    (match Symbolic_executor.lower_program candidate with
    | Error { unsupported = Symbolic_executor.Malformed_sst _; _ } -> ()
    | Error error ->
        fail "%s produced the wrong pre-VIR rejection: %s" label
          (Symbolic_executor.error_to_string error)
    | Ok _ -> fail "%s raw application substitution reached VIR" label);
    check (label ^ " created solver work")
      (Solver_backend.For_testing.solver_creation_count () = 0)
  in
  let with_first_leaf_type typ =
    replace_tuple_match original (fun expression ->
        let scrutinee, values, cases = tuple_match_parts expression in
        let values =
          match values with
          | (label, first) :: rest ->
              (label, { first with Sst.typ = typ }) :: rest
          | [] -> fail "application matrix tuple has no leaves"
        in
        {
          expression with
          Sst.expression_desc =
            Sst.Match
              ({ scrutinee with expression_desc = Sst.Tuple_value values }, cases);
        })
  in
  malformed "unknown application descriptor"
    { original with parametric_adts = [] };
  let forged_constructor =
    { constructor with constructor_identity = "forged:node" }
  in
  malformed "forged application descriptor"
    (with_first_leaf_type (Sst.Application (forged_constructor, [ Sst.Int ])));
  let open_actual =
    Sst.Application
      ( constructor,
        [
          Sst.Parameter
            {
              owner = { owner_index = 77; owner_name = "Foreign.open" };
              ordinal = 0;
            };
        ] )
  in
  malformed "open application actual" (with_first_leaf_type open_actual);
  malformed "wrong application arity"
    (with_first_leaf_type (Sst.Application (constructor, [])));
  malformed "tuple application actual"
    (with_first_leaf_type
       (Sst.Application (constructor, [ Sst.Tuple [ (None, Sst.Int) ] ])));
  print_endline
    "raw application matrix: unknown/forged/open/wrong-arity/tuple-excluded rejected pre-VIR solver-delta=0"

let () =
  match Array.to_list Sys.argv with
  | [ _; "structural" ] -> structural ()
  | [ _; "dumps" ] -> dumps ()
  | [ _; "tuple-matrix"; filename ] -> raw_tuple_matrix filename
  | [ _; "application-matrix"; filename ] -> raw_application_matrix filename
  | _ ->
      fail
        "usage: semantic_sst_tool (structural|dumps|tuple-matrix|application-matrix FILE)"
