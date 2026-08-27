open Typedtree

let fail format = Printf.ksprintf failwith format

let span line =
  let position column = Diagnostic.{ line; column } in
  Diagnostic.
    {
      file = "pure_specifications.ml";
      start_pos = position 0;
      end_pos = position 1;
    }

let expression line typ expression_desc =
  Sst.{ expression_desc; typ; span = span line }

let binding line id name typ : Sst.binding =
  Sst.
    {
      id;
      name;
      typ;
      uniqueness = Definitely_aliased;
      span = span line;
    }

let bind_pattern line binding =
  Sst.
    {
      pattern_desc = Bind binding;
      typ = binding.typ;
      span = span line;
    }

let variable line (binding : Sst.binding) =
  expression line binding.Sst.typ
    (Sst.Variable { binding; use_uniqueness = Sst.Definitely_aliased })

let int line value = expression line Sst.Int (Sst.Int_constant (Z.of_int value))
let bool line value = expression line Sst.Bool (Sst.Bool_constant value)

let call line typ call_form callee arguments =
  expression line typ
    (Sst.Direct_call { call_form; callee; type_arguments = []; arguments; recursive = false })

let empty_exec function_id parameter body =
  Sst_normalize.checked_exec_raw ~function_id ~recursive:false
    ~parameters:[ Sst.Value_parameter
      { Sst.label = None; pattern = bind_pattern 50 parameter; optional_default = None } ]
    ~contracts:Sst.empty_contracts ~body ~result_type:body.Sst.typ
    ~returns_unique_parameter:None ~span:(span 50)

let spec ?(contracts = Sst.empty_contracts) ?(recursive = false) function_id
    parameter body result_type =
  Sst.
    {
      function_id;
      type_binders = [];
      mode = Spec;
      recursive;
      parameters =
        [ Sst.Value_parameter
          { label = None; pattern = bind_pattern 10 parameter; optional_default = None } ];
      contracts;
      body = Spec_definition { stage = Logical; expression = body };
      policy = Default_linear_z3;
      result_type;
      returns_unique_parameter = None;
      span = span 10;
    }

let program functions =
  Sst.{ policy = Default_linear_z3; parametric_adts = []; types = []; functions }

let expect_rejected label program =
  match Sst_validation.validate program with
  | Error _ -> Printf.printf "rejected: %s\n" label
  | Ok _ -> fail "raw semantic case was accepted: %s" label

let structural () =
  let f_id = Sst.{ function_index = 0; function_name = "math_succ" } in
  let x = binding 10 0 "x" Sst.Int in
  let f_body =
    expression 11 Sst.Int
      (Sst.Checked_arithmetic (Sst.Add, [ variable 11 x; int 11 1 ]))
  in
  let f = spec f_id x f_body Sst.Int in
  let caller_id = Sst.{ function_index = 1; function_name = "caller" } in
  let y = binding 50 0 "y" Sst.Int in
  let f_y = call 51 Sst.Int Sst.Specification_call f_id
    [ Sst.Value_argument { label = None; value = variable 51 y } ] in
  let assertion =
    Sst.
      {
        clause_index = 0;
        predicate =
          {
            stage = Logical;
            expression =
              expression 51 Bool
                (Compare (Greater_than, f_y, variable 51 y));
          };
        span = span 51;
      }
  in
  let caller =
    let base = empty_exec caller_id y (variable 52 y) in
    { base with contracts = { Sst.empty_contracts with assertions = [ assertion ] } }
  in
  let accepted = program [ f; caller ] in
  (match Sst_validation.validate accepted with
  | Error error -> fail "valid spec program rejected: %s" (Sst_validation.error_to_string error)
  | Ok _ -> ());
  let vir =
    match Symbolic_executor.lower_program accepted with
    | Ok vir -> vir
    | Error error -> fail "valid spec lowering failed: %s" (Symbolic_executor.error_to_string error)
  in
  if List.length vir.Vir.functions <> 1 then
    fail "spec declaration produced standalone VIR";
  let printed = Vir.to_string vir in
  if String.contains printed '\000' then fail "impossible VIR byte";
  if String.starts_with ~prefix:"function math_succ" printed
     || String.contains printed '\t'
  then fail "unexpected spec execution in VIR";
  let execution = List.hd vir.functions in
  if List.length execution.obligations <> 1 then
    fail "expected exactly the caller assertion obligation";
  print_endline "accepted: logical expansion emits caller-only VIR and no spec overflow VC";

  let runtime_call =
    call 60 Sst.Int Sst.Specification_call f_id
      [ Sst.Value_argument { label = None; value = variable 60 y } ]
  in
  expect_rejected "runtime spec use"
    (program [ f; empty_exec caller_id y runtime_call ]);

  let exec_id = Sst.{ function_index = 0; function_name = "exec" } in
  let exec_x = binding 70 0 "x" Sst.Int in
  let exec = empty_exec exec_id exec_x (variable 70 exec_x) in
  let bad_spec_id = Sst.{ function_index = 1; function_name = "bad_spec" } in
  let bad_x = binding 71 0 "x" Sst.Int in
  let exec_call = call 71 Sst.Int Sst.Exec_call exec_id
    [ Sst.Value_argument { label = None; value = variable 71 bad_x } ] in
  expect_rejected "spec-to-exec call"
    (program [ exec; spec bad_spec_id bad_x exec_call Sst.Int ]);

  let a_id = Sst.{ function_index = 0; function_name = "a" } in
  let b_id = Sst.{ function_index = 1; function_name = "b" } in
  let a_x = binding 80 0 "x" Sst.Int in
  let b_x = binding 81 0 "x" Sst.Int in
  let a_call = call 80 Sst.Int Sst.Specification_call b_id
    [ Sst.Value_argument { label = None; value = variable 80 a_x } ] in
  let b_call = call 81 Sst.Int Sst.Specification_call a_id
    [ Sst.Value_argument { label = None; value = variable 81 b_x } ] in
  expect_rejected "cyclic spec graph"
    (program [ spec a_id a_x a_call Sst.Int; spec b_id b_x b_call Sst.Int ]);

  let no_arguments = call 90 Sst.Int Sst.Specification_call f_id [] in
  expect_rejected "bad spec-call arity"
    (program [ f; spec caller_id y no_arguments Sst.Int ]);
  let bool_argument = call 91 Sst.Int Sst.Specification_call f_id
    [ Sst.Value_argument { label = None; value = bool 91 true } ] in
  expect_rejected "bad spec-call type"
    (program [ f; spec caller_id y bool_argument Sst.Int ]);

  let requires =
    Sst.
      {
        clause_index = 0;
        predicate = { stage = Logical; expression = bool 92 true };
        span = span 92;
      }
  in
  expect_rejected "contract on spec"
    (program [ spec ~contracts:{ Sst.empty_contracts with requires = [ requires ] } f_id x f_body Sst.Int ]);
  expect_rejected "recursive spec"
    (program [ spec ~recursive:true f_id x f_body Sst.Int ]);
  let forbidden = expression 93 Sst.Int (Sst.Sequence (int 93 0, int 93 1)) in
  expect_rejected "forbidden spec body"
    (program [ spec f_id x forbidden Sst.Int ]);
  let tuple_body = expression 94 (Sst.Tuple [ (None, Sst.Int) ]) (Sst.Tuple_value [ (None, int 94 1) ]) in
  (match Sst_validation.validate (program [ spec f_id x tuple_body tuple_body.typ ]) with
  | Ok _ -> print_endline "accepted: immutable tuple spec result"
  | Error error ->
      fail "immutable tuple spec result rejected: %s"
        (Sst_validation.error_to_string error));
  print_endline "raw semantic rejection matrix passed"

let load_cmt filename =
  match Cmt_input.load filename with
  | Ok implementation -> implementation
  | Error diagnostic -> fail "%s" diagnostic.Diagnostic.message

let carrier_count structure =
  let count = ref 0 in
  let non_ghost = ref 0 in
  let raw_attributes = ref 0 in
  let default = Tast_iterator.default_iterator in
  let iterator =
    {
      default with
      value_binding =
        (fun self binding ->
          List.iter
            (fun attribute ->
              if String.equal attribute.Parsetree.attr_name.txt "verocaml.spec" then
                incr raw_attributes)
            binding.vb_attributes;
          default.value_binding self binding);
      expr =
        (fun self expression ->
          (match expression.exp_desc with
          | Texp_apply
              ( { exp_desc = Texp_ident (path, _, _, _, _); _ },
                _, _, _, _ )
            when String.equal (Path.name path) "Vero_ghost.spec_definition" ->
              incr count;
              if not expression.exp_loc.Location.loc_ghost then incr non_ghost
          | _ -> ());
          default.expr self expression);
    }
  in
  iterator.structure iterator structure;
  (!count, !non_ghost, !raw_attributes)

let inspect mode filename =
  let implementation = load_cmt filename in
  let count, non_ghost, raw_attributes = carrier_count implementation.structure in
  match mode with
  | "erased" when count = 0 && raw_attributes = 0 ->
      print_endline "ordinary CMT has no spec carrier or raw attribute"
  | "retained" when count = 4 && non_ghost = 0 && raw_attributes = 0 ->
      print_endline "retained CMT has four ghost-location authenticated carriers"
  | _ ->
      fail "unexpected %s carrier state: count=%d non-ghost=%d raw=%d" mode count
        non_ghost raw_attributes

let dump filename =
  match Typedtree_lowering.lower_file filename with
  | Error diagnostic -> fail "%s" diagnostic.Diagnostic.message
  | Ok sst ->
      print_string (Sst.to_string sst);
      (match Symbolic_executor.lower_program sst with
      | Error error -> fail "%s" (Symbolic_executor.error_to_string error)
      | Ok vir -> print_string (Vir.to_string vir))

let lower filename =
  match Typedtree_lowering.lower_file filename with
  | Error diagnostic -> fail "%s" diagnostic.Diagnostic.message
  | Ok sst -> (
      match Symbolic_executor.lower_program sst with
      | Error error -> fail "%s" (Symbolic_executor.error_to_string error)
      | Ok vir -> (sst, vir))

let reject filename =
  match Typedtree_lowering.lower_file filename with
  | Error diagnostic ->
      Printf.printf "adapter rejected: %s\n" diagnostic.Diagnostic.code
  | Ok sst -> (
      match Symbolic_executor.lower_program sst with
      | Error { Symbolic_executor.unsupported = Malformed_sst _; _ } ->
          print_endline "semantic validation rejected before VIR"
      | Error error -> fail "unexpected lowering error: %s" (Symbolic_executor.error_to_string error)
      | Ok _ -> fail "negative CMT was accepted")

let solve filename =
  let _, vir = lower filename in
  let config =
    match Solver_backend.config ~timeout_ms:5000 with
    | Ok config -> config
    | Error error -> fail "%s" (Solver_backend.error_to_string error)
  in
  List.iter
    (fun execution ->
      match
        Solver_backend.solve_in_order config execution.Vir.obligations
      with
      | Error error -> fail "%s" (Solver_backend.error_to_string error)
      | Ok results ->
          let verified =
            List.for_all
              (fun result ->
                match result.Solver_backend.outcome with
                | Solver_backend.Verified -> true
                | Solver_backend.Counterexample _
                | Solver_backend.Inconclusive _ ->
                    false)
              results
          in
          Printf.printf "%s: %s (%d obligations)\n"
            execution.function_ref.function_name
            (if verified then "verified" else "not verified")
            (List.length execution.obligations))
    vir.functions

let () =
  match Array.to_list Sys.argv with
  | [ _; "structural" ] -> structural ()
  | [ _; "inspect"; mode; filename ] -> inspect mode filename
  | [ _; "dump"; filename ] -> dump filename
  | [ _; "sst"; filename ] ->
      let sst, _ = lower filename in
      print_string (Sst.to_string sst)
  | [ _; "vir"; filename ] ->
      let _, vir = lower filename in
      print_string (Vir.to_string vir)
  | [ _; "solve"; filename ] -> solve filename
  | [ _; "reject"; filename ] -> reject filename
  | _ ->
      fail
        "usage: pure_specifications_tool \
         structural|inspect MODE CMT|dump|sst|vir CMT|reject CMT"
