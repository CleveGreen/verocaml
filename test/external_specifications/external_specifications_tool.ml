open Typedtree

let fail format = Printf.ksprintf failwith format

let span line =
  let position column = Diagnostic.{ line; column } in
  Diagnostic.{ file = "external.ml"; start_pos = position 0; end_pos = position 1 }

let id function_index function_name = Sst.{ function_index; function_name }
let expression line typ expression_desc = Sst.{ expression_desc; typ; span = span line }

let binding line id name typ =
  Sst.{ id; name; typ; uniqueness = Definitely_aliased; span = span line }

let pattern line binding =
  Sst.{ pattern_desc = Bind binding; typ = binding.typ; span = span line }

let variable line (binding : Sst.binding) =
  expression line binding.Sst.typ
    (Sst.Variable { binding; use_uniqueness = Sst.Definitely_aliased })

let structural () =
  let target_id = id 0 "target" and wrapper_id = id 1 "wrapper" in
  let x = binding 2 0 "x" Sst.Int in
  let parameter = Sst.Value_parameter
    { Sst.label = None; pattern = pattern 2 x; optional_default = None } in
  let wrapper, target =
    Sst_normalize.authenticated_external_specification ~wrapper_id ~target_id
      ~parameters:[ parameter ] ~contracts:Sst.empty_contracts
      ~result_type:Sst.Int ~wrapper_span:(span 2) ~witness_span:(span 3)
      ~target_span:(span 1)
  in
  let caller_id = id 2 "caller" in
  let y = binding 4 0 "y" Sst.Int in
  let call callee =
    expression 4 Sst.Int
      (Sst.Direct_call
         {
           call_form = Sst.Exec_call;
           callee;
           arguments = [ Sst.Value_argument { label = None; value = variable 4 y } ];
           recursive = false;
           type_arguments = [];
         })
  in
  let caller callee =
    Sst_normalize.checked_exec_raw ~function_id:caller_id ~recursive:false
      ~parameters:[ Sst.Value_parameter
        { Sst.label = None; pattern = pattern 4 y; optional_default = None } ]
      ~contracts:Sst.empty_contracts ~body:(call callee) ~result_type:Sst.Int
      ~returns_unique_parameter:None ~span:(span 4)
  in
  let program caller =
    Sst.{ policy = Default_linear_z3; parametric_adts = []; types = []; functions = [ target; wrapper; caller ] }
  in
  (match Sst_validation.validate (program (caller target_id)) with
  | Ok _ -> print_endline "accepted: symmetric same-unit trusted linkage"
  | Error error -> fail "%s" (Sst_validation.error_to_string error));
  (match Sst_validation.validate (program (caller wrapper_id)) with
  | Error _ -> print_endline "rejected: external wrapper invocation"
  | Ok _ -> fail "external wrapper invocation accepted");
  print_endline "raw external-specification linkage boundary passed"

let load filename =
  match Typedtree_lowering.lower_file filename with
  | Ok sst -> sst
  | Error diagnostic -> fail "%s" diagnostic.Diagnostic.message

let lower filename =
  let sst = load filename in
  match Symbolic_executor.lower_program sst with
  | Ok vir -> (sst, vir)
  | Error error -> fail "%s" (Symbolic_executor.error_to_string error)

let inspect mode filename =
  let implementation =
    match Cmt_input.load filename with
    | Ok implementation -> implementation
    | Error diagnostic -> fail "%s" diagnostic.Diagnostic.message
  in
  let carriers = ref 0 and raw = ref 0 and nonghost = ref 0 in
  let default = Tast_iterator.default_iterator in
  let iterator =
    {
      default with
      value_binding =
        (fun self binding ->
          List.iter
            (fun attribute ->
              if
                String.equal attribute.Parsetree.attr_name.txt
                  "verocaml.external_specification"
              then incr raw)
            binding.vb_attributes;
          default.value_binding self binding);
      expr =
        (fun self expression ->
          (match expression.exp_desc with
          | Texp_apply
              ( { exp_desc = Texp_ident (path, _, _, _, _); _ },
                _, _, _, _ )
            when String.equal (Path.name path)
                   "Vero_ghost.external_specification" ->
              incr carriers;
              if not expression.exp_loc.loc_ghost then incr nonghost
          | _ -> ());
          default.expr self expression);
    }
  in
  iterator.structure iterator implementation.structure;
  match mode with
  | "erased" when !carriers = 0 && !raw = 0 ->
      print_endline "ordinary CMT erased external-specification wrappers"
  | "retained" when !carriers = 3 && !raw = 0 && !nonghost = 0 ->
      print_endline "retained CMT authenticated three external specifications"
  | _ ->
      fail "carrier mismatch carriers=%d raw=%d nonghost=%d" !carriers !raw
        !nonghost

let reject filename =
  match Typedtree_lowering.lower_file filename with
  | Error diagnostic -> Printf.printf "adapter rejected: %s\n" diagnostic.code
  | Ok sst -> (
      match Symbolic_executor.lower_program sst with
      | Error { unsupported = Malformed_sst _; _ } ->
          print_endline "semantic validation rejected before VIR"
      | Error error -> fail "%s" (Symbolic_executor.error_to_string error)
      | Ok _ -> fail "accepted")

let rec integer_symbols = function
  | Vir.Integer_constant _ -> []
  | Integer_symbol symbol -> [ symbol ]
  | Integer_add (left, right) | Integer_subtract (left, right) ->
      integer_symbols left @ integer_symbols right
  | Integer_negate term | Integer_absolute_value term -> integer_symbols term
  | Integer_multiply_constant (_, term) -> integer_symbols term
  | Integer_multiply (left, right) ->
      integer_symbols left @ integer_symbols right
  | Integer_conditional (condition, consequent, alternative) ->
      boolean_symbols condition
      @ integer_symbols consequent
      @ integer_symbols alternative
  | (Integer_symbolic_application _ as application) ->
      List.concat_map argument_symbols
        (Option.get
           (Vir.symbolic_application_arguments
              (Integer_application application)))
  | Aggregate_tag _ | Integer_selector _
  | Integer_recursive_spec_application _ | Integer_rank_project _ ->
      []

and boolean_symbols = function
  | Vir.Forall_term quantifier | Vir.Exists_term quantifier ->
      let bound_ids =
        List.map
          (fun binder -> binder.Vir.symbol_id)
          quantifier.boolean_quantifier_binders
      in
      boolean_symbols quantifier.boolean_quantifier_body
      |> List.filter (fun symbol ->
             not (List.mem symbol.Vir.symbol_id bound_ids))
  | Vir.Boolean_constant _ -> []
  | Boolean_symbol symbol -> [ symbol ]
  | Boolean_not term -> boolean_symbols term
  | Boolean_and (left, right) | Boolean_or (left, right)
  | Boolean_equal (left, right) | Boolean_not_equal (left, right) ->
      boolean_symbols left @ boolean_symbols right
  | Integer_compare (_, left, right) ->
      integer_symbols left @ integer_symbols right
  | Parametric_equal (left, right) ->
      parametric_symbols left @ parametric_symbols right
  | Boolean_selector _ | Aggregate_equal _
  | Boolean_invariant_application _ | Logical_adt_schema _
  | Boolean_recursive_spec_application _
  | Boolean_specification_application _
  | Callback_requires _ | Callback_ensures _ ->
      []
  | (Boolean_symbolic_application _ as application) ->
      List.concat_map argument_symbols
        (Option.get
           (Vir.symbolic_application_arguments
              (Boolean_application application)))

and parametric_symbols term =
  match term.Vir.parametric_desc with
  | Vir.Parametric_symbol _ | Vir.Parametric_selector _ -> []
  | Vir.Parametric_conditional (condition, consequent, alternative) ->
      boolean_symbols condition @ parametric_symbols consequent
      @ parametric_symbols alternative
  | Vir.Parametric_symbolic_application _ ->
      List.concat_map argument_symbols
        (Option.get
           (Vir.symbolic_application_arguments
              (Parametric_application term)))

and argument_symbols = function
  | Vir.Recursive_integer_argument term -> integer_symbols term
  | Vir.Recursive_boolean_argument term -> boolean_symbols term
  | Vir.Recursive_aggregate_argument term -> aggregate_symbols term
  | Vir.Recursive_parametric_argument term -> parametric_symbols term

and aggregate_symbols term =
  match term.Vir.aggregate_desc with
  | Vir.Aggregate_symbol symbol -> [ symbol ]
  | Vir.Aggregate_selector (_, aggregate) -> aggregate_symbols aggregate
  | Vir.Aggregate_imported_model_application { arguments; _ }
  | Vir.Aggregate_constructor { arguments; _ }
  | Vir.Aggregate_recursive_spec_application { arguments; _ } ->
      List.concat_map argument_symbols arguments
  | Vir.Aggregate_record { fields; _ } ->
      List.concat_map (fun (_, argument) -> argument_symbols argument) fields
  | Vir.Aggregate_conditional (condition, consequent, alternative) ->
      boolean_symbols condition @ aggregate_symbols consequent
      @ aggregate_symbols alternative
  | Vir.Aggregate_symbolic_application _ ->
      List.concat_map argument_symbols
        (Option.get
           (Vir.symbolic_application_arguments
              (Aggregate_application term)))

let havoc filename =
  let _, vir = lower filename in
  if List.length vir.Vir.functions <> 2 then
    fail "trusted targets or wrappers produced standalone VIR";
  let execution =
    match
      List.find_opt
        (fun execution ->
          String.equal execution.Vir.function_ref.function_name "use_havoc")
        vir.functions
    with
    | Some execution -> execution
    | None -> fail "missing use_havoc execution"
  in
  if List.length execution.trusted_summary_uses <> 2 then
    fail "expected two trusted contractless uses";
  if
    List.exists
      (function
        | Vir.Trusted_external_specification_use use ->
            use.requires_count <> 0 || use.ensures_count <> 0
        | Vir.Trusted_external_target_specification_use _ ->
            fail
              "imported external use reached same-unit external specification test"
        | Vir.Trusted_external_body_use _ ->
            fail "external body use reached external specification test")
      execution.trusted_summary_uses
  then fail "contractless use gained a clause";
  let result_symbols =
    execution.exits
    |> List.concat_map (fun exit -> exit.Vir.projection_symbols)
    |> List.filter (fun symbol -> String.equal symbol.Vir.source_name "opaque.result")
  in
  let result_ids =
    result_symbols |> List.map (fun symbol -> symbol.Vir.symbol_id)
    |> List.sort_uniq Int.compare
  in
  if List.length result_ids <> 2 then fail "havoc results were not fresh";
  let left = List.nth result_ids 0 and right = List.nth result_ids 1 in
  let relates_both term =
    let ids = boolean_symbols term |> List.map (fun symbol -> symbol.Vir.symbol_id) in
    List.mem left ids && List.mem right ids
  in
  if List.exists relates_both (List.concat_map (fun exit -> exit.Vir.assumptions) execution.exits)
  then fail "contractless calls gained a relationship";
  print_endline
    "havoc: two fresh unconstrained results, zero clauses, no relationship or target VIR"

let solve filename =
  let _, vir = lower filename in
  let config =
    match Solver_backend.config ~timeout_ms:5000 with
    | Ok config -> config
    | Error error -> fail "%s" (Solver_backend.error_to_string error)
  in
  List.iter
    (fun execution ->
      match Solver_backend.solve_in_order config execution.Vir.obligations with
      | Ok results
        when List.for_all
               (fun result ->
                 match result.Solver_backend.outcome with
                 | Verified -> true
                 | Counterexample _ | Inconclusive _ -> false)
               results ->
          Printf.printf "%s: verified-with-trusted-axioms (%d obligations, %d uses)\n"
            execution.function_ref.function_name
            (List.length execution.obligations)
            (List.length execution.trusted_summary_uses)
      | Ok _ -> fail "not verified"
      | Error error -> fail "%s" (Solver_backend.error_to_string error))
    vir.functions

let counterexample filename =
  let _, vir = lower filename in
  let config =
    match Solver_backend.config ~timeout_ms:5000 with
    | Ok config -> config
    | Error error -> fail "%s" (Solver_backend.error_to_string error)
  in
  let outcomes =
    List.concat_map
      (fun execution ->
        match Solver_backend.solve_in_order config execution.Vir.obligations with
        | Ok results -> results
        | Error error -> fail "%s" (Solver_backend.error_to_string error))
      vir.functions
  in
  if
    List.exists
      (fun result ->
        match result.Solver_backend.outcome with
        | Counterexample _ -> true
        | Verified | Inconclusive _ -> false)
      outcomes
  then print_endline "counterexample: trusted summary cannot prove the claim"
  else fail "expected a counterexample"

let () =
  match Array.to_list Sys.argv with
  | [ _; "structural" ] -> structural ()
  | [ _; "inspect"; mode; filename ] -> inspect mode filename
  | [ _; "sst"; filename ] ->
      let sst, _ = lower filename in
      print_string (Sst.to_string sst)
  | [ _; "vir"; filename ] ->
      let _, vir = lower filename in
      print_string (Vir.to_string vir)
  | [ _; "havoc"; filename ] -> havoc filename
  | [ _; "solve"; filename ] -> solve filename
  | [ _; "counterexample"; filename ] -> counterexample filename
  | [ _; "reject"; filename ] -> reject filename
  | _ -> fail "usage"
