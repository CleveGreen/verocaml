let fail format = Printf.ksprintf (fun message -> prerr_endline message; exit 3) format

let span_to_string span =
  Printf.sprintf "%s:%d:%d-%d:%d" (Filename.basename span.Diagnostic.file)
    span.start_pos.line span.start_pos.column span.end_pos.line span.end_pos.column

let load filename =
  match Cmt_input.load filename with
  | Ok implementation -> implementation
  | Error diagnostic -> fail "%s @ %s" diagnostic.code (span_to_string diagnostic.span)

let lower filename =
  match Typedtree_lowering.lower_file filename with
  | Ok program -> program
  | Error diagnostic -> fail "%s @ %s" diagnostic.code (span_to_string diagnostic.span)

let lower_vir filename =
  match Symbolic_executor.lower_program (lower filename) with
  | Ok program -> program
  | Error error -> fail "%s" (Symbolic_executor.error_to_string error)

let classify filename =
  match Typedtree_lowering.lower_file filename with
  | Ok _ -> print_endline "accepted"
  | Error diagnostic ->
      Printf.printf "%s @ %s\n" diagnostic.Diagnostic.code
        (span_to_string diagnostic.span)

let kind = function
  | Vir.Arithmetic_safety { violated_bound = Lower_bound; _ } -> "arithmetic-lower"
  | Vir.Arithmetic_safety { violated_bound = Upper_bound; _ } -> "arithmetic-upper"
  | Vir.Postcondition _ -> "postcondition"
  | Vir.Assertion _ -> "assertion"
  | Vir.Local_assertion _ -> "local-assertion"
  | Vir.Call_precondition _ -> "call-precondition"
  | Vir.Callback_precondition _ -> "callback-precondition"
  | Vir.Invariant_validity _ -> "invariant-validity"
  | Vir.Entry_measure_nonnegative _ -> "entry-measure"
  | Vir.Recursive_call_measure_nonnegative _ -> "call-measure"
  | Vir.Recursive_call_strict_descent _ -> "strict-descent"

let solve filename =
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
          let failed =
            List.find_opt
              (fun result ->
                match result.Solver_backend.outcome with
                | Solver_backend.Verified -> false
                | Counterexample _ | Inconclusive _ -> true)
              results
          in
          match failed with
          | None ->
              Printf.printf "%s: verified (%d obligations)\n"
                execution.function_ref.function_name
                (List.length execution.obligations)
          | Some result ->
              let outcome =
                match result.outcome with
                | Solver_backend.Counterexample _ -> "counterexample"
                | Inconclusive _ -> "inconclusive"
                | Verified -> assert false
              in
              Printf.printf "%s: %s %s @ %s\n"
                execution.function_ref.function_name outcome
                (kind result.obligation.kind)
                (span_to_string result.obligation.span))
    (lower_vir filename).Vir.functions

let uniqueness_of_pattern mode =
  Mode.Uniqueness.zap_to_floor
    (Mode.Value.proj_monadic Mode.Axis.Uniqueness mode)

let uniqueness_of_use ((mode, _) : Typedtree.unique_use) =
  Mode.Uniqueness.zap_to_ceil mode

let is_unique = function
  | Mode.Uniqueness.Const.Unique -> true
  | Aliased -> false

let uniqueness_name mode =
  if is_unique mode then "unique" else "aliased"

let linearity_name = function
  | Mode.Linearity.Const.Once -> "once"
  | Many -> "many"

let ghost_application name expression =
  match expression.Typedtree.exp_desc with
  | Texp_apply
      ({ exp_desc = Texp_ident (path, _, _, _, _); _ }, _, _, _, _)
    when String.equal (Path.name path) ("Vero_ghost." ^ name) ->
      true
  | _ -> false

let contains_sidecar expression =
  let found = ref false in
  let default = Tast_iterator.default_iterator in
  let iterator =
    {
      default with
      expr =
        (fun self expression ->
          if ghost_application "sidecar" expression then found := true
          else default.expr self expression);
    }
  in
  iterator.expr iterator expression;
  !found

let proof_sidecar expression =
  match expression.Typedtree.exp_desc with
  | Texp_ifthenelse
      ( {
          exp_desc =
            Texp_construct (_, description, [], None);
          _;
        },
        proof,
        Some _ )
    when String.equal description.Types.cstr_name "false" ->
      contains_sidecar proof
  | _ -> false

let executable_evidence filename =
  let implementation = load filename in
  let evidence = ref [] in
  let add format =
    Printf.ksprintf (fun line -> evidence := line :: !evidence) format
  in
  let use_evidence ((uniqueness, linearity) : Typedtree.unique_use) =
    Printf.sprintf "demand=%s actual=%s"
      (uniqueness_name (Mode.Uniqueness.zap_to_ceil uniqueness))
      (linearity_name (Mode.Linearity.zap_to_floor linearity))
  in
  let parameter_patterns function_name parameters =
    let default = Tast_iterator.default_iterator in
    let pat : type k.
        Tast_iterator.iterator -> k Typedtree.general_pattern -> unit =
     fun self pattern ->
      (match pattern.pat_desc with
      | Tpat_var (_, name, _, _, mode) ->
          add "parameter function=%s name=%s uniqueness=%s" function_name
            name.txt
            (uniqueness_name (uniqueness_of_pattern mode))
      | _ -> ());
      default.pat self pattern
    in
    let iterator = { default with pat } in
    List.iter
      (fun parameter ->
        match parameter.Typedtree.fp_kind with
        | Tparam_pat pattern -> iterator.pat iterator pattern
        | Tparam_optional_default (pattern, _, _) ->
            iterator.pat iterator pattern)
      parameters
  in
  let rec terminal function_name expression =
    match expression.Typedtree.exp_desc with
    | Texp_sequence (_, _, tail)
    | Texp_let (_, _, tail)
    | Texp_letmutable (_, tail) ->
        terminal function_name tail
    | Texp_ifthenelse (_, consequent, Some alternate) ->
        terminal function_name consequent;
        terminal function_name alternate
    | Texp_match (_, _, cases, _) ->
        List.iter
          (fun case -> terminal function_name case.Typedtree.c_rhs)
          cases
    | Texp_ident (Path.Pident ident, _, _, _, use) ->
        add "terminal function=%s name=%s %s" function_name
          (Ident.name ident) (use_evidence use)
    | _ -> ()
  in
  let expressions function_name body =
    let default = Tast_iterator.default_iterator in
    let expr self expression =
      if proof_sidecar expression then ()
      else if
        List.exists
          (fun name -> ghost_application name expression)
          [ "marker"; "sidecar"; "requires"; "ensures"; "decreases"; "assert_"; "old" ]
      then ()
      else (
        (match expression.Typedtree.exp_desc with
        | Texp_ident (Path.Pident ident, _, _, _, use) ->
            add "identifier function=%s name=%s %s" function_name
              (Ident.name ident) (use_evidence use)
        | Texp_field (_, _, longident, _, _, barrier) ->
            add "field-read function=%s field=%s barrier=%s" function_name
              (Longident.last longident.txt)
              (uniqueness_name (Typedtree.Unique_barrier.resolve barrier))
        | Texp_setfield
            ( { exp_desc = Texp_ident (path, _, _, _, use); _ },
              _,
              longident,
              description,
              _ ) ->
            add
              "field-write function=%s field=%s receiver=%s %s barrier=none mutable=%b public=%b"
              function_name (Longident.last longident.txt) (Path.name path)
              (use_evidence use)
              (Types.is_mutable description.Types.lbl_mut)
              (description.lbl_private = Asttypes.Public)
        | _ -> ());
        default.expr self expression)
    in
    let iterator = { default with expr } in
    iterator.expr iterator body
  in
  let function_binding value_binding =
    match
      ( value_binding.Typedtree.vb_pat.pat_desc,
        value_binding.Typedtree.vb_expr.exp_desc )
    with
    | ( Tpat_var (_, name, _, _, _),
        Texp_function { params; body = Tfunction_body body; _ } ) ->
        parameter_patterns name.txt params;
        expressions name.txt body;
        terminal name.txt body
    | _ -> ()
  in
  List.iter
    (fun item ->
      match item.Typedtree.str_desc with
      | Tstr_value (_, bindings) -> List.iter function_binding bindings
      | _ -> ())
    implementation.structure.str_items;
  List.sort String.compare !evidence |> List.iter print_endline

let probe filename =
  let implementation = load filename in
  let patterns = ref [] in
  let setfields = ref [] in
  let unique_terminal_uses = ref 0 in
  let field_barriers = ref [] in
  let local_mutables = ref 0 in
  let mutable_writes = ref 0 in
  let ref_paths = ref [] in
  let ghost_paths = ref [] in
  let default = Tast_iterator.default_iterator in
  let pat : type k. Tast_iterator.iterator -> k Typedtree.general_pattern -> unit =
   fun self pattern ->
    (match pattern.pat_desc with
    | Typedtree.Tpat_var (_, name, _, _, mode) ->
        patterns :=
          (name.txt, is_unique (uniqueness_of_pattern mode)) :: !patterns
    | _ -> ());
    default.pat self pattern
  in
  let expr self expression =
    (match expression.Typedtree.exp_desc with
    | Texp_setfield
        ( { exp_desc = Texp_ident (path, _, _, _, use); _ },
          _, longident, description, _ ) ->
        setfields :=
          ( Longident.last longident.txt,
            Path.name path,
            is_unique (uniqueness_of_use use),
            Types.is_mutable description.Types.lbl_mut,
            description.lbl_private = Asttypes.Public )
          :: !setfields
    | Texp_ident (_, _, _, _, use) when is_unique (uniqueness_of_use use) ->
        incr unique_terminal_uses
    | Texp_field (_, _, longident, _, _, barrier) ->
        field_barriers :=
          ( Longident.last longident.txt,
            is_unique (Typedtree.Unique_barrier.resolve barrier) )
          :: !field_barriers
    | Texp_letmutable _ -> incr local_mutables
    | Texp_setmutvar _ -> incr mutable_writes
    | Texp_apply ({ exp_desc = Texp_ident (path, _, _, _, _); _ }, _, _, _, _)
      when List.exists
             (fun suffix -> Filename.check_suffix (Path.name path) suffix)
             [ "Stdlib.ref"; "Stdlib.!"; "Stdlib.:=" ] ->
        ref_paths := Path.name path :: !ref_paths
    | Texp_apply ({ exp_desc = Texp_ident (path, _, _, _, _); _ }, _, _, _, _)
      when String.starts_with ~prefix:"Vero_ghost." (Path.name path) ->
        let shape =
          match Path.flatten path with
          | `Ok (root, components) ->
              Printf.sprintf "%s root=%s global=%b parts=%s" (Path.name path)
                (Ident.name root) (Ident.is_global_or_predef root)
                (String.concat "." components)
          | `Contains_apply -> Path.name path ^ " contains-apply"
        in
        ghost_paths := shape :: !ghost_paths
    | _ -> ());
    default.expr self expression
  in
  let iterator = { default with pat; expr } in
  iterator.structure iterator implementation.structure;
  let box_unique =
    List.exists (fun (name, unique) -> String.equal name "box" && unique) !patterns
  in
  let setfield =
    List.find_opt (fun (name, _, _, _, _) -> String.equal name "value") !setfields
  in
  (match setfield with
  | Some (name, path, use_unique, mutable_, public_) ->
      Printf.printf
        "Texp_setfield field=%s receiver=%s pattern=%s use=%s mutable=%b public=%b\n"
        name path (if box_unique then "unique" else "aliased")
        (if use_unique then "unique" else "aliased") mutable_ public_
  | None -> print_endline "Texp_setfield absent");
  Printf.printf "Texp_field barriers=%s (write node carries no barrier)\n"
    (if List.exists (fun (_, unique) -> unique) !field_barriers then "unique-observed"
     else "aliased-observed");
  Printf.printf "terminal/root unique identifier uses=%d\n" !unique_terminal_uses;
  Printf.printf "Texp_letmutable=%d Texp_setmutvar=%d\n" !local_mutables
    !mutable_writes;
  Printf.printf "pattern modes=%s\n"
    (List.rev !patterns
    |> List.map (fun (name, unique) ->
           Printf.sprintf "%s:%s" name (if unique then "unique" else "aliased"))
    |> String.concat ",");
  if !ref_paths <> [] then
    Printf.printf "resolved ref paths=%s\n"
      (List.sort_uniq String.compare !ref_paths |> String.concat ",");
  if !ghost_paths <> [] then
    Printf.printf "resolved ghost paths=%s\n"
      (List.sort_uniq String.compare !ghost_paths |> String.concat ",");
  Array.iter
    (fun (import : Cmt_input.import) ->
      if String.equal import.Cmt_input.unit_name "Vero_ghost" then
        Printf.printf "ghost import crc=%s\n"
          (Option.value ~default:"none" import.crc))
    implementation.imports

let () =
  match Array.to_list Sys.argv with
  | [ _; "classify"; filename ] -> classify filename
  | [ _; "dump-sst"; filename ] -> print_string (Sst.to_string (lower filename))
  | [ _; "dump-vir"; filename ] -> print_string (Vir.to_string (lower_vir filename))
  | [ _; "solve"; filename ] -> solve filename
  | [ _; "probe"; filename ] -> probe filename
  | [ _; "executable-evidence"; filename ] -> executable_evidence filename
  | _ ->
      fail
        "usage: unique_mutation_tool \
         (classify|dump-sst|dump-vir|solve|probe|executable-evidence) FILE.cmt"
