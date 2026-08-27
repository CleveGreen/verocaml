let color_enabled () =
  match Sys.getenv_opt "OCAML_COLOR" with
  | Some value when String.equal (String.lowercase_ascii value) "never" -> false
  | _ -> true

let styled code text =
  if color_enabled () then Printf.sprintf "\027[%sm%s\027[0m" code text
  else text

let error_label () = styled "1;31" "error"

let usage =
  "usage: verocaml verify FILE.ml|FILE.cmt [--solver z3] [--timeout-ms N] \
   [--rlimit N] [--threads N] [--dependency FILE.cmt]... [--dump-sst FILE] \
   [--dump-vir FILE]\n\
  \       verocaml verify-project --root FILE.cmt FILE.cmi [--root FILE.cmt \
   FILE.cmi]... [--dependency FILE.cmt FILE.cmi]... [--solver z3] \
   [--timeout-ms N] [--rlimit N] [--threads N]"

type message =
  | Positive_integer of string
  | Option_once of string
  | Option_requires of string * string
  | Unsupported_solver of string
  | Unknown_option of string
  | Unexpected_argument of string
  | Dump_write_failure of { label : string; filename : string; detail : string }
  | Source_cmt_identity_count of int
  | Unix_failure of {
      function_name : string;
      argument : string;
      detail : string;
    }
  | Path_inspection_failure of {
      label : string;
      path : string;
      detail : string;
    }
  | Same_path of {
      left_label : string;
      left_path : string;
      right_label : string;
      right_path : string;
    }
  | Compiler_unavailable of string
  | Ppx_unavailable
  | Ghost_unavailable
  | Compiler_invocation_failure of string
  | Compiler_missing_cmt
  | Temporary_storage_failure of string
  | Consumer_cmt_rejected of string * Diagnostic.t
  | Dependency_cmt_rejected of string * Diagnostic.t
  | Project_inventory_required
  | Project_entry_requires of string

let message = function
  | Positive_integer flag ->
      Printf.sprintf "%s requires a positive integer" flag
  | Option_once flag -> Printf.sprintf "%s may be specified only once" flag
  | Option_requires (flag, operand) ->
      Printf.sprintf "%s requires %s" flag operand
  | Unsupported_solver value ->
      Printf.sprintf "unsupported solver %S; expected z3" value
  | Unknown_option flag -> Printf.sprintf "unknown or incomplete option %s" flag
  | Unexpected_argument argument ->
      Printf.sprintf "unexpected argument %S" argument
  | Dump_write_failure { label; filename; detail } ->
      Printf.sprintf "could not write %s dump %S: %s" label filename detail
  | Source_cmt_identity_count count ->
      Printf.sprintf
        "expected one rendered private source CMT identity, found %d" count
  | Unix_failure { function_name; argument; detail } ->
      Printf.sprintf "%s(%s): %s" function_name argument detail
  | Path_inspection_failure { label; path; detail } ->
      Printf.sprintf "could not inspect %s path %S: %s" label path detail
  | Same_path { left_label; left_path; right_label; right_path } ->
      Printf.sprintf "%s path %S and %s path %S identify the same file"
        left_label left_path right_label right_path
  | Compiler_unavailable path ->
      Printf.sprintf
        "the pinned OxCaml compiler is unavailable at %S; reinstall or run \
         VeroCaml through its Nix package"
        path
  | Ppx_unavailable ->
      "the VeroCaml PPX executable is unavailable; reinstall or run VeroCaml \
       through its Nix package"
  | Ghost_unavailable ->
      "the VeroCaml ghost interface is unavailable; reinstall or run VeroCaml \
       through its Nix package"
  | Compiler_invocation_failure detail ->
      Printf.sprintf "could not invoke the pinned compiler: %s" detail
  | Compiler_missing_cmt ->
      "the pinned compiler succeeded without producing a CMT"
  | Temporary_storage_failure detail ->
      Printf.sprintf "could not create private source-compilation storage: %s"
        detail
  | Consumer_cmt_rejected (filename, diagnostic) ->
      Printf.sprintf "consumer CMT %S rejected [%s]: %s" filename
        diagnostic.code diagnostic.message
  | Dependency_cmt_rejected (filename, diagnostic) ->
      Printf.sprintf "dependency CMT %S rejected [%s]: %s" filename
        diagnostic.code diagnostic.message
  | Project_inventory_required ->
      "verify-project requires at least one --root FILE.cmt FILE.cmi entry"
  | Project_entry_requires flag ->
      Printf.sprintf "%s requires FILE.cmt FILE.cmi" flag

let span (value : Diagnostic.span) =
  Printf.sprintf "%s:%d:%d-%d:%d" value.file value.start_pos.line
    value.start_pos.column value.end_pos.line value.end_pos.column

let cli_error message =
  Printf.sprintf "verocaml: %s[VERO_CLI] %s" (error_label ()) message

let internal_error message =
  Printf.sprintf "verocaml: %s[VERO_INTERNAL] %s" (error_label ()) message

let frontend_error (diagnostic : Diagnostic.t) =
  Printf.sprintf "verocaml: %s[%s] %s @ %s" (error_label ()) diagnostic.code
    diagnostic.message (span diagnostic.span)

let dependency_error ~unit_name ~message =
  let message =
    Option.fold ~none:message
      ~some:(fun unit_name -> Printf.sprintf "unit %s: %s" unit_name message)
      unit_name
  in
  Printf.sprintf "verocaml: %s[VERO_DEPENDENCY] %s" (error_label ()) message

let process_status = function
  | Unix.WEXITED code -> Printf.sprintf "exit %d" code
  | Unix.WSIGNALED signal -> Printf.sprintf "signal %d" signal
  | Unix.WSTOPPED signal -> Printf.sprintf "stopped by signal %d" signal

let source_compile_error ~source ~process_status:status =
  Printf.sprintf "verocaml: %s[VERO_SOURCE_COMPILE] source=%s compiler=%s"
    (error_label ()) source (process_status status)

let function_ref value =
  Printf.sprintf "%s#%d"
    (Verifier_service.function_name value)
    (Verifier_service.function_index value)

let operation = function
  | Verifier_service.Add -> "add"
  | Subtract -> "subtract"
  | Negate -> "negate"
  | Multiply_constant value -> Printf.sprintf "multiply-constant(%s)" value
  | Successor -> "successor"
  | Predecessor -> "predecessor"
  | Absolute_value -> "absolute-value"

let diagnostic_kind = function
  | Verifier_service.Arithmetic_safety { violated_bound = Lower; _ } ->
      "arithmetic-safety-lower"
  | Arithmetic_safety { violated_bound = Upper; _ } -> "arithmetic-safety-upper"
  | Assertion { assertion_ordinal } ->
      Printf.sprintf "assertion[%d]" assertion_ordinal
  | Local_assertion { local_assertion_ordinal } ->
      Printf.sprintf "local-assertion[%d]" local_assertion_ordinal
  | Postcondition { postcondition_ordinal } ->
      Printf.sprintf "postcondition[%d]" postcondition_ordinal
  | Call_precondition { callee; precondition_ordinal } ->
      Printf.sprintf "call-precondition[%s,%d]" (function_ref callee)
        precondition_ordinal
  | Callback_precondition { callback_name; callback_id } ->
      Printf.sprintf "callback-precondition[%s#%d]" callback_name callback_id
  | Invariant_validity { invariant_id; boundary } ->
      let boundary =
        match boundary with
        | Constructor_establishment -> "constructor-establishment"
        | Transition_preservation -> "transition-preservation"
        | Call_argument -> "call-argument"
        | Call_result -> "call-result"
        | Function_return -> "function-return"
        | Shared_invariant_close -> "cell-close"
        | Terminal_observation -> "terminal-observation"
      in
      Printf.sprintf "invariant-%s[%s]" boundary invariant_id
  | Entry_measure_nonnegative -> "entry-measure-nonnegative"
  | Recursive_call_measure_nonnegative { callee } ->
      Printf.sprintf "recursive-call-measure-nonnegative[%s]"
        (function_ref callee)
  | Recursive_call_strict_descent { callee } ->
      Printf.sprintf "recursive-call-strict-descent[%s]" (function_ref callee)

let model_value = function
  | Verifier_service.Integer value -> value
  | Boolean value -> string_of_bool value
  | Aggregate_identity value -> "aggregate#" ^ value

let inconclusive_reason = function
  | Verifier_service.Resource_exhausted -> "reason=resource-exhausted"
  | Timed_out -> "reason=timeout"
  | Backend_unknown detail ->
      Printf.sprintf "reason=backend-unknown detail=%s" (String.escaped detail)

let verification_stderr_lines result =
  Verifier_service.diagnostics result
  |> List.concat_map (fun diagnostic ->
      let function_ =
        Verifier_service.diagnostic_function diagnostic |> function_ref
      in
      let kind =
        Verifier_service.diagnostic_kind diagnostic |> diagnostic_kind
      in
      let location = Verifier_service.diagnostic_span diagnostic |> span in
      match Verifier_service.diagnostic_outcome diagnostic with
      | Diagnostic_inconclusive
          { configured_timeout_ms; configured_rlimit; reason } ->
          [
            Printf.sprintf
              "verocaml: inconclusive function=%s vc=%s span=%s result=unknown \
               %s rlimit=%d timeout-ms=%d"
              function_ kind location
              (inconclusive_reason reason)
              configured_rlimit configured_timeout_ms;
          ]
      | Diagnostic_counterexample ->
          let detail =
            match Verifier_service.diagnostic_kind diagnostic with
            | Arithmetic_safety
                {
                  operation = value;
                  mathematical_result;
                  violated_bound = Lower;
                } ->
                [
                  Printf.sprintf
                    "  overflow: operation=%s mathematical-result=%s \
                     violated-bound=lower minimum=%s"
                    (operation value) mathematical_result
                    (Z.to_string Int_bounds.minimum);
                ]
            | Arithmetic_safety
                {
                  operation = value;
                  mathematical_result;
                  violated_bound = Upper;
                } ->
                [
                  Printf.sprintf
                    "  overflow: operation=%s mathematical-result=%s \
                     violated-bound=upper maximum=%s"
                    (operation value) mathematical_result
                    (Z.to_string Int_bounds.maximum);
                ]
            | _ -> []
          in
          let bindings =
            Verifier_service.diagnostic_model_bindings diagnostic
          in
          let model =
            match bindings with
            | [] -> [ "  model: (no projected bindings)" ]
            | _ ->
                List.map
                  (fun binding ->
                    Printf.sprintf "  model: %s#%d=%s"
                      (Verifier_service.model_binding_source_name binding)
                      (Verifier_service.model_binding_symbol_id binding)
                      (Option.fold ~none:"<unavailable>" ~some:model_value
                         (Verifier_service.model_binding_value binding)))
                  bindings
          in
          [
            Printf.sprintf
              "verocaml: counterexample function=%s vc=%s span=%s \
               result=counterexample"
              function_ kind location;
          ]
          @ detail @ model)

let provenance_line = Verocaml_bin_trusted_external_private.provenance_line
let trusted_external_line = Verocaml_bin_trusted_external_private.line

let verification_stdout_lines ~display_file result =
  let dependencies =
    Verifier_service.provenance result |> List.map provenance_line
  in
  match Verifier_service.status result with
  | Counterexample | Inconclusive | Incomplete_source -> dependencies
  | Verified ->
      let observations =
        Verifier_service.trusted_external_observations result
      in
      let uses, declarations =
        List.partition
          (fun observation ->
            match Verifier_service.trusted_external_view observation with
            | Trusted_external_body_declaration _ -> false
            | Trusted_external_specification_use _
            | Trusted_external_target_specification_use _
            | Trusted_external_body_use _ ->
                true)
          observations
      in
      let details =
        List.map
          (fun observation ->
            Verifier_service.trusted_external_view observation
            |> trusted_external_line)
          observations
      in
      let summary =
        if uses = [] && declarations = [] then
          Printf.sprintf "verocaml: %s file=%s functions=%d obligations=%d"
            (styled "1;32" "verified") display_file
            (Verifier_service.functions result)
            (Verifier_service.obligations result)
        else if declarations = [] then
          Printf.sprintf
            "verocaml: %s file=%s functions=%d obligations=%d \
             trusted-external-spec-uses=%d"
            (styled "1;32" "verified-with-trusted-axioms")
            display_file
            (Verifier_service.functions result)
            (Verifier_service.obligations result)
            (List.length uses)
        else
          let external_spec_uses =
            List.fold_left
              (fun count observation ->
                match Verifier_service.trusted_external_view observation with
                | Trusted_external_specification_use _
                | Trusted_external_target_specification_use _ ->
                    count + 1
                | Trusted_external_body_use _
                | Trusted_external_body_declaration _ ->
                    count)
              0 uses
          in
          Printf.sprintf
            "verocaml: %s file=%s functions=%d obligations=%d \
             trusted-external-bodies=%d trusted-external-body-uses=%d \
             trusted-external-spec-uses=%d"
            (styled "1;32" "verified-with-trusted-axioms")
            display_file
            (Verifier_service.functions result)
            (Verifier_service.obligations result)
            (List.length declarations)
            (List.length uses - external_spec_uses)
            external_spec_uses
      in
      dependencies @ details @ [ summary ]
