type declaration_kind = Numeric_semantics_correlation_private.declaration_kind =
  | Completed_proof_declaration
  | Explicit_axiom_declaration
  | Ordinary_specification_declaration

type t = Numeric_semantics_correlation_private.t = private {
  role : Cmt_input.interface_numeric_role;
  definition : Sst.function_definition;
  callable_definition : Sst.function_definition option;
  kind : declaration_kind;
}

let complete_local ~completion ~implementation ~validated =
  let result =
    if
      Verification_driver_private.completion_matches completion ~implementation
        ~validated
    then
      Numeric_semantics_correlation_private.correlate_local ~implementation
        ~validated
    else
      Error
        "Numeric semantics require successful verification of their exact provider."
  in
  [%log.debug "completed numeric semantics declaration correlation"
    ~provider:(Delator.Field.string implementation.Cmt_input.unit_name)
    ~decision:
      (Delator.Field.string
         (if Result.is_ok result then "correlated" else "rejected"))
    ~reason:
      (Delator.Field.string
         (match result with
         | Ok _ -> "exact-completed-declarations"
         | Error reason -> reason))
    ~declaration_count:
      (Delator.Field.int
         (match result with Ok bindings -> List.length bindings | Error _ -> 0))];
  result
[@@delator.instrument] [@@delator.level debug]

let selects_explicit_axiom =
  Numeric_semantics_correlation_private.selects_explicit_axiom
