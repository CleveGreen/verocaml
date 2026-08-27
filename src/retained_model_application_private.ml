let nested_imported_application = function
  | Vir.Recursive_aggregate_argument
      {
        aggregate_desc = Vir.Aggregate_imported_model_application _;
        _;
      } ->
      true
  | Recursive_integer_argument _ | Recursive_boolean_argument _
  | Recursive_aggregate_argument _ | Recursive_parametric_argument _ ->
      false

let resolve (term : Vir.aggregate_term) =
  match term.aggregate_desc with
  | Vir.Aggregate_imported_model_application
      {
        callee;
        callable_path;
        callable_uid;
        provider_unit;
        provider_interface;
        provider_source;
        provider_family;
        provider_import;
        summary_digest;
        closure_digest;
        call_snapshot;
        registration_snapshot;
        invocation_ordinal;
        application_identity;
        arguments;
        result_type;
        span;
        _;
      } ->
      if result_type <> term.aggregate_type then
        Error
          "retained aggregate model application result type provenance changed"
      else if List.exists nested_imported_application arguments then
        Error
          "retained aggregate model applications cannot be nested as imported model actuals"
      else
        let logical_digest =
          Digest.to_hex
            (Digest.string
               (String.concat "\000"
                  [
                    provider_unit;
                    provider_interface;
                    provider_source;
                    provider_family;
                    provider_import;
                    callable_path;
                    callable_uid;
                    summary_digest;
                    closure_digest;
                  ]))
        in
        let application_snapshot =
          Vir.imported_model_application_snapshot ~callee ~arguments
            ~result_type ~span
        in
        if
          Imported_callable.aggregate_application_identity_matches
            application_identity ~logical_digest ~call_snapshot
            ~registration_snapshot ~invocation_ordinal ~application_snapshot
        then Ok ("verocaml_imported_model_" ^ logical_digest, arguments)
        else
          Error
            "retained aggregate model application identity or provenance is forged"
  | Aggregate_symbol _ | Aggregate_selector _
  | Aggregate_recursive_spec_application _ | Aggregate_constructor _
  | Aggregate_record _ | Aggregate_conditional _
  | Aggregate_symbolic_application _ ->
      Error "expected a retained aggregate model application"
