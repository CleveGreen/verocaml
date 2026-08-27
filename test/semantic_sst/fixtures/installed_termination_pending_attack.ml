let reveal (pending : Termination.pending_summary) =
  Sst_validation.callable_definition
    (Termination.pending_callable pending)
