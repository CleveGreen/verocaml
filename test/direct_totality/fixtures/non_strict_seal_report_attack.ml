let report_true_for_non_strict
    (pending : Termination.pending_summary)
    (edge : Termination.edge_intent) =
  Termination.seal pending ~entry_nonnegative:true
    ~edge_outcomes:
      [
        Termination.edge_outcome edge ~nonnegative:true
          ~strictly_smaller:true;
      ]
