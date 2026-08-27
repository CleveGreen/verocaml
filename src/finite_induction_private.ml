  type context = {
    plan : Termination.plan;
    callable : Sst.function_id;
    domain_key : string;
  }

  type hypothesis = {
    context : context;
    caller : Sst.function_id;
    callee : Sst.function_id;
    call_span : Diagnostic.span;
    actual_snapshot : string;
    finite_actuals : string list;
    nonnegative_obligation : Vir.obligation;
    strict_descent_obligation : Vir.obligation;
  }

  let ( let* ) result continuation =
    match result with
    | Ok value -> continuation value
    | Error _ as error -> error

  let same_function left right =
    left.Sst.function_index = right.Sst.function_index
    && String.equal left.function_name right.function_name

  let classify_domain = function
    | Termination.Integer_height -> Ok "integer-height"
    | Termination.Structural_rank rank ->
        Ok ("structural-rank:" ^ Termination.structural_rank_id rank)
    | Termination.Parametric_direct_edge _ ->
        Error "parametric direct-edge recursion is not finite induction"
    | Termination.Frozen_spine_direct_edge _ ->
        Error "frozen-spine recursion is not immutable direct induction"

  let authenticate plan function_id =
    match Termination.find_pending_summary plan function_id with
    | None -> Ok None
    | Some pending -> (
      let group = Termination.pending_group pending in
      let members =
        Termination.scc_members group |> List.map Termination.callable_id
      in
        match members with
      | [ member ] when same_function member function_id ->
          let* domain_key =
            classify_domain (Termination.pending_domain pending)
          in
          Ok (Some { plan; callable = member; domain_key })
        | [ _ ] -> Error "direct recursion termination identity is mismatched"
      | [] | _ :: _ :: _ ->
          Error "production mutual recursion has no finite induction context")

  let hypothesis context ~caller ~callee ~call_span ~actual_snapshot
      ~finite_actuals ~nonnegative_obligation ~strict_descent_obligation =
    if
      (not (same_function caller context.callable))
      || not (same_function callee context.callable)
    then Error "recursive finite hypothesis crosses its authenticated direct callable"
    else
      match
        Termination.find_edge_intent context.plan ~caller ~callee ~span:call_span
      with
      | None -> Error "recursive finite hypothesis has no authenticated edge"
      | Some edge ->
          let* edge_key = classify_domain (Termination.edge_domain edge) in
          if not (String.equal edge_key context.domain_key) then
            Error "recursive finite hypothesis has an incomparable edge domain"
          else
            Ok
              {
                context;
                caller;
                callee;
                call_span;
                actual_snapshot;
                finite_actuals;
                nonnegative_obligation;
                strict_descent_obligation;
              }

  let callable context = context.callable
  let domain_key context = context.domain_key
  let hypothesis_callee hypothesis = hypothesis.callee

  let hypothesis_snapshot hypothesis =
    Digest.string
      (Marshal.to_string
         ( hypothesis.context.domain_key,
           hypothesis.caller,
           hypothesis.callee,
           hypothesis.call_span,
           hypothesis.actual_snapshot,
           hypothesis.finite_actuals,
           hypothesis.nonnegative_obligation.Vir.obligation_index,
           hypothesis.strict_descent_obligation.Vir.obligation_index )
         [ Marshal.No_sharing ])
    |> Digest.to_hex
