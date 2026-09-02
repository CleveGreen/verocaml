let same_function_id (left : Sst.function_id) (right : Sst.function_id) =
  left.function_index = right.function_index
  && String.equal left.function_name right.function_name

let reconcile_definition definition =
  if
    (not definition.Sst.recursive)
    || definition.result_type <> Sst.Mathematical_int
  then definition
  else
    let rewrites = ref 0 in
    let reconcile expression =
      match expression.Sst.expression_desc with
      | Sst.Lift_runtime_int
          {
            expression_desc =
              Sst.Direct_call
                ({ callee; recursive = true; _ } as application);
            typ = Sst.Int;
            _;
          }
        when same_function_id callee definition.function_id ->
          incr rewrites;
          Some (Sst.Direct_call application)
      | _ -> None
    in
    let expression value =
      Sst_expression_rewrite_private.bottom_up ~rules:[ reconcile ] value
    in
    let staged value =
      { value with Sst.expression = expression value.Sst.expression }
    in
    let predicate (clause : Sst.predicate_clause) =
      { clause with Sst.predicate = staged clause.Sst.predicate }
    in
    let ensures (clause : Sst.ensures_clause) =
      { clause with Sst.predicate = staged clause.Sst.predicate }
    in
    let contracts : Sst.contracts =
      {
        requires = List.map predicate definition.contracts.requires;
        ensures = List.map ensures definition.contracts.ensures;
        decreases = List.map predicate definition.contracts.decreases;
        assertions = List.map predicate definition.contracts.assertions;
      }
    in
    let body =
      match definition.body with
      | Sst.Checked_exec value ->
          Sst.Checked_exec { value with body = staged value.body }
      | Spec_definition value -> Spec_definition (staged value)
      | Recursive_spec_definition value ->
          Recursive_spec_definition { value with body = staged value.body }
      | Proof_body value ->
          Proof_body { value with body = staged value.body }
      | External_specification _ | Trusted_external_spec_target _
      | Trusted_external_body _ | Symbolic_declaration _ as body ->
          body
    in
    [%log.debug "reconciled provisional recursive logical call results"
      ~stage:(Delator.Field.string "local-logical-call-rewrite")
      ~function_name:(Delator.Field.string definition.function_id.function_name)
      ~result_sort:(Delator.Field.string "mathematical-int")
      ~rewrite_count:(Delator.Field.int !rewrites)
      ~decision:
        (Delator.Field.string
           (if !rewrites = 0 then "unchanged" else "rewritten"))];
    { definition with contracts; body }
[@@delator.instrument] [@@delator.level debug]
