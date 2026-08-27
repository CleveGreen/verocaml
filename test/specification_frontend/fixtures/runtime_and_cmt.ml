let executions = ref 0

let req_bool value =
  incr executions;
  value >= 0

let assert_bool value =
  incr executions;
  value >= 0

let measure value =
  incr executions;
  value

let old_value value =
  incr executions;
  value

let checked n =
  [%verocaml.requires req_bool n];
  [%verocaml.ensures fun result -> result > [%verocaml.old old_value n]];
  [%verocaml.decreases measure n];
  [%verocaml.assert assert_bool n];
  n + 1

let () =
  if checked 41 <> 42 then failwith "body result changed";
  if !executions <> 0 then failwith "a ghost specification executed"
