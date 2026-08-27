let persistent_runtime () =
  let open Persistent_collections in
  let queue = enqueue 3 (enqueue 2 (singleton 1)) in
  assert (queue_length queue = 3);
  (match dequeue queue with
  | Queue_empty -> assert false
  | Queue_item (value, rest) ->
      assert (value = 1);
      assert (queue_length rest = 2));
  let values = Link (1, Link (2, Link (3, End))) in
  assert (length values = 3);
  assert (nth 1 values = Some 2);
  let tree = Branch (1, Branch (2, Leaf, Leaf), Branch (3, Leaf, Leaf)) in
  assert (tree_size tree = 3);
  assert (tree_height tree = 2);
  assert (leftmost tree = Some 2);
  assert (same_shape tree (mirror tree))

let callback_runtime () =
  let open Callback_workflows in
  assert (int_identity_client 7 = 7);
  assert (bool_identity_client true);
  assert (bool_transform_client true = Some false);
  assert (result_transform_client (-4) = Ok 0);
  assert (labelled_client 5 7 = 12);
  assert (envelope_client 9 "payload" = { sequence = 9; payload = "payload" })

let cross_module_runtime () =
  let open Workflow_provider in
  assert (Workflow_consumer.use_int 11 = 11);
  assert (Workflow_consumer.use_bool true);
  assert (Workflow_consumer.option_round_trip (Some "ready") = Some "ready");
  let batch = make_batch [ 1; 2 ] [ 4; 3 ] |> normalize_batch in
  assert (batch.ready = [ 1; 2; 3; 4 ]);
  assert (Workflow_consumer.batch_size batch.ready = 4);
  let tree = Branch (1, Branch (2, Leaf, Leaf), Leaf) in
  assert (Workflow_consumer.tree_size tree = 2)

let () =
  persistent_runtime ();
  callback_runtime ();
  cross_module_runtime ();
  print_endline "parametric-programs-runtime: ok"
