[@@@verocaml.verify]

let relay value =
  [%verocaml.ensures fun result -> result = value]
  ;
  Workflow_provider.identity value

let route flag left right =
  [%verocaml.ensures fun result -> result = left || result = right]
  ;
  Workflow_provider.choose flag left right

let option_round_trip value = Workflow_provider.copy_option value
let result_round_trip value = Workflow_provider.copy_result value
let outcome_round_trip value = Workflow_provider.copy_outcome value
let append_values left right = Workflow_provider.append left right
let reverse_values values = Workflow_provider.reverse values
let batch_size values = Workflow_provider.length values
let mirror tree = Workflow_provider.mirror tree
let tree_size tree = Workflow_provider.tree_size tree

let use_int value =
  [%verocaml.ensures fun result -> result = value] ; relay value

let use_bool value =
  [%verocaml.ensures fun result -> result = value] ; relay value

let use_abstract value =
  [%verocaml.ensures fun result -> result = value] ; relay value

let use_nested value =
  option_round_trip
    (Some (Workflow_provider.Accepted (Workflow_provider.identity value)))
