type ('node, 'replacement) rule = 'node -> 'replacement option

type direction = Bottom_up | Top_down

type repetition =
  | Once
  | Until_stable of { max_passes : int }

type ('node, 'replacement) phase = {
  direction : direction;
  repetition : repetition;
  max_nodes : int option;
  max_rewrites : int option;
  rules : ('node, 'replacement) rule list;
}

type phase_stats = {
  passes : int;
  nodes_visited : int;
  rule_attempts : int;
  rewrites : int;
  stabilized : bool;
  pass_limit_reached : bool;
  rewrite_budget_exhausted : bool;
  node_budget_exhausted : bool;
}

type budget = {
  rewrite_limit : int option;
  node_limit : int option;
  mutable nodes_visited : int;
  mutable rule_attempts : int;
  mutable rewrites : int;
  mutable rewrite_exhausted : bool;
  mutable node_exhausted : bool;
}

let validate_non_negative name = function
  | Some limit when limit < 0 ->
      invalid_arg ("Rewrite_private: " ^ name ^ " must be non-negative")
  | limit -> limit

let make_budget ~max_nodes ~max_rewrites =
  let rewrite_limit = validate_non_negative "max_rewrites" max_rewrites in
  let node_limit = validate_non_negative "max_nodes" max_nodes in
  {
    rewrite_limit;
    node_limit;
    nodes_visited = 0;
    rule_attempts = 0;
    rewrites = 0;
    rewrite_exhausted = false;
    node_exhausted = false;
  }

let unlimited_budget () = make_budget ~max_nodes:None ~max_rewrites:None

let budget_exhausted budget =
  budget.rewrite_exhausted || budget.node_exhausted

let can_visit_node budget =
  match budget.node_limit with
  | None ->
      budget.nodes_visited <- budget.nodes_visited + 1;
      true
  | Some limit when budget.nodes_visited < limit ->
      budget.nodes_visited <- budget.nodes_visited + 1;
      true
  | Some _ ->
      budget.node_exhausted <- true;
      false

let can_try_rule budget =
  match budget.rewrite_limit with
  | None -> true
  | Some limit when budget.rewrites < limit -> true
  | Some _ ->
      budget.rewrite_exhausted <- true;
      false

let apply_rules_once ~budget:(budget [@delator.skip])
    ~replace:(replace [@delator.skip]) ~rules:(rules [@delator.skip])
    (node [@delator.skip]) =
  let rec loop node changed = function
    | [] -> (node, changed)
    | _ when not (can_try_rule budget) -> (node, changed)
    | rule :: remaining -> (
        budget.rule_attempts <- budget.rule_attempts + 1;
        match rule node with
        | None -> loop node changed remaining
        | Some replacement ->
            let replaced = replace node replacement in
            if replaced == node then loop node changed remaining
            else (
              budget.rewrites <- budget.rewrites + 1;
              [%log.trace "applied generic rewrite rule"
                ~stage:(Delator.Field.string "rewrite")
                ~rule_count:(Delator.Field.int (List.length rules))
                ~rewrite_ordinal:(Delator.Field.int budget.rewrites)
                ~decision:(Delator.Field.string "rewritten")];
              loop replaced true remaining))
  in
  loop node false rules
[@@delator.instrument] [@@delator.level trace] [@@delator.no_exn_log]

let apply_rules ~replace:(replace [@delator.skip])
    ~rules:(rules [@delator.skip]) (node [@delator.skip]) =
  fst
    (apply_rules_once ~budget:(unlimited_budget ()) ~replace ~rules node)
[@@delator.instrument] [@@delator.level trace] [@@delator.no_exn_log]

let rec bottom_up_once ~budget ~map_children:(map_children [@delator.skip])
    ~replace:(replace [@delator.skip]) ~rules:(rules [@delator.skip])
    (node [@delator.skip]) =
  if budget_exhausted budget || not (can_visit_node budget) then (node, false)
  else (
    let original = node in
    let children_changed = ref false in
    let node =
      map_children
        (fun child ->
          let child, changed =
            bottom_up_once ~budget ~map_children ~replace ~rules child
          in
          children_changed := !children_changed || changed;
          child)
        node
    in
    let children_changed = !children_changed && node != original in
    if budget_exhausted budget then (node, children_changed)
    else
      let node, changed =
        apply_rules_once ~budget ~replace ~rules node
      in
      (node, children_changed || changed))

let rec top_down_once ~budget ~map_children:(map_children [@delator.skip])
    ~replace:(replace [@delator.skip]) ~rules:(rules [@delator.skip])
    (node [@delator.skip]) =
  if budget_exhausted budget || not (can_visit_node budget) then (node, false)
  else (
    let node, node_changed =
      apply_rules_once ~budget ~replace ~rules node
    in
    if budget_exhausted budget then (node, node_changed)
    else
      let before_children = node in
      let children_changed = ref false in
      let node =
        map_children
          (fun child ->
            let child, changed =
              top_down_once ~budget ~map_children ~replace ~rules child
            in
            children_changed := !children_changed || changed;
            child)
          node
      in
      let children_changed = !children_changed && node != before_children in
      (node, node_changed || children_changed))

let bottom_up ~map_children:(map_children [@delator.skip])
    ~replace:(replace [@delator.skip]) ~rules:(rules [@delator.skip])
    (node [@delator.skip]) =
  fst
    (bottom_up_once ~budget:(unlimited_budget ()) ~map_children ~replace
       ~rules node)
[@@delator.instrument] [@@delator.level trace] [@@delator.no_exn_log]

let top_down ~max_nodes ~map_children:(map_children [@delator.skip])
    ~replace:(replace [@delator.skip]) ~rules:(rules [@delator.skip])
    (node [@delator.skip]) =
  if max_nodes <= 0 then
    invalid_arg "Rewrite_private: max_nodes must be strictly positive";
  fst
    (top_down_once
       ~budget:(make_budget ~max_nodes:(Some max_nodes) ~max_rewrites:None)
       ~map_children ~replace ~rules node)
[@@delator.instrument] [@@delator.level trace] [@@delator.no_exn_log]

let validate_repetition = function
  | Once -> ()
  | Until_stable { max_passes } when max_passes > 0 -> ()
  | Until_stable _ ->
      invalid_arg "Rewrite_private: max_passes must be strictly positive"

let phase_stats ~passes ~stabilized ~pass_limit_reached budget =
  {
    passes;
    nodes_visited = budget.nodes_visited;
    rule_attempts = budget.rule_attempts;
    rewrites = budget.rewrites;
    stabilized;
    pass_limit_reached;
    rewrite_budget_exhausted = budget.rewrite_exhausted;
    node_budget_exhausted = budget.node_exhausted;
  }

let apply_phase_with_stats ~map_children:(map_children [@delator.skip])
    ~replace:(replace [@delator.skip]) (phase [@delator.skip])
    (initial [@delator.skip]) =
  let { direction; repetition; max_nodes; max_rewrites; rules } = phase in
  validate_repetition repetition;
  (match (direction, max_nodes) with
  | Top_down, None ->
      invalid_arg
        "Rewrite_private: top-down phases require a finite max_nodes bound"
  | Bottom_up, (None | Some _) | Top_down, Some _ -> ());
  let budget = make_budget ~max_nodes ~max_rewrites in
  let rec loop pass node =
    let node, changed =
      match direction with
      | Bottom_up ->
          bottom_up_once ~budget ~map_children ~replace ~rules node
      | Top_down ->
          top_down_once ~budget ~map_children ~replace ~rules node
    in
    if budget_exhausted budget then
      (node, phase_stats ~passes:pass ~stabilized:false
               ~pass_limit_reached:false budget)
    else
      match repetition with
      | Once ->
          (node, phase_stats ~passes:pass ~stabilized:(not changed)
                   ~pass_limit_reached:false budget)
      | Until_stable _ when not changed ->
          (node, phase_stats ~passes:pass ~stabilized:true
                   ~pass_limit_reached:false budget)
      | Until_stable { max_passes } when pass >= max_passes ->
          (node, phase_stats ~passes:pass ~stabilized:false
                   ~pass_limit_reached:true budget)
      | Until_stable _ -> loop (pass + 1) node
  in
  let node, stats = loop 1 initial in
  [%log.debug "completed generic rewrite phase"
    ~stage:(Delator.Field.string "rewrite")
    ~pass_count:(Delator.Field.int stats.passes)
    ~node_visit_count:(Delator.Field.int stats.nodes_visited)
    ~rule_attempt_count:(Delator.Field.int stats.rule_attempts)
    ~rewrite_count:(Delator.Field.int stats.rewrites)
    ~stabilized:
      (Delator.Field.bool stats.stabilized)
    ~pass_limit_reached:
      (Delator.Field.bool stats.pass_limit_reached)
    ~rewrite_budget_exhausted:
      (Delator.Field.bool stats.rewrite_budget_exhausted)
    ~node_budget_exhausted:
      (Delator.Field.bool stats.node_budget_exhausted)
    ~decision:(Delator.Field.string "phase-complete")];
  (node, stats)
[@@delator.instrument] [@@delator.level debug] [@@delator.no_exn_log]

let apply_phase ~map_children ~replace phase node =
  fst (apply_phase_with_stats ~map_children ~replace phase node)

let apply_phases_with_stats ~map_children ~replace phases initial =
  let node, reversed_stats =
    List.fold_left
      (fun (node, stats) phase ->
        let node, phase_stats =
          apply_phase_with_stats ~map_children ~replace phase node
        in
        (node, phase_stats :: stats))
      (initial, []) phases
  in
  (node, List.rev reversed_stats)

let apply_phases ~map_children ~replace phases node =
  fst (apply_phases_with_stats ~map_children ~replace phases node)
