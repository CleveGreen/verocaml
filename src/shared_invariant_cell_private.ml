type identity = {
  abstract_type : Sst.type_id;
  hidden_type : Sst.type_id;
  field : Sst.field_id;
  model : Sst.function_id;
  invariant : Sst.function_id;
}

type hooks = {
  entry_eligibility_issued : unit -> unit;
  constructor_eligibility_issued : unit -> unit;
  closed_initialized : unit -> unit;
  opened : unit -> unit;
  updated : unit -> unit;
  closed : unit -> unit;
  effect_instantiated : unit -> unit;
  terminal_read : unit -> unit;
  torn_down : unit -> unit;
}

type origin =
  | Entry of { formal : Sst.binding; ordinal : int }
  | Constructor of {
      constructor : Sst.function_id;
      call_span : Diagnostic.span;
    }

type eligibility = {
  issuer : unit ref;
  session : unit ref;
  token : unit ref;
  program_snapshot : string;
  owner : Sst.function_id;
  identity : identity;
  origin : origin;
  location : Vir.aggregate_term;
  mutable live : bool;
}

type open_capability = {
  issuer : unit ref;
  session : unit ref;
  affinity : unit ref;
  token : unit ref;
  program_snapshot : string;
  owner : Sst.function_id;
  identity : identity;
  operation : Sst.function_id;
  entry_epoch : int;
  mutable current_epoch : int;
  mutable updates : int;
  mutable consumed : bool;
}

type t = {
  issuer : unit ref;
  session : unit ref;
  hooks : hooks;
  program_snapshot : string;
  session_active : unit -> bool;
  owner : Sst.function_id;
  mutable eligibilities : eligibility list;
  mutable closed_epoch : int option;
  mutable opened : open_capability option;
  mutable destroyed : bool;
}

let process_issuer = ref ()

let ( let* ) result continuation =
  match result with Ok value -> continuation value | Error _ as error -> error

let aggregate_type identity =
  {
    Vir.aggregate_type_index = identity.abstract_type.Sst.type_index;
    aggregate_type_name = identity.abstract_type.type_name;
      aggregate_type_arguments = [];
  }

let live manager =
  manager.issuer == process_issuer
  && not manager.destroyed && manager.session_active ()

let create ~hooks ~session_token ~program_snapshot ~session_active ~owner =
  {
    issuer = process_issuer;
    session = session_token;
    hooks;
    program_snapshot;
    session_active;
    owner;
    eligibilities = [];
    closed_epoch = None;
    opened = None;
    destroyed = false;
  }

let issue manager ~identity ~origin ~location notify =
  if not (live manager) then
    Error "invariant-cell eligibility rejected an inactive session"
  else if location.Vir.aggregate_type <> aggregate_type identity then
    Error "invariant-cell eligibility location has the wrong exact type"
  else
    let eligibility =
      {
        issuer = process_issuer;
        session = manager.session;
        token = ref ();
        program_snapshot = manager.program_snapshot;
        owner = manager.owner;
        identity;
        origin;
        location;
        live = true;
      }
    in
    manager.eligibilities <- eligibility :: manager.eligibilities;
    if Option.is_none manager.closed_epoch && Option.is_none manager.opened then (
      manager.closed_epoch <- Some 0;
      manager.hooks.closed_initialized ());
    notify ();
    Ok ()

let issue_entry manager ~identity ~(formal : Sst.binding) ~ordinal ~location =
  if
    formal.Sst.typ <> Sst.Aggregate identity.abstract_type
    || formal.uniqueness <> Sst.Definitely_aliased
  then Error "invariant-cell entry eligibility requires one exact aliased formal"
  else
    issue manager ~identity ~origin:(Entry { formal; ordinal }) ~location
      manager.hooks.entry_eligibility_issued

let issue_constructor manager ~identity ~constructor ~call_span ~location =
  issue manager ~identity ~origin:(Constructor { constructor; call_span })
    ~location manager.hooks.constructor_eligibility_issued

let eligibility_matches manager identity location
    (eligibility : eligibility) =
  let origin_is_exact =
    match eligibility.origin with
    | Entry { formal; ordinal } ->
        ordinal >= 0 && formal.typ = Sst.Aggregate identity.abstract_type
    | Constructor { constructor; call_span } ->
        constructor.function_index >= 0 && call_span.start_pos.line >= 0
  in
  eligibility.issuer == process_issuer
  && eligibility.session == manager.session
  && eligibility.token != process_issuer
  && eligibility.live
  && String.equal eligibility.program_snapshot manager.program_snapshot
  && eligibility.owner = manager.owner
  && eligibility.identity = identity
  && eligibility.location = location
  && origin_is_exact

let open_cell manager ~identity ~(operation : Sst.function_id) ~location
    ~entry_epoch =
  let matching =
    List.filter
      (eligibility_matches manager identity location)
      manager.eligibilities
  in
  if not (live manager) then
    Error "invariant-cell open rejected an inactive session"
  else if Option.is_some manager.opened then
    Error "invariant-cell open rejected duplicate affine authority"
  else if
    not
      (matching <> [])
  then Error "invariant-cell open has no exact persistent eligibility"
  else
    let* () =
      match manager.closed_epoch with
      | None when entry_epoch = 0 ->
          manager.closed_epoch <- Some 0;
          manager.hooks.closed_initialized ();
          Ok ()
      | Some epoch when epoch = entry_epoch -> Ok ()
      | None ->
          Error
            (Printf.sprintf
               "invariant-cell open rejected an uninitialized path epoch %d"
               entry_epoch)
      | Some epoch ->
          Error
            (Printf.sprintf
               "invariant-cell open rejected a stale path epoch %d (closed %d)"
               entry_epoch epoch)
    in
    let capability =
      {
        issuer = process_issuer;
        session = manager.session;
        affinity = ref ();
        token = ref ();
        program_snapshot = manager.program_snapshot;
        owner = manager.owner;
        identity;
        operation;
        entry_epoch;
        current_epoch = entry_epoch;
        updates = 0;
        consumed = false;
      }
    in
    manager.closed_epoch <- None;
    manager.opened <- Some capability;
    manager.hooks.opened ();
    Ok ()

let authenticate_open manager operation (capability : open_capability) =
  live manager
  && capability.issuer == process_issuer
  && capability.session == manager.session
  && capability.affinity != process_issuer
  && capability.token != process_issuer
  && String.equal capability.program_snapshot manager.program_snapshot
  && capability.owner = manager.owner
  && capability.operation = operation
  && capability.current_epoch >= capability.entry_epoch
  && capability.current_epoch = capability.entry_epoch + capability.updates
  && not capability.consumed

let note_update manager ~operation ~transition ~effective_predecessor_epoch
    ~effective_successor_epoch =
  match manager.opened with
  | Some capability
    when
      authenticate_open manager operation capability
      && capability.identity.hidden_type = transition.Sst.shared_record_type
      && capability.identity.field = transition.shared_target_field
      && effective_predecessor_epoch = capability.current_epoch
      && effective_successor_epoch = effective_predecessor_epoch + 1
      && transition.shared_predecessor_epoch = capability.updates
      && transition.shared_successor_epoch = capability.updates + 1
      && capability.updates < 2 ->
      capability.current_epoch <- effective_successor_epoch;
      capability.updates <- capability.updates + 1;
      manager.hooks.updated ();
      Ok ()
  | Some _ | None ->
      Error "invariant-cell update rejected unauthenticated order or epoch"

let close_cell manager ~operation ~final_epoch =
  match manager.opened with
  | Some capability
    when
      authenticate_open manager operation capability
      && capability.updates >= 1 && capability.updates <= 2
      && capability.current_epoch = final_epoch ->
      capability.consumed <- true;
      manager.opened <- None;
      manager.closed_epoch <- Some final_epoch;
      manager.hooks.closed ();
      Ok ()
  | Some _ | None ->
      Error "invariant-cell close has no exact live open capability"

let note_effect_instantiation manager =
  if live manager then (
    manager.hooks.effect_instantiated ();
    Ok ())
  else Error "invariant-cell effect rejected an inactive session"

let note_terminal_read manager ~identity ~operation:_ ~location ~epoch =
  if not (live manager) then
    Error "invariant-cell terminal read rejected an inactive session"
  else if manager.closed_epoch <> Some epoch || Option.is_some manager.opened then
    Error "invariant-cell terminal read requires the exact closed epoch"
  else if
    not
      (List.exists
         (eligibility_matches manager identity location)
         manager.eligibilities)
  then Error "invariant-cell terminal read has no exact eligibility"
  else (
    manager.hooks.terminal_read ();
    Ok ())

let destroy manager =
  if manager.destroyed then
    Error "invariant-cell authority rejected duplicate teardown"
  else (
    Option.iter (fun capability -> capability.consumed <- true) manager.opened;
    List.iter (fun eligibility -> eligibility.live <- false)
      manager.eligibilities;
    let issued =
      manager.eligibilities <> [] || Option.is_some manager.closed_epoch
      || Option.is_some manager.opened
    in
    manager.opened <- None;
    manager.closed_epoch <- None;
    manager.destroyed <- true;
    if issued then manager.hooks.torn_down ();
    Ok ())

let destroy_if_live manager =
  if not manager.destroyed then ignore (destroy manager)

module For_testing = struct
  let span = Diagnostic.file_span "shared_invariant_cell_private_test.ml"

  let binding =
    {
      Sst.id = 0;
      name = "cell";
      typ = Sst.Aggregate { type_index = 0; type_name = "cell" };
      uniqueness = Sst.Definitely_aliased;
      span;
    }

  let identity =
    let typ = { Sst.type_index = 0; type_name = "cell" } in
    {
      abstract_type = typ;
      hidden_type = typ;
      field =
        {
          Sst.field_owner = Sst.Record_owner typ;
          field_index = 0;
          field_name = "value";
        };
      model = { Sst.function_index = 0; function_name = "model" };
      invariant = { Sst.function_index = 1; function_name = "invariant" };
    }

  let location =
    let aggregate_type = aggregate_type identity in
    {
      Vir.aggregate_type;
      aggregate_desc =
        Vir.Aggregate_symbol
          {
            symbol_id = 0;
            source_name = "cell";
            sort = Vir.Aggregate aggregate_type;
            role = Vir.Input;
            span;
          };
    }

  let hooks () =
    {
      entry_eligibility_issued = (fun () -> ());
      constructor_eligibility_issued = (fun () -> ());
      closed_initialized = (fun () -> ());
      opened = (fun () -> ());
      updated = (fun () -> ());
      closed = (fun () -> ());
      effect_instantiated = (fun () -> ());
      terminal_read = (fun () -> ());
      torn_down = (fun () -> ());
    }

  let manager active =
    create ~hooks:(hooks ()) ~session_token:(ref ())
      ~program_snapshot:"program" ~session_active:(fun () -> !active)
      ~owner:{ Sst.function_index = 2; function_name = "client" }

  let transition predecessor successor =
    {
      Sst.shared_policy = Sst.Bounded_shared_scalar_heap_v1;
      shared_function_index = 3;
      shared_function_name = "increment";
      shared_path_id = 0;
      shared_record_type = identity.hidden_type;
      shared_formal_roots = [ binding ];
      shared_target = binding;
      shared_canonical_root = binding;
      shared_alias_chain = [ binding ];
      shared_target_field = identity.field;
      shared_predecessor_epoch = predecessor;
      shared_successor_epoch = successor;
    }

  let lifecycle_matrix () =
    let active = ref true in
    let primary = manager active in
    let operation = { Sst.function_index = 3; function_name = "increment" } in
    let ok =
      match
        issue_entry primary ~identity ~formal:binding ~ordinal:0 ~location
      with
      | Ok () -> ()
      | Error message -> failwith message
    in
    ignore ok;
    let () =
      match open_cell primary ~identity ~operation ~location ~entry_epoch:0 with
      | Ok () -> ()
      | Error message -> failwith message
    in
    let duplicate =
      match open_cell primary ~identity ~operation ~location ~entry_epoch:0 with
      | Error _ -> "duplicate-open: rejected"
      | Ok () -> "duplicate-open: accepted"
    in
    let close_without_update =
      match close_cell primary ~operation ~final_epoch:0 with
      | Error _ -> "close-without-update: rejected"
      | Ok () -> "close-without-update: accepted"
    in
    let unopened = manager (ref true) in
    let () =
      ignore
        (issue_entry unopened ~identity ~formal:binding ~ordinal:0 ~location)
    in
    let close_without_open =
      match close_cell unopened ~operation ~final_epoch:0 with
      | Error _ -> "close-without-open: rejected"
      | Ok () -> "close-without-open: accepted"
    in
    let closed = manager (ref true) in
    let () =
      ignore (issue_entry closed ~identity ~formal:binding ~ordinal:0 ~location);
      ignore (open_cell closed ~identity ~operation ~location ~entry_epoch:0);
      ignore
        (note_update closed ~operation ~transition:(transition 0 1)
           ~effective_predecessor_epoch:0 ~effective_successor_epoch:1);
      ignore (close_cell closed ~operation ~final_epoch:1)
    in
    let open_after_close =
      match open_cell closed ~identity ~operation ~location ~entry_epoch:0 with
      | Error _ -> "open-after-close: rejected"
      | Ok () -> "open-after-close: accepted"
    in
    let duplicate_close =
      match close_cell closed ~operation ~final_epoch:1 with
      | Error _ -> "duplicate-close: rejected"
      | Ok () -> "duplicate-close: accepted"
    in
    let constructor = manager (ref true) in
    let () =
      ignore
        (issue_constructor constructor ~identity
           ~constructor:
             { Sst.function_index = 4; function_name = "make" }
           ~call_span:span ~location);
      ignore
        (open_cell constructor ~identity ~operation ~location ~entry_epoch:0);
      ignore
        (note_update constructor ~operation ~transition:(transition 0 1)
           ~effective_predecessor_epoch:0 ~effective_successor_epoch:1);
      ignore (close_cell constructor ~operation ~final_epoch:1)
    in
    let stale_constructor_epoch =
      match
        open_cell constructor ~identity ~operation ~location ~entry_epoch:0
      with
      | Error _ -> "stale-constructor-epoch: rejected"
      | Ok () -> "stale-constructor-epoch: accepted"
    in
    let forged_manager = manager (ref true) in
    let () =
      ignore
        (issue_entry forged_manager ~identity ~formal:binding ~ordinal:0
           ~location)
    in
    let original = List.hd forged_manager.eligibilities in
    forged_manager.eligibilities <-
      [ { original with issuer = ref (); token = ref () } ];
    let copied_eligibility =
      match
        open_cell forged_manager ~identity ~operation ~location ~entry_epoch:0
      with
      | Error _ -> "copied-eligibility: rejected"
      | Ok () -> "copied-eligibility: accepted"
    in
    let inactive_flag = ref true in
    let inactive = manager inactive_flag in
    let () =
      ignore
        (issue_entry inactive ~identity ~formal:binding ~ordinal:0 ~location);
      inactive_flag := false
    in
    let inactive_session =
      match open_cell inactive ~identity ~operation ~location ~entry_epoch:0 with
      | Error _ -> "inactive-session: rejected"
      | Ok () -> "inactive-session: accepted"
    in
    let () = ignore (destroy primary) in
    let after_destroy =
      match open_cell primary ~identity ~operation ~location ~entry_epoch:0 with
      | Error _ -> "open-after-destroy: rejected"
      | Ok () -> "open-after-destroy: accepted"
    in
    [
      duplicate;
      close_without_update;
      close_without_open;
      open_after_close;
      duplicate_close;
      stale_constructor_epoch;
      copied_eligibility;
      inactive_session;
      after_destroy;
    ]
end
