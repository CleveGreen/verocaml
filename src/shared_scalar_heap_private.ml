type hooks = {
  issued : unit -> unit;
  write_authenticated : unit -> unit;
  read_logged : unit -> unit;
  epoch_advanced : unit -> unit;
  torn_down : unit -> unit;
}

type read_view = Entry_view | Current_view | Epoch_view of int

type read_event = {
  read_field : Sst.field_id;
  read_path_id : int;
  read_epoch : int;
  read_location : Vir.aggregate_term;
  read_term : Vir.integer_term;
  read_entry_view : bool;
}

type write_event = {
  write_transition : Sst.shared_scalar_heap_transition;
  write_location : Vir.aggregate_term;
  write_value : Vir.integer_term;
}

type capability = {
  issuer : unit ref;
  session : unit ref;
  affinity : unit ref;
  token : unit ref;
  program_snapshot : string;
  function_index : int;
  function_name : string;
  path_id : int;
  record_type : Sst.type_id;
  field : Sst.field_id;
  formals : Sst.binding list;
  predecessor_epoch : int;
  successor_epoch : int;
  mutable consumed : bool;
}

type audit = {
  mutable reads : read_event list;
  mutable writes : write_event list;
}

type t = {
  private_issuer : unit ref;
  session_token : unit ref;
  hooks : hooks;
  session_active : unit -> bool;
  program_snapshot : string;
  function_index : int;
  function_name : string;
  path_id : int;
  record_type : Sst.type_id;
  field : Sst.field_id;
  formals : Sst.binding list;
  epoch : int;
  log : (int * Vir.aggregate_term * Vir.integer_term) list;
  capability : capability;
  audit : audit;
  destroyed : bool ref;
}

let process_issuer = ref ()

let aggregate_type_of_sst (type_id : Sst.type_id) =
  {
    Vir.aggregate_type_index = type_id.type_index;
    aggregate_type_name = type_id.type_name;
      aggregate_type_arguments = [];
  }

let same_function transition state =
  transition.Sst.shared_function_index = state.function_index
  && String.equal transition.shared_function_name state.function_name
  && transition.shared_path_id = state.path_id
  && transition.shared_record_type = state.record_type
  && transition.shared_target_field = state.field
  && transition.shared_formal_roots = state.formals

let make_capability state predecessor_epoch successor_epoch =
  {
    issuer = state.private_issuer;
    session = state.session_token;
    affinity = ref ();
    token = ref ();
    program_snapshot = state.program_snapshot;
    function_index = state.function_index;
    function_name = state.function_name;
    path_id = state.path_id;
    record_type = state.record_type;
    field = state.field;
    formals = state.formals;
    predecessor_epoch;
    successor_epoch;
    consumed = false;
  }

let authenticate state capability =
  state.private_issuer == process_issuer
  && capability.issuer == process_issuer
  && capability.session == state.session_token
  && capability.affinity != process_issuer
  && capability.token != process_issuer
  && String.equal capability.program_snapshot state.program_snapshot
  && capability.function_index = state.function_index
  && String.equal capability.function_name state.function_name
  && capability.path_id = state.path_id
  && capability.record_type = state.record_type
  && capability.field = state.field
  && capability.formals = state.formals
  && capability.predecessor_epoch = state.epoch
  && capability.successor_epoch = state.epoch + 1
  && not capability.consumed
  && not !(state.destroyed)
  && state.session_active ()

let create ~hooks ~session_token ~program_snapshot ~session_active transition =
  if
    transition.Sst.shared_policy <> Sst.Bounded_shared_scalar_heap_v1
    || transition.shared_path_id <> 0
    || transition.shared_predecessor_epoch <> 0
    || transition.shared_successor_epoch <> 1
    ||
    match transition.shared_formal_roots with
    | [ _ ] | [ _; _ ] -> false
    | [] | _ :: _ -> true
  then Error "shared heap creation rejected a malformed initial descriptor"
  else
    let seed =
      {
        private_issuer = process_issuer;
        session_token;
        hooks;
        session_active;
        program_snapshot;
        function_index = transition.shared_function_index;
        function_name = transition.shared_function_name;
        path_id = transition.shared_path_id;
        record_type = transition.shared_record_type;
        field = transition.shared_target_field;
        formals = transition.shared_formal_roots;
        epoch = 0;
        log = [];
        capability =
          {
            issuer = process_issuer;
            session = session_token;
            affinity = ref ();
            token = ref ();
            program_snapshot;
            function_index = transition.shared_function_index;
            function_name = transition.shared_function_name;
            path_id = transition.shared_path_id;
            record_type = transition.shared_record_type;
            field = transition.shared_target_field;
            formals = transition.shared_formal_roots;
            predecessor_epoch = 0;
            successor_epoch = 1;
            consumed = false;
          };
        audit = { reads = []; writes = [] };
        destroyed = ref false;
      }
    in
    hooks.issued ();
    Ok seed

let rec current_read max_epoch location entry = function
  | [] -> entry
  | (written_epoch, written_location, written_value) :: rest ->
      if written_epoch > max_epoch then current_read max_epoch location entry rest
      else
        Vir_integer_conditional_private.create
          ~condition:(Vir.Aggregate_equal (location, written_location))
          ~consequent:written_value
          ~alternative:(current_read max_epoch location entry rest)

let accepts_read state ~field ~(location : Vir.aggregate_term) =
  (not !(state.destroyed))
  && state.session_active ()
  && field = state.field
  && location.aggregate_type = aggregate_type_of_sst state.record_type

let read state ~view ~field ~(location : Vir.aggregate_term) ~entry =
  if !(state.destroyed) || not (state.session_active ()) then
    Error "shared heap read rejected a destroyed session"
  else if not (accepts_read state ~field ~location) then
    Error "shared heap read crossed its authenticated type or field"
  else
    let entry_view = view = Entry_view in
    let read_epoch =
      match view with
      | Entry_view -> 0
      | Current_view -> state.epoch
      | Epoch_view epoch -> epoch
    in
    let term =
      match view with
      | Entry_view -> entry
      | Current_view -> current_read state.epoch location entry state.log
      | Epoch_view epoch ->
          if epoch < 0 || epoch > state.epoch then entry
          else current_read epoch location entry state.log
    in
    state.hooks.read_logged ();
    state.audit.reads <-
      {
        read_field = field;
        read_path_id = state.path_id;
        read_epoch;
        read_location = location;
        read_term = term;
        read_entry_view = entry_view;
      }
      :: state.audit.reads;
    Ok term

let write_rebased state ~base_epoch ~transition
    ~(location : Vir.aggregate_term) ~value =
  let capability = state.capability in
  if not (same_function transition state) then
    Error "shared heap write descriptor crossed function, path, type, or field"
  else if
    transition.shared_predecessor_epoch + base_epoch <> state.epoch
    || transition.shared_successor_epoch + base_epoch <> state.epoch + 1
  then Error "shared heap write rejected a stale or non-successor epoch"
  else if not (authenticate state capability) then
    Error "shared heap write rejected an unauthenticated or spent capability"
  else if location.aggregate_type <> aggregate_type_of_sst state.record_type then
    Error "shared heap write location has the wrong aggregate type"
  else (
    capability.consumed <- true;
    state.hooks.write_authenticated ();
    state.hooks.epoch_advanced ();
    let successor =
      {
        state with
        epoch = state.epoch + 1;
        log = (state.epoch + 1, location, value) :: state.log;
        capability =
          make_capability state (state.epoch + 1) (state.epoch + 2);
      }
    in
    let transition =
      {
        transition with
        Sst.shared_predecessor_epoch =
          transition.shared_predecessor_epoch + base_epoch;
        shared_successor_epoch =
          transition.shared_successor_epoch + base_epoch;
      }
    in
    state.audit.writes <-
      { write_transition = transition; write_location = location; write_value = value }
      :: state.audit.writes;
    Ok successor)

let write state ~transition ~location ~value =
  write_rebased state ~base_epoch:0 ~transition ~location ~value

let current_epoch state = state.epoch
let read_events state = List.rev state.audit.reads
let write_events state = List.rev state.audit.writes

let fork state =
  if !(state.destroyed) || not (state.session_active ()) then
    Error "shared heap branch fork requires an active capability"
  else
    Ok
      {
        state with
        capability =
          make_capability state state.epoch (state.epoch + 1);
      }

let destroy state =
  if !(state.destroyed) then
    Error "shared heap teardown rejected duplicate destroy"
  else (
    state.destroyed := true;
    state.capability.consumed <- true;
    state.hooks.torn_down ();
    Ok ())

let destroy_if_live state =
  if not !(state.destroyed) then ignore (destroy state)

module For_testing = struct
  let span = Diagnostic.file_span "shared_scalar_heap_private_test.ml"

  let binding id name =
    {
      Sst.id;
      name;
      typ = Sst.Aggregate { type_index = 0; type_name = "box" };
      uniqueness = Sst.Definitely_aliased;
      span;
    }

  let transition predecessor successor =
    let root = binding 0 "box" in
    let type_id = { Sst.type_index = 0; type_name = "box" } in
    let field =
      {
        Sst.field_owner = Sst.Record_owner type_id;
        field_index = 0;
        field_name = "value";
      }
    in
    {
      Sst.shared_policy = Sst.Bounded_shared_scalar_heap_v1;
      shared_function_index = 0;
      shared_function_name = "increment";
      shared_path_id = 0;
      shared_record_type = type_id;
      shared_formal_roots = [ root ];
      shared_target = root;
      shared_canonical_root = root;
      shared_alias_chain = [ root ];
      shared_target_field = field;
      shared_predecessor_epoch = predecessor;
      shared_successor_epoch = successor;
    }

  let location () =
    let aggregate_type =
      { Vir.aggregate_type_index = 0; aggregate_type_name = "box" ; aggregate_type_arguments = []}
    in
    let symbol =
      {
        Vir.symbol_id = 0;
        source_name = "box";
        sort = Vir.Aggregate aggregate_type;
        role = Vir.Input;
        span;
      }
    in
    { Vir.aggregate_type; aggregate_desc = Vir.Aggregate_symbol symbol }

  let hooks () =
    {
      issued = (fun () -> ());
      write_authenticated = (fun () -> ());
      read_logged = (fun () -> ());
      epoch_advanced = (fun () -> ());
      torn_down = (fun () -> ());
    }

  let lifecycle_matrix () =
    let active = ref true in
    let make () =
      match
        create ~hooks:(hooks ()) ~session_token:(ref ())
          ~program_snapshot:"program"
          ~session_active:(fun () -> !active) (transition 0 1)
      with
      | Ok state -> state
      | Error message -> failwith message
    in
    let target = location () in
    let first = make () in
    let copied = first in
    let successor =
      match
        write first ~transition:(transition 0 1) ~location:target
          ~value:(Vir.Integer_constant Z.one)
      with
      | Ok state -> state
      | Error message -> failwith message
    in
    let duplicate =
      match
        write first ~transition:(transition 0 1) ~location:target
          ~value:(Vir.Integer_constant Z.one)
      with
      | Error _ -> "duplicate-consume: rejected"
      | Ok _ -> "duplicate-consume: accepted"
    in
    let copied_capability =
      match
        write copied ~transition:(transition 0 1) ~location:target
          ~value:(Vir.Integer_constant Z.one)
      with
      | Error _ -> "copied-capability: rejected"
      | Ok _ -> "copied-capability: accepted"
    in
    let stale =
      match
        write successor ~transition:(transition 0 1) ~location:target
          ~value:(Vir.Integer_constant Z.one)
      with
      | Error _ -> "stale-epoch: rejected"
      | Ok _ -> "stale-epoch: accepted"
    in
    let destroyed_state = make () in
    let () =
      match destroy destroyed_state with
      | Ok () -> ()
      | Error message -> failwith message
    in
    let destroyed =
      match
        write destroyed_state ~transition:(transition 0 1) ~location:target
          ~value:(Vir.Integer_constant Z.one)
      with
      | Error _ -> "destroyed-session: rejected"
      | Ok _ -> "destroyed-session: accepted"
    in
    let active_state = make () in
    active := false;
    let inactive_session =
      match
        write active_state ~transition:(transition 0 1) ~location:target
          ~value:(Vir.Integer_constant Z.one)
      with
      | Error _ -> "inactive-session: rejected"
      | Ok _ -> "inactive-session: accepted"
    in
    [ duplicate; copied_capability; stale; destroyed; inactive_session ]
end
