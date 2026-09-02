type observation = {
  identity_digest : string;
  presentation_name : string;
  backend_head : string;
}

type scope = {
  declaration_names : string list;
  presentation_name : string;
  observation_lock : Mutex.t;
  mutable observations : observation list;
}

let active_scope = Atomic.make None
let scope_lock = Mutex.create ()

let with_scope_lock action =
  Mutex.lock scope_lock;
  Fun.protect ~finally:(fun () -> Mutex.unlock scope_lock) action

let snapshot scope =
  Mutex.lock scope.observation_lock;
  Fun.protect
    ~finally:(fun () -> Mutex.unlock scope.observation_lock)
    (fun () -> List.sort_uniq compare scope.observations)

let with_forced_presentation_name
    ~declaration_names:
      (declaration_names
        [@delator.field (fun names -> string_of_int (List.length names))])
    ~presentation_name:(presentation_name [@delator.skip])
    (action [@delator.skip]) =
  let declaration_names = List.sort_uniq String.compare declaration_names in
  if declaration_names = [] || List.exists (String.equal "") declaration_names
  then (
    [%log.warn "rejected forced symbolic presentation scope"
      ~stage:(Delator.Field.string "forced-presentation-scope")
      ~decision:(Delator.Field.string "rejected")
      ~reason_class:(Delator.Field.string "declaration-set")];
    invalid_arg "forced presentation scope requires declaration names")
  else if String.equal presentation_name "" then (
    [%log.warn "rejected forced symbolic presentation scope"
      ~stage:(Delator.Field.string "forced-presentation-scope")
      ~decision:(Delator.Field.string "rejected")
      ~reason_class:(Delator.Field.string "presentation-name")];
    invalid_arg "forced presentation scope requires a presentation name"
  ) else
    let scope =
      {
        declaration_names;
        presentation_name;
        observation_lock = Mutex.create ();
        observations = [];
      }
    in
    with_scope_lock (fun () ->
        match Atomic.get active_scope with
        | Some _ ->
            [%log.warn "rejected nested forced symbolic presentation scope"
              ~stage:(Delator.Field.string "forced-presentation-scope")
              ~declaration_count:
                (Delator.Field.int (List.length declaration_names))
              ~decision:(Delator.Field.string "rejected")
              ~reason_class:(Delator.Field.string "scope-already-active")];
            invalid_arg "a forced symbolic presentation scope is already active"
        | None ->
            Atomic.set active_scope (Some scope);
            [%log.debug "opened forced symbolic presentation scope"
              ~stage:(Delator.Field.string "forced-presentation-scope")
              ~declaration_count:
                (Delator.Field.int (List.length declaration_names))
              ~decision:(Delator.Field.string "opened")]);
    let observations = ref [] in
    let result =
      Fun.protect
        ~finally:(fun () ->
          with_scope_lock (fun () ->
              match Atomic.get active_scope with
              | Some active when active == scope -> Atomic.set active_scope None
              | Some _ | None ->
                  [%log.error "forced symbolic presentation scope lost ownership"
                    ~stage:(Delator.Field.string "forced-presentation-scope")
                    ~decision:(Delator.Field.string "failed")
                    ~reason_class:(Delator.Field.string "scope-replaced")];
                  invalid_arg "forced symbolic presentation scope was replaced");
          observations := snapshot scope;
          [%log.debug "closed forced symbolic presentation scope"
            ~stage:(Delator.Field.string "forced-presentation-scope")
            ~declaration_count:
              (Delator.Field.int (List.length declaration_names))
            ~observation_count:
              (Delator.Field.int (List.length !observations))
            ~decision:(Delator.Field.string "closed")])
        action
    in
    (result, !observations)
[@@delator.instrument]
[@@delator.level debug]
[@@delator.no_exn_log]

let matching_scope declaration_name =
  match Atomic.get active_scope with
  | Some scope when List.mem declaration_name scope.declaration_names ->
      Some scope
  | Some _ | None -> None

let select_presentation_name ~declaration_name ~default =
  match matching_scope declaration_name with
  | Some scope ->
      [%log.trace "selected forced symbolic presentation"
        ~stage:(Delator.Field.string "forced-presentation-selection")
        ~decision:(Delator.Field.string "forced")];
      scope.presentation_name
  | None ->
      [%log.trace "selected default symbolic presentation"
        ~stage:(Delator.Field.string "forced-presentation-selection")
        ~decision:(Delator.Field.string "default")];
      default

let observe_backend_head ~declaration_name ~identity_digest ~presentation_name
    ~backend_head =
  match matching_scope declaration_name with
  | Some scope when String.equal presentation_name scope.presentation_name ->
      Mutex.lock scope.observation_lock;
      Fun.protect
        ~finally:(fun () -> Mutex.unlock scope.observation_lock)
        (fun () ->
          scope.observations <-
            { identity_digest; presentation_name; backend_head }
            :: scope.observations);
      [%log.trace "recorded forced symbolic backend observation"
        ~stage:(Delator.Field.string "forced-presentation-observation")
        ~decision:(Delator.Field.string "recorded")]
  | Some _ ->
      [%log.trace "ignored symbolic backend observation outside forced presentation"
        ~stage:(Delator.Field.string "forced-presentation-observation")
        ~decision:(Delator.Field.string "ignored")
        ~reason_class:(Delator.Field.string "presentation-mismatch")]
  | None -> ()
