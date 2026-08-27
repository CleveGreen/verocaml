type theorem_kind = Proved_lemma | Trusted_axiom
type source_role = Proved_body | Trusted_proof_body | Other_body

type theorem = {
  theorem_id : string;
  theorem_kind : theorem_kind;
  theorem_definition : Sst.function_definition;
  theorem_formals : Sst.binding list;
  theorem_trigger : Sst.expression;
  theorem_trigger_head : Sst.function_id;
  theorem_trigger_type_pattern : Parametric_type.t list;
  theorem_declaration_span : Diagnostic.span;
  theorem_witness_span : Diagnostic.span option;
}

type binding = {
  source_binding : Typedtree.value_binding;
  definition : Sst.function_definition;
}

type entry = { program : Sst.program Weak.t; theorems : theorem list }

let entries : entry list ref = ref []

let ( let* ) result continuation =
  match result with Ok value -> continuation value | Error _ as error -> error

let authenticate_typedtree ~source_file ~imports ~artifact structure =
  Typedtree_broadcast_private.authenticate ~source_file ~artifact
    ~resolves_to_marker:(Broadcast_scope_private.canonical_marker_path imports)
    structure
  |> Result.map_error (fun error ->
         Diagnostic.make
           (Diagnostic.Invalid_broadcast
              error.Typedtree_broadcast_private.message)
           (Diagnostic.span_of_location ~fallback_file:source_file
              error.Typedtree_broadcast_private.location))

let same_span left right =
  left.Diagnostic.file = right.Diagnostic.file
  && left.start_pos = right.start_pos
  && left.end_pos = right.end_pos

let validate_source_role ~scan ~binding ~recursive ~role ~span =
  let validation =
    match Typedtree_broadcast_private.declaration_id scan binding with
    | None -> Ok ()
    | Some _ ->
        let trigger_count =
          List.length
            (Typedtree_broadcast_private.trigger_locations scan binding)
        in
        if recursive then Error "broadcast declaration cannot be recursive"
        else if trigger_count <> 1 then
          Error "broadcast declaration requires exactly one outer trigger"
        else
          match role with
          | Proved_body | Trusted_proof_body -> Ok ()
          | Other_body ->
              Error
                "broadcast requires an authenticated proved or trusted proof \
                 body"
  in
  Result.map_error
    (fun message -> Diagnostic.make (Diagnostic.Invalid_broadcast message) span)
    validation

let is_declaration scan binding =
  Option.is_some (Typedtree_broadcast_private.declaration_id scan binding)

let source_bindings scan =
  List.filter
    (Fun.negate (Typedtree_broadcast_private.carrier_binding scan))

let activation_body scan expression =
  Option.bind scan
    (Fun.flip Typedtree_broadcast_private.activation_body expression)

let rec expression_uses binding expression =
  match expression.Sst.expression_desc with
  | Sst.Variable { binding = candidate; _ } -> binding.Sst.id = candidate.id
  | Sst.Forall quantifier | Sst.Exists quantifier ->
      if binding.id = quantifier.quantifier_binder.id then false
      else
        expression_uses binding quantifier.quantifier_body
        || Option.fold ~none:false ~some:(expression_uses binding)
             quantifier.quantifier_trigger
  | _ ->
      List.exists (expression_uses binding)
        (Sst_callback_private.expression_children expression)

let trigger_head expression =
  match expression.Sst.expression_desc with
  | Sst.Direct_call _
    when Spec_function_sst_private.application_has_lambda_head expression ->
      Error "broadcast trigger cannot have a closure-literal head"
  | Sst.Direct_call
      {
        call_form = Sst.Specification_call | Sst.Proof_call;
        callee;
        type_arguments;
        arguments = _ :: _;
        _;
      } ->
      Ok (callee, type_arguments)
  | Sst.Symbolic_application application
    when not (Symbolic_application_private.is_nullary application) ->
      let declaration =
        Symbolic_application_private.declaration application
      in
      Ok
        ( {
            Sst.function_index =
              Symbolic_application_private.declaration_index declaration;
            function_name =
              Symbolic_application_private.declaration_name declaration;
          },
          Symbolic_application_private.type_arguments application )
  | _ ->
      Error
        "broadcast trigger must be one supported non-nullary logical application"

let find_trigger locations definition =
  let trigger_spans =
    List.map
      (Diagnostic.span_of_location ~fallback_file:definition.Sst.span.file)
      locations
  in
  let found = ref [] in
  let rec visit (expression : Sst.expression) =
    if List.exists (same_span expression.Sst.span) trigger_spans then
      found := expression :: !found;
    match expression.expression_desc with
    | Sst.Forall _ | Sst.Exists _ -> ()
    | _ -> List.iter visit (Sst_callback_private.expression_children expression)
  in
  List.iter
    (fun (clause : Sst.predicate_clause) ->
      visit clause.Sst.predicate.expression)
    definition.contracts.requires;
  List.iter
    (fun (clause : Sst.ensures_clause) -> visit clause.Sst.predicate.expression)
    definition.contracts.ensures;
  match List.rev !found with
  | [ trigger ] -> Ok trigger
  | [] -> Error "broadcast declaration has no authenticated outer trigger"
  | _ -> Error "broadcast declaration has multiple authenticated outer triggers"

let first_order_type = function
  | Parametric_type.Int | Bool | Parameter _ | Application _ -> true
  | Unit | Tuple _ | Aggregate _ -> false

let declaration_formals definition =
  let rec loop formals = function
    | [] ->
        if formals = [] then
          Error "broadcast theorem must bind at least one value formal"
        else Ok (List.rev formals)
    | Sst.Value_parameter
        {
          pattern = { pattern_desc = Sst.Bind binding; _ };
          optional_default = None;
          _;
        }
      :: rest
      when first_order_type binding.typ ->
        loop (binding :: formals) rest
    | Sst.Value_parameter _ :: _ ->
        Error
          "broadcast theorem formals must be simple supported first-order \
           binders"
    | Sst.Callback_parameter _ :: _ ->
        Error "broadcast theorem cannot bind a callback"
  in
  loop [] definition.Sst.parameters

let kind_and_provenance definition =
  (*
    The PPX declaration marker is intentionally absent from this decision.
    Proved status requires the completed authenticated proof-body disposition;
    trusted status requires the authenticated external-body disposition in
    Proof mode together with its nonempty witness provenance.

    This is the sole proved/trusted classification point for broadcasts.  The
    preceding Typedtree scan can authorize a declaration identity and reject
    incompatible source roles, but cannot manufacture either theorem kind.
    Consequently a forged or legacy marker cannot promote an executable body,
    and ordinary trusted proof bodies remain outside broadcast registration
    unless the independent neutral declaration identity was authenticated.
  *)
  match (definition.Sst.body, definition.mode, definition.recursive) with
  | Sst.Proof_body _, Sst.Proof, false ->
      Ok (Proved_lemma, None)
  | ( Sst.Trusted_external_body
        (Sst.Authenticated_external_body { witness_span; _ }),
      Sst.Proof,
      false ) ->
      Ok (Trusted_axiom, Some witness_span)
  | _ ->
      Error
        "broadcast is not one authenticated completed or trusted external \
         proof body"

let contains_binder binder typ =
  let rec contains = function
    | Parametric_type.Parameter candidate ->
        Parametric_type.compare_binder binder candidate = 0
    | Application (_, arguments) -> List.exists contains arguments
    | Tuple arguments ->
        List.exists (fun (_, argument) -> contains argument) arguments
    | Int | Bool | Unit | Aggregate _ -> false
  in
  contains typ

let validate_type_coverage definition type_pattern =
  match
    List.find_opt
      (fun binder -> not (List.exists (contains_binder binder) type_pattern))
      definition.Sst.type_binders
  with
  | None -> Ok ()
  | Some _ ->
      Error "broadcast trigger does not determine every theorem type binder"

let theorem_of_binding scan binding =
  match Typedtree_broadcast_private.declaration_id scan binding.source_binding with
  | None -> Ok None
  | Some theorem_id ->
      let definition = binding.definition in
      let* theorem_kind, theorem_witness_span =
        kind_and_provenance definition
      in
      let* theorem_formals = declaration_formals definition in
      let* theorem_trigger =
        find_trigger
          (Typedtree_broadcast_private.trigger_locations scan
             binding.source_binding)
          definition
      in
      let* theorem_trigger_head, theorem_trigger_type_pattern =
        trigger_head theorem_trigger
      in
      let* () =
        match
          List.find_opt
            (fun formal -> not (expression_uses formal theorem_trigger))
            theorem_formals
        with
        | None -> Ok ()
        | Some formal -> Error ("broadcast trigger omits formal " ^ formal.name)
      in
      let* () =
        validate_type_coverage definition theorem_trigger_type_pattern
      in
      Ok
        (Some
           {
             theorem_id;
             theorem_kind;
             theorem_definition = definition;
             theorem_formals;
             theorem_trigger;
             theorem_trigger_head;
             theorem_trigger_type_pattern;
             theorem_declaration_span = definition.span;
             theorem_witness_span;
           })

let weak_program program =
  let weak = Weak.create 1 in
  Weak.set weak 0 (Some program);
  weak

let live_entries () =
  let live =
    List.filter
      (fun entry -> Option.is_some (Weak.get entry.program 0))
      !entries
  in
  entries := live;
  live

let entry program =
  List.find_opt
    (fun entry ->
      Option.fold ~none:false
        ~some:(fun candidate -> candidate == program)
        (Weak.get entry.program 0))
    (live_entries ())

let register_bindings ~scan ~program ~bindings =
  let* theorems =
    List.fold_left
      (fun result binding ->
        let* theorems = result in
        let* theorem = theorem_of_binding scan binding in
        Ok
          (Option.fold ~none:theorems
             ~some:(fun theorem -> theorem :: theorems)
             theorem))
      (Ok []) bindings
    |> Result.map List.rev
  in
  let declarations =
    List.map
      (fun theorem ->
        {
          Broadcast_scope_private.declaration_id = theorem.theorem_id;
          function_id = theorem.theorem_definition.function_id;
          kind =
            (match theorem.theorem_kind with
            | Proved_lemma -> Broadcast_scope_private.Proved
            | Trusted_axiom -> Trusted);
          declaration_span = theorem.theorem_declaration_span;
          witness_span = theorem.theorem_witness_span;
        })
      theorems
  in
  let groups =
    Typedtree_broadcast_private.groups scan
    |> List.map (fun (group : Typedtree_broadcast_private.group) ->
        {
          Broadcast_scope_private.group_id = group.group_id;
          group_name = group.group_name;
          targets = group.group_targets;
          span = group.group_span;
        })
  in
  let scopes =
    List.map
      (fun binding ->
        {
          Broadcast_scope_private.function_id = binding.definition.function_id;
          targets =
            Typedtree_broadcast_private.active_targets scan
              binding.source_binding;
          expressions =
            Typedtree_broadcast_private.expression_scopes scan
              binding.source_binding
            |> List.map
                 (fun (scope : Typedtree_broadcast_private.expression_scope) ->
                   {
                     Broadcast_scope_private.scope_id =
                       scope.expression_scope_id;
                     scope_span = scope.expression_scope_span;
                     targets = scope.expression_scope_targets;
                   });
        })
      bindings
  in
  let* () =
    Broadcast_scope_private.register ~program ~declarations ~groups ~scopes
  in
  entries :=
    { program = weak_program program; theorems }
    :: List.filter
         (fun entry ->
           Option.fold ~none:false
             ~some:(fun candidate -> candidate != program)
             (Weak.get entry.program 0))
         (live_entries ());
  Ok ()

let register ~source_file ~scan ~program ~sources =
  let bindings =
    List.filter_map
      (fun (source_binding, function_id) ->
        Option.map
          (fun definition -> { source_binding; definition })
          (List.find_opt
             (fun definition ->
               definition.Sst.function_id = function_id)
             program.Sst.functions))
      sources
  in
  register_bindings ~scan ~program ~bindings
  |> Result.map_error (fun message ->
         Diagnostic.make (Diagnostic.Invalid_broadcast message)
           (match program.Sst.functions with
           | definition :: _ -> definition.span
           | [] -> Diagnostic.file_span source_file))

let theorems ~program =
  Option.fold ~none:[] ~some:(fun entry -> entry.theorems) (entry program)

let find ~program ~id =
  List.find_opt
    (fun theorem -> String.equal theorem.theorem_id id)
    (theorems ~program)

let id theorem = theorem.theorem_id
let kind theorem = theorem.theorem_kind
let definition theorem = theorem.theorem_definition
let formals theorem = theorem.theorem_formals
let trigger theorem = theorem.theorem_trigger
let trigger_head theorem = theorem.theorem_trigger_head
let trigger_type_pattern theorem = theorem.theorem_trigger_type_pattern
let declaration_span theorem = theorem.theorem_declaration_span
let witness_span theorem = theorem.theorem_witness_span

let infer_type_vector theorem actual_pattern =
  if
    List.length actual_pattern
    <> List.length theorem.theorem_trigger_type_pattern
  then Ok None
  else
    let binders = theorem.theorem_definition.Sst.type_binders in
    let is_theorem_binder binder =
      List.exists
        (fun candidate -> Parametric_type.compare_binder binder candidate = 0)
        binders
    in
    let rec unify substitutions expected actual =
      match expected with
      | Parametric_type.Parameter binder when is_theorem_binder binder -> (
          match
            List.find_opt
              (fun (candidate, _) ->
                Parametric_type.compare_binder binder candidate = 0)
              substitutions
          with
          | None -> Ok (Some ((binder, actual) :: substitutions))
          | Some (_, previous) when Parametric_type.equal previous actual ->
              Ok (Some substitutions)
          | Some _ ->
              Error "broadcast trigger type occurrence has inconsistent actuals"
          )
      | Int | Bool | Unit | Aggregate _ | Parameter _ ->
          Ok
            (if Parametric_type.equal expected actual then Some substitutions
             else None)
      | Application (constructor, expected_arguments) -> (
          match actual with
          | Application (actual_constructor, actual_arguments)
            when Parametric_type.compare_constructor constructor
                   actual_constructor
                 = 0
                 && List.length expected_arguments
                    = List.length actual_arguments ->
              unify_lists substitutions expected_arguments actual_arguments
          | _ -> Ok None)
      | Tuple expected -> (
          match actual with
          | Tuple actual
            when List.length expected = List.length actual
                 && List.map fst expected = List.map fst actual ->
              unify_lists substitutions (List.map snd expected)
                (List.map snd actual)
          | _ -> Ok None)
    and unify_lists substitutions expected actual =
      match (expected, actual) with
      | [], [] -> Ok (Some substitutions)
      | expected :: expected_rest, actual :: actual_rest -> (
          let* unified = unify substitutions expected actual in
          match unified with
          | None -> Ok None
          | Some substitutions ->
              unify_lists substitutions expected_rest actual_rest)
      | _ -> Ok None
    in
    let* substitutions =
      unify_lists [] theorem.theorem_trigger_type_pattern actual_pattern
    in
    match substitutions with
    | None -> Ok None
    | Some substitutions ->
        let vector =
          List.map
            (fun binder ->
              Option.map snd
                (List.find_opt
                   (fun (candidate, _) ->
                     Parametric_type.compare_binder binder candidate = 0)
                   substitutions))
            binders
        in
        if List.for_all Option.is_some vector then
          Ok (Some (List.map Option.get vector))
        else Ok None

let schema theorem ~actual_types =
  let binders = theorem.theorem_definition.Sst.type_binders in
  if List.length binders <> List.length actual_types then
    Error "broadcast theorem type-vector arity mismatch"
  else
    let substitution = List.combine binders actual_types in
    theorem.theorem_formals
    |> List.mapi (fun ordinal (formal : Sst.binding) ->
        Logic_quantifier_private.create ~kind:Forall ~owner:theorem.theorem_id
          ~binder_index:(-(ordinal + 1))
          ~binder_type:(Parametric_type.substitute substitution formal.Sst.typ)
          ~span:theorem.theorem_declaration_span)
    |> Logic_quantifier_private.vector

let destroy program =
  entries :=
    List.filter
      (fun entry ->
        Option.fold ~none:false
          ~some:(fun candidate -> candidate != program)
          (Weak.get entry.program 0))
      (live_entries ());
  Broadcast_scope_private.destroy program
