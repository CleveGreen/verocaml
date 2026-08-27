type arrow = {
  label : string option;
  domain : Parametric_type.t;
  range : Parametric_type.t;
}

let make ~label ~domain ~range =
  Parametric_type.spec_function ~label ~domain ~range

let classify typ =
  Option.map
    (fun (label, domain, range) -> { label; domain; range })
    (Parametric_type.spec_function_view typ)

let require typ =
  match classify typ with
  | Some arrow -> Ok arrow
  | None -> Error "type is not a canonical specification-function arrow"

let equal left right =
  Option.equal String.equal left.label right.label
  && Parametric_type.equal left.domain right.domain
  && Parametric_type.equal left.range right.range

let digest arrow =
  Parametric_type.structural_identity_digest
    (make ~label:arrow.label ~domain:arrow.domain ~range:arrow.range)

let sort_name arrow = "verocaml_spec_fn_" ^ digest arrow
let apply_name arrow = "verocaml_spec_apply_" ^ digest arrow

let constructor_name ~site arrow =
  let site = Digest.to_hex (Digest.string site) in
  Printf.sprintf "verocaml_spec_closure_%s_%s" (digest arrow) site

let rec source_type typ =
  match Types.get_desc typ with
  | Types.Tarrow ((label, _, _), domain, range, _) -> (
      match label with
      | Types.Nolabel | Types.Labelled _ ->
          source_type domain && source_type range
      | Types.Optional _ | Types.Position _ -> false)
  | Types.Tvar _ | Types.Tunivar _ -> true
  | Types.Tconstr (_, arguments, _) -> List.for_all source_type arguments
  | Types.Ttuple components | Types.Tunboxed_tuple components ->
      List.for_all (fun (_, component) -> source_type component) components
  | Types.Tpoly (body, []) | Types.Tlink body | Types.Tsubst (body, _) ->
      source_type body
  | Types.Tpoly (_, _ :: _)
  | Types.Tobject _ | Types.Tfield _ | Types.Tnil | Types.Tvariant _
  | Types.Tpackage _ | Types.Tquote _ | Types.Tsplice _ | Types.Tof_kind _ ->
      false

let rec contains_source_arrow typ =
  match Types.get_desc typ with
  | Types.Tarrow _ -> true
  | Types.Tconstr (_, arguments, _) ->
      List.exists contains_source_arrow arguments
  | Types.Ttuple components | Types.Tunboxed_tuple components ->
      List.exists
        (fun (_, component) -> contains_source_arrow component)
        components
  | Types.Tpoly (body, _) | Types.Tlink body | Types.Tsubst (body, _) ->
      contains_source_arrow body
  | Types.Tvar _ | Types.Tunivar _ | Types.Tobject _ | Types.Tfield _
  | Types.Tnil | Types.Tvariant _ | Types.Tpackage _ | Types.Tquote _
  | Types.Tsplice _ | Types.Tof_kind _ ->
      false

let rec contains typ =
  Parametric_type.is_spec_function typ
  ||
  match typ with
  | Parametric_type.Application (_, arguments) -> List.exists contains arguments
  | Tuple components -> List.exists (fun (_, typ) -> contains typ) components
  | Int | Bool | Unit | Aggregate _ | Parameter _ -> false

let arrow_digest = Parametric_type.structural_identity_digest

let binder arrow =
  let digest = arrow_digest arrow in
  let owner =
    Parametric_type.owner
      ~index:(Hashtbl.hash ("spec-function:" ^ digest))
      ~name:("$verocaml.spec-function:" ^ digest)
  in
  Parametric_type.binder owner ~ordinal:0

let is_binder_for arrow candidate =
  Parametric_type.compare_binder (binder arrow) candidate = 0

let is_function_binder candidate =
  candidate.Parametric_type.ordinal = 0
  && String.starts_with ~prefix:"$verocaml.spec-function:"
       candidate.owner.owner_name

let declaration ~identity ~name ~span ~parameter_types ~result_type =
  Symbolic_application_private.declare
    ~marker_id:("$verocaml.spec-function:" ^ identity)
    ~declaration_index:(Hashtbl.hash identity) ~declaration_name:name
    ~canonical_path:("$verocaml.spec-function." ^ identity)
    ~value_uid:identity ~source_file:span.Diagnostic.file
    ~compilation_identity:"same-unit-spec-function" ~declaration_span:span
    ~type_binders:[] ~parameter_types ~result_type

let lower_source_arrow ~lower typ =
  match Types.get_desc typ with
  | Types.Tarrow ((label, _, _), domain, range, _) ->
      let label =
        match label with
        | Types.Nolabel -> None
        | Labelled label -> Some label
        | Optional _ | Position _ -> None
      in
      Some
        (match lower domain with
        | Error _ as error -> error
        | Ok domain ->
            Result.map (fun range -> make ~label ~domain ~range) (lower range))
  | Types.Tvar _ | Types.Tunivar _ | Types.Ttuple _ | Types.Tunboxed_tuple _
  | Types.Tconstr _ | Types.Tobject _ | Types.Tfield _ | Types.Tnil
  | Types.Tlink _ | Types.Tsubst _ | Types.Tvariant _ | Types.Tpoly _
  | Types.Tpackage _ | Types.Tquote _ | Types.Tsplice _ | Types.Tof_kind _ ->
      None

let imports_have_canonical_unit imports unit_name expected_crc =
  Array.exists
    (fun (import : Cmt_input.import) ->
      String.equal import.Cmt_input.unit_name unit_name
      && import.crc = Some expected_crc)
    imports

let resolves_to_spec_carrier imports path =
  imports_have_canonical_unit imports "Vero_ghost" Trusted_imports.ghost_crc
  &&
  match Path.flatten path with
  | `Ok (root, [ "spec_definition" ]) ->
      Ident.is_global_or_predef root
      && String.equal (Ident.name root) "Vero_ghost"
  | `Ok _ | `Contains_apply -> false

let canonical_module_imports imports = function
  | "Stdlib" ->
      imports_have_canonical_unit imports "Stdlib" Trusted_imports.stdlib_crc
  | "Effect" ->
      imports_have_canonical_unit imports "Stdlib" Trusted_imports.stdlib_crc
      && imports_have_canonical_unit imports "Stdlib__Effect"
           Trusted_imports.effect_crc
  | "Domain" ->
      imports_have_canonical_unit imports "Stdlib" Trusted_imports.stdlib_crc
      && imports_have_canonical_unit imports "Stdlib__Domain"
           Trusted_imports.domain_crc
  | "Vero_ghost" ->
      imports_have_canonical_unit imports "Vero_ghost" Trusted_imports.ghost_crc
  | _ -> false

let path_resolves_to ~imports path module_name value =
  canonical_module_imports imports module_name
  &&
  match Path.flatten path with
  | `Ok (root, [ component ]) ->
      Ident.is_global_or_predef root
      && (String.equal (Ident.name root) module_name
         ||
         match module_name with
         | "Effect" -> String.equal (Ident.name root) "Stdlib__Effect"
         | "Domain" -> String.equal (Ident.name root) "Stdlib__Domain"
         | _ -> false)
      && String.equal component value
  | `Ok (root, [ nested_module; component ]) ->
      Ident.is_global_or_predef root
      && String.equal (Ident.name root) "Stdlib"
      && String.equal nested_module module_name
      && String.equal component value
  | `Ok _ | `Contains_apply -> false

let lower_arrow ~logical label domain range =
  if not logical then Error Parametric_lowering_private.Higher_order_source_type
  else
    match label with
    | Types.Nolabel -> Ok (make ~label:None ~domain ~range)
    | Types.Labelled label -> Ok (make ~label:(Some label) ~domain ~range)
    | Types.Optional _ | Types.Position _ ->
        Error Parametric_lowering_private.Higher_order_source_type

type source_application =
  | Parametric_application of Parametric_adt.t
  | Aggregate_application of Sst.type_id
  | Polymorphic_application
  | Unsupported_application

let lower_compiler_type ~substitutions ~binders ~logical ~resolve typ =
  let arrow = lower_arrow ~logical in
  let rec lower_argument argument =
    Parametric_lowering_private.lower_source_type ~substitutions ~binders ~arrow
      ~application argument
  and lower_arguments descriptor lowered = function
    | [] ->
        Parametric_adt.application descriptor (List.rev lowered)
        |> Result.map_error (fun _ ->
            Parametric_lowering_private.Unsupported_source_type)
    | argument :: rest ->
        let ( let* ) result continuation =
          match result with
          | Ok value -> continuation value
          | Error _ as error -> error
        in
        let* argument = lower_argument argument in
        lower_arguments descriptor (argument :: lowered) rest
  and application path arguments =
    match resolve path with
    | Parametric_application descriptor ->
        lower_arguments descriptor [] arguments
    | Aggregate_application type_id when arguments = [] ->
        Ok (Sst.Aggregate type_id)
    | Aggregate_application _ | Polymorphic_application ->
        Error Parametric_lowering_private.Polymorphic_source_type
    | Unsupported_application ->
        Error Parametric_lowering_private.Unsupported_source_type
  in
  Parametric_lowering_private.lower_source_type ~substitutions ~binders ~arrow
    ~application typ

let generic_eligible ~kind ~type_variables ~signature =
  (kind = Parametric_function_selection_private.Spec
  || kind = Parametric_function_selection_private.Recursive_spec)
  && type_variables <> []
  && List.for_all source_type signature

type ('result, 'error) quantifier_value_services = {
  lower : Logic_quantifier_private.first_order_binder -> 'result option;
  value_error : Diagnostic.span -> string -> 'error;
}

let quantifier_value services ~typ ~span =
  match Logic_quantifier_private.first_order_binder typ with
  | Ok binder -> (
      match services.lower binder with
      | Some value -> Ok value
      | None ->
          Error
            (services.value_error span
               "quantifier generic ADT binder has no authenticated descriptor"))
  | Error message -> Error (services.value_error span message)

type 'term quantifier_body_services = {
  integer_range : unit -> 'term list;
  truth : 'term;
  conjunction : 'term -> 'term -> 'term;
  disjunction : 'term -> 'term -> 'term;
  negation : 'term -> 'term;
}

let quantifier_body services kind binder_type body =
  match binder_type with
  | Parametric_type.Int -> (
      let range =
        match services.integer_range () with
        | [] -> services.truth
        | first :: rest -> List.fold_left services.conjunction first rest
      in
      match kind with
      | Logic_quantifier_private.Forall ->
          services.disjunction (services.negation range) body
      | Logic_quantifier_private.Exists -> services.conjunction range body)
  | Parametric_type.Bool | Parametric_type.Parameter _
  | Parametric_type.Application _ | Parametric_type.Unit
  | Parametric_type.Tuple _ | Parametric_type.Aggregate _ ->
      body
