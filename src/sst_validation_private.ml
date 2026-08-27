module Owned_recursive_contents_private : sig
(** Private, process-local authentication for an exhaustive owned-recursive
    contents model.  Values of this module are not serializable authority. *)
type t
type scalar_kind =
  | Scalar_int
  | Scalar_bool
  | Scalar_unit
type source =
  | Scalar_source of Sst.field_id * scalar_kind
  | Recursive_source of Sst.field_id * Sst.expression
  | Constant_source
type case
type origin_class =
  | Closed_origin_construction
  | Exact_predecessor_transfer_construction
  | Exact_successor_reconstruction
  | Exact_child_reroot
val collect :
  program:Sst.program ->
  model:Sst.function_definition ->
  (t, string) result
val authenticate :
  program:Sst.program ->
  model:Sst.function_definition ->
  t ->
  bool
val model : t -> Sst.function_definition
val helper : t -> Sst.function_definition
val evidence : t -> Sst.same_cmt_abstraction_evidence
val owned : t -> Sst.owned_tree_prerequisite
val root_type : t -> Sst.type_id
val carrier_type : t -> Sst.type_id
val result_type : t -> Sst.typ
val root_formal : t -> Sst.binding
val helper_formal : t -> Sst.binding
val root_path : t -> Sst.field_id list
val cases : t -> case list
val digest : t -> string
val case_carrier_constructor : case -> Sst.constructor_id
val case_result_expression : case -> Sst.expression
val case_sources : case -> source list
val case_recursive_edges : case -> Sst.field_id list
val is_helper : t -> Sst.function_definition -> bool
val helper_for_program : Sst.program -> Sst.function_definition -> t option
val model_for_program : Sst.program -> Sst.function_definition -> t option
val authenticate_closed_origin_expression :
  program:Sst.program ->
  grammar:t ->
  caller:Sst.function_definition ->
  expression:Sst.expression ->
  (origin_class, string) result
val authenticate_successor_origin_expression :
  program:Sst.program ->
  grammar:t ->
  caller:Sst.function_definition ->
  expression:Sst.expression ->
  (origin_class, string) result
module For_testing : sig
  val set_attack : string option -> unit
end
end = struct
  type scalar_kind = Scalar_int | Scalar_bool | Scalar_unit
type source =
  | Scalar_source of Sst.field_id * scalar_kind
  | Recursive_source of Sst.field_id * Sst.expression
  | Constant_source
type case = {
  carrier_constructor : Sst.constructor_id;
  result_expression : Sst.expression;
  sources : source list;
  recursive_edges : Sst.field_id list;
}
type t = {
  issuer : unit ref;
  program : Sst.program;
  program_snapshot : string;
  model_definition : Sst.function_definition;
  model_snapshot : string;
  helper_definition : Sst.function_definition;
  helper_snapshot : string;
  abstraction : Sst.same_cmt_abstraction_evidence;
  owned_tree : Sst.owned_tree_prerequisite;
  owned_root_type : Sst.type_id;
  carrier : Sst.type_id;
  result : Sst.typ;
  model_formal : Sst.binding;
  recursive_formal : Sst.binding;
  model_root_path : Sst.field_id list;
  mappings : case list;
  grammar_digest : string;
}
type origin_class =
  | Closed_origin_construction
  | Exact_predecessor_transfer_construction
  | Exact_successor_reconstruction
  | Exact_child_reroot
let issuer = ref ()
let issued = ref []
let attack = ref None
let ( let* ) result continuation =
  match result with Ok value -> continuation value | Error _ as error -> error
let same_type (left : Sst.type_id) (right : Sst.type_id) =
  left.type_index = right.type_index
  && String.equal left.type_name right.type_name
let same_function (left : Sst.function_id) (right : Sst.function_id) =
  left.function_index = right.function_index
  && String.equal left.function_name right.function_name
let snapshot value =
  Marshal.to_string value [ Marshal.No_sharing ] |> Digest.string |> Digest.to_hex
let fail format = Printf.ksprintf (fun message -> Error message) format
let find_type program type_id =
  List.find_opt
    (fun (definition : Sst.type_definition) ->
      same_type definition.type_id type_id)
    program.Sst.types
let find_function program function_id =
  List.find_opt
    (fun (definition : Sst.function_definition) ->
      same_function definition.function_id function_id)
    program.Sst.functions
let fields = function
  | Sst.Record_definition fields -> fields
  | Sst.Variant_definition constructors ->
      List.concat_map
        (fun (constructor : Sst.constructor_definition) ->
          constructor.constructor_fields)
        constructors
let deeply_immutable program typ =
  let rec loop visiting = function
    | Sst.Unit | Sst.Bool | Sst.Int -> true
    | Sst.Parameter _ | Sst.Application _ -> false
    | Sst.Tuple components ->
        List.for_all (fun (_, typ) -> loop visiting typ) components
      | Sst.Aggregate type_id -> (
        if List.exists (same_type type_id) visiting then true
        else
          match find_type program type_id with
            | Some { representation = Sst.Revealed; type_kind; _ } ->
              List.for_all
                (fun (field : Sst.field_definition) ->
                  field.field_mutability = Sst.Immutable_field
                  && field.field_modalities.uniqueness_modality
                     = Sst.Preserve_uniqueness
                  && field.field_modalities.linearity_modality
                     = Sst.Preserve_linearity
                  && loop (type_id :: visiting) field.field_type)
                (fields type_kind)
            | Some { representation = Sst.Abstract_with_evidence _; _ } | None
              ->
                false)
  in
  loop [] typ
let simple_formal = function
  | [
      Sst.Value_parameter
        {
          Sst.label = None;
          pattern =
            {
              pattern_desc =
                Sst.Bind
                  ({
                     typ = Sst.Aggregate _;
                     uniqueness = Sst.Definitely_aliased;
                     _;
                   } as binding);
              typ = Sst.Aggregate _;
              _;
            };
            optional_default = _;
        };
    ] ->
      Some binding
  | _ -> None
let abstraction_for_model program model =
  match simple_formal model.Sst.parameters with
  | Some { typ = Sst.Aggregate root; _ } -> (
      match find_type program root with
      | Some
          {
            representation =
              Sst.Abstract_with_evidence
                (Sst.Authenticated_same_cmt_abstraction evidence);
            _;
          } -> (
          match evidence.owned_tree_prerequisite with
          | Some owned when same_type owned.owned_root root ->
              Some (root, evidence, owned)
          | Some _ | None -> None)
      | Some _ | None -> None)
  | Some { typ = (Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _ | Sst.Parameter _
            | Sst.Application _ );
          _;
        }
    | None ->
      None
let field_definition program field =
  program.Sst.types
  |> List.concat_map (fun definition -> fields definition.Sst.type_kind)
  |> List.find_opt (fun candidate -> candidate.Sst.field_id = field)
let constructor_definition program constructor =
  match find_type program constructor.Sst.constructor_type with
  | Some { type_kind = Sst.Variant_definition constructors; _ } ->
      List.find_opt
        (fun candidate -> candidate.Sst.constructor_id = constructor)
        constructors
  | Some { type_kind = Sst.Record_definition _; _ } | None -> None
let constructor_fields program constructor arguments =
  let* declared =
    match constructor_definition program constructor with
    | Some declared -> Ok declared
    | None -> fail "owned-contents origin names an unknown constructor"
  in
  match (declared.Sst.constructor_fields, arguments) with
  | [], [] -> Ok []
  | ( fields,
        [ { Sst.expression_desc = Sst.Record_value { fields = values; _ }; _ } ]
      )
    when List.length fields = List.length values ->
      List.fold_left2
        (fun result (expected : Sst.field_definition) (field, value) ->
          let* fields = result in
          if field = expected.field_id && value.Sst.typ = expected.field_type
          then Ok ((field, value) :: fields)
          else
            fail
                "owned-contents origin constructor field identity or type is \
                 substituted")
        (Ok []) fields values
      |> Result.map List.rev
  | fields, values when List.length fields = List.length values ->
      List.fold_left2
        (fun result (expected : Sst.field_definition) value ->
          let* fields = result in
          if value.Sst.typ = expected.field_type then
            Ok ((expected.field_id, value) :: fields)
          else
            fail
              "owned-contents origin constructor argument type is substituted")
        (Ok []) fields values
      |> Result.map List.rev
  | _ ->
      fail
          "owned-contents origin constructor does not exactly reconstruct its \
           fields"
let result_constructor_fields program result_type constructor arguments =
  match result_type with
  | Sst.Aggregate type_id when same_type type_id constructor.Sst.constructor_type
    ->
      constructor_fields program constructor arguments
  | Sst.Application (application_constructor, application_arguments) -> (
      match Parametric_adt.find program.Sst.parametric_adts application_constructor with
      | Some descriptor
        when Parametric_adt.same_application descriptor result_type
             && same_type (Parametric_adt.type_id descriptor)
                  constructor.constructor_type -> (
          match Parametric_adt.kind descriptor with
          | Parametric_adt.Record _ ->
              fail "owned-contents application result cannot be a record"
          | Parametric_adt.Variant constructors -> (
              match
                List.find_opt
                  (fun candidate ->
                    candidate.Parametric_adt.constructor_index
                    = constructor.constructor_index
                    && String.equal candidate.constructor_name
                         constructor.constructor_name)
                  constructors
              with
              | None ->
                  fail
                    "owned-contents application constructor identity is \
                     substituted"
              | Some declared
                when
                  List.length declared.constructor_fields
                  = List.length arguments ->
                  List.fold_left2
                    (fun result (index, field) argument ->
                      let* fields = result in
                      if field.Parametric_adt.field_index <> index then
                        fail
                          "owned-contents application constructor field \
                           identity is substituted"
                      else
                        let* expected =
                          Parametric_adt.instantiate_field_by_index descriptor
                            application_arguments
                            ~constructor_index:
                              (Some constructor.constructor_index)
                            ~field_index:index
                          |> Result.map_error (fun message -> message)
                        in
                        if argument.Sst.typ = expected then
                          Ok
                            ( ( {
                                  Sst.field_owner =
                                    Sst.Constructor_owner constructor;
                                  field_index = index;
                                  field_name = field.field_name;
                                },
                                argument )
                              :: fields )
                        else
                          fail
                            "owned-contents application constructor field type \
                             is substituted")
                    (Ok [])
                    (List.mapi
                       (fun index field -> (index, field))
                       declared.constructor_fields)
                    arguments
                  |> Result.map List.rev
              | Some _ ->
                  fail
                    "owned-contents application constructor fields are \
                     incomplete or ambiguous"))
      | Some _ | None ->
          fail
            "owned-contents application result has no exact authenticated \
             descriptor")
  | Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _ | Sst.Parameter _
  | Sst.Aggregate _ ->
      fail "owned-contents result constructor has a substituted result type"
let model_call program model formal =
  let rec path acc expression =
    match expression.Sst.expression_desc with
    | Sst.Variable { binding; _ } when binding.id = formal.Sst.id ->
        Ok (List.rev acc, formal.typ)
    | Sst.Field_read { record; field } ->
        let* path, owner = path (field :: acc) record in
        let* definition =
          match field_definition program field with
          | Some definition -> Ok definition
          | None -> fail "owned-contents root path has an unknown field"
        in
        let owner_type =
          match field.field_owner with
          | Sst.Record_owner type_id -> type_id
          | Sst.Constructor_owner constructor -> constructor.constructor_type
        in
        if owner <> Sst.Aggregate owner_type
           || definition.field_type <> expression.typ
        then fail "owned-contents root path has a substituted owner or type"
        else Ok (path, expression.typ)
    | _ ->
        fail
            "owned-contents public model must pass one exact rooted projection \
             to its helper"
  in
  match model.Sst.body with
  | Sst.Spec_definition
      {
        stage = Sst.Logical;
        expression =
          {
            expression_desc =
              Sst.Direct_call
                {
                  call_form = Sst.Specification_call;
                  callee;
                  arguments = [ Sst.Value_argument { label = None; value = actual } ];
                  recursive = false;
                  _;
                };
            _;
          };
      } ->
      let* path, typ = path [] actual in
      (match typ with
      | Sst.Aggregate carrier -> Ok (callee, path, carrier)
      | Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _ | Sst.Parameter _
      | Sst.Application _ ->
          fail "owned-contents helper carrier is not an aggregate")
  | _ ->
      fail
        "owned-contents public model must be one exact direct private-helper call"
type binding_source =
  | Field of Sst.field_id * Sst.typ
let scalar_kind = function
  | Sst.Int -> Some Scalar_int
  | Sst.Bool -> Some Scalar_bool
  | Sst.Unit -> Some Scalar_unit
  | Sst.Tuple _ | Sst.Aggregate _ | Sst.Parameter _ | Sst.Application _ -> None
let collect_case program owned helper result_type carrier (case : Sst.case) =
  if Option.is_some case.case_guard then
    fail "owned-contents carrier cases cannot have guards"
  else
    match case.case_pattern.pattern_desc with
    | Sst.Constructor_pattern (constructor, arguments) ->
        let* declared =
          match constructor_definition program constructor with
          | Some declared
            when same_type constructor.constructor_type carrier ->
              Ok declared
          | Some _ | None ->
              fail "owned-contents carrier constructor identity is substituted"
        in
        let* environment =
          match (declared.constructor_fields, arguments) with
          | [], [] -> Ok []
          | fields, [ { pattern_desc = Sst.Record_pattern patterns; _ } ]
            when List.length fields = List.length patterns ->
              List.fold_left2
                (fun result (field, (pattern : Sst.pattern))
                     (expected : Sst.field_definition) ->
                  let* environment = result in
                  if field <> expected.Sst.field_id
                     || pattern.Sst.typ <> expected.field_type
                  then
                    fail
                        "owned-contents carrier pattern field identity or type \
                         is substituted"
                  else
                    match pattern.pattern_desc with
                    | Sst.Bind binding
                      when binding.typ = expected.field_type ->
                        Ok ((binding.id, Field (field, binding.typ)) :: environment)
                    | Sst.Wildcard ->
                        fail
                          "owned-contents carrier fields cannot be omitted"
                    | _ ->
                        fail
                            "owned-contents carrier fields require exact \
                             first-order bindings")
                (Ok []) patterns fields
          | _ ->
              fail
                  "owned-contents carrier constructor payload does not exactly \
                   bind its fields"
        in
        let sources = ref [] in
        let rec output ~root expression =
          match expression.Sst.expression_desc with
          | Sst.Int_constant _ | Sst.Bool_constant _ | Sst.Unit_constant ->
              sources := Constant_source :: !sources;
              Ok ()
          | Sst.Variable { binding; _ } -> (
              match List.assoc_opt binding.id environment with
                | Some (Field (field, typ)) when typ = expression.typ -> (
                  match scalar_kind typ with
                  | Some kind ->
                      sources := Scalar_source (field, kind) :: !sources;
                      Ok ()
                  | None ->
                      fail
                          "owned-contents result cannot return or transfer its \
                           mutable carrier")
              | Some (Field _) | None ->
                  fail
                      "owned-contents result contains a copied, stale, or \
                       substituted field")
          | Sst.Constructor_value { constructor; arguments } ->
                if (not root) && expression.typ = result_type then
                fail
                    "owned-contents recursive result position is replaced by a \
                     constant construction"
              else if
                (match expression.typ with
                | Sst.Aggregate type_id ->
                    not
                      (deeply_immutable program expression.typ
                      && same_type type_id constructor.constructor_type)
                | Sst.Application _ -> expression.typ <> result_type
                | Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _
                | Sst.Parameter _ ->
                    true)
              then
                fail
                    "owned-contents result constructor has a wrong or mutable \
                     result type"
                else
                let* declared_arguments =
                  result_constructor_fields program expression.typ constructor
                    arguments
                in
                if List.length declared_arguments <> List.length arguments
                then
                  fail
                      "owned-contents result constructor fields are incomplete \
                       or ambiguous"
                else
                  List.fold_left
                    (fun result argument ->
                      let* () = result in
                      output ~root:false argument)
                      (Ok ()) arguments
          | Sst.Record_value { record_type; fields } ->
                if (not root) && expression.typ = result_type then
                fail
                    "owned-contents recursive result position is replaced by a \
                     constant record"
              else if
                expression.typ <> Sst.Aggregate record_type
                || not (deeply_immutable program expression.typ)
              then
                fail
                    "owned-contents result record has a wrong or mutable \
                     result type"
                else
                let* declared =
                  match find_type program record_type with
                  | Some
                      {
                        representation = Sst.Revealed;
                        type_kind = Sst.Record_definition declared;
                        _;
                      } ->
                      Ok declared
                    | Some { representation = Sst.Abstract_with_evidence _; _ }
                    | Some { type_kind = Sst.Variant_definition _; _ }
                  | None ->
                      fail
                          "owned-contents result record identity is not \
                           exactly revealed"
                in
                if List.length declared <> List.length fields then
                  fail
                      "owned-contents result record fields are incomplete or \
                       ambiguous"
                else
                  List.fold_left2
                    (fun result (expected : Sst.field_definition)
                         (field, value) ->
                      let* () = result in
                      if
                        field <> expected.field_id
                        || value.Sst.typ <> expected.field_type
                      then
                        fail
                            "owned-contents result record field identity, \
                             order, or type is substituted"
                      else output ~root:false value)
                      (Ok ()) declared fields
          | Sst.Tuple_value values ->
              if not (deeply_immutable program expression.typ) then
                fail "owned-contents result tuple is not deeply immutable"
              else
                List.fold_left
                  (fun result (_, value) ->
                    let* () = result in
                    output ~root:false value)
                  (Ok ()) values
          | Sst.Direct_call
              {
                call_form = Sst.Specification_call;
                callee;
                arguments = [ Sst.Value_argument { label = None; value = actual } ];
                recursive = true;
                  _;
              }
              when same_function callee helper.Sst.function_id
              && expression.typ = result_type -> (
              match actual.expression_desc with
              | Sst.Variable { binding; _ } -> (
                  match List.assoc_opt binding.id environment with
                  | Some (Field (field, Sst.Aggregate child))
                      when same_type child carrier
                           && List.exists (( = ) field)
                                owned.Sst.recursive_edges ->
                        sources :=
                          Recursive_source (field, expression) :: !sources;
                      Ok ()
                  | Some _ | None ->
                      fail
                          "owned-contents recursion does not consume an exact \
                           descriptor child")
              | _ ->
                  fail
                      "owned-contents recursion must pass one exact \
                       direct-child binding")
          | Sst.Direct_call _ ->
              fail
                  "owned-contents grammar forbids indirect, mutual, helper, or \
                   forged recursion"
          | Sst.Callback_call _ | Sst.Callback_requires _ | Sst.Callback_ensures _
          | Sst.Symbolic_application _
          | Sst.Forall _ | Sst.Exists _
          | Sst.Field_read _ | Sst.Match _ | Sst.Let _ | Sst.If _
          | Sst.Compare _ | Sst.Boolean_not _ | Sst.Boolean_binary _
          | Sst.Checked_arithmetic _ | Sst.Sequence _ | Sst.Old _
          | Sst.Field_write _ | Sst.Shared_scalar_field_write _
          | Sst.Owned_tree_nested_write _ | Sst.Owned_tree_rebase _
          | Sst.Let_mutable _ | Sst.Mutable_read _ | Sst.Mutable_write _
            | Sst.Reveal _ | Sst.Reveal_with_fuel _ | Sst.Use_type_invariant _
            | Sst.Local_assert _ | Sst.Proof_region _ | Sst.Optional_absent
            | Sst.Optional_present _ | Sst.Optional_forward _ ->
              fail
                  "owned-contents grammar permits only exact immutable \
                   construction from mapped fields"
        in
        let* () =
          if case.case_body.typ = result_type then
            output ~root:true case.case_body
          else fail "owned-contents case has a substituted result type"
        in
        let sources = List.rev !sources in
        let recursive_edges =
          List.filter_map
            (function Recursive_source (field, _) -> Some field | _ -> None)
            sources
        and scalar_fields =
          List.filter_map
            (function Scalar_source (field, _) -> Some field | _ -> None)
            sources
        in
        let declared_recursive =
          List.filter
            (fun field ->
              List.exists
                (fun declared -> declared.Sst.field_id = field)
                declared.constructor_fields)
            owned.recursive_edges
        and declared_scalars =
          List.filter_map
            (fun field ->
              if Option.is_some (scalar_kind field.Sst.field_type) then
                Some field.field_id
              else None)
            declared.constructor_fields
        in
        if List.sort compare recursive_edges
           <> List.sort compare declared_recursive
           || List.length recursive_edges
              <> List.length (List.sort_uniq compare recursive_edges)
        then
          fail
              "owned-contents case omits, duplicates, substitutes, or reorders \
               a recursive descriptor edge"
        else if
          List.sort compare scalar_fields <> List.sort compare declared_scalars
          || List.length scalar_fields
             <> List.length (List.sort_uniq compare scalar_fields)
        then
          fail
              "owned-contents case omits, duplicates, or substitutes a scalar \
               payload read"
        else
          Ok
            {
              carrier_constructor = constructor;
              result_expression = case.case_body;
              sources;
              recursive_edges;
            }
    | _ ->
        fail "owned-contents helper requires exact exhaustive constructor cases"
let collect ~program ~model =
  match !attack with
  | Some "forged-source" -> fail "owned-contents retained-source identity is forged"
  | Some _ | None ->
      let* root, abstraction, owned =
        match abstraction_for_model program model with
        | Some value -> Ok value
        | None ->
            fail
                "owned-contents model lacks exact same-CMT owned-root \
                 abstraction evidence"
      in
      let* model_formal =
        match simple_formal model.parameters with
        | Some formal -> Ok formal
        | None -> fail "owned-contents model formal is not one exact @read root"
      in
      let* helper_id, root_path, carrier =
        model_call program model model_formal
      in
      let* helper =
        match find_function program helper_id with
        | Some helper -> Ok helper
        | None -> fail "owned-contents helper is not local to the exact program"
      in
      let* helper_formal =
        match simple_formal helper.parameters with
        | Some formal when formal.typ = Sst.Aggregate carrier -> Ok formal
        | Some _ | None ->
            fail "owned-contents helper formal has a substituted carrier"
      in
      let* result =
        if model.result_type <> helper.result_type then
          fail "owned-contents model/helper result type is substituted"
        else
          match model.result_type with
          | Sst.Aggregate _ as result when deeply_immutable program result ->
              Ok result
          | Sst.Application (constructor, _) as application -> (
              match Parametric_adt.find program.parametric_adts constructor with
              | Some descriptor
                when
                  Parametric_adt.same_application descriptor application
                  && Parametric_adt.deeply_immutable_instance
                       program.parametric_adts application -> (
                  match
                    Parametric_rank_domain_private.derive_application ~program
                      ~span:model.span application
                  with
                  | Ok domain
                    when
                      Parametric_rank_domain_private.authenticate ~program
                        domain
                      && Parametric_rank_domain_private.immutable domain
                      &&
                      (match
                         Parametric_rank_domain_private.application domain
                       with
                      | Some exact ->
                          Parametric_type.equal exact application
                      | None -> false) ->
                      Ok application
                  | Ok _ | Error _ ->
                      fail
                        "owned-contents application result lacks an exact \
                         immutable rank capability")
              | Some _ | None ->
                  fail
                    "owned-contents application result lacks an exact \
                     immutable descriptor")
          | Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _ | Sst.Parameter _
          | Sst.Aggregate _ ->
            fail
                "owned-contents model/helper result is substituted or not \
                 deeply immutable"
      in
      let* body, constructors =
        match (helper.body, find_type program carrier) with
        | ( Sst.Recursive_spec_definition
              {
                body = { stage = Sst.Logical; expression };
                visibility = `Opaque;
                provenance = Sst.Authenticated_typedtree _;
              },
            Some
              {
                representation = Sst.Revealed;
                type_kind = Sst.Variant_definition constructors;
                _;
              } )
            when helper.recursive && constructors <> []
            && Typedtree_adapter_private.Public
                    .authenticate_recursive_specification_in_program ~program
                      ~definition:helper ->
            Ok (expression, constructors)
        | _ ->
            fail
                "owned-contents helper lacks exact opaque direct retained \
                 Typedtree/CMT recursion"
      in
      let* () =
        match helper.contracts.decreases with
        | [
            {
              predicate =
                {
                  expression =
                   { expression_desc = Sst.Variable { binding; _ }; _ };
                  _;
                };
              _;
            };
          ]
          when binding.id = helper_formal.id ->
            Ok ()
        | _ ->
            fail
                "owned-contents helper decrease must name its exact carrier \
                 formal"
      in
      let* cases =
        match body.expression_desc with
        | Sst.Match
            ( {
                expression_desc = Sst.Variable { binding; _ };
                typ = Sst.Aggregate scrutinee;
                _;
              },
              cases )
          when binding.id = helper_formal.id && same_type scrutinee carrier ->
            if List.length cases <> List.length constructors then
              fail "owned-contents helper is nonexhaustive"
            else
              List.fold_left
                (fun accumulated case ->
                  let* mappings = accumulated in
                  let* mapping =
                    collect_case program owned helper result carrier case
                  in
                  Ok (mapping :: mappings))
                (Ok []) cases
              |> Result.map List.rev
        | _ ->
            fail
                "owned-contents helper must be one exhaustive match on its \
                 exact carrier"
      in
      let seen =
        List.map (fun case -> case.carrier_constructor) cases
        |> List.sort compare
      and declared =
        List.map
          (fun constructor -> constructor.Sst.constructor_id)
          constructors
        |> List.sort compare
      in
      if
        seen <> declared
        || List.length seen <> List.length (List.sort_uniq compare seen)
      then fail "owned-contents helper constructor mapping is ambiguous"
      else
        let grammar_digest =
          snapshot
            ( root,
              carrier,
              result,
              model.function_id,
              helper.function_id,
              root_path,
              List.map
                (fun case ->
                  ( case.carrier_constructor,
                    case.recursive_edges,
                    snapshot case.result_expression ))
                cases )
        in
        let value =
          {
            issuer;
            program;
            program_snapshot = Sst.to_string program;
            model_definition = model;
            model_snapshot = snapshot model;
            helper_definition = helper;
            helper_snapshot = snapshot helper;
            abstraction;
            owned_tree = owned;
            owned_root_type = root;
            carrier;
            result;
            model_formal;
            recursive_formal = helper_formal;
            model_root_path = root_path;
            mappings = cases;
            grammar_digest;
          }
        in
        issued :=
          value
          :: List.filter
               (fun candidate -> candidate.program != program)
               !issued;
        Ok value
let authenticate ~program ~model value =
  value.issuer == issuer
  && value.program == program
  && value.model_definition == model
  && String.equal value.program_snapshot (Sst.to_string program)
  && String.equal value.model_snapshot (snapshot model)
  && String.equal value.helper_snapshot (snapshot value.helper_definition)
  && List.exists (( == ) value) !issued
let model value = value.model_definition
let helper value = value.helper_definition
let evidence value = value.abstraction
let owned value = value.owned_tree
let root_type value = value.owned_root_type
let carrier_type value = value.carrier
let result_type value = value.result
let root_formal value = value.model_formal
let helper_formal value = value.recursive_formal
let root_path value = value.model_root_path
let cases value = value.mappings
let digest value = value.grammar_digest
let case_carrier_constructor value = value.carrier_constructor
let case_result_expression value = value.result_expression
let case_sources value = value.sources
let case_recursive_edges value = value.recursive_edges
let is_helper value definition = value.helper_definition == definition
let helper_for_program program definition =
  List.find_opt
    (fun value ->
      authenticate ~program ~model:value.model_definition value
      && is_helper value definition)
    !issued
let model_for_program program definition =
  List.find_opt
    (fun value -> authenticate ~program ~model:definition value)
    !issued
let expression_children = Sst_callback_private.expression_children
let body_expression = function
  | Sst.Checked_exec { body; _ } | Sst.Spec_definition body ->
      Some body.Sst.expression
  | Sst.Proof_body { body; _ } -> Some body.expression
  | Sst.Recursive_spec_definition { body; _ } -> Some body.expression
  | Sst.External_specification _ | Sst.Trusted_external_spec_target _
  | Sst.Trusted_external_body _ | Sst.Symbolic_declaration _ ->
      None
let retained_expression caller target =
  let rec contains expression =
    expression == target
    || List.exists contains (expression_children expression)
  in
  Option.fold ~none:false ~some:contains (body_expression caller.Sst.body)
let field_expression program field expression =
  match expression.Sst.expression_desc with
  | Sst.Record_value { fields; _ } ->
      List.find_map
        (fun (candidate, value) ->
          if candidate = field then Some value else None)
        fields
  | Sst.Constructor_value { constructor; arguments } -> (
      match constructor_fields program constructor arguments with
      | Ok fields -> List.assoc_opt field fields
      | Error _ -> None)
  | Sst.Int_constant _ | Sst.Bool_constant _ | Sst.Unit_constant
  | Sst.Variable _ | Sst.Tuple_value _ | Sst.Field_read _ | Sst.Match _
  | Sst.Let _ | Sst.If _ | Sst.Compare _ | Sst.Boolean_not _
  | Sst.Boolean_binary _ | Sst.Checked_arithmetic _ | Sst.Sequence _
  | Sst.Old _ | Sst.Field_write _ | Sst.Shared_scalar_field_write _
  | Sst.Owned_tree_nested_write _ | Sst.Owned_tree_rebase _
  | Sst.Let_mutable _ | Sst.Mutable_read _ | Sst.Mutable_write _
  | Sst.Direct_call _ | Sst.Callback_call _ | Sst.Callback_requires _
  | Sst.Callback_ensures _ | Sst.Reveal _ | Sst.Reveal_with_fuel _
    | Sst.Forall _ | Sst.Exists _ | Sst.Symbolic_application _
    | Sst.Use_type_invariant _ | Sst.Local_assert _ | Sst.Proof_region _
    | Sst.Optional_absent | Sst.Optional_present _ | Sst.Optional_forward _ ->
      None
let expression_at_path program expression path =
  List.fold_left
    (fun current field ->
      Option.bind current (field_expression program field))
    (Some expression) path
let scalar_construction_expression = function
  | {
      Sst.expression_desc =
        (Sst.Int_constant _ | Sst.Bool_constant _ | Sst.Unit_constant
        | Sst.Variable _);
      typ = (Sst.Int | Sst.Bool | Sst.Unit);
      _;
    } ->
      true
  | _ -> false
let case_for_constructor grammar constructor =
  List.find_opt
    (fun case -> case.carrier_constructor = constructor)
    grammar.mappings
let rec authenticate_closed_carrier program grammar expression =
  match expression.Sst.expression_desc with
  | Sst.Constructor_value { constructor; arguments }
      when same_type constructor.constructor_type grammar.carrier -> (
      let* case =
        match case_for_constructor grammar constructor with
        | Some case -> Ok case
        | None ->
            fail
                "owned-contents closed origin has an unmapped carrier \
                 constructor"
      in
      let* fields = constructor_fields program constructor arguments in
      let declared =
        match constructor_definition program constructor with
        | Some declared -> declared.constructor_fields
        | None -> assert false
      in
      List.fold_left
        (fun result (field : Sst.field_definition) ->
          let* () = result in
          let* value =
            match List.assoc_opt field.field_id fields with
            | Some value -> Ok value
            | None ->
                  fail "owned-contents closed origin omits a constructor field"
          in
          if List.mem field.field_id case.recursive_edges then
            authenticate_closed_carrier program grammar value
          else
            match scalar_kind field.field_type with
            | Some _ when scalar_construction_expression value -> Ok ()
            | Some _ ->
                fail
                  "owned-contents closed origin scalar is not one exact retained value"
            | None ->
                fail
                  "owned-contents closed origin contains a non-descriptor aggregate field")
        (Ok ()) declared)
  | _ ->
      fail
        "owned-contents closed origin is not an exact recursive carrier construction"
let exact_predecessor_read transition expression =
  match expression.Sst.expression_desc with
  | Sst.Field_read
      {
        record =
          {
            expression_desc = Sst.Variable { binding; _ };
            _;
          };
        field;
      } ->
      binding == transition.Sst.root && field = transition.target_field
  | _ -> false
let authenticate_transfer_carrier program grammar transition expression =
  let transferred = ref 0 in
  let rec walk expression =
    if exact_predecessor_read transition expression then (
      incr transferred;
      Ok ())
    else
      match expression.Sst.expression_desc with
      | Sst.Constructor_value { constructor; arguments }
          when same_type constructor.constructor_type grammar.carrier -> (
          let* case =
            match case_for_constructor grammar constructor with
            | Some case -> Ok case
            | None ->
                fail
                    "owned-contents transfer has an unmapped carrier \
                     constructor"
          in
          let* fields = constructor_fields program constructor arguments in
          let declared =
            match constructor_definition program constructor with
            | Some declared -> declared.constructor_fields
            | None -> assert false
          in
          List.fold_left
            (fun result (field : Sst.field_definition) ->
              let* () = result in
              let* value =
                match List.assoc_opt field.field_id fields with
                | Some value -> Ok value
                | None ->
                    fail "owned-contents transfer omits a constructor field"
              in
              if List.mem field.field_id case.recursive_edges then walk value
              else
                match scalar_kind field.field_type with
                | Some _ when scalar_construction_expression value -> Ok ()
                | Some _ ->
                    fail
                      "owned-contents transfer scalar is not one exact retained value"
                | None ->
                    fail
                      "owned-contents transfer contains a non-descriptor aggregate field")
            (Ok ()) declared)
      | _ ->
          fail
            "owned-contents transfer child is neither closed nor the exact predecessor"
  in
  let* () = walk expression in
  if !transferred = 1 then Ok ()
  else
    fail
      "owned-contents transfer must consume the exact predecessor exactly once"
let root_target grammar transition =
  match grammar.model_root_path with
  | [ field ] ->
      field = transition.Sst.target_field
      && transition.cursor = None
  | [] | _ :: _ :: _ -> false
let scalar_descriptor_field grammar field =
  grammar.mappings
  |> List.exists (fun case ->
         case.sources
         |> List.exists (function
              | Scalar_source (candidate, _) -> candidate = field
              | Recursive_source _ | Constant_source -> false))
let authenticate_successor_expression program grammar expression =
  match expression.Sst.expression_desc with
  | Sst.Field_write { value; transition = Some transition; _ }
      when transition.rhs_provenance = Sst.Ground_owned_tree_value
      && root_target grammar transition -> (
      match authenticate_closed_carrier program grammar value with
      | Ok () -> Ok Exact_successor_reconstruction
      | Error _ ->
          let* () =
            authenticate_transfer_carrier program grammar transition value
          in
          Ok Exact_predecessor_transfer_construction)
  | Sst.Owned_tree_rebase { transition } -> (
      match transition.rhs_provenance with
      | Sst.Guarded_descendant_move cursor
        when
          root_target grammar transition
          &&
          (match cursor.guarded_path with
          | Sst.Owned_tree_field field :: _ ->
              field = transition.target_field
          | [] | Sst.Owned_tree_constructor _ :: _ -> false) ->
          Ok Exact_child_reroot
      | Sst.Guarded_descendant_move _ | Sst.Ground_owned_tree_value ->
          fail
            "owned-contents re-root does not select the exact predecessor child")
  | Sst.Owned_tree_nested_write { transition; value } -> (
      match transition.cursor with
      | Some cursor
        when
          (match (grammar.model_root_path, cursor.guarded_path) with
          | root_field :: _, Sst.Owned_tree_field reached :: _ ->
              root_field = reached
          | [], _ | _ :: _, [] | _ :: _, Sst.Owned_tree_constructor _ :: _ ->
              false)
          &&
          (List.mem transition.target_field grammar.owned_tree.recursive_edges
          || scalar_descriptor_field grammar transition.target_field) ->
          if List.mem transition.target_field grammar.owned_tree.recursive_edges
          then
            let* () =
              authenticate_closed_carrier program grammar value
            in
            Ok Exact_successor_reconstruction
          else if scalar_construction_expression value then
            Ok Exact_successor_reconstruction
          else
            fail
              "owned-contents nested scalar reconstruction is not exact"
      | Some _ | None ->
          fail
            "owned-contents nested reconstruction has no exact predecessor path")
  | Sst.Field_write { transition = None; _ }
  | Sst.Field_write { transition = Some _; _ }
  | Sst.Shared_scalar_field_write _ | Sst.Int_constant _
  | Sst.Bool_constant _ | Sst.Unit_constant | Sst.Variable _
  | Sst.Tuple_value _ | Sst.Record_value _ | Sst.Constructor_value _
  | Sst.Field_read _ | Sst.Match _ | Sst.Let _ | Sst.If _
  | Sst.Compare _ | Sst.Boolean_not _ | Sst.Boolean_binary _
  | Sst.Checked_arithmetic _ | Sst.Sequence _ | Sst.Old _
  | Sst.Let_mutable _ | Sst.Mutable_read _ | Sst.Mutable_write _
  | Sst.Direct_call _ | Sst.Callback_call _ | Sst.Callback_requires _
  | Sst.Callback_ensures _ | Sst.Reveal _ | Sst.Reveal_with_fuel _
  | Sst.Forall _ | Sst.Exists _ | Sst.Symbolic_application _
  | Sst.Use_type_invariant _ | Sst.Local_assert _ | Sst.Proof_region _
  | Sst.Optional_absent | Sst.Optional_present _ | Sst.Optional_forward _ ->
      fail
        "owned-contents successor origin is not an exact retained owned-tree transition"
let authenticate_closed_origin_expression ~program ~grammar ~caller
    ~expression =
  if
    not (authenticate ~program ~model:grammar.model_definition grammar)
    || not (retained_expression caller expression)
  then
    fail
      "owned-contents closed origin is not the exact retained same-program expression"
  else
    let* carrier =
      match expression_at_path program expression grammar.model_root_path with
      | Some carrier -> Ok carrier
      | None ->
          fail
            "owned-contents closed origin does not reconstruct the exact model root path"
    in
    let* () = authenticate_closed_carrier program grammar carrier in
    Ok Closed_origin_construction
let authenticate_successor_origin_expression ~program ~grammar ~caller
    ~expression =
  if
      (not (authenticate ~program ~model:grammar.model_definition grammar))
    || not (retained_expression caller expression)
  then
    fail
        "owned-contents successor is not the exact retained same-program \
         expression"
  else authenticate_successor_expression program grammar expression
module For_testing = struct
  let set_attack value = attack := value
end
end
module Public = struct
let program_mutator_for_testing = ref None
let validation_boundary_observer_for_testing = ref None
type error_kind =
  | Unsupported_policy of Sst.verification_policy
  | Duplicate_type_id of Sst.type_id
  | Duplicate_function_id of Sst.function_id
  | Duplicate_binding_id of int
  | Unknown_type_id of Sst.type_id
  | Unknown_function_id of Sst.function_id
  | Conflicting_identity of string
  | Invalid_clause of string
  | Invalid_body of string
  | Invalid_call of string
  | Invalid_recursive_marker of string
  | Invalid_unique_return of string
  | Invalid_target_link of string
  | Forged_abstract_evidence of string
  | Forged_rank_domain of string
  | Invalid_instance_mode of string
  | Invalid_finite_requirement of string
  | Unbound_binding of Sst.binding
  | Malformed_expression of string
type error = {
  function_id : Sst.function_id option;
  span : Diagnostic.span;
  kind : error_kind;
}
let ( let* ) result f = match result with Ok value -> f value | Error _ as e -> e
let fail ?function_id span kind = Error { function_id; span; kind }
let same_type_id (left : Sst.type_id) (right : Sst.type_id) =
  left.type_index = right.type_index
  && String.equal left.type_name right.type_name
let same_function_id (left : Sst.function_id) (right : Sst.function_id) =
  left.function_index = right.function_index
  && String.equal left.function_name right.function_name
let rec iter_result f = function
  | [] -> Ok ()
  | value :: rest ->
      let* () = f value in
      iter_result f rest
let rec fold_result f state = function
  | [] -> Ok state
  | value :: rest ->
      let* state = f state value in
      fold_result f state rest
let validate_policy span policy =
  match Sst_policy.resolve policy with
  | Ok _ -> Ok ()
  | Error (Sst_policy.Unsupported policy) ->
      fail span (Unsupported_policy policy)
let lookup_type types id =
  List.find_opt
    (fun (definition : Sst.type_definition) ->
      same_type_id definition.type_id id)
    types
let rec validate_type_reference types function_id span = function
  | Sst.Unit | Sst.Bool | Sst.Int -> Ok ()
  | Sst.Tuple components ->
      iter_result
        (fun (_, typ) -> validate_type_reference types function_id span typ)
        components
  | Sst.Aggregate id ->
      if Option.is_some (lookup_type types id) then Ok ()
      else fail ?function_id span (Unknown_type_id id)
    | Sst.Parameter _ -> Ok ()
    | Sst.Application (_, arguments) ->
        iter_result (validate_type_reference types function_id span) arguments
let add_binding function_id seen (binding : Sst.binding) =
  if List.mem binding.id seen then
    fail ~function_id binding.span (Duplicate_binding_id binding.id)
  else Ok (binding.id :: seen)
let rec pattern_bindings function_id types seen (pattern : Sst.pattern) =
  let* () =
    validate_type_reference types (Some function_id) pattern.span pattern.typ
  in
  match pattern.pattern_desc with
  | Sst.Wildcard | Sst.Int_pattern _ | Sst.Bool_pattern _ | Sst.Unit_pattern ->
      Ok seen
  | Sst.Bind binding ->
      let* () =
        validate_type_reference types (Some function_id) binding.span binding.typ
      in
      if binding.typ <> pattern.typ then
        fail ~function_id pattern.span
          (Malformed_expression "binding pattern type does not match binding")
      else add_binding function_id seen binding
  | Sst.Owned_tree_cursor_pattern cursor ->
      let binding = cursor.cursor_binding in
      let* () =
        validate_type_reference types (Some function_id) binding.span binding.typ
      in
      if binding.typ <> pattern.typ then
        fail ~function_id pattern.span
          (Malformed_expression "owned-tree cursor pattern type mismatch")
      else add_binding function_id seen binding
  | Sst.Tuple_pattern components ->
      fold_result
        (fun seen (_, nested) ->
          pattern_bindings function_id types seen nested)
        seen components
  | Sst.Record_pattern fields ->
      fold_result
        (fun seen (_, nested) ->
          pattern_bindings function_id types seen nested)
        seen fields
  | Sst.Constructor_pattern (_, arguments) ->
      fold_result
        (pattern_bindings function_id types)
        seen arguments
  | Sst.Or_pattern (left, right) ->
      let* left_seen = pattern_bindings function_id types seen left in
      pattern_bindings function_id types left_seen right
let expression_children expression =
  match expression.Sst.expression_desc with
  | Sst.Proof_region _ -> []
  | _ -> Sst_callback_private.expression_children expression
let rec contains_shared_scalar_write (expression : Sst.expression) =
  match expression.expression_desc with
  | Sst.Shared_scalar_field_write _ -> true
  | _ ->
      List.exists contains_shared_scalar_write
        (expression_children expression)
let function_contains_shared_scalar_write
    (definition : Sst.function_definition) =
  match definition.body with
    | Sst.Checked_exec { body; _ } ->
        contains_shared_scalar_write body.expression
    | Sst.Spec_definition _ | Sst.Recursive_spec_definition _ | Sst.Proof_body _
    | Sst.External_specification _ | Sst.Trusted_external_spec_target _
    | Sst.Trusted_external_body _ | Sst.Symbolic_declaration _ ->
      false
let authenticated_shared_invariant_operation types
    (definition : Sst.function_definition) =
  List.exists
    (fun (type_definition : Sst.type_definition) ->
      match type_definition.representation with
      | Sst.Abstract_with_evidence
          (Sst.Authenticated_same_cmt_abstraction evidence) ->
          List.exists
            (fun (operation : Sst.abstract_public_operation) ->
              operation.public_role = Sst.Shared_invariant_transition
              && operation.public_function_index
                 = definition.function_id.function_index
              && String.equal operation.public_function_name
                   definition.function_id.function_name)
            evidence.public_surface
      | Sst.Revealed
      | Sst.Abstract_with_evidence
          (Sst.Incomplete_abstraction_evidence _
          | Sst.Proposed_same_cmt_abstraction _) ->
          false)
    types
let authenticated_shared_invariant_cell_operation types function_id =
  List.exists
    (fun (type_definition : Sst.type_definition) ->
      match type_definition.representation with
      | Sst.Abstract_with_evidence
          (Sst.Authenticated_same_cmt_abstraction evidence) ->
          List.exists
            (fun (operation : Sst.abstract_public_operation) ->
              operation.public_role = Sst.Shared_invariant_transition)
            evidence.public_surface
          && List.exists
               (fun (operation : Sst.abstract_public_operation) ->
                 operation.public_function_index
                   = function_id.Sst.function_index
                 && String.equal operation.public_function_name
                      function_id.function_name
                 &&
                 match operation.public_role with
                 | Sst.Abstract_constructor | Sst.Shared_invariant_transition
                 | Sst.Current_terminal_read ->
                     true
                 | Sst.Abstract_model | Sst.Abstract_invariant
                 | Sst.Terminal_read | Sst.Terminal_snapshot
                 | Sst.Unique_transition | Sst.Current_model ->
                     false)
               evidence.public_surface
      | Sst.Revealed
      | Sst.Abstract_with_evidence
          (Sst.Incomplete_abstraction_evidence _
          | Sst.Proposed_same_cmt_abstraction _) ->
          false)
    types
  let frozen_spine_for_helper types (definition : Sst.function_definition) =
  List.find_map
    (fun (type_definition : Sst.type_definition) ->
      match type_definition.representation with
      | Sst.Abstract_with_evidence
          (Sst.Authenticated_same_cmt_abstraction evidence) -> (
          match Sst.frozen_spine_prerequisite evidence with
          | Some frozen
              when same_function_id frozen.frozen_helper definition.function_id
              && same_type_id frozen.frozen_root type_definition.type_id ->
              Some frozen
          | Some _ | None -> None)
      | Sst.Revealed
      | Sst.Abstract_with_evidence
          (Sst.Incomplete_abstraction_evidence _
          | Sst.Proposed_same_cmt_abstraction _) ->
          None)
    types
let authenticated_frozen_spine_helper types definition =
  Option.is_some (frozen_spine_for_helper types definition)
let authenticated_frozen_spine_logical_role types
    (definition : Sst.function_definition) =
  List.exists
    (fun (type_definition : Sst.type_definition) ->
      match type_definition.representation with
      | Sst.Abstract_with_evidence
          (Sst.Authenticated_same_cmt_abstraction evidence) -> (
          match Sst.frozen_spine_prerequisite evidence with
          | Some frozen ->
              same_function_id definition.function_id frozen.frozen_model
              || same_function_id definition.function_id
                   frozen.frozen_invariant
          | None -> false)
      | Sst.Revealed
      | Sst.Abstract_with_evidence
          (Sst.Incomplete_abstraction_evidence _
          | Sst.Proposed_same_cmt_abstraction _) ->
          false)
    types
let authenticated_frozen_spine_terminal types function_id =
  List.exists
    (fun (type_definition : Sst.type_definition) ->
      match type_definition.representation with
      | Sst.Abstract_with_evidence
          (Sst.Authenticated_same_cmt_abstraction evidence) -> (
          match Sst.frozen_spine_prerequisite evidence with
            | Some frozen -> same_function_id function_id frozen.frozen_terminal
          | None -> false)
      | Sst.Revealed
      | Sst.Abstract_with_evidence
          (Sst.Incomplete_abstraction_evidence _
          | Sst.Proposed_same_cmt_abstraction _) ->
          false)
    types
let frozen_spine_for_type types type_id =
  List.find_map
    (fun (definition : Sst.type_definition) ->
      if not (same_type_id definition.type_id type_id) then None
      else
        match definition.representation with
        | Sst.Abstract_with_evidence
            (Sst.Authenticated_same_cmt_abstraction evidence) ->
            Sst.frozen_spine_prerequisite evidence
        | Sst.Revealed
        | Sst.Abstract_with_evidence
            (Sst.Incomplete_abstraction_evidence _
            | Sst.Proposed_same_cmt_abstraction _) ->
            None)
    types
let validate_shared_invariant_client_paths types
    (functions : Sst.function_definition list) =
  let rec classify expression =
    let branch =
      match expression.Sst.expression_desc with
      | Sst.If _ | Sst.Match _ -> true
      | Sst.Int_constant _ | Sst.Bool_constant _ | Sst.Unit_constant
      | Sst.Variable _ | Sst.Tuple_value _ | Sst.Record_value _
      | Sst.Constructor_value _ | Sst.Field_read _ | Sst.Field_write _
      | Sst.Shared_scalar_field_write _ | Sst.Checked_arithmetic _
      | Sst.Boolean_not _ | Sst.Boolean_binary _ | Sst.Compare _
      | Sst.Let_mutable _ | Sst.Mutable_read _ | Sst.Mutable_write _
      | Sst.Let _ | Sst.Sequence _ | Sst.Direct_call _
      | Sst.Callback_call _ | Sst.Callback_requires _ | Sst.Callback_ensures _
      | Sst.Forall _ | Sst.Exists _ | Sst.Symbolic_application _
      | Sst.Use_type_invariant _ | Sst.Owned_tree_nested_write _
      | Sst.Owned_tree_rebase _ | Sst.Reveal _ | Sst.Reveal_with_fuel _
        | Sst.Local_assert _ | Sst.Proof_region _ | Sst.Old _
        | Sst.Optional_absent | Sst.Optional_present _ | Sst.Optional_forward _ ->
          false
    in
    let invariant_cell_call =
      match expression.expression_desc with
      | Sst.Direct_call { callee; _ } ->
          authenticated_shared_invariant_cell_operation types callee
      | Sst.Int_constant _ | Sst.Bool_constant _ | Sst.Unit_constant
      | Sst.Variable _ | Sst.Tuple_value _ | Sst.Record_value _
      | Sst.Constructor_value _ | Sst.Field_read _ | Sst.Field_write _
      | Sst.Shared_scalar_field_write _ | Sst.Checked_arithmetic _
      | Sst.Boolean_not _ | Sst.Boolean_binary _ | Sst.Compare _
      | Sst.Let_mutable _ | Sst.Mutable_read _ | Sst.Mutable_write _
      | Sst.Let _ | Sst.Sequence _ | Sst.If _ | Sst.Match _
      | Sst.Callback_call _ | Sst.Callback_requires _ | Sst.Callback_ensures _
      | Sst.Forall _ | Sst.Exists _ | Sst.Symbolic_application _
      | Sst.Use_type_invariant _ | Sst.Owned_tree_nested_write _
      | Sst.Owned_tree_rebase _ | Sst.Reveal _ | Sst.Reveal_with_fuel _
      | Sst.Local_assert _ | Sst.Proof_region _ | Sst.Old _
        | Sst.Optional_absent | Sst.Optional_present _ | Sst.Optional_forward _ ->
          false
    in
    List.fold_left
      (fun (has_branch, has_call) child ->
        let child_branch, child_call = classify child in
        (has_branch || child_branch, has_call || child_call))
      (branch, invariant_cell_call)
      (match expression.expression_desc with
      | Sst.Proof_region body -> [ body ]
      | _ -> expression_children expression)
  in
  iter_result
    (fun (definition : Sst.function_definition) ->
      match definition.body with
      | Sst.Checked_exec { body; _ } ->
          let has_branch, has_call = classify body.expression in
          if has_branch && has_call then
            fail ~function_id:definition.function_id definition.span
              (Invalid_call
                   "shared invariant-cell clients require one straight-line \
                    path")
          else Ok ()
      | Sst.Spec_definition _ | Sst.Recursive_spec_definition _
      | Sst.Proof_body _ | Sst.External_specification _
      | Sst.Trusted_external_spec_target _ | Sst.Trusted_external_body _
      | Sst.Symbolic_declaration _ ->
          Ok ())
    functions
let expected_call_form stage callee_mode =
  match (stage, callee_mode) with
  | Sst.Logical, Sst.Spec -> Some Sst.Specification_call
  | Sst.Proof_stage, Sst.Spec -> Some Sst.Specification_call
  | Sst.Proof_stage, Sst.Proof -> Some Sst.Proof_call
  | Sst.Runtime, Sst.Spec -> Some Sst.Specification_call
  | Sst.Runtime, Sst.Proof -> Some Sst.Proof_call
  | Sst.Runtime, Sst.Exec -> Some Sst.Exec_call
  | _ -> None
let fields_of_owner types = function
  | Sst.Record_owner type_id -> (
      match lookup_type types type_id with
      | Some { type_kind = Sst.Record_definition fields; _ } -> fields
      | _ -> [])
  | Sst.Constructor_owner constructor -> (
      match lookup_type types constructor.constructor_type with
      | Some { type_kind = Sst.Variant_definition constructors; _ } ->
          Option.value ~default:[]
            (List.find_map
               (fun (definition : Sst.constructor_definition) ->
                 if definition.constructor_id = constructor then
                   Some definition.constructor_fields
                 else None)
               constructors)
      | _ -> [])
let lookup_field types field =
  List.find_opt
      (fun (definition : Sst.field_definition) -> definition.field_id = field)
    (fields_of_owner types field.Sst.field_owner)
let owner_type = function
  | Sst.Record_owner type_id -> type_id
  | Sst.Constructor_owner constructor -> constructor.constructor_type
let validate_cursor_path types function_id span cursor =
  let malformed detail =
    fail ~function_id span (Malformed_expression detail)
  in
  let rec walk typ = function
    | [] ->
        if typ = cursor.Sst.cursor_binding.typ then Ok ()
        else malformed "owned-tree cursor path result type mismatch"
    | Sst.Owned_tree_field field :: rest -> (
        match (typ, lookup_field types field) with
        | Sst.Aggregate current, Some definition
          when owner_type field.field_owner = current ->
            walk definition.field_type rest
        | _ -> malformed "owned-tree cursor path contains an unknown field")
    | Sst.Owned_tree_constructor constructor :: rest -> (
        match (typ, lookup_type types constructor.constructor_type) with
          | ( Sst.Aggregate current,
              Some { type_kind = Sst.Variant_definition constructors; _ } )
          when current = constructor.constructor_type
               && List.exists
                    (fun (definition : Sst.constructor_definition) ->
                      definition.constructor_id = constructor
                      && definition.constructor_fields <> [])
                    constructors ->
            walk (Sst.Aggregate constructor.constructor_type) rest
        | _ ->
            malformed
              "owned-tree cursor path contains an unregistered constructor")
  in
  walk cursor.root.typ cursor.guarded_path
let record_reconstruction types field =
  let preserved_fields =
    fields_of_owner types field.Sst.field_owner
    |> List.filter_map (fun (definition : Sst.field_definition) ->
           if definition.field_id = field then None
           else Some definition.field_id)
  in
  Sst.Reconstruct_record
    {
      record_type = owner_type field.field_owner;
      changed_field = field;
      preserved_fields;
    }
let expected_nested_reconstruction types cursor target =
  record_reconstruction types target
  :: List.map
       (function
         | Sst.Owned_tree_constructor constructor ->
             Sst.Reconstruct_constructor constructor
         | Sst.Owned_tree_field field -> record_reconstruction types field)
       (List.rev cursor.Sst.guarded_path)
type owned_tree_version_state =
  | Known_owned_tree_version of int
  | Divergent_owned_tree_versions
let join_owned_tree_version_maps branches =
  let ids =
    branches
    |> List.concat_map (List.map fst)
    |> List.sort_uniq Int.compare
  in
  let version branch id =
    Option.value ~default:(Known_owned_tree_version 0)
      (List.assoc_opt id branch)
  in
  List.map
    (fun id ->
      let versions = List.map (fun branch -> version branch id) branches in
      match versions with
      | [] -> assert false
      | first :: rest
        when List.for_all (fun candidate -> candidate = first) rest ->
          (id, first)
      | _ -> (id, Divergent_owned_tree_versions))
    ids
let rec validate_owned_version_flow function_id versions
    (expression : Sst.expression) =
  let transition versions transition =
    let current =
      Option.value ~default:(Known_owned_tree_version 0)
        (List.assoc_opt transition.Sst.root.id versions)
    in
    if current <> Known_owned_tree_version transition.pre_version then
      fail ~function_id expression.Sst.span
        (Malformed_expression "owned-tree transition uses a stale root version")
    else
      Ok
        ( ( transition.root.id,
            Known_owned_tree_version transition.successor_version )
        :: List.remove_assoc transition.root.id versions)
  in
  match expression.Sst.expression_desc with
  | Sst.Owned_tree_nested_write { transition = owned; value } ->
      let* versions = validate_owned_version_flow function_id versions value in
      transition versions owned
  | Sst.Owned_tree_rebase { transition = owned } ->
      transition versions owned
  | Sst.Field_write { transition = Some owned; value; _ } ->
      let* versions = validate_owned_version_flow function_id versions value in
      transition versions owned
  | Sst.Sequence (left, right) ->
      let* versions = validate_owned_version_flow function_id versions left in
      validate_owned_version_flow function_id versions right
  | Sst.Let (bindings, body) ->
      let* versions =
        fold_result
          (fun versions (_, value) ->
            validate_owned_version_flow function_id versions value)
          versions bindings
      in
      validate_owned_version_flow function_id versions body
  | Sst.Let_mutable (_, initial, body) ->
      let* versions =
        validate_owned_version_flow function_id versions initial
      in
      validate_owned_version_flow function_id versions body
  | Sst.If (condition, consequent, alternative) ->
      let* versions =
        validate_owned_version_flow function_id versions condition
      in
      let* consequent_versions =
        validate_owned_version_flow function_id versions consequent
      in
      let* alternative_versions =
        match alternative with
        | None -> Ok versions
        | Some alternative ->
            validate_owned_version_flow function_id versions alternative
      in
      Ok
        (join_owned_tree_version_maps
           [ consequent_versions; alternative_versions ])
  | Sst.Match (scrutinee, cases) ->
      let* versions =
        validate_owned_version_flow function_id versions scrutinee
      in
      let* case_versions =
        fold_result
          (fun case_versions case ->
            let* after_guard =
              match case.Sst.case_guard with
              | None -> Ok versions
              | Some guard ->
                  validate_owned_version_flow function_id versions guard
            in
            let* case_versions' =
              validate_owned_version_flow function_id after_guard case.case_body
            in
            Ok (case_versions' :: case_versions))
          [] cases
      in
      Ok (join_owned_tree_version_maps (List.rev case_versions))
  | _ ->
      fold_result
        (validate_owned_version_flow function_id)
        versions (expression_children expression)
let owned_tree_evidence types (root : Sst.binding) =
  match root.Sst.typ with
  | Sst.Aggregate type_id -> (
      match lookup_type types type_id with
      | Some
          {
            representation =
              Sst.Abstract_with_evidence
                (Sst.Authenticated_same_cmt_abstraction evidence);
            _;
          } -> (
          match evidence.owned_tree_prerequisite with
          | Some prerequisite when prerequisite.owned_root = type_id ->
              Some prerequisite
          | Some _ | None -> None)
      | _ -> None)
  | _ -> None
let rec cursor_ids_of_pattern (pattern : Sst.pattern) =
  match pattern.pattern_desc with
  | Sst.Owned_tree_cursor_pattern cursor ->
      [ cursor.cursor_binding.id ]
  | Sst.Tuple_pattern components ->
      List.concat_map (fun (_, pattern) -> cursor_ids_of_pattern pattern) components
  | Sst.Record_pattern fields ->
      List.concat_map (fun (_, pattern) -> cursor_ids_of_pattern pattern) fields
  | Sst.Constructor_pattern (_, arguments) ->
      List.concat_map cursor_ids_of_pattern arguments
  | Sst.Or_pattern (left, right) ->
      cursor_ids_of_pattern left @ cursor_ids_of_pattern right
  | Sst.Wildcard | Sst.Bind _ | Sst.Int_pattern _ | Sst.Bool_pattern _
  | Sst.Unit_pattern ->
      []
let rec cursors_of_pattern (pattern : Sst.pattern) =
  match pattern.pattern_desc with
  | Sst.Owned_tree_cursor_pattern cursor -> [ cursor ]
  | Sst.Tuple_pattern components ->
      List.concat_map (fun (_, pattern) -> cursors_of_pattern pattern) components
  | Sst.Record_pattern fields ->
      List.concat_map (fun (_, pattern) -> cursors_of_pattern pattern) fields
  | Sst.Constructor_pattern (_, arguments) ->
      List.concat_map cursors_of_pattern arguments
  | Sst.Or_pattern (left, right) ->
      cursors_of_pattern left @ cursors_of_pattern right
  | Sst.Wildcard | Sst.Bind _ | Sst.Int_pattern _ | Sst.Bool_pattern _
  | Sst.Unit_pattern ->
      []
let rec cursor_ids_of_expression (expression : Sst.expression) =
  let nested =
    List.concat_map cursor_ids_of_expression (expression_children expression)
  in
  match expression.expression_desc with
  | Sst.Let (bindings, _) ->
      List.concat_map (fun (pattern, _) -> cursor_ids_of_pattern pattern) bindings
      @ nested
  | Sst.Match (_, cases) ->
      List.concat_map
        (fun case -> cursor_ids_of_pattern case.Sst.case_pattern)
        cases
      @ nested
  | _ -> nested
let rec cursors_of_expression (expression : Sst.expression) =
  let nested =
    List.concat_map cursors_of_expression (expression_children expression)
  in
  match expression.expression_desc with
  | Sst.Let (bindings, _) ->
      List.concat_map (fun (pattern, _) -> cursors_of_pattern pattern) bindings
      @ nested
  | Sst.Match (_, cases) ->
      List.concat_map
        (fun case -> cursors_of_pattern case.Sst.case_pattern)
        cases
      @ nested
  | _ -> nested
let validate_owned_tree_transition ~require_authenticated_root types function_id
    span transition =
  let malformed detail =
    fail ~function_id span (Malformed_expression detail)
  in
  let root = transition.Sst.root in
  let* () =
    if not require_authenticated_root then Ok ()
    else
      match owned_tree_evidence types root with
      | Some _ -> Ok ()
      | None ->
          malformed
              "owned-tree transition requires authenticated closed abstract \
               root"
  in
  let* () =
    if root.uniqueness = Sst.Definitely_unique then Ok ()
    else malformed "owned-tree transition root is not definitely unique"
  in
  let* () =
    if
      transition.policy = Sst.Functional_owned_tree_no_heap
      && transition.successor_version = transition.pre_version + 1
    then Ok ()
    else malformed "owned-tree transition has invalid policy or root version"
  in
  let* target =
    match lookup_field types transition.target_field with
    | Some target -> Ok target
    | None -> malformed "owned-tree transition target field is not registered"
  in
  if
    target.field_mutability <> Sst.Mutable_field
    || transition.target_mutability <> target.field_mutability
    || transition.target_modalities <> target.field_modalities
  then malformed "owned-tree transition target metadata does not match registry"
  else
    match transition.cursor with
    | None -> Ok ()
    | Some cursor -> (
          let* () = validate_cursor_path types function_id span cursor in
        if
          cursor.cursor_binding.uniqueness <> Sst.Definitely_aliased
          || cursor.root.id <> root.id
          || cursor.root_version <> transition.pre_version
          || not
               (List.mem cursor.cursor_binding.id
                  transition.invalidated_cursor_ids)
        then malformed "owned-tree cursor is stale or not invalidated"
        else
          match
              (List.rev cursor.guarded_path, transition.target_field.field_owner)
          with
            | ( Sst.Owned_tree_constructor selected :: _,
                Sst.Constructor_owner owner )
            when selected = owner ->
              Ok ()
            | _ ->
                malformed "owned-tree cursor path does not guard target owner")
let rec validate_expression ~require_authenticated_owned_tree_roots ~types
    ~parametric_adts ~functions ~physical_program ~external_specifications
    ~function_id ~enclosing_recursive ~stage ~allow_old initial_bound
    expression =
  let callback_bindings =
    List.find_opt
      (fun (definition : Sst.function_definition) ->
        same_function_id definition.function_id function_id)
      functions
    |> Option.fold ~none:[]
         ~some:Sst_callback_private.bindings_in_definition
  in
  let cursor_ids = cursor_ids_of_expression expression in
  let declared_cursors = cursors_of_expression expression in
  let exact_owned_contents_construction
      (transition : Sst.owned_tree_transition) value =
    let owned_tree_prerequisite = function
      | Sst.Aggregate root_type -> (
          match lookup_type types root_type with
          | Some
              {
                representation =
                  Sst.Abstract_with_evidence
                    (Sst.Authenticated_same_cmt_abstraction evidence);
                _;
              } ->
              evidence.owned_tree_prerequisite
          | Some
              {
                representation =
                  Sst.Abstract_with_evidence
                    (Sst.Incomplete_abstraction_evidence _
                    | Sst.Proposed_same_cmt_abstraction _);
                _;
              }
          | Some { representation = Sst.Revealed; _ } | None ->
              None)
      | Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _ | Sst.Parameter _
        | Sst.Application _ ->
            None
    in
    Spec_function_sst_private.exact_owned_contents_construction
      ~owned_tree_prerequisite transition value
  in
  let rec loop bound (expression : Sst.expression) =
    let* () =
      validate_type_reference types (Some function_id) expression.span
        expression.typ
    in
    match expression.expression_desc with
    | Sst.Int_constant _ when expression.typ <> Sst.Int ->
        fail ~function_id expression.span
          (Malformed_expression "integer constant has a non-integer type")
    | Sst.Bool_constant _ when expression.typ <> Sst.Bool ->
        fail ~function_id expression.span
          (Malformed_expression "Boolean constant has a non-Boolean type")
    | Sst.Unit_constant when expression.typ <> Sst.Unit ->
        fail ~function_id expression.span
          (Malformed_expression "unit constant has a non-unit type")
      | (Sst.Variable { binding; _ } | Sst.Mutable_read binding)
      when List.mem binding.id cursor_ids ->
        fail ~function_id expression.span
          (Malformed_expression
             "owned-tree cursor cannot be used as a standalone value")
    | Sst.Variable { binding; _ } | Sst.Mutable_read binding ->
        if List.mem binding.id bound && binding.typ = expression.typ then Ok bound
        else if List.mem binding.id bound then
          fail ~function_id expression.span
            (Malformed_expression "variable use type differs from its binding")
        else fail ~function_id expression.span (Unbound_binding binding)
    | (Sst.Optional_absent | Sst.Optional_present _
        | Sst.Optional_forward _) -> (
          match Parametric_lowering_private.validate_sst_optional ~descriptors:parametric_adts expression with
          | Ok None -> Ok bound
          | Ok (Some child) -> loop bound child
          | Error message ->
              fail ~function_id expression.span
                (Malformed_expression message))
    | Sst.Forall quantifier | Sst.Exists quantifier ->
        Quantifier_validation_private.validate_scoped_sst
          {
            admit_application =
              (fun typ ->
                Parametric_type.is_spec_function typ
                || Parametric_adt.deeply_immutable_instance parametric_adts
                     typ);
            validate_type_reference =
              (fun binder ->
                validate_type_reference types (Some function_id) binder.Sst.span
                  binder.typ);
            add_binding = add_binding function_id;
            validate_expression = loop;
            malformed_sst =
              (fun span message ->
                {
                  function_id = Some function_id;
                  span;
                  kind = Malformed_expression message;
                });
          }
          ~function_id ~outer:bound expression quantifier
    | Sst.Direct_call _
      when
        Spec_function_sst_private.lambda expression <> None
        || Spec_function_sst_private.application expression <> None
        || Spec_function_sst_private.is_reference expression ->
        Option.get
          (Spec_function_sst_private.validate_special
             {
               recurse = loop;
               add_binding = add_binding function_id;
               validate_type =
                 validate_type_reference types (Some function_id)
                   expression.span;
               find_definition =
                 (fun callee ->
                   List.find_opt
                     (fun (definition : Sst.function_definition) ->
                       same_function_id definition.function_id callee)
                     functions);
               invalid =
                 (fun span message ->
                   {
                     function_id = Some function_id;
                     span;
                     kind = Invalid_call message;
                   });
             }
             bound expression)
    | Sst.Symbolic_application application ->
        let logical = stage = Sst.Logical || stage = Sst.Proof_stage in
        let* arguments =
          Symbolic_declaration_private.validate_application
            ~program:physical_program ~logical ~expression_type:expression.typ
            application
          |> Result.map_error (fun message ->
                 {
                   function_id = Some function_id;
                   span = expression.span;
                   kind = Invalid_call message;
                 })
        in
        fold_result loop bound arguments
      | Sst.Direct_call
          { call_form; callee; type_arguments; recursive; arguments } ->
          let* () =
            iter_result
              (validate_type_reference types (Some function_id) expression.span)
              type_arguments
          in
        let* callee_definition =
          match
            List.find_opt
              (fun (definition : Sst.function_definition) ->
                same_function_id definition.function_id callee)
              functions
          with
          | Some definition -> Ok definition
          | None -> fail ~function_id expression.span (Unknown_function_id callee)
        in
        let* () =
          if
            function_contains_shared_scalar_write callee_definition
            && not
                 (authenticated_shared_invariant_operation types
                    callee_definition)
          then
            fail ~function_id expression.span
              (Invalid_call
                 "calls to bounded shared-scalar mutators require a deferred \
                  effect summary")
          else
          match callee_definition.body with
          | Sst.External_specification
              (Sst.Imported_unverified_target _) ->
              External_target_specification_private.validate_call
                external_specifications ~program:physical_program ~functions
                ~caller_id:function_id ~callee:callee_definition ~expression
              |> Result.map_error (fun message ->
                     {
                       function_id = Some function_id;
                       span = expression.span;
                       kind = Invalid_call message;
                     })
          | Sst.External_specification
              (Sst.Same_unit_target _ | Sst.Unresolved_target _) ->
              fail ~function_id expression.span
                (Invalid_call
                       "external-specification wrappers are semantic \
                        declarations and cannot be called")
          | Sst.Trusted_external_spec_target
                  (Sst.Same_unit_target { wrapper; _ }) -> (
              let position id =
                let rec loop index = function
                  | [] -> None
                  | (candidate : Sst.function_definition) :: rest ->
                      if same_function_id candidate.function_id id then
                        Some index
                      else loop (index + 1) rest
                in
                loop 0 functions
                  in
                  match (position function_id, position wrapper) with
              | Some caller, Some specification when caller > specification ->
                  Ok ()
              | _ ->
                  fail ~function_id expression.span
                    (Invalid_call
                           "trusted external target calls must occur after \
                            their authenticated specification wrapper"))
          | Sst.Trusted_external_spec_target
              (Sst.Imported_unverified_target _) ->
              fail ~function_id expression.span
                (Invalid_target_link
                   "imported external summaries cannot masquerade as trusted \
                    same-unit targets")
          | Sst.Trusted_external_spec_target (Sst.Unresolved_target _) ->
              fail ~function_id expression.span
                (Invalid_target_link
                   "trusted external target has an unresolved specification")
          | Sst.Checked_exec _ | Sst.Spec_definition _
          | Sst.Recursive_spec_definition _ | Sst.Proof_body _
          | Sst.Trusted_external_body _ | Sst.Symbolic_declaration _ ->
              Ok ()
        in
        let self_call = same_function_id callee function_id in
          let should_be_recursive = self_call && enclosing_recursive in
        let* () =
          if self_call && not enclosing_recursive then
            fail ~function_id expression.span
              (Invalid_recursive_marker
                 "self-call occurs in a nonrecursive declaration")
          else if recursive = should_be_recursive then Ok ()
          else
            fail ~function_id expression.span
              (Invalid_recursive_marker
                 "call recursion marker does not match its semantic target")
        in
        let* () =
          match expected_call_form stage callee_definition.mode with
          | Some expected when expected = call_form -> Ok ()
          | Some _ ->
              fail ~function_id expression.span
                (Invalid_call
                   "call form does not match expression stage and callee mode")
          | None
              when stage = Sst.Logical && call_form = Sst.Exec_call
              && authenticated_frozen_spine_terminal types
                   callee_definition.function_id ->
              Ok ()
          | None ->
              fail ~function_id expression.span
                (Invalid_call
                   "callee mode is not available from this expression stage")
        in
        let* value_edges =
          let substitutions =
            if
              List.length callee_definition.type_binders
              = List.length type_arguments
            then List.combine callee_definition.type_binders type_arguments
            else []
          in
          match
            Sst_callback_private.split_direct_arguments
              ~substitutions
              callee_definition.parameters arguments
          with
          | Ok edges -> Ok edges
          | Error message ->
              fail ~function_id expression.span (Invalid_call message)
        in
        let arguments = List.map snd value_edges in
        let* () =
            match
              Parametric_lowering_private.validate_sst_direct_call
                ~definition:callee_definition ~type_arguments
                ~actual_result:expression.typ ~call_span:expression.span
                ~arguments
            with
            | Ok () -> Ok ()
            | Error (span, message) ->
                fail ~function_id span (Invalid_call message)
        in
        fold_result
          (fun bound (_, argument) -> loop bound argument)
          bound arguments
    | (Sst.Callback_call _ | Sst.Callback_requires _ | Sst.Callback_ensures _) ->
        let* children =
          match
            Sst_callback_private.validate_expression ~known:callback_bindings
              ~stage expression
          with
          | Ok children -> Ok children
          | Error message ->
              fail ~function_id expression.span (Invalid_call message)
        in
        fold_result (fun bound child -> loop bound child) bound children
    | Sst.Reveal target | Sst.Reveal_with_fuel { function_id = target; _ } -> (
        let* target_definition =
          match
            List.find_opt
              (fun (definition : Sst.function_definition) ->
                same_function_id definition.function_id target)
              functions
          with
          | Some definition -> Ok definition
            | None ->
                fail ~function_id expression.span (Unknown_function_id target)
        in
        let* () =
          if stage <> Sst.Proof_stage || expression.typ <> Sst.Unit then
            fail ~function_id expression.span
              (Invalid_call
                 "reveal is a unit statement available only in proof stage")
          else
            match target_definition.body with
            | Sst.Recursive_spec_definition _
                when target_definition.mode = Sst.Spec
                && target_definition.function_id.function_index
                   < function_id.function_index ->
                Ok ()
            | Sst.Recursive_spec_definition _ ->
                fail ~function_id expression.span
                  (Invalid_call
                       "recursive specification is not accessible in this \
                        proof context")
            | _ ->
                fail ~function_id expression.span
                  (Invalid_call
                       "reveal requires an authenticated recursive \
                        specification")
          in
          match expression.expression_desc with
        | Sst.Reveal _ -> Ok bound
        | Sst.Reveal_with_fuel { literal_depth; _ } -> (
            let parsed =
                try Some (Z.of_string literal_depth)
                with Invalid_argument _ -> None
            in
              match parsed with
            | Some depth
              when Z.sign depth >= 0 && Z.compare depth (Z.of_int 64) <= 0 ->
                Ok bound
            | Some _ | None ->
                fail ~function_id expression.span
                  (Invalid_call
                     "reveal_with_fuel depth must be a literal in 0..64"))
        | _ -> assert false)
    | Sst.Use_type_invariant { use_id; value } ->
        let* () =
          if
              stage = Sst.Proof_stage && expression.typ = Sst.Unit
              && String.starts_with ~prefix:"verocaml:use-type-invariant:1:"
                   use_id
          then Ok ()
          else
            fail ~function_id expression.span
              (Invalid_call
                 "use_type_invariant is an authenticated unit statement \
                  available only in proof stage")
        in
        let* () =
          match value.expression_desc with
            | Sst.Variable { binding = { typ = Sst.Aggregate type_id; _ }; _ }
            when value.typ = Sst.Aggregate type_id -> (
              match lookup_type types type_id with
              | Some
                  {
                    representation =
                      Sst.Abstract_with_evidence
                        (Sst.Authenticated_same_cmt_abstraction _);
                    _;
                  } ->
                  Ok ()
              | Some _ | None ->
                  fail ~function_id value.span
                    (Invalid_call
                         "use_type_invariant requires an authenticated \
                          abstract exact-type value"))
          | _ ->
              fail ~function_id value.span
                (Invalid_call
                     "use_type_invariant requires one closed in-scope \
                      exact-type variable")
        in
        loop bound value
    | Sst.Local_assert { predicate; _ } ->
        if
          (stage <> Sst.Proof_stage && stage <> Sst.Runtime)
          || expression.typ <> Sst.Unit
        then
          fail ~function_id expression.span
            (Malformed_expression
               "local assertions are unit statements available only in proof \
                stage or authenticated Exec")
        else
          let* bound = loop bound predicate in
          if predicate.typ = Sst.Bool then Ok bound
          else
            fail ~function_id predicate.span
              (Malformed_expression
                 "local assertion predicate must be Boolean")
    | Sst.Proof_region proof_body ->
        if stage <> Sst.Runtime || expression.typ <> Sst.Unit then
          fail ~function_id expression.span
            (Malformed_expression
               "proof regions are unit statements in runtime sequences only")
        else
          let* _ =
            validate_expression ~types ~parametric_adts ~functions
              ~physical_program ~external_specifications ~function_id
              ~require_authenticated_owned_tree_roots
              ~enclosing_recursive:false ~stage:Sst.Proof_stage
              ~allow_old:false bound proof_body
          in
          if proof_body.typ = Sst.Unit then Ok bound
          else
            fail ~function_id proof_body.span
              (Malformed_expression "proof region body must return unit")
    | Sst.Old payload ->
        if allow_old && stage = Sst.Logical then loop bound payload
        else
          fail ~function_id expression.span
            (Malformed_expression "old is only valid in an ensures predicate")
    | Sst.Owned_tree_nested_write { transition; value } ->
        let* () =
          if List.mem transition.root.id bound then Ok ()
          else fail ~function_id expression.span (Unbound_binding transition.root)
        in
        let* () =
          if stage = Sst.Runtime then Ok ()
          else
            fail ~function_id expression.span
              (Malformed_expression
                 "owned-tree mutation is not valid outside runtime stage")
        in
        let* () =
          validate_owned_tree_transition
            ~require_authenticated_root:require_authenticated_owned_tree_roots
            types function_id expression.span transition
        in
        let* () =
          match transition.cursor with
          | Some cursor
            when List.mem cursor declared_cursors
                 && transition.reconstruction
                    = expected_nested_reconstruction types cursor
                        transition.target_field ->
              Ok ()
          | Some _ | None ->
              fail ~function_id expression.span
                (Malformed_expression
                   "owned-tree nested reconstruction metadata is incomplete")
        in
        let* () =
          match transition.rhs_provenance with
          | Sst.Ground_owned_tree_value -> Ok ()
          | Sst.Guarded_descendant_move _ ->
              fail ~function_id expression.span
                (Malformed_expression
                     "nested replacement cannot carry descendant-move \
                      provenance")
        in
        let ground =
          match value.expression_desc with
          | Sst.Int_constant _ | Sst.Bool_constant _ | Sst.Unit_constant
          | Sst.Constructor_value { arguments = []; _ } ->
              true
          | _ -> false
        in
          if
            (not ground) || value.typ <> (Option.get (lookup_field types transition.target_field)).field_type
        then
          fail ~function_id value.span
            (Malformed_expression
               "owned-tree nested replacement requires a typed ground value")
        else loop bound value
    | Sst.Owned_tree_rebase { transition } -> (
        let* () =
          if List.mem transition.root.id bound then Ok ()
          else fail ~function_id expression.span (Unbound_binding transition.root)
        in
        let* () =
          if stage = Sst.Runtime then Ok ()
          else
            fail ~function_id expression.span
              (Malformed_expression
                 "owned-tree move is not valid outside runtime stage")
        in
        let* () =
          validate_owned_tree_transition
            ~require_authenticated_root:require_authenticated_owned_tree_roots
            types function_id expression.span transition
        in
        match transition.rhs_provenance with
        | Sst.Guarded_descendant_move cursor
          when List.mem cursor declared_cursors
               && cursor.cursor_binding.uniqueness = Sst.Definitely_aliased
               && cursor.root.id = transition.root.id
               && cursor.root_version = transition.pre_version
               &&
               (match cursor.guarded_path with
               | Sst.Owned_tree_field source :: _ ->
                   source = transition.target_field
               | _ -> false)
               && List.mem cursor.cursor_binding.id
                    transition.invalidated_cursor_ids ->
            let* () =
              validate_cursor_path types function_id expression.span cursor
            in
            if
              transition.reconstruction
              = [ record_reconstruction types transition.target_field ]
            then Ok bound
            else
              fail ~function_id expression.span
                (Malformed_expression
                   "owned-tree rebase reconstruction metadata is incomplete")
        | Sst.Guarded_descendant_move _ | Sst.Ground_owned_tree_value ->
            fail ~function_id expression.span
              (Malformed_expression
                 "owned-tree rebase requires one live guarded descendant"))
    | Sst.Field_write
        { provenance; field; value; transition = Some transition } ->
        let* () =
          if List.mem transition.root.id bound then Ok ()
          else fail ~function_id expression.span (Unbound_binding transition.root)
        in
        let* () =
          if stage = Sst.Runtime then Ok ()
          else
            fail ~function_id expression.span
              (Malformed_expression
                 "owned-tree root write is not valid outside runtime stage")
        in
        let* () =
          validate_owned_tree_transition
            ~require_authenticated_root:require_authenticated_owned_tree_roots
            types function_id expression.span transition
        in
        let* () =
          if
            transition.root.id = provenance.root.id
            && transition.target_field = field
            && transition.cursor = None
            && transition.reconstruction
               = [ record_reconstruction types transition.target_field ]
          then Ok ()
          else
            fail ~function_id expression.span
              (Malformed_expression
                 "owned-tree root-write transition metadata is inconsistent")
        in
        let ground =
          match value.expression_desc with
          | Sst.Int_constant _ | Sst.Bool_constant _ | Sst.Unit_constant
          | Sst.Constructor_value { arguments = []; _ } ->
              true
          | _ -> false
        in
        let authenticated_rhs =
          match transition.rhs_provenance with
          | Sst.Ground_owned_tree_value ->
              ground || exact_owned_contents_construction transition value
          | Sst.Guarded_descendant_move _ -> false
        in
        if
          authenticated_rhs
          && value.typ
             = (Option.get (lookup_field types transition.target_field)).field_type
        then loop bound value
        else
          fail ~function_id value.span
            (Malformed_expression
                 "owned-tree root write requires a typed ground value or one \
                  exact predecessor-backed construction")
    | Sst.Shared_scalar_field_write { provenance; field; value; _ } ->
        let* () =
          if stage = Sst.Runtime && expression.typ = Sst.Unit then Ok ()
          else
            fail ~function_id expression.span
              (Malformed_expression
                 "shared-scalar write is a unit operation available only at \
                  runtime")
        in
        let* () =
          if
            List.mem provenance.root.id bound
            && provenance.root.uniqueness = Sst.Definitely_aliased
              && provenance.binding_pattern_uniqueness = Sst.Definitely_aliased
            && provenance.field_is_local && provenance.field_is_public
            && provenance.field_is_mutable
          then Ok ()
          else
            fail ~function_id expression.span
              (Malformed_expression
                 "shared-scalar write lacks exact aliased provenance")
        in
        let* target =
          match lookup_field types field with
          | Some target -> Ok target
          | None ->
              fail ~function_id expression.span
                (Malformed_expression
                   "shared-scalar write target is not registered")
        in
        if
          target.field_type = Sst.Int
          && target.field_mutability = Sst.Mutable_field
          && value.typ = Sst.Int
        then loop bound value
        else
          fail ~function_id value.span
            (Malformed_expression
               "shared-scalar write requires one mutable integer field")
      | (Sst.Field_write _ | Sst.Mutable_write _ | Sst.Let_mutable _)
      when stage <> Sst.Runtime ->
        fail ~function_id expression.span
          (Malformed_expression "mutation is not valid outside runtime stage")
      | Sst.Compare ((Sst.Equal | Sst.Not_equal), left, right)
        when stage = Sst.Runtime
             && (Parametric_type.is_open left.typ
                || Parametric_type.is_open right.typ) ->
          fail ~function_id expression.span
            (Malformed_expression
               "runtime equality is not defined for an open parametric type")
    | Sst.Let (bindings, body) ->
        let* () =
          iter_result
            (fun (_, value) ->
              let* _ = loop bound value in
              Ok ())
            bindings
        in
        let* bound =
          fold_result
            (fun bound (pattern, _) ->
              pattern_bindings function_id types bound pattern)
            bound bindings
        in
        loop bound body
    | Sst.Let_mutable (binding, initial, body) ->
        let* bound = loop bound initial in
        let* bound = add_binding function_id bound binding in
        loop bound body
    | Sst.Match (scrutinee, cases) ->
        let* _ = loop bound scrutinee in
        let* () =
          iter_result
            (fun case ->
              let* case_bound =
                pattern_bindings function_id types bound case.Sst.case_pattern
              in
              let* _ =
                match case.case_guard with
                | None -> Ok case_bound
                | Some guard -> loop case_bound guard
              in
              let* _ = loop case_bound case.case_body in
              Ok ())
            cases
        in
        Ok bound
    | _ ->
        let* () =
          iter_result
            (fun child ->
              let* _ = loop bound child in
              Ok ())
            (expression_children expression)
        in
        Ok bound
  in
  let* _ = validate_owned_version_flow function_id [] expression in
  let* _ = loop initial_bound expression in
  Ok ()
let check_clause_indices function_id label clauses =
  let rec loop expected = function
    | [] -> Ok ()
    | (index, span) :: rest ->
        if index = expected then loop (expected + 1) rest
        else
          fail ~function_id span
            (Invalid_clause
               (Printf.sprintf "%s clauses must have dense zero-based indices"
                  label))
  in
  loop 0 clauses
let scalar_type = function Sst.Int | Sst.Bool -> true | _ -> false
let logical_type types typ =
  Spec_definition.classify_logical_type types typ
let is_logical_type types typ = Option.is_some (logical_type types typ)
let rank_domain_for_aggregate rank_domains type_id =
  List.find_opt
    (fun domain ->
      Parametric_rank_domain_private.immutable domain
      && List.exists
           (fun (identity : Typedtree_adapter_issuance_private.rank_type_identity) ->
             identity.rank_type_id = type_id)
           (Parametric_rank_domain_private.component domain))
    rank_domains
let locally_ranked_type rank_domains = function
  | Sst.Aggregate type_id ->
      Option.is_some (rank_domain_for_aggregate rank_domains type_id)
  | Sst.Application (constructor, arguments) ->
      List.exists
        (fun domain ->
          Parametric_rank_domain_private.immutable domain
          &&
          match Parametric_rank_domain_private.application domain with
          | Some (Parametric_type.Application (candidate, actuals)) ->
              Parametric_type.compare_constructor candidate constructor = 0
              && List.equal Parametric_type.equal actuals arguments
          | Some (Unit | Bool | Int | Tuple _ | Aggregate _ | Parameter _)
          | None ->
              false)
        rank_domains
  | Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _ | Sst.Parameter _ -> false
(* Rank-backed logical shape is deliberately private to SST validation.  An
   issued rank domain authenticates a local immutable type-instance snapshot,
   including the generic-profile/actual evidence sealed into that snapshot;
   it does not authenticate any value of the type.  Consequently this
   classifier only admits the nominal carrier grammar and never exposes a
   logical descriptor, rank projection, or finite-value fact.
   A shared [selected] cell covers one complete nonrecursive specification.
   Classification commits a newly selected domain only after the whole type
   succeeds, so a failed branch cannot influence a later diagnostic. *)
let admit_rank_backed_logical_shape ~rank_domains ~parametric_adts types selected
    typ =
  let type_fields = function
    | Sst.Record_definition fields -> fields
    | Sst.Variant_definition constructors ->
        List.concat_map
          (fun (constructor : Sst.constructor_definition) ->
            constructor.constructor_fields)
          constructors
  in
  let domains_for type_id =
    List.filter
      (fun domain ->
        Parametric_rank_domain_private.immutable domain
        && List.exists
             (fun (identity : Typedtree_adapter_issuance_private.rank_type_identity) ->
               same_type_id identity.rank_type_id type_id)
             (Parametric_rank_domain_private.component domain))
      rank_domains
  in
  let select_domain current type_id =
    match (current, domains_for type_id) with
    | current, [] -> Some current
    | None, [ domain ] -> Some (Some domain)
      | Some selected, [ domain ] when selected == domain -> Some current
      | None, _ :: _ :: _ | Some _, [ _ ] | Some _, _ :: _ :: _ -> None
  in
  let rec classify visiting current = function
    | Sst.Unit | Sst.Bool | Sst.Int -> Some current
    | Sst.Parameter _ -> None
    | Sst.Application (constructor, arguments) -> (
        match Parametric_adt.find parametric_adts constructor with
        | Some descriptor
          when Result.is_ok
                 (Logical_adt_schema_private.instantiate
                    ~descriptors:parametric_adts
                    ~applications:
                      [ ((Parametric_adt.type_id descriptor).type_index, arguments) ])
          ->
            Some current
        | Some _ | None -> None)
    | Sst.Tuple components ->
        List.fold_left
          (fun state (_, component) ->
            Option.bind state (fun current ->
                classify visiting current component))
          (Some current) components
    | Sst.Aggregate type_id ->
        Option.bind (select_domain current type_id) (fun current ->
            if List.exists (same_type_id type_id) visiting then
              match current with
              | Some domain
                  when List.exists
                         (fun (identity : Typedtree_adapter_issuance_private.rank_type_identity)
                            -> same_type_id identity.rank_type_id type_id)
                    (Parametric_rank_domain_private.component domain) ->
                  Some current
              | None | Some _ -> None
            else
              match lookup_type types type_id with
                | Some { representation = Sst.Revealed; type_kind; _ } ->
                  List.fold_left
                    (fun state (field : Sst.field_definition) ->
                      Option.bind state (fun current ->
                          if
                            field.field_mutability = Sst.Immutable_field
                            && field.field_modalities.uniqueness_modality
                               = Sst.Preserve_uniqueness
                            && field.field_modalities.linearity_modality
                               = Sst.Preserve_linearity
                          then
                            classify (type_id :: visiting) current
                              field.field_type
                          else None))
                    (Some current) (type_fields type_kind)
              | Some { representation = Sst.Abstract_with_evidence _; _ }
              | None ->
                  None)
  in
  if is_logical_type types typ then true
  else
    match classify [] !selected typ with
    | None -> false
    | Some domain ->
        selected := domain;
        true
let contracts_are_empty (contracts : Sst.contracts) =
  contracts.requires = [] && contracts.ensures = []
  && contracts.decreases = [] && contracts.assertions = []
type owned_scalar_terminal =
  | Owned_terminal_int
  | Owned_terminal_bool
  | Owned_terminal_unit
  | Owned_terminal_tag of Sst.constructor_id
type owned_scalar_path = {
  owned_path_steps : Sst.owned_tree_path_step list;
  owned_path_terminal : owned_scalar_terminal;
}
type owned_scalar_read = {
  owned_read_expression : Sst.expression;
  owned_read_path : owned_scalar_path;
  owned_read_result_field : Sst.field_id;
}
type owned_scalar_projection = {
  owned_projection_expression : Sst.expression;
  owned_projection_path : Sst.owned_tree_path_step list;
  owned_projection_type : Sst.typ;
}
type owned_scalar_match = {
  owned_match_expression : Sst.expression;
  owned_match_scrutinee : Sst.expression;
  owned_match_path : Sst.owned_tree_path_step list;
  owned_match_constructors : Sst.constructor_id list;
}
type owned_scalar_result_source =
  | Owned_result_read of owned_scalar_read
  | Owned_result_constant of {
      owned_constant_expression : Sst.expression;
      owned_constant_field : Sst.field_id;
    }
type owned_root_scalar_model_template = {
  owned_template_issuer : unit ref;
  owned_template_program : Sst.program;
  owned_template_program_snapshot : string;
  owned_template_definition : Sst.function_definition;
  owned_template_body : Sst.expression;
  owned_template_body_snapshot : string;
  owned_template_domain : Sst.type_id;
  owned_template_formal : Sst.binding;
  owned_template_evidence : Sst.same_cmt_abstraction_evidence;
  owned_template_result : Sst.type_id;
  owned_template_result_fields : Sst.field_definition list;
  owned_template_paths : owned_scalar_path list;
  owned_template_reads : owned_scalar_read list;
  owned_template_projections : owned_scalar_projection list;
  owned_template_matches : owned_scalar_match list;
  owned_template_result_sources : owned_scalar_result_source list;
}
let owned_scalar_model_issuer = ref ()
let collect_owned_recursive_contents_model ~program
    (definition : Sst.function_definition) =
  Owned_recursive_contents_private.collect ~program ~model:definition
let owned_recursive_contents_for_helper ~program definition =
  program.Sst.functions
  |> List.find_map (fun model ->
         match collect_owned_recursive_contents_model ~program model with
         | Ok grammar
           when Owned_recursive_contents_private.is_helper grammar definition ->
             Some grammar
         | Ok _ | Error _ -> None)
let scalar_terminal = function
  | Sst.Int -> Some Owned_terminal_int
  | Sst.Bool -> Some Owned_terminal_bool
  | Sst.Unit -> Some Owned_terminal_unit
    | Sst.Tuple _ | Sst.Aggregate _ | Sst.Parameter _ | Sst.Application _ -> None
let same_owned_path left right =
  left.owned_path_steps = right.owned_path_steps
  && left.owned_path_terminal = right.owned_path_terminal
let collect_owned_root_scalar_model ~program ~types
    (definition : Sst.function_definition) =
  let function_id = definition.function_id in
  let malformed span detail =
    fail ~function_id span (Malformed_expression detail)
  in
  let* domain =
    match Spec_definition.authenticated_model_domain types definition with
    | Some domain -> Ok domain
    | None ->
        malformed definition.span
            "owned-root scalar observation requires an authenticated local \
             model"
  in
  let* evidence, owned =
    match lookup_type types domain with
    | Some
        {
          representation =
            Sst.Abstract_with_evidence
              (Sst.Authenticated_same_cmt_abstraction evidence);
          _;
        } -> (
        match evidence.owned_tree_prerequisite with
        | Some owned when same_type_id owned.owned_root domain ->
            Ok (evidence, owned)
        | Some _ | None ->
            malformed definition.span
                "owned-root scalar observation requires the exact \
                 authenticated owned root")
    | Some _ | None ->
        malformed definition.span
            "owned-root scalar observation requires same-CMT abstraction \
             evidence"
  in
  let* formal =
    match definition.parameters with
    | [
        Sst.Value_parameter
          {
          label = None;
          pattern =
            {
              pattern_desc =
                Sst.Bind
                  ({
                     typ = Sst.Aggregate formal_domain;
                     uniqueness = Sst.Definitely_aliased;
                     _;
                   } as formal);
              typ = Sst.Aggregate pattern_domain;
              _;
            };
         optional_default = _;
        };
      ]
        when same_type_id formal_domain domain
        && same_type_id pattern_domain domain ->
        Ok formal
    | _ ->
        malformed definition.span
            "owned-root scalar observation requires one exact unlabelled @read \
             formal"
  in
  let* result_id, result_fields =
    match definition.result_type with
    | Sst.Aggregate result_id -> (
        match lookup_type types result_id with
        | Some
            {
              representation = Sst.Revealed;
              type_kind = Sst.Record_definition fields;
              _;
            }
            when fields <> []
            && List.for_all
                 (fun (field : Sst.field_definition) ->
                   field.field_mutability = Sst.Immutable_field
                   && field.field_modalities.uniqueness_modality
                      = Sst.Preserve_uniqueness
                   && field.field_modalities.linearity_modality
                      = Sst.Preserve_linearity
                   && Option.is_some (scalar_terminal field.field_type))
                 fields ->
            Ok (result_id, fields)
        | Some _ | None ->
            malformed definition.span
                "owned-root scalar model result must be a local immutable \
                 scalar record")
      | Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _ | Sst.Parameter _
      | Sst.Application _ ->
        malformed definition.span
          "owned-root scalar model must return an immutable scalar record"
  in
  let type_definition type_id = lookup_type types type_id in
  let constructor_definition constructor =
    match type_definition constructor.Sst.constructor_type with
    | Some { type_kind = Sst.Variant_definition constructors; _ } ->
        List.find_opt
          (fun (candidate : Sst.constructor_definition) ->
            candidate.constructor_id = constructor)
          constructors
    | Some { type_kind = Sst.Record_definition _; _ } | None -> None
  in
  let field_definition field = lookup_field types field in
  let descriptor_recursive_edge field =
    List.exists
      (fun descriptor_field -> descriptor_field = field)
      owned.Sst.recursive_edges
  in
  let recursive_edges_in_path path =
    List.fold_left
      (fun count -> function
        | Sst.Owned_tree_field field when descriptor_recursive_edge field ->
            count + 1
        | Sst.Owned_tree_field _ | Sst.Owned_tree_constructor _ -> count)
      0 path
  in
  let descriptor_recursive_carrier type_id =
    List.exists
      (fun field ->
        same_type_id (owner_type field.Sst.field_owner) type_id
        ||
        match field_definition field with
        | Some { Sst.field_type = Sst.Aggregate target; _ } ->
            same_type_id target type_id
        | Some
            {
                Sst.field_type = Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _;
              _;
            }
          | Some { Sst.field_type = Sst.Parameter _ | Sst.Application _; _ }
        | None ->
            false)
      owned.Sst.recursive_edges
  in
  let owner_matches typ owner =
    match typ with
    | Sst.Aggregate type_id -> same_type_id type_id (owner_type owner)
      | Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _ | Sst.Parameter _
      | Sst.Application _ -> false
  in
  let paths = ref [] in
  let reads = ref [] in
  let projections = ref [] in
  let matches = ref [] in
  let result_sources = ref [] in
  let note_path path =
    if not (List.exists (same_owned_path path) !paths) then paths := path :: !paths
  in
  let rec path_expression environment expression =
    match expression.Sst.expression_desc with
    | Sst.Variable { binding; _ } -> (
        match List.assoc_opt binding.id environment with
        | Some (path, typ)
          when binding.typ = expression.typ && expression.typ = typ ->
            Ok (path, typ)
        | Some _ | None ->
            malformed expression.span
                "owned-root scalar path is copied, rebound, or not rooted at \
                 the sole formal")
    | Sst.Field_read { record; field } ->
        let* path, record_type = path_expression environment record in
        let* field_definition =
          match field_definition field with
          | Some field_definition -> Ok field_definition
          | None ->
              malformed expression.span
                "owned-root scalar path contains an unregistered field"
        in
        if
            descriptor_recursive_edge field && recursive_edges_in_path path >= 1
        then
          malformed expression.span
              "owned-root scalar model may follow at most one descriptor \
               recursive edge for a bounded tag observation"
        else if
            (not (owner_matches record_type field.field_owner))
          || field_definition.field_type <> expression.typ
        then
          malformed expression.span
            "owned-root scalar path field has the wrong exact owner or type"
        else
          let path = path @ [ Sst.Owned_tree_field field ] in
          projections :=
            {
              owned_projection_expression = expression;
              owned_projection_path = path;
              owned_projection_type = expression.typ;
            }
            :: !projections;
          Ok (path, expression.typ)
    | _ ->
        malformed expression.span
            "owned-root scalar observation paths may contain only exact field \
             projections"
  in
  let rec bind_constructor_pattern environment path current_type
      (pattern : Sst.pattern) =
    match pattern.pattern_desc with
      | Sst.Constructor_pattern (constructor, arguments) -> (
        let* constructor_definition =
          match (current_type, constructor_definition constructor) with
          | Sst.Aggregate type_id, Some constructor_definition
              when same_type_id type_id constructor.constructor_type
              && pattern.typ = current_type ->
              Ok constructor_definition
          | _ ->
              malformed pattern.span
                  "owned-root scalar match constructor has the wrong exact \
                   owner"
        in
        let constructor_path =
          path @ [ Sst.Owned_tree_constructor constructor ]
        in
        let expected = constructor_definition.constructor_fields in
          match (arguments, expected) with
        | [], [] -> Ok environment
        | [ ({ pattern_desc = Sst.Bind binding; _ } as argument) ], _
            when argument.typ = binding.typ
            && binding.uniqueness = Sst.Definitely_aliased
            && binding.typ = Sst.Aggregate constructor.constructor_type ->
            Ok
              ( ( binding.id,
                  (constructor_path, Sst.Aggregate constructor.constructor_type)
                )
              :: environment )
        | [ { pattern_desc = Sst.Wildcard; _ } ], _ -> Ok environment
        | _ ->
            malformed pattern.span
                "owned-root constructor payload may only bind one scoped \
                 inline-record path or be ignored by a tag-only branch")
    | Sst.Wildcard | Sst.Bind _ | Sst.Int_pattern _ | Sst.Bool_pattern _
    | Sst.Unit_pattern | Sst.Tuple_pattern _ | Sst.Record_pattern _
    | Sst.Owned_tree_cursor_pattern _ | Sst.Or_pattern _ ->
        malformed pattern.span
          "owned-root scalar match requires exact constructor cases"
  and validate_result environment expression =
    match expression.Sst.expression_desc with
    | Sst.Record_value { record_type; fields }
      when
        same_type_id record_type result_id
        && expression.typ = Sst.Aggregate result_id
        && List.length fields = List.length result_fields ->
        List.fold_left2
          (fun result (field, value) (expected : Sst.field_definition) ->
            let* () = result in
            if field <> expected.field_id || value.Sst.typ <> expected.field_type
            then
              malformed value.span
                  "owned-root scalar model result field identity or type \
                   mismatch"
            else
              match value.expression_desc with
              | Sst.Int_constant _ | Sst.Bool_constant _ | Sst.Unit_constant ->
                  result_sources :=
                    Owned_result_constant
                      {
                        owned_constant_expression = value;
                        owned_constant_field = field;
                      }
                    :: !result_sources;
                  Ok ()
              | Sst.Field_read _ ->
                  let* path, terminal_type =
                    path_expression environment value
                  in
                  let* () =
                    if recursive_edges_in_path path = 0 then Ok ()
                    else
                      malformed value.span
                          "owned-root scalar model may not project a scalar \
                           through a descriptor recursive edge"
                  in
                  let* terminal =
                    match scalar_terminal terminal_type with
                    | Some terminal -> Ok terminal
                    | None ->
                        malformed value.span
                            "owned-root observation path does not terminate at \
                             a primitive scalar"
                  in
                  let path =
                    {
                      owned_path_steps = path;
                      owned_path_terminal = terminal;
                    }
                  in
                  let read =
                    {
                      owned_read_expression = value;
                      owned_read_path = path;
                      owned_read_result_field = field;
                    }
                  in
                  note_path path;
                  reads := read :: !reads;
                  result_sources := Owned_result_read read :: !result_sources;
                  Ok ()
              | _ ->
                  malformed value.span
                      "owned-root model result fields may contain only \
                       constants or exact scalar paths")
          (Ok ()) fields result_fields
    | _ ->
        malformed expression.span
            "owned-root scalar model branches must construct the exact \
             immutable result"
  and validate_body environment expression =
    match expression.Sst.expression_desc with
    | Sst.Match (scrutinee, cases) ->
        let* path, scrutinee_type = path_expression environment scrutinee in
        let* constructors =
          match scrutinee_type with
          | Sst.Aggregate type_id -> (
              match type_definition type_id with
              | Some { type_kind = Sst.Variant_definition constructors; _ }
                  when constructors <> []
                  && descriptor_recursive_carrier type_id ->
                  Ok constructors
              | Some { type_kind = Sst.Record_definition _; _ }
              | Some { type_kind = Sst.Variant_definition _; _ }
              | None ->
                  malformed scrutinee.span
                      "owned-root scalar model may match only a registered \
                       local variant")
            | Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _ | Sst.Parameter _
            | Sst.Application _ ->
              malformed scrutinee.span
                  "owned-root scalar model match scrutinee is not a variant \
                   path"
        in
        if List.length cases <> List.length constructors then
          malformed expression.span
              "owned-root scalar model match must cover every constructor \
               exactly once"
        else
          let* seen =
            fold_result
              (fun seen (case : Sst.case) ->
                if Option.is_some case.case_guard then
                  malformed case.case_span
                    "owned-root scalar model match guards are not permitted"
                else
                  match case.case_pattern.pattern_desc with
                  | Sst.Constructor_pattern (constructor, _) ->
                      if List.mem constructor seen then
                        malformed case.case_pattern.span
                          "owned-root scalar model repeats a constructor"
                      else
                        let* environment =
                            bind_constructor_pattern environment path
                              scrutinee_type case.case_pattern
                        in
                        let* () = validate_body environment case.case_body in
                        Ok (constructor :: seen)
                  | _ ->
                      malformed case.case_pattern.span
                          "owned-root scalar model requires exact constructor \
                           cases")
              [] cases
          in
          let declared =
            List.map
              (fun (constructor : Sst.constructor_definition) ->
                constructor.constructor_id)
              constructors
          in
          if List.sort compare seen <> List.sort compare declared then
            malformed expression.span
              "owned-root scalar model constructor coverage is ambiguous"
            else
            let tag_paths =
              List.map
                (fun constructor ->
                  {
                    owned_path_steps = path;
                    owned_path_terminal = Owned_terminal_tag constructor;
                  })
                declared
            in
            List.iter note_path tag_paths;
            matches :=
              {
                owned_match_expression = expression;
                owned_match_scrutinee = scrutinee;
                owned_match_path = path;
                owned_match_constructors = declared;
              }
              :: !matches;
              Ok ()
    | Sst.Record_value _ -> validate_result environment expression
    | _ ->
        malformed expression.span
            "owned-root scalar model permits only bounded constructor matches \
             and exact result construction"
  in
  let* body =
    match definition.body with
    | Sst.Spec_definition body when body.stage = Sst.Logical ->
        Ok body.expression
      | Sst.Checked_exec _ | Sst.Recursive_spec_definition _ | Sst.Proof_body _
      | Sst.External_specification _ | Sst.Trusted_external_spec_target _
      | Sst.Trusted_external_body _ | Sst.Symbolic_declaration _
      | Sst.Spec_definition _ ->
        malformed definition.span
            "owned-root scalar observation requires one nonrecursive logical \
             body"
  in
    let* () = validate_body [ (formal.id, ([], Sst.Aggregate domain)) ] body in
  if !matches = [] then
    malformed body.span
      "owned-root scalar model must contain a nonvacuous variant match"
  else
    Ok
      {
        owned_template_issuer = owned_scalar_model_issuer;
        owned_template_program = program;
        owned_template_program_snapshot = Sst.to_string program;
        owned_template_definition = definition;
        owned_template_body = body;
        owned_template_body_snapshot =
          Marshal.to_string body [ Marshal.No_sharing ];
        owned_template_domain = domain;
        owned_template_formal = formal;
        owned_template_evidence = evidence;
        owned_template_result = result_id;
        owned_template_result_fields = result_fields;
        owned_template_paths = List.rev !paths;
        owned_template_reads = List.rev !reads;
        owned_template_projections = List.rev !projections;
        owned_template_matches = List.rev !matches;
        owned_template_result_sources = List.rev !result_sources;
      }
  let validate_spec_expression ?ranked_shape_domain ~rank_domains
      ~physical_program ~parametric_adts types functions definition
      (expression : Sst.expression) =
  let function_id = definition.Sst.function_id in
  let model_domain =
    Spec_definition.authenticated_model_domain types definition
  in
  let position id =
    let rec loop index = function
      | [] -> None
      | (candidate : Sst.function_definition) :: rest ->
          if same_function_id candidate.function_id id then Some index
          else loop (index + 1) rest
    in
    loop 0 functions
  in
  let current_position = position function_id in
  let malformed span detail =
    fail ~function_id span (Malformed_expression detail)
  in
  let admitted_logical_type typ =
    (match typ with
    | Sst.Application _ ->
        Result.is_ok
          (Parametric_rank_domain_private.derive_application
             ~program:physical_program ~span:expression.span typ)
    | Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _ | Sst.Aggregate _
    | Sst.Parameter _ ->
        false)
    ||
    match ranked_shape_domain with
    | Some selected ->
        admit_rank_backed_logical_shape ~rank_domains ~parametric_adts types
          selected typ
    | None ->
        is_logical_type types typ || locally_ranked_type rank_domains typ
  in
  let application_descriptor = function
    | Sst.Application (constructor, arguments) ->
        Option.map (fun descriptor -> (descriptor, arguments))
          (Parametric_adt.find parametric_adts constructor)
    | Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _ | Sst.Aggregate _
    | Sst.Parameter _ -> None
  in
  let application_owner typ expected =
    match application_descriptor typ with
    | Some (descriptor, _) -> Parametric_adt.type_id descriptor = expected
    | None -> typ = Sst.Aggregate expected
  in
  let instantiated_field_type typ (field : Sst.field_id) =
    match application_descriptor typ with
    | None ->
        Option.map (fun definition -> definition.Sst.field_type)
          (lookup_field types field)
    | Some (descriptor, arguments) ->
        let constructor_index =
          match field.field_owner with
          | Sst.Record_owner _ -> None
          | Sst.Constructor_owner constructor -> Some constructor.constructor_index
        in
        Parametric_adt.instantiate_field_by_index descriptor arguments
          ~constructor_index ~field_index:field.field_index
        |> Result.to_option
  in
  let type_definition type_id = lookup_type types type_id in
  let constructor_definition constructor =
    match type_definition constructor.Sst.constructor_type with
    | Some { type_kind = Sst.Variant_definition constructors; _ } ->
        List.find_opt
          (fun (candidate : Sst.constructor_definition) ->
            candidate.constructor_id = constructor)
          constructors
    | Some { type_kind = Sst.Record_definition _; _ } | None -> None
  in
    let constructor_argument_types (constructor : Sst.constructor_definition) =
    let fields = constructor.constructor_fields in
    if
      fields = []
      || List.for_all
           (fun (field : Sst.field_definition) ->
             String.equal field.field_id.field_name
               (Printf.sprintf "$%d" field.field_id.field_index))
           fields
    then List.map (fun field -> field.Sst.field_type) fields
    else [ Sst.Aggregate constructor.constructor_id.constructor_type ]
  in
  let constructor_argument_types_for typ (constructor : Sst.constructor_definition) =
    constructor_argument_types constructor
    |> List.mapi (fun index template ->
           match application_descriptor typ with
           | None -> template
           | Some (descriptor, arguments) ->
               Option.value ~default:template
                 (Parametric_adt.instantiate_field_by_index descriptor arguments
                    ~constructor_index:(Some constructor.constructor_id.constructor_index)
                    ~field_index:index
                  |> Result.to_option))
  in
  let record_fields record_type supplied =
    match type_definition record_type with
    | Some { type_kind = Sst.Record_definition fields; _ } -> Some fields
    | Some { type_kind = Sst.Variant_definition constructors; _ } ->
        List.find_map
          (fun (constructor : Sst.constructor_definition) ->
            if
              List.map (fun (field, _) -> field) supplied
              = List.map
                  (fun (field : Sst.field_definition) -> field.field_id)
                  constructor.constructor_fields
            then Some constructor.constructor_fields
            else None)
          constructors
    | None -> None
  in
  let rec validate_pattern ~irrefutable (pattern : Sst.pattern) =
    let* () =
      if
        admitted_logical_type pattern.typ
        || Parametric_type.is_spec_function pattern.typ
      then Ok ()
      else malformed pattern.span "spec pattern type is not a logical value"
    in
    match pattern.pattern_desc with
    | Sst.Wildcard -> Ok ()
    | Sst.Bind binding ->
        if binding.typ = pattern.typ then Ok ()
        else malformed pattern.span "spec binding pattern type mismatch"
    | Sst.Unit_pattern ->
        if pattern.typ = Sst.Unit then Ok ()
        else malformed pattern.span "spec unit pattern type mismatch"
    | Sst.Int_pattern _ ->
        if irrefutable then
          malformed pattern.span "spec let patterns must be irrefutable"
        else if pattern.typ = Sst.Int then Ok ()
        else malformed pattern.span "spec integer pattern type mismatch"
    | Sst.Bool_pattern _ ->
        if irrefutable then
          malformed pattern.span "spec let patterns must be irrefutable"
        else if pattern.typ = Sst.Bool then Ok ()
        else malformed pattern.span "spec Boolean pattern type mismatch"
    | Sst.Tuple_pattern components -> (
        match pattern.typ with
        | Sst.Tuple expected
          when List.length components = List.length expected ->
            List.fold_left2
              (fun result (label, (nested : Sst.pattern))
                   (expected_label, expected_type) ->
                let* () = result in
                  if label <> expected_label || nested.Sst.typ <> expected_type
                  then
                    malformed nested.span
                      "spec tuple pattern component mismatch"
                else validate_pattern ~irrefutable nested)
              (Ok ()) components expected
          | Sst.Tuple _ | Sst.Unit | Sst.Bool | Sst.Int | Sst.Aggregate _
          | Sst.Parameter _ | Sst.Application _ ->
            malformed pattern.span "spec tuple pattern type mismatch")
    | Sst.Record_pattern fields ->
        let validate_field (field, (nested : Sst.pattern)) =
          match instantiated_field_type pattern.typ field with
          | Some expected
            when application_owner pattern.typ (owner_type field.field_owner)
                 && expected = nested.Sst.typ ->
              validate_pattern ~irrefutable nested
          | Some _ | None ->
              malformed nested.span
                "spec record pattern field has the wrong nominal owner"
        in
        iter_result validate_field fields
      | Sst.Constructor_pattern (constructor, arguments) -> (
        if irrefutable then
          malformed pattern.span "spec let patterns must be irrefutable"
          else
            match (pattern.typ, constructor_definition constructor) with
          | typ, Some constructor_definition
            when application_owner typ constructor.constructor_type
                 && List.length arguments
                    = List.length
                        (constructor_argument_types_for typ constructor_definition) ->
              List.fold_left2
                (fun result (argument : Sst.pattern) expected_type ->
                  let* () = result in
                  if argument.Sst.typ <> expected_type then
                    malformed argument.span
                      "spec constructor pattern argument type mismatch"
                  else validate_pattern ~irrefutable:false argument)
                (Ok ()) arguments
                (constructor_argument_types_for typ constructor_definition)
          | ( ( Sst.Aggregate _ | Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _
                | Sst.Parameter _ | Sst.Application _ ),
                (Some _ | None) ) ->
              malformed pattern.span
                "spec constructor pattern has the wrong nominal owner")
    | Sst.Owned_tree_cursor_pattern _ | Sst.Or_pattern _ ->
        malformed pattern.span "pattern is outside the aggregate spec grammar"
  in
  let opaque_variable domain (expression : Sst.expression) =
    match expression.expression_desc with
    | Sst.Variable { binding = { typ = Sst.Aggregate actual; _ }; _ }
        when same_type_id domain actual && expression.typ = Sst.Aggregate domain
        ->
        Ok ()
    | _ ->
        malformed expression.span
            "authenticated abstract values may pass only opaquely to their \
             exact model"
  in
  let rec loop (expression : Sst.expression) =
    match expression.expression_desc with
    | Sst.Int_constant _ when expression.typ = Sst.Int -> Ok ()
    | Sst.Bool_constant _ when expression.typ = Sst.Bool -> Ok ()
    | Sst.Unit_constant when expression.typ = Sst.Unit -> Ok ()
    | Sst.Variable { binding; _ }
      when binding.typ = expression.typ
           && (admitted_logical_type binding.typ
              || Parametric_type.is_spec_function binding.typ) ->
        Ok ()
    | Sst.Tuple_value components -> (
        match expression.typ with
        | Sst.Tuple expected
          when List.length components = List.length expected ->
            List.fold_left2
              (fun result (label, component) (expected_label, expected_type) ->
                let* () = result in
                if
                  label <> expected_label
                  || component.Sst.typ <> expected_type
                then
                  malformed component.span
                    "spec tuple component type or label mismatch"
                else loop component)
              (Ok ()) components expected
        | Sst.Tuple _ | Sst.Unit | Sst.Bool | Sst.Int | Sst.Aggregate _
          | Sst.Parameter _ | Sst.Application _ ->
            malformed expression.span "spec tuple value type mismatch")
    | Sst.Record_value { record_type; fields } -> (
        match record_fields record_type fields with
        | Some expected_fields
            when application_owner expression.typ record_type
            && admitted_logical_type expression.typ
            && List.length fields = List.length expected_fields ->
            List.fold_left2
                (fun result (field, value) (expected : Sst.field_definition) ->
                let* () = result in
                if
                  field <> expected.field_id
                  || instantiated_field_type expression.typ field <> Some value.Sst.typ
                then
                  malformed value.span
                    "spec record construction field mismatch"
                else loop value)
              (Ok ()) fields expected_fields
        | Some _ | None ->
            malformed expression.span
              "spec record construction has the wrong nominal type")
    | Sst.Constructor_value { constructor; arguments } -> (
        match constructor_definition constructor with
        | Some expected
            when application_owner expression.typ constructor.constructor_type
            && admitted_logical_type expression.typ
            && List.length arguments
               = List.length (constructor_argument_types_for expression.typ expected) ->
            List.fold_left2
              (fun result argument expected_type ->
                let* () = result in
                if argument.Sst.typ <> expected_type then
                  malformed argument.span
                    "spec constructor argument type mismatch"
                else loop argument)
                (Ok ()) arguments
                (constructor_argument_types_for expression.typ expected)
        | Some _ | None ->
            malformed expression.span
              "spec constructor has the wrong nominal type")
      | Sst.Field_read { record; field } -> (
        let* () =
          match lookup_field types field with
          | Some _ -> Ok ()
          | None -> malformed expression.span "spec field is not registered"
        in
        let field_owner = owner_type field.field_owner in
        let* () =
          if
            application_owner record.typ field_owner
            && instantiated_field_type record.typ field = Some expression.typ
          then Ok ()
          else
            malformed expression.span
              "spec field projection has the wrong nominal domain"
        in
          match model_domain with
        | Some domain when same_type_id domain field_owner ->
            let* () = opaque_variable domain record in
            if admitted_logical_type expression.typ then Ok ()
            else
              malformed expression.span
                "model representation projection must produce a logical value"
        | Some _ | None ->
            let* () = loop record in
            if admitted_logical_type expression.typ then Ok ()
            else
              malformed expression.span
                "spec field projection result is not a logical value")
    | Sst.Let (bindings, body) ->
        let* () =
          if bindings = [] then
            malformed expression.span "spec let must bind at least one value"
          else
            iter_result
              (fun ((pattern : Sst.pattern), (value : Sst.expression)) ->
                let* () =
                  if pattern.Sst.typ = value.Sst.typ then
                    validate_pattern ~irrefutable:true pattern
                  else
                    malformed pattern.span
                      "spec let pattern and value types differ"
                in
                loop value)
              bindings
        in
        let* () = loop body in
        if body.typ = expression.typ then Ok ()
        else malformed expression.span "spec let body type mismatch"
    | Sst.If (condition, consequent, Some alternative) ->
        let* () =
          if condition.typ = Sst.Bool then loop condition
          else
            malformed condition.span
              "spec conditional condition must be Boolean"
        in
        let* () = loop consequent in
        let* () = loop alternative in
        if consequent.typ = expression.typ && alternative.typ = expression.typ
        then Ok ()
        else malformed expression.span "spec conditional branch type mismatch"
    | Sst.Match
        (({ expression_desc = Sst.Tuple_value _; _ } as scrutinee), cases)
      when definition.recursive ->
        Recursive_spec_body_validation_private.validate_logical_tuple_match
          ~malformed ~validate_expression:loop
          ~validate_pattern:(validate_pattern ~irrefutable:false)
          ~result_type:expression.typ scrutinee cases
    | Sst.Match (scrutinee, cases) ->
        let* () =
          if admitted_logical_type scrutinee.typ then loop scrutinee
          else
            malformed scrutinee.span
              "spec match scrutinee is not a logical value"
        in
        if cases = [] then malformed expression.span "spec match has no cases"
        else
          iter_result
            (fun (case : Sst.case) ->
              let* () =
                if case.case_pattern.typ = scrutinee.typ then
                  validate_pattern ~irrefutable:false case.case_pattern
                else
                  malformed case.case_pattern.span
                    "spec match pattern has the wrong nominal type"
              in
              let* () =
                match case.case_guard with
                | None -> Ok ()
                | Some guard ->
                    if guard.typ = Sst.Bool then loop guard
                    else
                        malformed guard.span "spec match guard must be Boolean"
              in
              if case.case_body.typ = expression.typ then loop case.case_body
              else
                malformed case.case_body.span
                  "spec match case result type mismatch")
            cases
    | Sst.Checked_arithmetic (operation, operands) ->
        let expected_arity =
          match operation with
          | Sst.Add | Sst.Subtract -> 2
          | Sst.Negate | Sst.Multiply_constant _ | Sst.Successor
          | Sst.Predecessor | Sst.Absolute_value ->
              1
        in
        if
          expression.typ <> Sst.Int
          || List.length operands <> expected_arity
          || List.exists (fun operand -> operand.Sst.typ <> Sst.Int) operands
        then
          malformed expression.span
            "spec arithmetic has an invalid scalar type or arity"
        else iter_result loop operands
    | Sst.Compare (comparison, left, right) ->
        let supported_operands =
          left.typ = right.typ
          &&
          match comparison with
          | Sst.Equal | Sst.Not_equal ->
                scalar_type left.typ || admitted_logical_type left.typ
                || Parametric_type.is_spec_function left.typ
          | Sst.Less_than | Sst.Less_or_equal | Sst.Greater_than
          | Sst.Greater_or_equal ->
              left.typ = Sst.Int
        in
        if expression.typ <> Sst.Bool || not supported_operands then
          malformed expression.span
            "spec comparison has invalid scalar operand types"
        else
          let* () = loop left in
          loop right
    | Sst.Boolean_not operand ->
        if expression.typ <> Sst.Bool || operand.typ <> Sst.Bool then
          malformed expression.span "spec Boolean negation has invalid type"
        else loop operand
    | Sst.Boolean_binary (_, left, right) ->
        if
            expression.typ <> Sst.Bool || left.typ <> Sst.Bool
            || right.typ <> Sst.Bool
        then
          malformed expression.span "spec Boolean operation has invalid type"
        else
          let* () = loop left in
          loop right
    | Sst.Forall quantifier | Sst.Exists quantifier ->
        Quantifier_validation_private.validate_sst_contents
          {
            validate_content = loop;
            invalid_sst = malformed expression.span;
          }
          ~function_id expression quantifier
    | Sst.Direct_call _
      when
        Spec_function_sst_private.lambda expression <> None
        || Spec_function_sst_private.application expression <> None
        || Spec_function_sst_private.is_reference expression ->
        Option.get
          (Spec_function_sst_private.validate_logical_special
             {
               recurse = loop;
               position;
               current_position;
               invalid =
                 (fun span message ->
                   {
                     function_id = Some function_id;
                     span;
                     kind = Invalid_call message;
                   });
             }
             expression)
    | Sst.Direct_call
        {
          call_form = Sst.Specification_call;
          callee;
          arguments;
          recursive = true;
            _;
        }
      when definition.recursive && same_function_id callee function_id ->
        iter_result
          (fun argument ->
            let _, argument = Sst.require_value_argument argument in
            loop argument)
          arguments
    | Sst.Direct_call
        {
          call_form = Sst.Specification_call;
          callee;
          arguments;
          recursive = false;
            _;
          } -> (
        let* () =
          if admitted_logical_type expression.typ then Ok ()
          else
            malformed expression.span
              "specification call result is not a logical value"
        in
        let* () =
          match (current_position, position callee) with
          | Some current, Some callee_position when callee_position < current ->
              Ok ()
          | _ ->
              fail ~function_id expression.span
                (Invalid_call
                   "spec definitions may call only earlier validated spec \
                    declarations")
        in
        let callee_definition =
          List.find_opt
            (fun (candidate : Sst.function_definition) ->
              same_function_id candidate.function_id callee)
            functions
        in
          match callee_definition with
        | Some callee_definition -> (
            match
              Spec_definition.authenticated_model_domain types
                callee_definition
            with
            | Some domain -> (
                match arguments with
                | [ Sst.Value_argument { label = None; value = argument } ] ->
                    opaque_variable domain argument
                | _ ->
                    malformed expression.span
                      "authenticated model call has an invalid signature")
            | None ->
                iter_result
                  (fun argument ->
                    let _, argument = Sst.require_value_argument argument in
                    loop argument)
                  arguments)
        | None ->
            malformed expression.span
              "specification call target is not registered")
    | Sst.Symbolic_application application ->
        if
          not
            (admitted_logical_type expression.typ
            || Parametric_type.is_spec_function expression.typ
            || Parametric_adt.deeply_immutable_instance parametric_adts
                 expression.typ)
        then
          malformed expression.span
            "symbolic application result is not a logical value"
        else
          iter_result loop
            (Symbolic_application_private.arguments application)
    | Sst.Int_constant _ | Sst.Bool_constant _ | Sst.Unit_constant ->
        malformed expression.span "spec constant type mismatch"
      | Sst.Variable _ | Sst.Field_write _ | Sst.Shared_scalar_field_write _
    | Sst.Owned_tree_nested_write _ | Sst.Owned_tree_rebase _
    | Sst.Let_mutable _ | Sst.Mutable_read _ | Sst.Mutable_write _
      | Sst.Sequence _
      | Sst.If (_, _, None)
      | Sst.Proof_region _ | Sst.Reveal _ | Sst.Reveal_with_fuel _ | Sst.Old _
      | Sst.Use_type_invariant _ | Sst.Local_assert _ | Sst.Direct_call _
      | Sst.Callback_call _ | Sst.Callback_requires _ | Sst.Callback_ensures _
      | Sst.Optional_absent | Sst.Optional_present _ | Sst.Optional_forward _ ->
        malformed expression.span
          "expression is outside the aggregate spec grammar"
  in
  loop expression
let validate_spec_graph functions =
  let spec_definitions =
    List.filter
      (fun (definition : Sst.function_definition) ->
        definition.mode = Sst.Spec)
      functions
  in
  let rec calls expression =
    let nested = List.concat_map calls (expression_children expression) in
    match expression.Sst.expression_desc with
      | Sst.Direct_call { call_form = Sst.Specification_call; callee; _ } ->
        callee :: nested
    | _ -> nested
  in
  let outgoing definition =
    match definition.Sst.body with
    | Sst.Spec_definition body -> calls body.expression
    | Sst.Recursive_spec_definition { body; _ } ->
        List.filter
          (fun callee ->
            not
              (definition.recursive
              && same_function_id callee definition.function_id))
          (calls body.expression)
    | _ -> []
  in
  let rec visit active complete definition =
    let id = definition.Sst.function_id in
    if List.exists (same_function_id id) active then
      fail ~function_id:id definition.span
        (Invalid_call "cyclic pure specification call graph")
    else if List.exists (same_function_id id) complete then Ok complete
    else
      let active = id :: active in
      let* complete =
        fold_result
          (fun complete callee ->
            match
              List.find_opt
                (fun candidate ->
                    same_function_id candidate.Sst.function_id callee)
                spec_definitions
            with
            | None -> Ok complete
            | Some callee_definition ->
                visit active complete callee_definition)
          complete (outgoing definition)
      in
      Ok (id :: complete)
  in
  let* _ =
    fold_result
      (fun complete definition -> visit [] complete definition)
      [] spec_definitions
  in
  Ok ()
  let validate_proof_expression ~rank_domains ~parametric_adts ~types functions
      definition expression =
  let function_id = definition.Sst.function_id in
  let ranked_shape_domain = ref None in
  let admitted_value_type typ =
    if Parametric_adt.deeply_immutable_instance parametric_adts typ then true
    else if definition.recursive then
      locally_ranked_type rank_domains typ
      ||
      match typ with
      | Sst.Aggregate _ -> is_logical_type types typ
      | Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _ | Sst.Parameter _
      | Sst.Application _ ->
          false
    else
      admit_rank_backed_logical_shape ~rank_domains ~parametric_adts types
        ranked_shape_domain typ
  in
  let rec admitted_pattern (pattern : Sst.pattern) =
    admitted_value_type pattern.typ
    &&
    match pattern.pattern_desc with
    | Sst.Tuple_pattern components ->
        List.for_all (fun (_, nested) -> admitted_pattern nested) components
    | Sst.Record_pattern fields ->
        List.for_all (fun (_, nested) -> admitted_pattern nested) fields
    | Sst.Constructor_pattern (_, arguments) ->
        List.for_all admitted_pattern arguments
    | Sst.Or_pattern (left, right) ->
        admitted_pattern left && admitted_pattern right
    | Sst.Wildcard | Sst.Bind _ | Sst.Int_pattern _ | Sst.Bool_pattern _
    | Sst.Unit_pattern | Sst.Owned_tree_cursor_pattern _ ->
        true
  in
  let position id =
    let rec loop index = function
      | [] -> None
      | (candidate : Sst.function_definition) :: rest ->
          if same_function_id candidate.function_id id then Some index
          else loop (index + 1) rest
    in
    loop 0 functions
  in
  let current = position function_id in
  let rec loop (expression : Sst.expression) =
    let reject () =
      fail ~function_id expression.span
          (Malformed_expression
             "expression is outside the initial proof grammar")
    in
    match expression.expression_desc with
      | Sst.Int_constant _ | Sst.Bool_constant _ | Sst.Unit_constant -> Ok ()
    | Sst.Variable _
      when scalar_type expression.typ || expression.typ = Sst.Unit
           || admitted_value_type expression.typ ->
        Ok ()
    | Sst.Let (bindings, body) ->
        let* () =
          iter_result
            (fun (pattern, value) ->
              match pattern.Sst.pattern_desc with
                | Sst.Bind { typ = Sst.Int | Sst.Bool | Sst.Unit; _ }
                when pattern.typ = value.Sst.typ ->
                  loop value
                | Sst.Bind { typ; _ }
                  when pattern.typ = value.Sst.typ && admitted_value_type typ ->
                  loop value
              | Sst.Wildcard
                  when (scalar_type pattern.typ || pattern.typ = Sst.Unit
                  || admitted_value_type pattern.typ)
                  && pattern.typ = value.Sst.typ ->
                  loop value
              | _ -> reject ())
            bindings
        in
        loop body
    | Sst.Sequence (first, second) ->
        if first.typ <> Sst.Unit then reject ()
        else
          let* () = loop first in
          loop second
    | Sst.If (condition, consequent, alternative) ->
        if condition.typ <> Sst.Bool then reject ()
        else
          let* () = loop condition in
          let* () = loop consequent in
          iter_result loop (Option.to_list alternative)
    | Sst.Checked_arithmetic (_, operands) -> iter_result loop operands
    | Sst.Compare (comparison, left, right) ->
        let supported =
          left.typ = right.typ
          &&
          match comparison with
          | Sst.Equal | Sst.Not_equal ->
                scalar_type left.typ || admitted_value_type left.typ
          | Sst.Less_than | Sst.Less_or_equal | Sst.Greater_than
          | Sst.Greater_or_equal ->
              left.typ = Sst.Int
        in
        if not supported then reject ()
        else
          let* () = loop left in
          loop right
    | Sst.Boolean_binary (_, left, right) ->
        let* () = loop left in
        loop right
    | Sst.Boolean_not operand -> loop operand
    | Sst.Forall quantifier | Sst.Exists quantifier ->
        Quantifier_validation_private.validate_sst_contents
          {
            validate_content = loop;
            invalid_sst = (fun _ -> reject ());
          }
          ~function_id expression quantifier
    | Sst.Constructor_value { arguments; _ }
      when admitted_value_type expression.typ ->
        iter_result loop arguments
      | Sst.Record_value { fields; _ } when admitted_value_type expression.typ
        ->
        iter_result (fun (_, value) -> loop value) fields
      | Sst.Field_read { record; _ } when admitted_value_type record.typ ->
        loop record
      | Sst.Match (scrutinee, cases) when admitted_value_type scrutinee.typ ->
        let* () = loop scrutinee in
        iter_result
          (fun (case : Sst.case) ->
            if
              (not definition.recursive)
              && not (admitted_pattern case.case_pattern)
            then reject ()
            else
              let* () = iter_result loop (Option.to_list case.case_guard) in
              loop case.case_body)
          cases
    | Sst.Direct_call
        {
            call_form = Sst.Specification_call | Sst.Proof_call;
          callee;
          arguments;
          recursive;
            _;
        } ->
        let* () =
          if
            recursive && definition.recursive
            && same_function_id callee function_id
          then Ok ()
          else
            match (recursive, current, position callee) with
            | false, Some current, Some callee when callee < current -> Ok ()
            | _ ->
                fail ~function_id expression.span
                  (Invalid_call
                       "proof bodies may call only their authenticated \
                        recursive self summary or earlier validated spec/proof \
                        declarations")
        in
        iter_result
          (fun argument ->
             let _, argument = Sst.require_value_argument argument in
             loop argument)
          arguments
    | Sst.Reveal _ | Sst.Reveal_with_fuel _ -> Ok ()
    | Sst.Use_type_invariant _ -> Ok ()
    | Sst.Local_assert { predicate; _ } -> loop predicate
    | Sst.Symbolic_application application ->
        if admitted_value_type expression.typ || scalar_type expression.typ then
          iter_result loop
            (Symbolic_application_private.arguments application)
        else reject ()
      | Sst.Variable _ | Sst.Tuple_value _ | Sst.Record_value _
      | Sst.Constructor_value _ | Sst.Field_read _ | Sst.Field_write _
      | Sst.Shared_scalar_field_write _ | Sst.Owned_tree_nested_write _
      | Sst.Owned_tree_rebase _ | Sst.Let_mutable _ | Sst.Mutable_read _
      | Sst.Mutable_write _ | Sst.Match _ | Sst.Proof_region _ | Sst.Old _
      | Sst.Direct_call _ | Sst.Callback_call _ | Sst.Callback_requires _
      | Sst.Callback_ensures _ | Sst.Optional_absent | Sst.Optional_present _
      | Sst.Optional_forward _ ->
        reject ()
  in
  loop expression
let validate_proof_graph functions =
  let rec calls expression =
    let nested = List.concat_map calls (expression_children expression) in
    match expression.Sst.expression_desc with
    | Sst.Direct_call { call_form = Sst.Proof_call; callee; _ } ->
        callee :: nested
    | Sst.Proof_region body -> calls body @ nested
    | _ -> nested
  in
  let proofs =
    List.filter
      (fun (definition : Sst.function_definition) ->
        definition.mode = Sst.Proof)
      functions
  in
  let outgoing definition =
    match definition.Sst.body with
    | Sst.Proof_body { body; _ } ->
        List.filter
          (fun callee ->
            not
              (definition.recursive
              && same_function_id callee definition.function_id))
          (calls body.expression)
    | _ -> []
  in
  let rec visit active done_ definition =
    let id = definition.Sst.function_id in
    if List.exists (same_function_id id) active then
      fail ~function_id:id definition.span
        (Invalid_call "cyclic proof call graph")
    else if List.exists (same_function_id id) done_ then Ok done_
    else
      fold_result
        (fun done_ callee ->
          match
            List.find_opt
              (fun candidate ->
                same_function_id candidate.Sst.function_id callee)
              proofs
          with
          | None -> Ok done_
          | Some target -> visit (id :: active) done_ target)
        done_ (outgoing definition)
      |> Result.map (fun done_ -> id :: done_)
  in
  let* _ =
    fold_result (fun done_ definition -> visit [] done_ definition) [] proofs
  in
  Ok ()
let validate_proof_region_positions ~physical_program
    (definition : Sst.function_definition) expression =
  let function_id = definition.function_id in
  let rec local_positions statement (expression : Sst.expression) =
    match expression.expression_desc with
    | Sst.Local_assert _ when statement && expression.typ = Sst.Unit -> Ok ()
    | Sst.Local_assert _ ->
        fail ~function_id expression.span
          (Malformed_expression
             "local assertion is only valid as a unit-valued Proof statement \
              or branch tail")
    | Sst.Sequence (first, second) ->
        let* () = local_positions true first in
        local_positions statement second
    | Sst.Let (bindings, body) ->
        let* () =
            iter_result (fun (_, value) -> local_positions false value) bindings
        in
        local_positions statement body
    | Sst.If (condition, consequent, alternative) ->
        let* () = local_positions false condition in
        let* () = local_positions statement consequent in
          iter_result (local_positions statement) (Option.to_list alternative)
    | Sst.Match (scrutinee, cases) ->
        let* () = local_positions false scrutinee in
        iter_result
          (fun (case : Sst.case) ->
            let* () =
                iter_result (local_positions false)
                (Option.to_list case.case_guard)
            in
            local_positions statement case.case_body)
          cases
    | _ ->
          iter_result (local_positions false) (expression_children expression)
  in
  let rec loop statement (expression : Sst.expression) =
    match expression.expression_desc with
    | Sst.Proof_region body when statement && expression.typ = Sst.Unit ->
        if
          Typedtree_adapter_private.Public.authenticate_proof_region
            ~program:physical_program ~definition ~expression
        then local_positions true body
        else
          fail ~function_id expression.span
            (Malformed_expression
               "proof region lacks its authenticated retained carrier")
    | Sst.Proof_region _ ->
        fail ~function_id expression.span
          (Malformed_expression
               "proof region is only valid as a unit-valued sequential \
                statement")
    | Sst.Sequence (first, second) ->
        let* () = loop true first in
        loop false second
    | _ -> iter_result (loop false) (expression_children expression)
  in
  loop false expression
let validate_local_assertion_authority ~physical_program
    (definition : Sst.function_definition) =
  let root =
    match definition.body with
    | Sst.Checked_exec { body; _ }
    | Sst.Spec_definition body
    | Sst.Proof_body { body; _ }
    | Sst.Recursive_spec_definition { body; _ } ->
        Some body.expression
      | Sst.External_specification _ | Sst.Trusted_external_spec_target _
    | Sst.Trusted_external_body _ | Sst.Symbolic_declaration _ ->
        None
  in
  let rec collect found expression =
    let found =
      match expression.Sst.expression_desc with
      | Sst.Local_assert { assertion_ordinal; _ } ->
          (assertion_ordinal, expression) :: found
      | _ -> found
    in
    let children =
      match expression.expression_desc with
      | Sst.Proof_region body -> [ body ]
      | _ -> expression_children expression
    in
    List.fold_left collect found children
  in
  let assertions =
    Option.map (collect []) root |> Option.value ~default:[] |> List.rev
  in
  let* () =
    iter_result
      (fun (_ordinal, expression) ->
        if
          Typedtree_adapter_private.Public
            .authenticate_local_assertion_candidate ~program:physical_program
              ~definition ~expression
        then Ok ()
        else
          fail ~function_id:definition.function_id expression.span
            (Malformed_expression
               "local assertion lacks its authenticated static carrier"))
      assertions
  in
  let ordinals = List.map fst assertions in
  if ordinals = List.init (List.length ordinals) Fun.id then Ok ()
  else
    fail ~function_id:definition.function_id definition.span
      (Malformed_expression
         "local assertion ordinals are missing, duplicated, or reordered")
let validate_local_assertion_positions function_id expression =
  let rec loop statement (expression : Sst.expression) =
    match expression.expression_desc with
    | Sst.Local_assert _ when statement && expression.typ = Sst.Unit -> Ok ()
    | Sst.Local_assert _ ->
        fail ~function_id expression.span
          (Malformed_expression
             "local assertion is only valid as a unit-valued Proof statement \
              or branch tail")
    | Sst.Sequence (first, second) ->
        let* () = loop true first in
        loop statement second
    | Sst.Let (bindings, body) ->
          let* () = iter_result (fun (_, value) -> loop false value) bindings in
        loop statement body
    | Sst.If (condition, consequent, alternative) ->
        let* () = loop false condition in
        let* () = loop statement consequent in
        iter_result (loop statement) (Option.to_list alternative)
    | Sst.Match (scrutinee, cases) ->
        let* () = loop false scrutinee in
        iter_result
          (fun (case : Sst.case) ->
            let* () =
              iter_result (loop false) (Option.to_list case.case_guard)
            in
            loop statement case.case_body)
          cases
    | Sst.Proof_region body -> loop true body
    | _ -> iter_result (loop false) (expression_children expression)
  in
  loop true expression
let imported_external_link = function
  | Sst.Imported_unverified_target _ -> true
  | Sst.Same_unit_target _ | Sst.Unresolved_target _ -> false
let parametric_decrease ~program ~span typ =
  match typ with
  | Sst.Application _ as application ->
      Result.is_ok
        (Parametric_rank_domain_private.derive_application ~program ~span
           application)
  | Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _ | Sst.Aggregate _
  | Sst.Parameter _ ->
      false
let parametric_formal ~program ~span = function
  | Sst.Application _ as typ ->
      Result.is_ok
        (Parametric_rank_domain_private.derive_application ~program ~span typ)
      || Parametric_adt.deeply_immutable_instance program.Sst.parametric_adts
           typ
  | Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _ | Sst.Aggregate _
  | Sst.Parameter _ ->
      false
let validate_function ~require_authenticated_owned_tree_roots ~rank_domains
    ~physical_program ~external_specifications types functions
    (definition : Sst.function_definition) =
  let function_id = definition.function_id in
  let parametric_adts = physical_program.Sst.parametric_adts in
    let* () = validate_local_assertion_authority ~physical_program definition in
  let* () =
    match
      Typedtree_adapter_private.Public.validate_builtin_assertion_conditions
        ~program:physical_program definition
    with
    | Ok () -> Ok ()
    | Error (span, message) ->
        fail ~function_id span (Malformed_expression message)
  in
  let validate_external_boundary role link =
    let role =
      match role with
      | `Wrapper -> External_specification_validation_private.Wrapper
      | `Target -> External_specification_validation_private.Target
    in
    External_specification_validation_private.validate
      ~external_specifications ~program:physical_program ~functions ~definition
      ~role link
    |> Result.map_error (fun error ->
           {
             function_id = Some function_id;
             span = error.External_specification_validation_private.span;
             kind = Invalid_target_link error.message;
           })
  in
  let* () = validate_policy definition.span definition.policy in
  let* bound =
    fold_result
      (fun bound parameter ->
        match parameter with
        | Sst.Callback_parameter formal -> (
            match
              Callback_certificate_private.authenticate_shape
                formal.binding.callback_certificate formal.binding.callback_shape
            with
            | Ok () -> Ok bound
            | Error message ->
                fail ~function_id formal.binding.callback_span
                  (Invalid_call message))
        | Sst.Value_parameter parameter ->
            let* bound =
              pattern_bindings function_id types bound parameter.pattern
            in
            match parameter.optional_default with
            | None -> Ok bound
            | Some optional_default ->
                pattern_bindings function_id types bound
                  optional_default.optional_pattern)
      [] definition.parameters
  in
  let* captures =
    Sst_callback_private.authenticated_captures physical_program definition
    |> Result.map_error (fun (span, message) ->
           {
             function_id = Some function_id;
             span;
             kind = Invalid_call message;
           })
  in
  let* bound =
    fold_result
      (fun bound (_, binding) -> add_binding function_id bound binding)
      bound captures
  in
  let* () =
    validate_type_reference types (Some function_id) definition.span
      definition.result_type
  in
  let validate_predicate allow_old (clause : Sst.predicate_clause) =
    if clause.Sst.predicate.stage <> Sst.Logical then
      fail ~function_id clause.span
        (Invalid_clause "contract predicates must be logical")
    else
      let* () =
        validate_expression ~types ~parametric_adts ~functions
          ~physical_program ~external_specifications ~function_id
          ~stage:Sst.Logical
          ~require_authenticated_owned_tree_roots
          ~enclosing_recursive:definition.recursive ~allow_old bound
          clause.predicate.expression
      in
      if clause.predicate.expression.typ = Sst.Bool then Ok ()
      else
        fail ~function_id clause.span
          (Invalid_clause "requires, ensures, and assertions must be Boolean")
  in
  let* () =
    check_clause_indices function_id "requires"
      (List.map
         (fun (clause : Sst.predicate_clause) ->
           (clause.Sst.clause_index, clause.span))
         definition.contracts.requires)
  in
  let* () =
    check_clause_indices function_id "ensures"
      (List.map
         (fun (clause : Sst.ensures_clause) ->
           (clause.Sst.clause_index, clause.span))
         definition.contracts.ensures)
  in
  let* () =
    check_clause_indices function_id "decreases"
      (List.map
         (fun (clause : Sst.predicate_clause) ->
           (clause.Sst.clause_index, clause.span))
         definition.contracts.decreases)
  in
  let* () =
    check_clause_indices function_id "assertions"
      (List.map
         (fun (clause : Sst.predicate_clause) ->
           (clause.Sst.clause_index, clause.span))
         definition.contracts.assertions)
  in
  let* () =
    iter_result (validate_predicate false) definition.contracts.requires
  in
  let* () =
    iter_result
      (fun (clause : Sst.ensures_clause) ->
        let* ensure_bound =
          match clause.Sst.binder with
          | None -> Ok bound
          | Some binder -> pattern_bindings function_id types bound binder
        in
        if clause.predicate.stage <> Sst.Logical then
          fail ~function_id clause.span
            (Invalid_clause "ensures predicates must be logical")
        else
          let* () =
            validate_expression ~types ~parametric_adts ~functions
              ~physical_program ~external_specifications ~function_id
              ~require_authenticated_owned_tree_roots
              ~enclosing_recursive:definition.recursive ~stage:Sst.Logical
                ~allow_old:true ensure_bound clause.predicate.expression
          in
          if clause.predicate.expression.typ = Sst.Bool then Ok ()
          else
            fail ~function_id clause.span
              (Invalid_clause "ensures predicate must be Boolean"))
      definition.contracts.ensures
  in
  let* () =
    iter_result
      (fun (clause : Sst.predicate_clause) ->
        if clause.Sst.predicate.stage <> Sst.Logical then
          fail ~function_id clause.span
            (Invalid_clause "decreases measures must be logical")
        else
          let rec contains_helper_call expression =
            match expression.Sst.expression_desc with
            | Sst.Direct_call
                  { call_form = Sst.Specification_call; recursive = false; _ }
                ->
                true
            | _ ->
                List.exists contains_helper_call
                  (expression_children expression)
          in
          let* () =
            if
              definition.mode = Sst.Spec && definition.recursive
              && contains_helper_call clause.predicate.expression
            then
              fail ~function_id clause.span
                (Invalid_clause
                     "recursive specification decreases clauses cannot call \
                      Spec helpers")
            else Ok ()
          in
          let* () =
            validate_expression ~types ~parametric_adts ~functions
              ~physical_program ~external_specifications ~function_id
              ~require_authenticated_owned_tree_roots
              ~enclosing_recursive:definition.recursive ~stage:Sst.Logical
                ~allow_old:false bound clause.predicate.expression
          in
          if clause.predicate.expression.typ = Sst.Int then Ok ()
          else if
            match
              owned_recursive_contents_for_helper ~program:physical_program
                definition
            with
            | Some grammar -> (
                match
                  ( Sst_callback_private.value_parameters definition.parameters,
                    clause.predicate.expression.expression_desc )
                with
                | ( [
                     {
                       pattern =
                         {
                           pattern_desc = Sst.Bind formal;
                           typ = Sst.Aggregate formal_type;
                           _;
                         };
                       _;
                     };
                   ],
                    Sst.Variable { binding = measure; _ } ) ->
                    formal = measure
                    && same_type_id formal_type
                           (Owned_recursive_contents_private.carrier_type
                              grammar)
                | _ -> false)
            | None -> false
          then Ok ()
          else if
            authenticated_frozen_spine_helper types definition
            &&
            match
              ( Sst_callback_private.value_parameters definition.parameters,
                clause.predicate.expression.expression_desc )
            with
            | ( [
                 {
                   pattern =
                     {
                       pattern_desc = Sst.Bind formal;
                       typ = Sst.Aggregate formal_type;
                       _;
                     };
                   _;
                 };
               ],
                Sst.Variable { binding = measure; _ } ) ->
                formal = measure
                  && List.exists
                  (fun (type_definition : Sst.type_definition) ->
                    same_type_id type_definition.type_id formal_type
                    &&
                    match type_definition.representation with
                    | Sst.Abstract_with_evidence
                             (Sst.Authenticated_same_cmt_abstraction evidence)
                           -> (
                        match Sst.frozen_spine_prerequisite evidence with
                        | Some frozen ->
                            same_type_id frozen.frozen_root formal_type
                        | None -> false)
                    | Sst.Revealed
                    | Sst.Abstract_with_evidence
                        (Sst.Incomplete_abstraction_evidence _
                        | Sst.Proposed_same_cmt_abstraction _) ->
                        false)
                  types
            | _ -> false
          then Ok ()
          else if
            match clause.predicate.expression.typ with
            | Sst.Aggregate type_id ->
                List.exists
                  (fun domain ->
                    Parametric_rank_domain_private.immutable domain
                    && List.exists
                         (fun (identity :
                                Typedtree_adapter_issuance_private
                                .rank_type_identity) ->
                           identity.rank_type_id = type_id)
                         (Parametric_rank_domain_private.component domain))
                  rank_domains
            | Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _ | Sst.Parameter _ ->
                false
            | Sst.Application _ as application ->
                parametric_decrease ~program:physical_program
                  ~span:clause.span application
          then Ok ()
          else
            fail ~function_id clause.span
              (Invalid_clause
                 "decreases measure must be int or one locally certified \
                  immutable datatype value"))
      definition.contracts.decreases
  in
  let* () =
    iter_result (validate_predicate false) definition.contracts.assertions
  in
  let* () =
    match (definition.mode, definition.body) with
    | Sst.Exec, Sst.Checked_exec { body; _ } when body.stage = Sst.Runtime ->
        let* () =
          validate_local_assertion_positions function_id body.expression
        in
        let* () =
          validate_proof_region_positions ~physical_program definition
            body.expression
        in
        let* () =
            validate_expression ~types ~parametric_adts ~functions
              ~physical_program ~external_specifications ~function_id
              ~stage:Sst.Runtime ~require_authenticated_owned_tree_roots
            ~enclosing_recursive:definition.recursive ~allow_old:false bound
            body.expression
        in
        if body.expression.typ = definition.result_type then Ok ()
        else
          fail ~function_id definition.span
            (Invalid_body "checked exec body result type mismatch")
    | Sst.Spec, Sst.Symbolic_declaration declaration ->
        let supported = function
          | Sst.Unit | Sst.Int | Sst.Bool | Sst.Parameter _ -> true
          | Sst.Application _ as typ
            when Parametric_type.is_spec_function typ ->
              true
          | Sst.Application _ as typ ->
              Parametric_adt.deeply_immutable_instance parametric_adts typ
          | Sst.Tuple _ | Sst.Aggregate _ -> false
        in
        Symbolic_declaration_private.validate_definition
          ~program:physical_program ~supported definition declaration
        |> Result.map_error (fun message ->
               {
                 function_id = Some function_id;
                 span = definition.span;
                 kind = Invalid_body message;
               })
    | Sst.Spec, Sst.Spec_definition body ->
        let ranked_shape_domain = ref None in
        let owned_recursive_contents =
          match
            collect_owned_recursive_contents_model ~program:physical_program
              definition
          with
          | Ok grammar -> Some grammar
          | Error _ -> None
        in
        let admitted_nonrecursive_shape typ =
          admit_rank_backed_logical_shape ~rank_domains ~parametric_adts types
            ranked_shape_domain typ
          || Parametric_adt.deeply_immutable_instance
               parametric_adts typ
          || Parametric_type.is_spec_function typ
          ||
          match owned_recursive_contents with
          | Some grammar ->
              typ = Owned_recursive_contents_private.result_type grammar
          | None -> false
        in
        let* () =
          if definition.recursive then
            fail ~function_id definition.span
              (Invalid_body "spec definitions must be nonrecursive")
          else if not (contracts_are_empty definition.contracts) then
            fail ~function_id definition.span
              (Invalid_body "spec definitions cannot carry contracts")
          else if definition.returns_unique_parameter <> None then
            fail ~function_id definition.span
              (Invalid_body "spec definitions cannot return unique state")
          else if body.stage <> Sst.Logical then
            fail ~function_id definition.span
              (Invalid_body "spec definitions must be logical")
          else
            let* () =
              validate_expression ~types ~parametric_adts ~functions
              ~physical_program ~external_specifications ~function_id
                ~require_authenticated_owned_tree_roots
                  ~enclosing_recursive:false ~stage:Sst.Logical ~allow_old:false
                  bound body.expression
            in
            let model_domain =
              Spec_definition.authenticated_model_domain types definition
            in
            let invariant_domain =
              Spec_definition.authenticated_invariant_domain types definition
            in
            if not (admitted_nonrecursive_shape definition.result_type) then
              fail ~function_id definition.span
                (Invalid_body
                     "spec definition result is not an admitted immutable \
                      logical value")
            else if
              match (model_domain, invariant_domain) with
              | Some _, _ | _, Some _ -> false
              | None, None ->
                  Sst_callback_private.has_callback_parameters definition.parameters
                  || List.exists
                    (fun (parameter : Sst.value_parameter) ->
                      match parameter.pattern.pattern_desc with
                      | Sst.Bind { typ; _ } ->
                          not (admitted_nonrecursive_shape typ)
                      | Sst.Wildcard | Sst.Owned_tree_cursor_pattern _
                      | Sst.Int_pattern _ | Sst.Bool_pattern _
                      | Sst.Unit_pattern | Sst.Tuple_pattern _
                      | Sst.Record_pattern _ | Sst.Constructor_pattern _
                      | Sst.Or_pattern _ ->
                          true)
                    (Sst_callback_private.value_parameters definition.parameters)
            then
              fail ~function_id definition.span
                (Invalid_body
                   "spec parameters must be unlabelled immutable logical \
                    variables")
            else if body.expression.typ <> definition.result_type then
              fail ~function_id definition.span
                (Invalid_body "spec definition result type mismatch")
            else if
              Option.is_some invariant_domain
              && definition.result_type <> Sst.Bool
            then
              fail ~function_id definition.span
                (Invalid_body "type invariant predicates must return bool")
              else if authenticated_frozen_spine_logical_role types definition
            then Ok ()
            else if Option.is_some owned_recursive_contents then Ok ()
            else
              match
                validate_spec_expression ~ranked_shape_domain ~rank_domains
                  ~physical_program
                  ~parametric_adts:parametric_adts types functions definition body.expression
              with
              | Ok () -> Ok ()
              | Error original -> (
                  match model_domain with
                  | Some _ -> (
                      match
                        collect_owned_root_scalar_model
                          ~program:physical_program ~types definition
                      with
                      | Ok _ -> Ok ()
                      | Error _ -> Error original)
                  | None -> Error original)
        in
        Ok ()
    | ( Sst.Spec,
          Sst.Recursive_spec_definition { body; provenance = _; visibility = _ }
        ) ->
        let ranked_shape_domain = ref None in
        let aggregate_result =
          match definition.result_type with
          | Sst.Aggregate _ | Sst.Application _ -> true
          | Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _ | Sst.Parameter _ ->
              false
        in
          let frozen_helper = frozen_spine_for_helper types definition in
        let owned_contents_helper =
          owned_recursive_contents_for_helper ~program:physical_program
            definition
        in
        let admitted_recursive_shape typ =
          match typ with
          | Sst.Int | Sst.Bool -> true
          | Sst.Aggregate type_id
              when Option.fold ~none:false
                ~some:(fun frozen ->
                  same_type_id type_id frozen.Sst.frozen_root
                  || same_type_id type_id frozen.frozen_result_type)
                frozen_helper ->
              true
          | (Sst.Aggregate _ | Sst.Application _) as typ ->
              if aggregate_result then
                admit_rank_backed_logical_shape ~rank_domains ~parametric_adts
                  types ranked_shape_domain typ
              else locally_ranked_type rank_domains typ
          | Sst.Unit | Sst.Tuple _ | Sst.Parameter _ -> false
        in
        let admitted_recursive_shape typ =
          admitted_recursive_shape typ
          || Parametric_type.is_spec_function typ
          ||
          (match typ with
          | Sst.Parameter binder ->
              List.exists
                (fun candidate ->
                  Parametric_type.compare_binder binder candidate = 0)
                definition.type_binders
          | Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _ | Sst.Aggregate _
          | Sst.Application _ ->
              false)
          || parametric_formal ~program:physical_program
               ~span:definition.span typ
          ||
          match owned_contents_helper with
          | Some grammar ->
              typ
              = Sst.Aggregate
                  (Owned_recursive_contents_private.carrier_type grammar)
              || typ = Owned_recursive_contents_private.result_type grammar
          | None -> false
        in
        let authenticated =
          Typedtree_adapter_private.Public
          .authenticate_recursive_specification_in_program
            ~program:physical_program ~definition
        in
        if not definition.recursive then
          fail ~function_id definition.span
            (Invalid_body
               "recursive spec definition requires a recursive declaration")
        else if not authenticated then
          fail ~function_id definition.span
            (Invalid_body
               "recursive spec definitions require authenticated retained \
                provenance")
        else if
          definition.contracts.requires <> []
          || definition.contracts.ensures <> []
          || definition.contracts.assertions <> []
        then
          fail ~function_id definition.span
            (Invalid_body
               "recursive spec definitions allow only one decreases clause")
        else if List.length definition.contracts.decreases <> 1 then
          fail ~function_id definition.span
            (Invalid_body
               "recursive spec definitions require exactly one decreases \
                clause")
        else if definition.returns_unique_parameter <> None then
          fail ~function_id definition.span
            (Invalid_body "recursive spec definitions cannot return unique state")
        else if not (admitted_recursive_shape definition.result_type) then
          fail ~function_id definition.span
            (Invalid_body
               "recursive spec result must be scalar or one exact local \
                revealed deeply immutable aggregate")
        else if
          definition.parameters = []
          || Sst_callback_private.has_callback_parameters definition.parameters
          || List.exists
               (fun (parameter : Sst.value_parameter) ->
                 parameter.Sst.label <> None
                 ||
                 match parameter.pattern.pattern_desc with
                   | Sst.Bind { typ = Sst.Int | Sst.Bool; _ } -> false
                   | Sst.Bind { typ; _ }
                     when Parametric_type.is_spec_function typ ->
                       false
                   | Sst.Bind { typ; uniqueness = Sst.Definitely_aliased; _ }
                   when admitted_recursive_shape typ ->
                     false
                 | _ -> true)
               (Sst_callback_private.value_parameters definition.parameters)
        then
          fail ~function_id definition.span
            (Invalid_body
                 "recursive spec parameters must be unlabelled scalar or \
                  locally certified immutable datatype variables")
        else if body.stage <> Sst.Logical then
          fail ~function_id definition.span
            (Invalid_body "recursive spec body must be logical")
        else
          let* () =
            validate_expression ~types ~parametric_adts ~functions
              ~physical_program ~external_specifications ~function_id
              ~require_authenticated_owned_tree_roots
              ~enclosing_recursive:true ~stage:Sst.Logical ~allow_old:false
              bound body.expression
          in
          if body.expression.typ <> definition.result_type then
            fail ~function_id definition.span
              (Invalid_body "recursive spec body result type mismatch")
          else
            let* () =
              if
                Option.is_some frozen_helper
                || Option.is_some owned_contents_helper
              then Ok ()
              else
                Recursive_spec_body_validation_private.validate
                  ~admitted_type:admitted_recursive_shape
                  ~admit_tuple_match:(not aggregate_result)
                  ~malformed:(fun expression ->
                    {
                      function_id = Some function_id;
                      span = expression.Sst.span;
                      kind =
                        Malformed_expression
                          "expression is outside the direct first-order \
                           recursive-spec grammar";
                    })
                  definition body.expression
            in
            let* expanded =
              match
                Typedtree_adapter_private.Public.expanded_recursive_specification
                  ~program:physical_program ~definition
              with
              | Ok expanded -> Ok expanded
              | Error message ->
                  fail ~function_id body.expression.span
                    (Malformed_expression message)
            in
            let* () =
              if
                Option.is_some frozen_helper
                || Option.is_some owned_contents_helper
              then Ok ()
              else
                Recursive_spec_body_validation_private.validate
                  ~admitted_type:admitted_recursive_shape
                  ~admit_tuple_match:(not aggregate_result)
                  ~malformed:(fun expression ->
                    {
                      function_id = Some function_id;
                      span = expression.Sst.span;
                      kind =
                        Malformed_expression
                          "expression is outside the direct first-order \
                           recursive-spec grammar";
                    })
                  definition expanded
            in
            let* () =
              if
                Option.is_some frozen_helper
                || Option.is_some owned_contents_helper
              then Ok ()
              else if aggregate_result then
                validate_spec_expression ~ranked_shape_domain ~rank_domains
                  ~physical_program
                  ~parametric_adts:parametric_adts types functions definition body.expression
              else
                validate_spec_expression ~rank_domains ~physical_program
                  ~parametric_adts:parametric_adts types functions
                  definition body.expression
            in
            if
              Option.is_some frozen_helper
              || Option.is_some owned_contents_helper
            then Ok ()
            else if aggregate_result then
                validate_spec_expression ~ranked_shape_domain ~rank_domains
                  ~physical_program
                  ~parametric_adts:parametric_adts types functions definition expanded
            else
              validate_spec_expression ~rank_domains ~physical_program
                  ~parametric_adts:parametric_adts types functions
                definition expanded
    | Sst.Proof, Sst.Proof_body { body; provenance } ->
        let* () =
          if
            definition.recursive
            &&
            match provenance with
            | Sst.Authenticated_typedtree _ -> false
            | Sst.Raw_semantic_body _ -> true
          then
            fail ~function_id definition.span
              (Invalid_body
                   "recursive proof declarations require authenticated \
                    retained provenance")
            else if definition.recursive && definition.result_type <> Sst.Unit
            then
            fail ~function_id definition.span
              (Invalid_body "recursive proof declarations must return unit")
            else if definition.recursive && definition.contracts.decreases = []
          then
            fail ~function_id definition.span
              (Invalid_body
                 "recursive proof declarations require exactly one decreases \
                  measure")
          else if
              (not definition.recursive) && definition.contracts.decreases <> []
          then
            fail ~function_id definition.span
              (Invalid_body "proof declarations cannot carry decreases")
          else if definition.returns_unique_parameter <> None then
            fail ~function_id definition.span
              (Invalid_body "proof declarations cannot return unique state")
          else if
            List.exists
              (fun (parameter : Sst.value_parameter) ->
                parameter.Sst.label <> None
                ||
                match parameter.pattern.pattern_desc with
                | Sst.Bind { typ; uniqueness = Sst.Definitely_aliased; _ }
                  when
                    parametric_formal ~program:physical_program
                      ~span:parameter.pattern.span typ ->
                    false
                | Sst.Bind { typ; uniqueness = Sst.Definitely_aliased; _ }
                    when (not definition.recursive) && is_logical_type types typ
                    ->
                    false
                | Sst.Bind { typ; _ }
                    when
                      (not definition.recursive)
                      && Parametric_type.is_spec_function typ ->
                    false
                | Sst.Bind
                    {
                      typ = Sst.Aggregate _;
                      uniqueness = Sst.Definitely_aliased;
                      _;
                    }
                  when not definition.recursive ->
                    false
                  | Sst.Unit_pattern | Sst.Bind { typ = Sst.Unit | Sst.Int | Sst.Bool; _ } -> false
                  | Sst.Bind { typ; uniqueness = Sst.Definitely_aliased; _ }
                  when locally_ranked_type rank_domains typ ->
                    false
                | Sst.Bind
                    {
                      id;
                      typ = Sst.Aggregate type_id;
                      uniqueness = Sst.Definitely_aliased;
                      _;
                    }
                    when match lookup_type types type_id with
                    | Some
                        {
                          representation =
                            Sst.Abstract_with_evidence
                              (Sst.Authenticated_same_cmt_abstraction _);
                          _;
                        } ->
                        let rec contains_use expression =
                          (match expression.Sst.expression_desc with
                          | Sst.Use_type_invariant
                              {
                                value =
                                  {
                                    expression_desc =
                                      Sst.Variable
                                               { binding = { id = used; _ }; _ };
                                    _;
                                  };
                                _;
                              } ->
                              used = id
                          | _ -> false)
                          || List.exists contains_use
                               (expression_children expression)
                        in
                        contains_use body.expression
                         | Some _ | None -> false ->
                    false
                | _ -> true)
              (Sst_callback_private.value_parameters definition.parameters)
          then
            fail ~function_id definition.span
              (Invalid_body
                 "proof parameters must be unlabelled unit, scalar, or locally \
                  certified immutable datatype variables")
          else if body.stage <> Sst.Proof_stage then
            fail ~function_id definition.span
              (Invalid_body "proof bodies must have proof stage")
          else
            let* () =
              validate_local_assertion_positions function_id body.expression
            in
            let* () =
              validate_expression ~types ~parametric_adts ~functions
              ~physical_program ~external_specifications ~function_id
                ~require_authenticated_owned_tree_roots
                ~enclosing_recursive:definition.recursive
                  ~stage:Sst.Proof_stage ~allow_old:false bound body.expression
            in
            if body.expression.typ <> definition.result_type then
              fail ~function_id definition.span
                (Invalid_body "proof body result type mismatch")
            else
                validate_proof_expression ~rank_domains
                  ~parametric_adts:parametric_adts ~types
                  functions definition body.expression
        in
        Ok ()
    | Sst.Exec, Sst.External_specification link ->
        let* () = validate_external_boundary `Wrapper link in
        if definition.recursive then
          fail ~function_id definition.span
            (Invalid_body
               "external specification wrappers must be nonrecursive")
        else if definition.returns_unique_parameter <> None then
          fail ~function_id definition.span
            (Invalid_body
               "external specification wrappers cannot return unique state")
        else if
          definition.contracts.decreases <> []
          || definition.contracts.assertions <> []
        then
          fail ~function_id definition.span
            (Invalid_body
               "external specifications allow only requires and ensures")
        else if imported_external_link link then Ok ()
        else if not (scalar_type definition.result_type) then
          fail ~function_id definition.span
            (Invalid_body
               "external specification result must be scalar int/bool")
        else if
          Sst_callback_private.has_callback_parameters definition.parameters
          || List.exists
            (fun (parameter : Sst.value_parameter) ->
              match parameter.Sst.pattern.pattern_desc with
              | Sst.Bind
                  {
                      typ = Sst.Int | Sst.Bool;
                    uniqueness = Sst.Definitely_aliased;
                    _;
                  } ->
                  false
              | _ -> true)
            (Sst_callback_private.value_parameters definition.parameters)
        then
          fail ~function_id definition.span
            (Invalid_body
               "external specification parameters must be nonunique scalar \
                variables")
        else Ok ()
    | Sst.Exec, Sst.Trusted_external_spec_target link ->
        let* () = validate_external_boundary `Target link in
        if definition.recursive then
          fail ~function_id definition.span
            (Invalid_body "trusted external targets must be nonrecursive")
        else if definition.returns_unique_parameter <> None then
          fail ~function_id definition.span
            (Invalid_body "trusted external targets cannot return unique state")
        else if
          definition.contracts.decreases <> []
          || definition.contracts.assertions <> []
        then
          fail ~function_id definition.span
            (Invalid_body
               "trusted external targets allow only requires and ensures")
        else if not (scalar_type definition.result_type) then
          fail ~function_id definition.span
            (Invalid_body "trusted external target result must be int or bool")
        else Ok ()
    | ( (Sst.Exec | Sst.Proof),
          Sst.Trusted_external_body (Sst.Raw_external_body _) ) ->
        fail ~function_id definition.span
          (Invalid_body
               "raw semantic programs cannot authenticate trusted external \
                bodies")
    | ( (Sst.Exec | Sst.Proof),
        Sst.Trusted_external_body
          (Sst.Authenticated_external_body
            { declaration_span; witness_span = _; source_file = _ }) ) ->
        if definition.recursive then
          fail ~function_id definition.span
            (Invalid_body "trusted external bodies must be nonrecursive")
        else if definition.contracts.ensures = [] then
          fail ~function_id definition.span
            (Invalid_body "trusted external bodies require an ensures clause")
        else if
          definition.contracts.decreases <> []
          || definition.contracts.assertions <> []
        then
          fail ~function_id definition.span
            (Invalid_body
               "trusted external bodies allow only requires and ensures")
        else if declaration_span <> definition.span then
          fail ~function_id definition.span
            (Invalid_body
               "trusted external body provenance must name its declaration")
        else if
            definition.mode = Sst.Proof && definition.result_type <> Sst.Unit
        then
          fail ~function_id definition.span
            (Invalid_body "trusted proof external bodies must return unit")
        else if
          definition.mode = Sst.Proof
          && definition.returns_unique_parameter <> None
        then
          fail ~function_id definition.span
            (Invalid_body
               "trusted proof external bodies cannot return unique state")
        else Ok ()
    | _ ->
        fail ~function_id definition.span
          (Invalid_body "declaration mode and body disposition disagree")
  in
  match definition.returns_unique_parameter with
  | None -> Ok ()
  | Some index -> (
      match
        Option.bind (List.nth_opt definition.parameters index)
          Sst.value_parameter
      with
      | Some
          {
            pattern =
              {
                pattern_desc = Sst.Bind binding;
                typ = Sst.Aggregate result_type;
                _;
              };
            _;
          }
        when binding.uniqueness = Sst.Definitely_unique
             && definition.result_type = Sst.Aggregate result_type ->
          Ok ()
      | _ ->
          fail ~function_id definition.span
            (Invalid_unique_return
                 "unique return must name a unique aggregate parameter of the \
                  result type"))
let same_field_id (left : Sst.field_id) (right : Sst.field_id) =
  left.field_index = right.field_index
  && String.equal left.field_name right.field_name
  &&
  match (left.field_owner, right.field_owner) with
  | Sst.Record_owner left, Sst.Record_owner right -> same_type_id left right
  | Sst.Constructor_owner left, Sst.Constructor_owner right ->
      same_type_id left.constructor_type right.constructor_type
      && left.constructor_index = right.constructor_index
      && String.equal left.constructor_name right.constructor_name
  | Sst.Record_owner _, Sst.Constructor_owner _
  | Sst.Constructor_owner _, Sst.Record_owner _ ->
      false
let same_constructor_identity (left : Sst.constructor_id)
    (right : Sst.constructor_id) =
  same_type_id left.constructor_type right.constructor_type
  && left.constructor_index = right.constructor_index
  && String.equal left.constructor_name right.constructor_name
let frozen_function functions id =
  List.find_opt
    (fun (definition : Sst.function_definition) ->
      same_function_id definition.function_id id)
    functions
  let frozen_binding_parameter root_type (definition : Sst.function_definition)
      =
  match definition.parameters with
  | [
   Sst.Value_parameter
   {
     label = None;
     pattern =
       {
         pattern_desc = Sst.Bind binding;
         typ = Sst.Aggregate pattern_type;
         _;
       };
       optional_default = _;
   };
  ]
    when same_type_id root_type pattern_type
         && binding.typ = Sst.Aggregate root_type
         && binding.uniqueness = Sst.Definitely_aliased ->
      Some binding
  | [] | _ :: _ -> None
let frozen_variable binding (expression : Sst.expression) =
  match expression.expression_desc with
  | Sst.Variable { binding = candidate; _ } -> candidate = binding
  | _ -> false
let validate_frozen_spine_prerequisite types functions
    (definition : Sst.type_definition)
    (evidence : Sst.same_cmt_abstraction_evidence)
    (frozen : Sst.frozen_spine_prerequisite) =
  let forged detail =
    fail definition.span (Forged_abstract_evidence detail)
  in
  let field_exact expected typ mutability (field : Sst.field_definition) =
    same_field_id field.field_id expected
    && field.field_type = typ
    && field.field_mutability = mutability
  in
  let* payload, edge =
    if not (same_type_id frozen.frozen_root definition.type_id) then
      forged "frozen-spine root identity mismatch"
    else
      match definition.type_kind with
      | Sst.Record_definition [ payload; edge ]
          when field_exact frozen.frozen_payload_field Sst.Int Sst.Mutable_field
            payload
          && field_exact frozen.frozen_edge_field
                    (Sst.Aggregate frozen.frozen_link) Sst.Immutable_field edge
          ->
          Ok (payload, edge)
      | Sst.Record_definition _ | Sst.Variant_definition _ ->
          forged
              "frozen-spine root must be the exact payload/immutable-edge \
               record"
  in
  let* nil, next, child =
    match lookup_type types frozen.frozen_link with
    | Some
        {
          type_kind =
            Sst.Variant_definition
              [
                ({ constructor_fields = []; _ } as nil);
                  ({ constructor_fields = [ child ]; _ } as next);
              ];
          representation = Sst.Revealed;
          _;
        }
        when same_constructor_identity nil.constructor_id
          frozen.frozen_nil_constructor
        && same_constructor_identity next.constructor_id
             frozen.frozen_next_constructor
        && field_exact frozen.frozen_next_child_field
                  (Sst.Aggregate frozen.frozen_root) Sst.Immutable_field child
        ->
        Ok (nil, next, child)
      | Some _ | None -> forged "frozen-spine link must be exact Nil/Next(root)"
  in
  let* end_, more, head, tail =
    match lookup_type types frozen.frozen_result_type with
    | Some
        {
          type_kind =
            Sst.Variant_definition
              [
                ({ constructor_fields = []; _ } as end_);
                  ({ constructor_fields = [ head; tail ]; _ } as more);
              ];
          representation = Sst.Revealed;
          _;
        }
        when same_constructor_identity end_.constructor_id
          frozen.frozen_end_constructor
        && same_constructor_identity more.constructor_id
             frozen.frozen_more_constructor
        && field_exact frozen.frozen_more_head_field Sst.Int
             Sst.Immutable_field head
        && field_exact frozen.frozen_more_tail_field
                  (Sst.Aggregate frozen.frozen_result_type) Sst.Immutable_field
                  tail ->
        Ok (end_, more, head, tail)
    | Some _ | None ->
        forged "frozen-spine model result must be exact immutable End/More"
  in
  let find id role =
    match frozen_function functions id with
    | Some definition -> Ok definition
    | None -> forged ("frozen-spine descriptor has no " ^ role)
  in
  let* helper = find frozen.frozen_helper "private helper" in
  let* model = find frozen.frozen_model "model" in
  let* invariant = find frozen.frozen_invariant "invariant" in
  let* constructor = find frozen.frozen_constructor "constructor" in
  let* mutator = find frozen.frozen_mutator "mutator" in
  let* terminal = find frozen.frozen_terminal "terminal" in
  let* helper_root =
    match frozen_binding_parameter frozen.frozen_root helper with
    | Some binding -> Ok binding
    | None -> forged "frozen-spine helper has the wrong formal"
  in
  let helper_decrease =
    match helper.contracts.decreases with
    | [
       { clause_index = 0; predicate = { stage = Sst.Logical; expression }; _ };
    ] ->
        frozen_variable helper_root expression
    | [] | _ :: _ -> false
  in
  let helper_body =
    match helper.body with
    | Sst.Recursive_spec_definition
        {
          visibility = `Opaque;
          provenance = Sst.Authenticated_typedtree _;
          body =
            {
              stage = Sst.Logical;
              expression =
                {
                  expression_desc =
                    Sst.Let
                      ( [
                          ( {
                              pattern_desc = Sst.Bind rest;
                              typ = Sst.Aggregate result_type;
                              _;
                            },
                            {
                              expression_desc =
                                Sst.Match
                                  ( {
                                      expression_desc =
                                        Sst.Field_read
                                          { record = edge_root; field };
                                      typ = Sst.Aggregate link_type;
                                      _;
                                    },
                                    [
                                      {
                                        case_pattern =
                                          {
                                            pattern_desc =
                                              Sst.Constructor_pattern
                                                (nil_id, []);
                                            _;
                                          };
                                        case_guard = None;
                                        case_body =
                                          {
                                            expression_desc =
                                              Sst.Constructor_value
                                                {
                                                  constructor = end_id;
                                                  arguments = [];
                                                };
                                            _;
                                          };
                                        _;
                                      };
                                      {
                                        case_pattern =
                                          {
                                            pattern_desc =
                                              Sst.Constructor_pattern
                                                (next_id,
                                                  [
                                                    {
                                                      pattern_desc =
                                                        Sst.Bind child_binding;
                                                      _;
                                                    };
                                                  ]);
                                            _;
                                          };
                                        case_guard = None;
                                        case_body =
                                          {
                                            expression_desc =
                                              Sst.Direct_call
                                                {
                                                  call_form =
                                                    Sst.Specification_call;
                                                  callee;
                                                  arguments =
                                                    [ Sst.Value_argument
                                                        { label = None;
                                                          value = child_actual } ];
                                                  recursive = true;
                                                    _;
                                                };
                                            _;
                                          };
                                        _;
                                      };
                                    ] );
                              _;
                            } );
                        ],
                        {
                          expression_desc =
                            Sst.Constructor_value
                              {
                                constructor = more_id;
                                arguments =
                                  [
                                    {
                                      expression_desc =
                                        Sst.Field_read
                                          {
                                            record = payload_root;
                                            field = payload_field;
                                          };
                                      _;
                                    };
                                    rest_value;
                                  ];
                              };
                          _;
                        } );
                  _;
                };
            };
        }
      when
        helper.mode = Sst.Spec && helper.recursive
        && helper.result_type = Sst.Aggregate frozen.frozen_result_type
        && helper.contracts.requires = [] && helper.contracts.ensures = []
        && helper.contracts.assertions = []
        && same_type_id result_type frozen.frozen_result_type
        && same_type_id link_type frozen.frozen_link
        && same_field_id field edge.field_id
        && frozen_variable helper_root edge_root
        && same_constructor_identity nil_id nil.constructor_id
        && same_constructor_identity end_id end_.constructor_id
        && same_constructor_identity next_id next.constructor_id
        && child_binding.typ = Sst.Aggregate frozen.frozen_root
        && child_binding.uniqueness = Sst.Definitely_aliased
        && frozen_variable child_binding child_actual
        && same_function_id callee helper.function_id
        && same_constructor_identity more_id more.constructor_id
        && same_field_id payload_field payload.field_id
        && frozen_variable helper_root payload_root
        && frozen_variable rest rest_value ->
          true
    | Sst.Checked_exec _ | Sst.Spec_definition _
    | Sst.Recursive_spec_definition _ | Sst.Proof_body _
    | Sst.External_specification _ | Sst.Trusted_external_spec_target _
    | Sst.Trusted_external_body _ | Sst.Symbolic_declaration _ ->
        false
  in
  let wrapper_call definition callee result_type =
    match (frozen_binding_parameter frozen.frozen_root definition, definition.body) with
    | ( Some root,
        Sst.Spec_definition
          {
            stage = Sst.Logical;
            expression =
              {
                expression_desc =
                  Sst.Direct_call
                    {
                      call_form = Sst.Specification_call;
                      callee = actual;
                      arguments = [ Sst.Value_argument { label = None; value = argument } ];
                      recursive = false;
                        _;
                    };
                _;
              };
          } ) ->
          definition.mode = Sst.Spec && (not definition.recursive)
        && definition.result_type = result_type
        && contracts_are_empty definition.contracts
        && same_function_id actual callee
        && frozen_variable root argument
    | _ -> false
  in
  let model_body =
    wrapper_call model helper.function_id
      (Sst.Aggregate frozen.frozen_result_type)
  in
  let invariant_body =
    match (frozen_binding_parameter frozen.frozen_root invariant, invariant.body) with
    | ( Some root,
        Sst.Spec_definition
          {
            stage = Sst.Logical;
            expression =
              {
                expression_desc =
                  Sst.Match
                    ( {
                        expression_desc =
                          Sst.Direct_call
                            {
                              call_form = Sst.Specification_call;
                              callee;
                              arguments = [ Sst.Value_argument { label = None; value = argument } ];
                              recursive = false;
                                _;
                            };
                        _;
                      },
                      [
                        {
                          case_pattern =
                            {
                              pattern_desc =
                                Sst.Constructor_pattern (end_id, []);
                              _;
                            };
                          case_guard = None;
                          case_body =
                            { expression_desc = Sst.Bool_constant false; _ };
                          _;
                        };
                        {
                          case_pattern =
                            {
                              pattern_desc =
                                Sst.Constructor_pattern (more_id, [ _; _ ]);
                              _;
                            };
                          case_guard = None;
                          case_body =
                            { expression_desc = Sst.Bool_constant true; _ };
                          _;
                        };
                      ] );
                _;
              };
          } ) ->
        invariant.mode = Sst.Spec && invariant.result_type = Sst.Bool
        && contracts_are_empty invariant.contracts
        && same_function_id callee model.function_id
        && frozen_variable root argument
        && same_constructor_identity end_id end_.constructor_id
        && same_constructor_identity more_id more.constructor_id
    | _ -> false
  in
  let constructor_body =
    match constructor.body with
    | Sst.Checked_exec
        {
          provenance = Sst.Authenticated_typedtree _;
          body =
            {
              stage = Sst.Runtime;
              expression =
                {
                  expression_desc =
                    Sst.Record_value
                      {
                        record_type;
                        fields =
                          [
                            (payload_id, first);
                            ( edge_id,
                              {
                                expression_desc =
                                  Sst.Constructor_value
                                    {
                                      constructor = next_id;
                                      arguments =
                                        [
                                          {
                                            expression_desc =
                                              Sst.Record_value
                                                {
                                                  record_type = child_type;
                                                  fields =
                                                    [
                                                      ( child_payload_id,
                                                        second );
                                                      ( child_edge_id,
                                                        {
                                                          expression_desc =
                                                            Sst.Constructor_value
                                                              {
                                                                constructor =
                                                                  nil_id;
                                                                arguments = [];
                                                              };
                                                          _;
                                                        } );
                                                    ];
                                                };
                                            _;
                                          };
                                        ];
                                    };
                                _;
                              } );
                          ];
                      };
                  _;
                };
            };
        } ->
        (match constructor.parameters with
        | [
         Sst.Value_parameter { pattern = { pattern_desc = Sst.Bind first_binding; _ }; _ };
         Sst.Value_parameter { pattern = { pattern_desc = Sst.Bind second_binding; _ }; _ };
        ] ->
            first_binding.typ = Sst.Int && second_binding.typ = Sst.Int
            && frozen_variable first_binding first
            && frozen_variable second_binding second
        | [] | [ _ ] | _ :: _ :: _ -> false)
        && constructor.mode = Sst.Exec && (not constructor.recursive)
        && constructor.result_type = Sst.Aggregate frozen.frozen_root
        && same_type_id record_type frozen.frozen_root
        && same_type_id child_type frozen.frozen_root
        && same_field_id payload_id payload.field_id
        && same_field_id edge_id edge.field_id
        && same_field_id child_payload_id payload.field_id
        && same_field_id child_edge_id edge.field_id
        && same_constructor_identity next_id next.constructor_id
        && same_constructor_identity nil_id nil.constructor_id
    | _ -> false
  in
  let constructor_contract =
    match
      ( constructor.parameters,
        constructor.contracts.requires,
        constructor.contracts.ensures,
        constructor.contracts.decreases,
        constructor.contracts.assertions )
    with
    | ( [
          Sst.Value_parameter { pattern = { pattern_desc = Sst.Bind first_binding; _ }; _ };
          Sst.Value_parameter { pattern = { pattern_desc = Sst.Bind second_binding; _ }; _ };
        ],
        [],
        [
          {
            clause_index = 0;
            binder =
              Some
                {
                  pattern_desc = Sst.Bind result_binding;
                  typ = Sst.Aggregate result_type;
                  _;
                };
            predicate =
              {
                stage = Sst.Logical;
                expression =
                  {
                    expression_desc =
                      Sst.Compare
                        ( Sst.Equal,
                          {
                            expression_desc =
                              Sst.Direct_call
                                {
                                  call_form = Sst.Specification_call;
                                  callee = model_id;
                                  arguments = [ Sst.Value_argument { label = None; value = model_argument } ];
                                  recursive = false;
                                    _;
                                };
                            _;
                          },
                          {
                            expression_desc =
                              Sst.Constructor_value
                                {
                                  constructor = outer_more;
                                  arguments =
                                    [
                                      first_value;
                                      {
                                        expression_desc =
                                          Sst.Constructor_value
                                            {
                                              constructor = inner_more;
                                              arguments =
                                                [
                                                  second_value;
                                                  {
                                                    expression_desc =
                                                      Sst.Constructor_value
                                                        {
                                                          constructor = end_id;
                                                          arguments = [];
                                                        };
                                                    _;
                                                  };
                                                ];
                                            };
                                        _;
                                      };
                                    ];
                                };
                            _;
                          } );
                    _;
                  };
              };
            _;
          };
        ],
        [],
        [] ) ->
        same_type_id result_type frozen.frozen_root
        && result_binding.typ = Sst.Aggregate frozen.frozen_root
        && frozen_variable result_binding model_argument
        && same_function_id model_id model.function_id
        && same_constructor_identity outer_more more.constructor_id
        && same_constructor_identity inner_more more.constructor_id
        && same_constructor_identity end_id end_.constructor_id
        && frozen_variable first_binding first_value
        && frozen_variable second_binding second_value
    | _ -> false
  in
  let mutator_body =
    match mutator.body with
    | Sst.Checked_exec
        {
          provenance = Sst.Authenticated_typedtree _;
          body =
            {
              stage = Sst.Runtime;
              expression =
                {
                  expression_desc =
                      Sst.Shared_scalar_field_write { field; transition; _ };
                  _;
                };
            };
        } ->
          mutator.mode = Sst.Exec && (not mutator.recursive)
        && mutator.result_type = Sst.Unit
        && same_field_id field payload.field_id
        && same_type_id transition.shared_record_type frozen.frozen_root
        && same_field_id transition.shared_target_field payload.field_id
        && transition.shared_path_id = 0
        && transition.shared_predecessor_epoch = 0
        && transition.shared_successor_epoch = 1
    | _ -> false
  in
  let terminal_body =
      match
        (frozen_binding_parameter frozen.frozen_root terminal, terminal.body)
      with
    | ( Some root,
        Sst.Checked_exec
          {
            provenance = Sst.Authenticated_typedtree _;
            body =
              {
                stage = Sst.Runtime;
                expression =
                    { expression_desc = Sst.Field_read { record; field }; _ };
              };
          } ) ->
          terminal.mode = Sst.Exec && (not terminal.recursive)
        && terminal.result_type = Sst.Int
        && same_field_id field payload.field_id
        && frozen_variable root record
    | _ -> false
  in
  let role function_id role =
    List.exists
      (fun (operation : Sst.abstract_public_operation) ->
        operation.public_role = role
        && operation.public_function_index = function_id.Sst.function_index
        && String.equal operation.public_function_name function_id.function_name)
      evidence.public_surface
  in
  let surface =
    List.length evidence.public_surface = 5
    && role model.function_id Sst.Current_model
    && role invariant.function_id Sst.Abstract_invariant
    && role constructor.function_id Sst.Abstract_constructor
    && role mutator.function_id Sst.Shared_invariant_transition
    && role terminal.function_id Sst.Current_terminal_read
  in
  let _ = (child, head, tail) in
  if
    helper_decrease && helper_body && model_body && invariant_body
    && constructor_body && constructor_contract && mutator_body && terminal_body
    && surface
  then Ok ()
  else
    forged
      "frozen-spine descriptor does not match the exact helper/model/constructor/mutator/terminal grammar"
let validate_type_definition types functions
    (definition : Sst.type_definition) =
  match definition.representation with
  | Sst.Abstract_with_evidence evidence ->
      (match evidence with
      | Sst.Incomplete_abstraction_evidence _ ->
          fail definition.span
            (Forged_abstract_evidence "incomplete abstract evidence")
      | Sst.Proposed_same_cmt_abstraction _ ->
          fail definition.span
            (Forged_abstract_evidence "forged same-CMT abstract evidence")
      | Sst.Authenticated_same_cmt_abstraction evidence ->
          if
            not
              (Typedtree_adapter.authenticate_abstraction ~definition ~types
                 ~functions evidence)
          then
            fail definition.span
              (Forged_abstract_evidence
                 "unissued same-CMT abstraction certificate")
          else if
            evidence.abstract_signature_type <> definition.type_id
            || evidence.hidden_implementation_type <> definition.type_id
          then
            fail definition.span
              (Forged_abstract_evidence
                 "abstraction certificate type identity mismatch")
          else if
            evidence.declaration_spans = []
            || evidence.public_surface = []
          then
            fail definition.span
              (Forged_abstract_evidence
                 "incomplete abstraction certificate")
          else
            let* () =
              iter_result
                (fun (operation : Sst.abstract_public_operation) ->
                  match
                    List.find_opt
                      (fun (function_ : Sst.function_definition) ->
                        function_.function_id.function_index
                        = operation.public_function_index
                        && String.equal
                             function_.function_id.function_name
                             operation.public_function_name)
                      functions
                  with
                  | Some _ -> Ok ()
                  | None ->
                      fail operation.public_span
                        (Forged_abstract_evidence
                           "abstract public surface names an unknown callable"))
                evidence.public_surface
            in
            (match evidence.owned_tree_prerequisite with
            | Some prerequisite
              when prerequisite.owned_root <> definition.type_id ->
                fail definition.span
                  (Forged_abstract_evidence
                     "owned-tree prerequisite root identity mismatch")
            | Some _ | None -> (
                match Sst.frozen_spine_prerequisite evidence with
                | Some frozen ->
                    validate_frozen_spine_prerequisite types functions
                      definition evidence frozen
                | None -> Ok ())))
  | Sst.Revealed ->
      let validate_field (field : Sst.field_definition) =
        validate_type_reference types None field.Sst.span field.field_type
      in
      (match definition.type_kind with
      | Sst.Record_definition fields -> iter_result validate_field fields
      | Sst.Variant_definition constructors ->
          iter_result
            (fun constructor ->
              iter_result validate_field constructor.Sst.constructor_fields)
            constructors)
let same_constructor_id (left : Sst.constructor_id)
    (right : Sst.constructor_id) =
  same_type_id left.constructor_type right.constructor_type
  && left.constructor_index = right.constructor_index
  && String.equal left.constructor_name right.constructor_name
  let validate_revealed_nominal_definition (definition : Sst.type_definition) =
  let invalid span detail = fail span (Conflicting_identity detail) in
  let validate_record_field index (field : Sst.field_definition) =
    let* () =
      match field.field_id.field_owner with
      | Sst.Record_owner owner when same_type_id owner definition.type_id ->
          Ok ()
      | Sst.Record_owner _ | Sst.Constructor_owner _ ->
          invalid field.span
            "record field owner must match enclosing record type"
    in
    if field.field_id.field_index = index then Ok ()
      else invalid field.span "record fields must have dense zero-based indices"
  in
    let validate_constructor index (constructor : Sst.constructor_definition) =
    let* () =
      if
        same_type_id constructor.constructor_id.constructor_type
          definition.type_id
      then Ok ()
      else
        invalid constructor.span
          "variant constructor owner must match enclosing variant type"
    in
    let* () =
      if constructor.constructor_id.constructor_index = index then Ok ()
      else
        invalid constructor.span
          "variant constructors must have dense zero-based indices"
    in
      List.mapi
        (fun index field -> (index, field))
        constructor.constructor_fields
    |> iter_result (fun (index, (field : Sst.field_definition)) ->
           let* () =
             match field.field_id.field_owner with
             | Sst.Constructor_owner owner
               when same_constructor_id owner constructor.constructor_id ->
                 Ok ()
             | Sst.Record_owner _ | Sst.Constructor_owner _ ->
                 invalid field.span
                   "constructor field owner must match enclosing variant \
                    constructor"
           in
           if field.field_id.field_index = index then Ok ()
           else
             invalid field.span
               "constructor fields must have dense zero-based indices")
  in
  match definition.representation with
  | Sst.Abstract_with_evidence _ -> Ok ()
  | Sst.Revealed -> (
      match definition.type_kind with
      | Sst.Record_definition fields ->
          List.mapi (fun index field -> (index, field)) fields
          |> iter_result (fun (index, field) ->
                 validate_record_field index field)
      | Sst.Variant_definition constructors ->
          List.mapi (fun index constructor -> (index, constructor)) constructors
          |> iter_result (fun (index, constructor) ->
                 validate_constructor index constructor))
let validate_unique_identities program =
  let rec unique_types seen_indices seen_names = function
    | [] -> Ok ()
    | (definition : Sst.type_definition) :: rest ->
        if List.mem definition.type_id.type_index seen_indices then
          fail definition.span (Duplicate_type_id definition.type_id)
        else if List.mem definition.type_id.type_name seen_names then
          fail definition.span
            (Conflicting_identity
               ("duplicate semantic type name " ^ definition.type_id.type_name))
        else
          unique_types
            (definition.type_id.type_index :: seen_indices)
            (definition.type_id.type_name :: seen_names)
            rest
  in
  let rec unique_functions seen_indices seen_names = function
    | [] -> Ok ()
    | (definition : Sst.function_definition) :: rest ->
        if List.mem definition.function_id.function_index seen_indices then
          fail ~function_id:definition.function_id definition.span
            (Duplicate_function_id definition.function_id)
        else if List.mem definition.function_id.function_name seen_names then
          fail ~function_id:definition.function_id definition.span
            (Conflicting_identity
               ("duplicate semantic function name "
              ^ definition.function_id.function_name))
        else
          unique_functions
            (definition.function_id.function_index :: seen_indices)
            (definition.function_id.function_name :: seen_names)
            rest
  in
  let* () = unique_types [] [] program.Sst.types in
  unique_functions [] [] program.functions
let validate_shared_scalar_function ~physical_program types functions
    (definition : Sst.function_definition) =
  let function_id = definition.function_id in
  let malformed span detail =
    fail ~function_id span (Malformed_expression detail)
  in
  let* () =
    if
      Typedtree_adapter_private.Public.authenticate_shared_scalar_function
        ~program:physical_program ~definition
    then Ok ()
    else
      malformed definition.span
        "bounded shared-scalar mutation has no private typedtree issuance"
  in
  let rec mentions_type owner = function
    | Sst.Aggregate type_id -> same_type_id owner type_id
    | Sst.Tuple components ->
        List.exists (fun (_, typ) -> mentions_type owner typ) components
    | Sst.Unit | Sst.Bool | Sst.Int | Sst.Parameter _ | Sst.Application _ -> false
  in
  let* body =
    match definition.body with
    | Sst.Checked_exec
        {
          body = { stage = Sst.Runtime; expression };
          provenance = Sst.Authenticated_typedtree _;
        } ->
        Ok expression
    | Sst.Checked_exec _ | Sst.Spec_definition _
    | Sst.Recursive_spec_definition _ | Sst.Proof_body _
    | Sst.External_specification _ | Sst.Trusted_external_spec_target _
    | Sst.Trusted_external_body _ | Sst.Symbolic_declaration _ ->
        malformed definition.span
          "bounded shared-scalar mutation requires an authenticated runtime \
           typedtree body"
  in
  let* () =
    if not definition.recursive then Ok ()
    else
      malformed definition.span
        "bounded shared-scalar mutation does not admit recursive functions"
  in
  let* () =
    match definition.result_type with
    | Sst.Unit | Sst.Bool | Sst.Int -> Ok ()
      | Sst.Tuple _ | Sst.Aggregate _ | Sst.Parameter _ | Sst.Application _ ->
        malformed definition.span
          "bounded shared-scalar mutation may return only unit or a scalar"
  in
  let aggregate_formals =
    List.filter_map
      (fun (parameter : Sst.value_parameter) ->
        match parameter.pattern.pattern_desc with
        | Sst.Bind
            ({
               typ = Sst.Aggregate type_id;
               uniqueness = Sst.Definitely_aliased;
               _;
             } as binding) ->
            Some (binding, type_id)
        | _ -> None)
      (Sst_callback_private.value_parameters definition.parameters)
  in
  let all_aggregate_formals =
    List.filter
      (fun (parameter : Sst.value_parameter) ->
        match parameter.pattern.typ with
        | Sst.Aggregate _ -> true
          | Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _ | Sst.Parameter _
          | Sst.Application _ -> false)
      (Sst_callback_private.value_parameters definition.parameters)
  in
  let* formal_roots, record_type =
    match aggregate_formals with
    | [ (root, type_id) ] when List.length all_aggregate_formals = 1 ->
        Ok ([ root ], type_id)
    | [ (left, left_type); (right, right_type) ]
      when List.length all_aggregate_formals = 2
           && same_type_id left_type right_type ->
        Ok ([ left; right ], left_type)
    | _ ->
        malformed definition.span
          "bounded shared-scalar mutation requires one or two exact aliased \
           aggregate formals"
  in
  let shared_invariant_operation =
    match lookup_type types record_type with
    | Some
        {
          representation =
            Sst.Abstract_with_evidence
              (Sst.Authenticated_same_cmt_abstraction evidence);
          _;
        } ->
        List.exists
          (fun (operation : Sst.abstract_public_operation) ->
            operation.public_role = Sst.Shared_invariant_transition
            && operation.public_function_index = function_id.function_index
            && String.equal operation.public_function_name
                 function_id.function_name)
          evidence.public_surface
    | Some
        {
          representation =
            ( Sst.Revealed
            | Sst.Abstract_with_evidence
                (Sst.Incomplete_abstraction_evidence _
                | Sst.Proposed_same_cmt_abstraction _) );
          _;
        }
    | None ->
        false
  in
  let frozen_descriptor =
    match lookup_type types record_type with
    | Some
        {
          representation =
            Sst.Abstract_with_evidence
              (Sst.Authenticated_same_cmt_abstraction evidence);
          _;
        } -> (
        match Sst.frozen_spine_prerequisite evidence with
        | Some frozen ->
            if
              same_type_id frozen.frozen_root record_type
              && same_function_id frozen.frozen_mutator function_id
            then Some frozen
            else None
        | None -> None)
    | Some
        {
          representation =
            ( Sst.Revealed
            | Sst.Abstract_with_evidence
                (Sst.Incomplete_abstraction_evidence _
                | Sst.Proposed_same_cmt_abstraction _) );
          _;
        }
    | None ->
        None
  in
  let frozen_shared_operation = Option.is_some frozen_descriptor in
  let* () =
    if (not shared_invariant_operation) || List.length formal_roots = 1 then
      Ok ()
    else
      malformed definition.span
        "shared invariant transition requires exactly one aliased cell formal"
  in
  let* () =
    match lookup_type types record_type with
    | Some { representation = Sst.Revealed; _ } -> Ok ()
    | Some { representation = Sst.Abstract_with_evidence _; _ }
      when shared_invariant_operation ->
        Ok ()
    | Some _ | None ->
        malformed definition.span
            "abstract shared mutation lacks an authenticated invariant-cell \
             role"
  in
  let* target_field =
    match lookup_type types record_type with
    | Some
        {
          type_kind = Sst.Record_definition fields;
            representation = Sst.Revealed | Sst.Abstract_with_evidence _;
          _;
          } -> (
        let mutable_fields =
          List.filter
            (fun (field : Sst.field_definition) ->
              field.field_mutability = Sst.Mutable_field)
            fields
        in
          match mutable_fields with
        | [ field ]
            when ((not shared_invariant_operation)
            || List.length fields = 1
                 || frozen_shared_operation
            && List.length fields = 2
            &&
            match
              List.find_opt
                (fun candidate ->
                  candidate.Sst.field_mutability = Sst.Immutable_field)
                fields
            with
            | Some edge -> (
                match edge.field_type with
                | Sst.Aggregate _ -> true
                        | Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _
                        | Sst.Parameter _ | Sst.Application _ ->
                            false)
                    | None -> false)
            && field.field_type = Sst.Int
                 && (frozen_shared_operation
               || not
                    (List.exists
                       (fun (candidate : Sst.field_definition) ->
                         mentions_type record_type candidate.field_type)
                       fields)) ->
            Ok field.field_id
        | _ ->
            malformed definition.span
              "bounded shared-scalar mutation requires a nonrecursive record \
               with exactly one mutable integer field")
    | Some { type_kind = Sst.Variant_definition _; _ } | None ->
        malformed definition.span
          "bounded shared-scalar mutation requires one exact local record"
  in
  let scalar_parameters =
    List.concat_map
      (fun (parameter : Sst.value_parameter) ->
        match parameter.pattern.pattern_desc with
        | Sst.Bind binding -> (
            match binding.typ with
            | Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _ -> [ binding.id ]
              | Sst.Aggregate _ | Sst.Parameter _ | Sst.Application _ -> [])
        | _ -> [])
      (Sst_callback_private.value_parameters definition.parameters)
  in
  let initial_aliases =
    List.map (fun binding -> (binding.Sst.id, [ binding ])) formal_roots
  in
  let alias_chain aliases binding = List.assoc_opt binding.Sst.id aliases in
  let current_model_call aliases = function
    | Sst.Direct_call
        {
          call_form = Sst.Specification_call;
          recursive = false;
          callee;
          arguments =
            [
              Sst.Value_argument
                {
                  label = _;
                  value =
                    {
                      expression_desc = Sst.Variable { binding; _ };
                      typ = Sst.Aggregate argument_type;
                      _;
                    };
                };
            ];
            _;
        }
        when same_type_id argument_type record_type
        && Option.is_some (alias_chain aliases binding) -> (
        match lookup_type types record_type with
        | Some
            {
              representation =
                Sst.Abstract_with_evidence
                  (Sst.Authenticated_same_cmt_abstraction evidence);
              _;
            } ->
            List.exists
              (fun (operation : Sst.abstract_public_operation) ->
                operation.public_role = Sst.Current_model
                && operation.public_function_index = callee.function_index
                && String.equal operation.public_function_name
                     callee.function_name)
              evidence.public_surface
        | Some
            {
              representation =
                ( Sst.Revealed
                | Sst.Abstract_with_evidence
                    (Sst.Incomplete_abstraction_evidence _
                    | Sst.Proposed_same_cmt_abstraction _) );
              _;
            }
        | None ->
            false)
    | _ -> false
  in
  let frozen_tail_call = function
    | Sst.Direct_call
        {
          call_form = Sst.Specification_call;
          recursive = false;
          callee;
          arguments = [ Sst.Value_argument { label = None; value = argument } ];
            _;
        } -> (
        match frozen_descriptor with
        | None -> false
          | Some frozen -> (
            argument.typ = Sst.Aggregate frozen.frozen_result_type
            &&
            match
              List.find_opt
                (fun (candidate : Sst.function_definition) ->
                  same_function_id candidate.function_id callee)
                functions
            with
            | Some
                {
                  mode = Sst.Spec;
                  recursive = false;
                  parameters =
                    [
                      Sst.Value_parameter
                      {
                        label = None;
                        pattern =
                          {
                            pattern_desc = Sst.Bind formal;
                            typ = Sst.Aggregate formal_type;
                            _;
                          };
                          optional_default = _;
                      };
                    ];
                  result_type = Sst.Aggregate result_type;
                  body =
                    Sst.Spec_definition
                      {
                        stage = Sst.Logical;
                        expression =
                          {
                            expression_desc =
                              Sst.Match
                                ( scrutinee,
                                  [
                                    {
                                      case_pattern =
                                        {
                                          pattern_desc =
                                            Sst.Constructor_pattern
                                              (end_pattern, []);
                                          _;
                                        };
                                      case_guard = None;
                                      case_body =
                                        {
                                          expression_desc =
                                            Sst.Constructor_value
                                              {
                                                constructor = end_value;
                                                arguments = [];
                                              };
                                          _;
                                        };
                                      _;
                                    };
                                    {
                                      case_pattern =
                                        {
                                          pattern_desc =
                                            Sst.Constructor_pattern
                                              ( more_pattern,
                                                [
                                                  _;
                                                  {
                                                    pattern_desc =
                                                      Sst.Bind rest;
                                                    _;
                                                  };
                                                ] );
                                          _;
                                        };
                                      case_guard = None;
                                      case_body = rest_value;
                                      _;
                                    };
                                  ] );
                            _;
                          };
                      };
                  contracts;
                  _;
                } ->
                same_type_id formal_type frozen.frozen_result_type
                && same_type_id result_type frozen.frozen_result_type
                && contracts_are_empty contracts
                && frozen_variable formal scrutinee
                && same_constructor_identity end_pattern
                     frozen.frozen_end_constructor
                && same_constructor_identity end_value
                     frozen.frozen_end_constructor
                && same_constructor_identity more_pattern
                     frozen.frozen_more_constructor
                && frozen_variable rest rest_value
              | Some _ | None -> false))
    | _ -> false
  in
  let rec pure ~allow_old aliases scalar_bound expression =
    let recurse = pure ~allow_old aliases scalar_bound in
    match expression.Sst.expression_desc with
    | Sst.Int_constant _ | Sst.Bool_constant _ | Sst.Unit_constant -> Ok ()
    | Sst.Variable { binding; _ } -> (
        match binding.typ with
          | (Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _)
          when List.mem binding.id scalar_bound ->
            Ok ()
        | _ ->
            malformed expression.span
              "shared-scalar grammar forbids standalone aggregate or unbound \
               scalar values")
    | Sst.Field_read
        {
          record =
            {
              expression_desc = Sst.Variable { binding; _ };
              typ = Sst.Aggregate read_type;
              _;
            };
          field;
        }
        when same_type_id read_type record_type
        && field = target_field
        && Option.is_some (alias_chain aliases binding)
        && expression.typ = Sst.Int ->
        Ok ()
    | Sst.Field_read { record; _ }
      when current_model_call aliases record.expression_desc ->
        Ok ()
    | Sst.Field_read _ ->
        malformed expression.span
            "shared-scalar reads require a direct admitted alias and exact \
             field"
    | Sst.Checked_arithmetic (_, operands) -> iter_result recurse operands
      | Sst.Compare (_, left, right) | Sst.Boolean_binary (_, left, right) ->
        let* () = recurse left in
        recurse right
    | Sst.Boolean_not operand -> recurse operand
    | Sst.Forall quantifier | Sst.Exists quantifier ->
        let scalar_bound =
          match quantifier.quantifier_binder.typ with
          | Sst.Int | Sst.Bool ->
              quantifier.quantifier_binder.id :: scalar_bound
          | Sst.Unit | Sst.Tuple _ | Sst.Aggregate _ | Sst.Parameter _
          | Sst.Application _ ->
              scalar_bound
        in
        Quantifier_validation_private.validate_sst_contents
          {
            validate_content = pure ~allow_old aliases scalar_bound;
            invalid_sst = malformed expression.span;
          }
          ~function_id expression quantifier
    | Sst.Old payload when allow_old ->
        pure ~allow_old:false aliases scalar_bound payload
    | Sst.Old _ ->
        malformed expression.span
          "old is available only in shared-scalar postconditions"
    | Sst.Local_assert { predicate; _ } -> recurse predicate
    | Sst.Direct_call _
      when current_model_call aliases expression.expression_desc ->
        Ok ()
    | Sst.Constructor_value { constructor; arguments }
        when Option.fold ~none:false
          ~some:(fun frozen ->
            (same_constructor_identity constructor
               frozen.Sst.frozen_end_constructor
            || same_constructor_identity constructor
                 frozen.frozen_more_constructor)
            && expression.typ = Sst.Aggregate frozen.frozen_result_type)
          frozen_descriptor ->
        iter_result recurse arguments
      | Sst.Direct_call _ when frozen_tail_call expression.expression_desc -> (
          match expression.expression_desc with
        | Sst.Direct_call { arguments; _ } ->
            iter_result
              (fun argument ->
                 let _, argument = Sst.require_value_argument argument in
                 recurse argument)
              arguments
        | _ -> assert false)
    | Sst.Tuple_value _ | Sst.Record_value _ | Sst.Constructor_value _
    | Sst.Field_write _ | Sst.Shared_scalar_field_write _
    | Sst.Owned_tree_nested_write _ | Sst.Owned_tree_rebase _
      | Sst.Let_mutable _ | Sst.Mutable_read _ | Sst.Mutable_write _ | Sst.Let _
      | Sst.Sequence _ | Sst.If _ | Sst.Match _ | Sst.Direct_call _
      | Sst.Callback_call _ | Sst.Callback_requires _ | Sst.Callback_ensures _
      | Sst.Reveal _ | Sst.Reveal_with_fuel _ | Sst.Use_type_invariant _
      | Sst.Proof_region _ | Sst.Optional_absent | Sst.Optional_present _
      | Sst.Optional_forward _ | Sst.Symbolic_application _ ->
        malformed expression.span
          "expression is outside the bounded shared-scalar grammar"
  in
  let* () =
    iter_result
      (fun (clause : Sst.predicate_clause) ->
        pure ~allow_old:false initial_aliases scalar_parameters
          clause.predicate.expression)
      definition.contracts.requires
  in
  let* () =
    iter_result
      (fun (clause : Sst.ensures_clause) ->
        let bound =
          match clause.binder with
          | Some { pattern_desc = Sst.Bind binding; _ } ->
              binding.id :: scalar_parameters
          | Some _ | None -> scalar_parameters
        in
          pure ~allow_old:true initial_aliases bound clause.predicate.expression)
      definition.contracts.ensures
  in
  let* () =
    iter_result
      (fun (clause : Sst.predicate_clause) ->
        pure ~allow_old:false initial_aliases scalar_parameters
          clause.predicate.expression)
      definition.contracts.assertions
  in
  let* () =
    if definition.contracts.decreases = [] then Ok ()
    else
      malformed definition.span
        "bounded shared-scalar mutation does not admit decreases authority"
  in
  let rec runtime aliases scalar_bound epoch writes expression =
    match expression.Sst.expression_desc with
    | Sst.Sequence (first, second) ->
        let* aliases, scalar_bound, epoch, writes =
          runtime aliases scalar_bound epoch writes first
        in
        runtime aliases scalar_bound epoch writes second
    | Sst.Let ([ (pattern, value) ], body) -> (
        match (pattern.pattern_desc, value.expression_desc) with
        | Sst.Bind binding, Sst.Variable { binding = source; _ }
          when binding.typ = Sst.Aggregate record_type -> (
            match alias_chain aliases source with
            | Some chain ->
                let shadowed =
                  List.exists
                    (fun candidate ->
                      String.equal candidate.Sst.name binding.name)
                    (List.concat_map snd aliases)
                in
                if shadowed then
                  malformed pattern.span
                    "shared-scalar alias shadowing is not admitted"
                else
                  runtime
                    ((binding.id, chain @ [ binding ])
                    :: List.remove_assoc binding.id aliases)
                    scalar_bound epoch writes body
            | None ->
                malformed value.span
                  "shared-scalar aggregate lets must be exact alias bindings")
        | Sst.Bind binding, _ -> (
            match binding.typ with
            | Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _ ->
                let* () = pure ~allow_old:false aliases scalar_bound value in
                runtime aliases (binding.id :: scalar_bound) epoch writes body
              | Sst.Aggregate _ | Sst.Parameter _ | Sst.Application _ ->
                malformed value.span
                  "shared-scalar aggregate construction or rebinding is not \
                   admitted")
        | Sst.Wildcard, _ ->
            let* () = pure ~allow_old:false aliases scalar_bound value in
            runtime aliases scalar_bound epoch writes body
        | _ ->
            malformed pattern.span
              "shared-scalar lets require exact identifier binders")
    | Sst.Let _ ->
        malformed expression.span
          "shared-scalar lets must contain one ordered binding"
      | Sst.Shared_scalar_field_write { provenance; field; value; transition }
        ->
        let* () = pure ~allow_old:false aliases scalar_bound value in
        let* chain =
          match alias_chain aliases provenance.root with
          | Some chain -> Ok chain
          | None ->
              malformed expression.span
                  "shared-scalar write target has no exact formal alias \
                   provenance"
        in
        let exact =
          transition.shared_policy = Sst.Bounded_shared_scalar_heap_v1
          && transition.shared_function_index = function_id.function_index
          && String.equal transition.shared_function_name
               function_id.function_name
          && transition.shared_path_id = 0
          && same_type_id transition.shared_record_type record_type
          && transition.shared_formal_roots = formal_roots
          && transition.shared_target = provenance.root
          && transition.shared_canonical_root = List.hd chain
          && transition.shared_alias_chain = chain
          && transition.shared_target_field = target_field
          && field = target_field
          && transition.shared_predecessor_epoch = epoch
          && transition.shared_successor_epoch = epoch + 1
            && provenance.binding_pattern_uniqueness = Sst.Definitely_aliased
          && provenance.root.uniqueness = Sst.Definitely_aliased
          && provenance.field_is_local && provenance.field_is_public
            && provenance.field_is_mutable && value.typ = Sst.Int
            && expression.typ = Sst.Unit
        in
        if not exact then
          malformed expression.span
            "shared-scalar write descriptor does not match independently \
             derived function, alias, field, path, or epoch authority"
        else if writes >= 2 then
          malformed expression.span
            "bounded shared-scalar mutation admits at most two ordered writes"
        else Ok (aliases, scalar_bound, epoch + 1, writes + 1)
    | _ ->
        let* () = pure ~allow_old:false aliases scalar_bound expression in
        Ok (aliases, scalar_bound, epoch, writes)
  in
  let* _, _, final_epoch, writes =
    runtime initial_aliases scalar_parameters 0 0 body
  in
  if writes >= 1 && writes <= 2 && final_epoch = writes then Ok ()
  else
    malformed body.span
      "bounded shared-scalar mutation requires one or two ordered writes"
let validate_functions ~require_authenticated_owned_tree_roots ~rank_program
    ~external_specifications ~definitions program =
  let* () =
    match Parametric_sst_validation_private.validate_program program with
    | Ok () -> Ok ()
    | Error error ->
        fail ~function_id:error.function_id error.span
          (Malformed_expression error.message)
  in
  let rank_domains = Parametric_rank_domain_private.nominal_domains rank_program in
  iter_result
    (fun (definition : Sst.function_definition) ->
      if definition.Sst.policy <> program.Sst.policy then
        fail ~function_id:definition.function_id definition.span
          (Conflicting_identity
             "declaration policy differs from the program policy")
      else
        let* () =
          if function_contains_shared_scalar_write definition then
            validate_shared_scalar_function ~physical_program:rank_program
              program.types program.functions definition
          else Ok ()
        in
        validate_function ~require_authenticated_owned_tree_roots ~rank_domains
          ~physical_program:rank_program ~external_specifications program.types
          program.functions
          definition)
    definitions
let rec contains_owned_tree_transition (expression : Sst.expression) =
  match expression.expression_desc with
  | Sst.Owned_tree_nested_write _
  | Sst.Owned_tree_rebase _
  | Sst.Field_write { transition = Some _; _ } ->
      true
  | _ -> List.exists contains_owned_tree_transition (expression_children expression)
let function_contains_owned_tree_transition
    (definition : Sst.function_definition) =
  let predicate_contains (clause : Sst.predicate_clause) =
    contains_owned_tree_transition clause.predicate.expression
  in
  let ensure_contains (clause : Sst.ensures_clause) =
    contains_owned_tree_transition clause.predicate.expression
  in
  List.exists predicate_contains definition.contracts.requires
  || List.exists ensure_contains definition.contracts.ensures
  || List.exists predicate_contains definition.contracts.decreases
  || List.exists predicate_contains definition.contracts.assertions
  ||
  match definition.body with
  | Sst.Checked_exec { body; _ }
  | Sst.Spec_definition body
  | Sst.Proof_body { body; _ } ->
      contains_owned_tree_transition body.expression
  | Sst.Recursive_spec_definition { body; _ } ->
      contains_owned_tree_transition body.expression
    | Sst.External_specification _ | Sst.Trusted_external_spec_target _
  | Sst.Trusted_external_body _ | Sst.Symbolic_declaration _ ->
      false
let finite_rank_domain_for_type ~program ~span ~types rank_domains type_id =
  let same_domain left right =
    String.equal
      (Parametric_rank_domain_private.domain_id left)
      (Parametric_rank_domain_private.domain_id right)
    && String.equal
         (Parametric_rank_domain_private.domain_version left)
         (Parametric_rank_domain_private.domain_version right)
    && String.equal
         (Parametric_rank_domain_private.snapshot_digest left)
         (Parametric_rank_domain_private.snapshot_digest right)
  in
  let direct type_id =
    List.filter
      (fun domain ->
        Parametric_rank_domain_private.immutable domain
        && List.exists
             (fun (identity : Typedtree_adapter_issuance_private.rank_type_identity) ->
               same_type_id identity.rank_type_id type_id)
             (Parametric_rank_domain_private.component domain))
      rank_domains
    |> List.sort_uniq (fun left right ->
           String.compare
             (Parametric_rank_domain_private.domain_id left)
             (Parametric_rank_domain_private.domain_id right))
  in
  let definition type_id =
    List.find_opt
      (fun (definition : Sst.type_definition) ->
        same_type_id definition.type_id type_id)
      types
  in
  let merge left right =
    match (left, right) with
    | None, domain | domain, None -> domain
    | Some left, Some right when same_domain left right -> Some left
    | Some _, Some _ -> raise Exit
  in
  let rec classify visiting = function
    | Sst.Application _ as application -> (
        match
          Parametric_rank_domain_private.derive_application ~program ~span
            application
        with
        | Ok domain -> Some domain
        | Error _ -> None)
    | Sst.Unit | Sst.Bool | Sst.Int | Sst.Parameter _ ->
        None
    | Sst.Tuple components ->
        List.fold_left
          (fun domain (_, typ) -> merge domain (classify visiting typ))
          None components
    | Sst.Aggregate type_id -> (
        match direct type_id with
        | [ domain ] -> Some domain
        | _ :: _ :: _ -> raise Exit
        | [] when List.exists (same_type_id type_id) visiting -> None
        | [] -> (
            match definition type_id with
            | None -> None
            | Some definition ->
                let fields =
                  match definition.type_kind with
                  | Sst.Record_definition fields -> fields
                  | Sst.Variant_definition constructors ->
                      List.concat_map
                        (fun constructor ->
                          constructor.Sst.constructor_fields)
                        constructors
                in
                if
                  List.exists
                    (fun (field : Sst.field_definition) ->
                      field.field_mutability <> Sst.Immutable_field
                      || field.field_modalities.uniqueness_modality
                         = Sst.Force_aliased)
                    fields
                then None
                else
                  List.fold_left
                    (fun domain (field : Sst.field_definition) ->
                      merge domain
                        (classify (type_id :: visiting) field.field_type))
                    None fields))
  in
  try classify [] (Sst.Aggregate type_id) with Exit -> None
module Semantic_environment = struct
  type visibility_descriptor = {
    representation : Sst.representation_visibility;
  }
  type logical_type_descriptor = {
    logical_type : Spec_definition.logical_type option;
    logical_source : Sst.typ;
    logical_nominal : Sst.type_id option;
  }
  type type_descriptor = {
    definition : Sst.type_definition;
    visibility : visibility_descriptor;
    logical_type : logical_type_descriptor option;
  }
  type contract_clause_descriptor = {
    clause_index : int;
    span : Diagnostic.span;
    binder : Sst.pattern option;
    expression : Sst.expression;
  }
    type decrease_descriptor = { clause : contract_clause_descriptor }
  type contract_descriptor = {
    requires : contract_clause_descriptor list;
    ensures : contract_clause_descriptor list;
    decreases : decrease_descriptor list;
    assertions : contract_clause_descriptor list;
  }
  type validated_rank_domain =
    Parametric_rank_domain_private.validated_rank_domain
  type decrease_disposition =
    | No_decrease
    | Direct_integer_decrease of decrease_descriptor
    | Direct_structural_decrease of
        decrease_descriptor * validated_rank_domain
    | Direct_parametric_decrease of
        decrease_descriptor * Parametric_adt.t
    | Direct_frozen_spine_decrease of
        decrease_descriptor * Sst.frozen_spine_prerequisite
    | Missing_decrease of Diagnostic.span
    | Duplicate_decrease of Diagnostic.span
    | Inapplicable_decrease of Diagnostic.span
    | Non_integer_decrease of Diagnostic.span
    | Recursive_decrease of Diagnostic.span
  type callable_descriptor = {
    definition : Sst.function_definition;
    contract : contract_descriptor;
    decrease : decrease_disposition;
    formal_modes : Sst.instance_mode list;
    result_mode : Sst.instance_mode;
  }
  type finite_formal_descriptor = {
    callable : Sst.function_id;
    ordinal : int;
    label : string option;
    pattern_digest : string;
    binding_ids : int list;
    typ : Sst.typ;
    mode : Sst.instance_mode;
    rank_domain : validated_rank_domain;
    requirement_digest : string;
  }
  type frozen_formal_descriptor = {
    frozen_callable : Sst.function_id;
    frozen_ordinal : int;
    frozen_type : Sst.type_id;
  }
  type model_descriptor = {
    callable : callable_descriptor;
    domain : type_descriptor;
    result : logical_type_descriptor;
    visibility : visibility_descriptor;
    owned_root_scalar : owned_root_scalar_model_template option;
    owned_recursive_contents : Owned_recursive_contents_private.t option;
  }
  type formal_actual_descriptor = {
    formal : Sst.parameter;
    actual_label : string option;
    actual : Sst.expression;
  }
  type call_edge_region =
    | Requires_region
    | Ensures_region
    | Decreases_region
    | Assertions_region
    | Body_region
  type call_edge_descriptor = {
    caller : callable_descriptor;
    callee : callable_descriptor;
    call_form : Sst.call_form;
    recursive : bool;
    span : Diagnostic.span;
    region : call_edge_region;
    actuals : formal_actual_descriptor list;
  }
  type semantic_feature_requirement =
    | Specification_semantics
    | Proof_semantics
    | External_specification_trust
    | Trusted_external_body
    | Owned_tree_reconstruction
    | Direct_recursion
  type feature_descriptor = {
    callable : callable_descriptor;
    requirement : semantic_feature_requirement;
  }
  type t = {
    program : Sst.program;
    imports : Imported_callable.registration option;
    external_specifications :
      External_target_specification_private.registration option;
    instance_modes : Instance_mode.environment;
    types : type_descriptor list;
    callables : callable_descriptor list;
    models : model_descriptor list;
    call_edges : call_edge_descriptor list;
    features : feature_descriptor list;
    rank_domains : validated_rank_domain list;
    finite_formals : finite_formal_descriptor list;
    frozen_formals : frozen_formal_descriptor list;
  }
  let predicate_clause (clause : Sst.predicate_clause) =
    {
      clause_index = clause.clause_index;
      span = clause.span;
      binder = None;
      expression = clause.predicate.expression;
    }
  let ensures_clause (clause : Sst.ensures_clause) =
    {
      clause_index = clause.clause_index;
      span = clause.span;
      binder = clause.binder;
      expression = clause.predicate.expression;
    }
  let contract (contracts : Sst.contracts) =
    {
      requires = List.map predicate_clause contracts.requires;
      ensures = List.map ensures_clause contracts.ensures;
      decreases =
        List.map
          (fun clause -> { clause = predicate_clause clause })
          contracts.decreases;
      assertions = List.map predicate_clause contracts.assertions;
    }
  let rec recursive_call_count (expression : Sst.expression) =
    let nested =
      match expression.expression_desc with
      | Sst.Proof_region body -> [ body ]
      | _ -> expression_children expression
    in
    let current =
      match expression.expression_desc with
      | Sst.Direct_call { recursive = true; _ } -> 1
      | _ -> 0
    in
    current
    + List.fold_left
        (fun count child -> count + recursive_call_count child)
        0 nested
  let executable_body (definition : Sst.function_definition) =
    match definition.body with
    | Sst.Checked_exec { body; _ }
    | Sst.Proof_body { body; _ }
    | Sst.Recursive_spec_definition { body; _ } ->
        Some body.expression
      | Sst.Spec_definition _ | Sst.External_specification _
      | Sst.Trusted_external_spec_target _ | Sst.Trusted_external_body _
      | Sst.Symbolic_declaration _ ->
        None
  let rank_domain_for_type rank_domains type_id =
    List.find_opt
      (fun domain ->
        Parametric_rank_domain_private.immutable domain
        && List.exists
             (fun (identity : Typedtree_adapter_issuance_private.rank_type_identity) ->
               identity.rank_type_id = type_id)
             (Parametric_rank_domain_private.component domain))
      rank_domains
  let rank_domain_for_application rank_domains descriptor arguments =
    List.find_opt
      (fun domain ->
        Parametric_rank_domain_private.immutable domain
        &&
        match Parametric_rank_domain_private.application domain with
        | Some
            (Parametric_type.Application (constructor, candidate_arguments)) ->
            Parametric_type.compare_constructor constructor
              (Parametric_adt.type_constructor descriptor)
            = 0
            && List.equal Parametric_type.equal candidate_arguments arguments
        | Some (Unit | Bool | Int | Tuple _ | Aggregate _ | Parameter _)
        | None ->
            false)
      rank_domains
  let decrease_disposition physical_program parametric_adts types
      rank_domains definition
      contract =
    match executable_body definition with
    | None -> No_decrease
    | Some body -> (
        match (recursive_call_count body, contract.decreases) with
        | 0, [] -> No_decrease
        | 0, decrease :: _ -> Inapplicable_decrease decrease.clause.span
        | _, [] -> Missing_decrease definition.Sst.span
          | _, [ decrease ] -> (
            if recursive_call_count decrease.clause.expression <> 0 then
              Recursive_decrease decrease.clause.span
              else
                match decrease.clause.expression.typ with
              | Sst.Int -> Direct_integer_decrease decrease
              | Sst.Aggregate type_id -> (
                  match rank_domain_for_type !rank_domains type_id with
                  | Some domain ->
                      Direct_structural_decrease (decrease, domain)
                  | None -> (
                      match frozen_spine_for_helper types definition with
                      | Some frozen
                        when same_type_id frozen.frozen_root type_id ->
                          Direct_frozen_spine_decrease (decrease, frozen)
                      | Some _ | None ->
                          Non_integer_decrease decrease.clause.span))
              | Sst.Application (constructor, arguments) as application -> (
                  match
                    Parametric_adt.find parametric_adts constructor
                  with
                  | Some descriptor
                    when Parametric_adt.same_application descriptor
                           application -> (
                      match
                        Parametric_adt_lowering_private
                        .authenticate_direct_recursion ~descriptor ~definition
                          ~measure:decrease.clause.expression
                      with
                      | Ok () -> (
                          match
                            rank_domain_for_application !rank_domains descriptor
                              arguments
                          with
                          | Some domain ->
                              Direct_structural_decrease (decrease, domain)
                          | None -> (
                              match
                                Parametric_rank_domain_private
                                .derive_application ~program:physical_program
                                  ~span:decrease.clause.span application
                              with
                              | Ok domain ->
                                  rank_domains := domain :: !rank_domains;
                                  Direct_structural_decrease (decrease, domain)
                              | Error _ ->
                                  Non_integer_decrease decrease.clause.span))
                      | Error _ -> (
                          match
                            Spec_function_sst_private.authenticate_rank1_recursion ~descriptor
                              definition decrease.clause.expression
                          with
                          | Ok () -> (
                              match
                                rank_domain_for_application !rank_domains
                                  descriptor arguments
                              with
                              | Some domain ->
                                  Direct_structural_decrease (decrease, domain)
                              | None -> (
                                  match
                                    Parametric_rank_domain_private
                                    .derive_application
                                      ~program:physical_program
                                      ~span:decrease.clause.span application
                                  with
                                  | Ok domain ->
                                      rank_domains := domain :: !rank_domains;
                                      Direct_structural_decrease
                                        (decrease, domain)
                                  | Error _ ->
                                      Non_integer_decrease decrease.clause.span))
                          | Error _ ->
                              Non_integer_decrease decrease.clause.span))
                  | Some _ | None ->
                      Non_integer_decrease decrease.clause.span)
              | Sst.Unit | Sst.Bool | Sst.Tuple _ | Sst.Parameter _
                  ->
                  Non_integer_decrease decrease.clause.span)
          | _, _ :: duplicate :: _ -> Duplicate_decrease duplicate.clause.span)
    let callable physical_program parametric_adts types rank_domains
        instance_modes imported_snapshots external_specifications definition =
    let contract = contract definition.Sst.contracts in
    let imported =
      List.find_opt
        (fun (snapshot : Imported_callable.callable_snapshot) ->
          snapshot.definition.Sst.function_id.function_index
          = definition.Sst.function_id.function_index
            && String.equal snapshot.definition.Sst.function_id.function_name
               definition.Sst.function_id.function_name)
        imported_snapshots
    in
    let formal_modes, result_mode =
      match
        Option.bind external_specifications (fun registration ->
            External_target_specification_private.signature_for_definition
              registration ~program:physical_program definition)
      with
      | Some signature -> Parametric_signature_private.modes signature
      | None -> (
          match imported with
          | Some snapshot ->
              Parametric_signature_private.modes snapshot.signature
          | None ->
          ( List.mapi
              (fun index parameter ->
                match parameter with
                | Sst.Callback_parameter _ -> Sst.Exec_instance
                | Sst.Value_parameter _ ->
                    Instance_mode.formal_mode instance_modes definition index
                      parameter)
              definition.parameters,
            Instance_mode.result_mode instance_modes definition ))
    in
    {
      definition;
      contract;
      decrease =
        decrease_disposition physical_program parametric_adts types rank_domains
          definition contract;
      formal_modes;
      result_mode;
    }
  let same_callable_id (left : Sst.function_id) (right : Sst.function_id) =
    left.function_index = right.function_index
    && String.equal left.function_name right.function_name
  let find_callable callables id =
    List.find_opt
      (fun descriptor ->
        same_callable_id descriptor.definition.Sst.function_id id)
      callables
  let expression_children_with_proof (expression : Sst.expression) =
    match expression.expression_desc with
    | Sst.Proof_region body -> [ body ]
    | _ -> expression_children expression
  let rec expression_call_edges imports callables caller region expression =
    let current =
      match expression.Sst.expression_desc with
      | Sst.Direct_call _
        when
          Spec_function_sst_private.lambda expression <> None
          || Spec_function_sst_private.is_reference expression
          || Spec_function_sst_private.application expression <> None ->
          []
        | Sst.Direct_call
            { call_form; callee; type_arguments = _; arguments; recursive } -> (
          if
            Option.fold ~none:false
              ~some:(fun registration ->
                Option.is_some
                  (Imported_callable.find_call registration expression))
              imports
          then []
            else
            match find_callable callables callee with
            | None ->
                (* Exact owned-contents helpers are deliberately absent from
                   the generic callable/termination graph.  Their only
                   recursive edge is authenticated by the private exhaustive
                   grammar and later requires an exact-child permit. *)
                []
            | Some callee ->
                let actuals =
                  List.fold_left2
                    (fun actuals formal argument ->
                      match formal, argument with
                      | ( (Sst.Value_parameter _) as formal,
                          Sst.Value_argument
                            { label = actual_label; value = actual } ) ->
                          { formal; actual_label; actual } :: actuals
                      | Sst.Callback_parameter _, Sst.Callback_argument _ ->
                          actuals
                      | Sst.Value_parameter _, Sst.Callback_argument _
                      | Sst.Callback_parameter _, Sst.Value_argument _ ->
                          actuals)
                    [] callee.definition.parameters arguments
                  |> List.rev
                in
                [
                  {
                    caller;
                    callee;
                    call_form;
                    recursive;
                    span = expression.span;
                    region;
                    actuals;
                  };
                ])
      | _ -> []
    in
    current
    @ List.concat_map
        (expression_call_edges imports callables caller region)
        (expression_children_with_proof expression)
  let callable_expressions descriptor =
    let contract = descriptor.contract in
    List.map
      (fun clause -> (Requires_region, clause.expression))
      contract.requires
    @ List.map
        (fun clause -> (Ensures_region, clause.expression))
        contract.ensures
    @ List.map
        (fun decrease -> (Decreases_region, decrease.clause.expression))
        contract.decreases
    @ List.map
        (fun clause -> (Assertions_region, clause.expression))
        contract.assertions
    @
    match descriptor.definition.Sst.body with
    | Sst.Checked_exec { body; _ }
    | Sst.Spec_definition body
    | Sst.Proof_body { body; _ } ->
        [ (Body_region, body.expression) ]
    | Sst.Recursive_spec_definition { body; _ } ->
        [ (Body_region, body.expression) ]
      | Sst.External_specification _ | Sst.Trusted_external_spec_target _
    | Sst.Trusted_external_body _ | Sst.Symbolic_declaration _ ->
        []
  let call_edges imports callables =
    List.concat_map
      (fun caller ->
        List.concat_map
          (fun (region, expression) ->
              expression_call_edges imports callables caller region expression)
          (callable_expressions caller))
      callables
  let base_features descriptor =
    match descriptor.definition.Sst.body with
    | Sst.Spec_definition _ | Sst.Recursive_spec_definition _ ->
        [ Specification_semantics ]
    | Sst.Proof_body _ -> [ Proof_semantics ]
      | Sst.External_specification _ | Sst.Trusted_external_spec_target _ ->
        [ External_specification_trust ]
    | Sst.Trusted_external_body _ -> [ Trusted_external_body ]
    | Sst.Symbolic_declaration _ -> []
    | Sst.Checked_exec _ -> []
  let features callables =
    List.concat_map
      (fun callable ->
        let requirements =
          base_features callable
            @ (if function_contains_owned_tree_transition callable.definition
               then [ Owned_tree_reconstruction ]
           else [])
          @
          match executable_body callable.definition with
            | Some body when recursive_call_count body > 0 ->
                [ Direct_recursion ]
          | Some _ | None -> []
        in
        List.map (fun requirement -> { callable; requirement }) requirements)
      callables
  (* This is the sole constructor for validated semantic handles.  It remains
     implementation-local to [Sst_validation] and is invoked only after every
     ordered validation phase has succeeded. *)
  let rec pattern_snapshot (pattern : Sst.pattern) =
    let nested tag patterns =
      tag ^ "(" ^ String.concat "," (List.map pattern_snapshot patterns) ^ ")"
    in
    match pattern.pattern_desc with
    | Sst.Wildcard -> "_"
    | Sst.Bind binding -> Printf.sprintf "bind:%s#%d" binding.name binding.id
    | Sst.Owned_tree_cursor_pattern cursor ->
        Printf.sprintf "cursor:%s#%d" cursor.cursor_binding.name
          cursor.cursor_binding.id
    | Sst.Int_pattern value -> "int:" ^ Z.to_string value
    | Sst.Bool_pattern value -> "bool:" ^ string_of_bool value
    | Sst.Unit_pattern -> "unit"
      | Sst.Tuple_pattern components -> nested "tuple" (List.map snd components)
    | Sst.Record_pattern fields -> nested "record" (List.map snd fields)
    | Sst.Constructor_pattern (constructor, arguments) ->
        nested
            (Printf.sprintf "constructor:%s#%d" constructor.constructor_name
               constructor.constructor_index)
          arguments
    | Sst.Or_pattern (left, right) -> nested "or" [ left; right ]
  let rec pattern_binding_ids (pattern : Sst.pattern) =
    match pattern.pattern_desc with
    | Sst.Bind binding -> [ binding.id ]
    | Sst.Owned_tree_cursor_pattern cursor -> [ cursor.cursor_binding.id ]
    | Sst.Tuple_pattern components ->
          List.concat_map
            (fun (_, nested) -> pattern_binding_ids nested)
            components
    | Sst.Record_pattern fields ->
        List.concat_map (fun (_, nested) -> pattern_binding_ids nested) fields
    | Sst.Constructor_pattern (_, arguments) ->
        List.concat_map pattern_binding_ids arguments
    | Sst.Or_pattern (left, right) ->
        pattern_binding_ids left @ pattern_binding_ids right
      | Sst.Wildcard | Sst.Int_pattern _ | Sst.Bool_pattern _ | Sst.Unit_pattern
        ->
        []
  let type_descriptors (program : Sst.program) =
    List.map
      (fun (definition : Sst.type_definition) ->
        let logical_type =
          Option.map
            (fun logical_type ->
              { logical_type = Some logical_type; logical_source = Spec_definition.source_type logical_type; logical_nominal = Spec_definition.nominal_type logical_type })
            (Spec_definition.classify_logical_type program.types (Sst.Aggregate definition.Sst.type_id))
        in
        { definition; visibility = { representation = definition.Sst.representation }; logical_type })
      program.types
  let rank_domain_for_formal ~physical_program ~types rank_domains (pattern : Sst.pattern) =
    let rank_domain =
      match pattern.typ with
      | Sst.Aggregate type_id -> finite_rank_domain_for_type ~program:physical_program ~span:pattern.span ~types !rank_domains type_id
      | Sst.Application _ as application -> (
          match Parametric_rank_domain_private.derive_application ~program:physical_program ~span:pattern.span application with Ok domain -> Some domain | Error _ -> None)
      | Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _ | Sst.Parameter _ -> None
    in
    Option.iter (fun domain -> rank_domains := domain :: !rank_domains) rank_domain;
    rank_domain
  let local_finite_formals ~physical_program ~types ~instance_modes finite_environment rank_domains callables =
    List.concat_map
      (fun (callable : callable_descriptor) ->
        List.mapi
          (fun ordinal parameter ->
            match parameter with
            | Sst.Callback_parameter _ -> None
            | Sst.Value_parameter value_parameter -> (
                match Finite_formal_requirement.find finite_environment callable.definition.function_id ~ordinal with
                | None -> None
                | Some requirement ->
                    rank_domain_for_formal ~physical_program ~types rank_domains value_parameter.pattern
                    |> Option.map (fun rank_domain ->
                        {
                          callable = callable.definition.function_id;
                          ordinal;
                          label = value_parameter.label;
                          pattern_digest = Digest.string (pattern_snapshot value_parameter.pattern) |> Digest.to_hex;
                          binding_ids = pattern_binding_ids value_parameter.pattern;
                          typ = value_parameter.pattern.typ;
                          mode = Instance_mode.formal_mode instance_modes callable.definition ordinal parameter;
                          rank_domain;
                          requirement_digest = Finite_formal_requirement.requirement_digest requirement;
                        })))
          callable.definition.parameters
        |> List.filter_map Fun.id)
      callables
  let imported_finite_formals ~physical_program ~types ~imports rank_domains callables =
    match imports with
    | None -> []
    | Some registration ->
        callables
        |> List.concat_map (fun (callable : callable_descriptor) ->
            List.mapi
              (fun ordinal parameter ->
                match parameter with
                | Sst.Callback_parameter _ -> None
                | Sst.Value_parameter value_parameter -> (
                    match Imported_callable.finite_requirement registration callable.definition.function_id ordinal with
                    | None -> None
                    | Some requirement ->
                        rank_domain_for_formal ~physical_program ~types rank_domains value_parameter.pattern
                        |> Option.map (fun rank_domain ->
                            {
                              callable = callable.definition.function_id;
                              ordinal;
                              label = requirement.label;
                              pattern_digest = requirement.pattern_digest;
                              binding_ids = requirement.binding_ids;
                              typ = requirement.typ;
                              mode = requirement.mode;
                              rank_domain;
                              requirement_digest = requirement.requirement_digest;
                            })))
              callable.definition.parameters
            |> List.filter_map Fun.id)
  let frozen_formals ~types finite_environment callables =
    List.concat_map
      (fun (callable : callable_descriptor) ->
        List.mapi
          (fun ordinal parameter ->
            match parameter with
            | Sst.Callback_parameter _ -> None
            | Sst.Value_parameter value_parameter -> (
                match (Finite_formal_requirement.find finite_environment callable.definition.function_id ~ordinal, value_parameter.pattern.typ) with
                | Some _, Sst.Aggregate type_id when Option.is_some (frozen_spine_for_type types type_id) ->
                    Some { frozen_callable = callable.definition.function_id; frozen_ordinal = ordinal; frozen_type = type_id }
                | (None | Some _), _ -> None))
          callable.definition.parameters
        |> List.filter_map Fun.id)
      callables
  let find_type_descriptor types type_id = List.find_opt (fun (descriptor : type_descriptor) -> same_type_id descriptor.definition.Sst.type_id type_id) types
  let model_descriptors ~physical_program (program : Sst.program) types callables =
    List.filter_map
      (fun (callable : callable_descriptor) ->
        let definition = callable.definition in
        match Spec_definition.authenticated_model_domain program.types definition with
        | Some domain -> (
            let owned_recursive_contents = match collect_owned_recursive_contents_model ~program:physical_program definition with Ok grammar -> Some grammar | Error _ -> None in
            let logical_result =
              match Spec_definition.classify_logical_type program.types definition.result_type with
              | Some logical -> Some { logical_type = Some logical; logical_source = Spec_definition.source_type logical; logical_nominal = Spec_definition.nominal_type logical }
              | None -> (
                  match owned_recursive_contents with
                  | Some grammar
                    when
                      definition.result_type
                      = Owned_recursive_contents_private.result_type grammar ->
                      Some
                        {
                          logical_type = None;
                          logical_source = definition.result_type;
                          logical_nominal =
                            (match definition.result_type with
                            | Sst.Aggregate type_id -> Some type_id
                            | Sst.Application _ -> None
                            | Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _
                            | Sst.Parameter _ ->
                                assert false);
                        }
                  | Some _ | None -> (
                      match frozen_spine_for_type program.types domain with
                      | Some frozen when same_function_id definition.function_id frozen.frozen_model && definition.result_type = Sst.Aggregate frozen.frozen_result_type ->
                          Option.map
                            (fun logical ->
                              { logical_type = Some logical; logical_source = Spec_definition.source_type logical; logical_nominal = Spec_definition.nominal_type logical })
                            (Spec_definition.classify_frozen_recursive_logical_type program.types definition.result_type)
                      | Some _ | None -> None))
            in
            match (find_type_descriptor types domain, logical_result) with
            | Some domain, Some result ->
                let owned_root_scalar =
                  match collect_owned_root_scalar_model ~program:physical_program ~types:program.types definition with Ok template -> Some template | Error _ -> None
                in
                Some { callable; domain; result; visibility = domain.visibility; owned_root_scalar; owned_recursive_contents }
            | (Some _ | None), (Some _ | None) -> assert false)
        | None -> None)
      callables
  let filtered_call_edges ~physical_program imports callables =
    call_edges imports callables
    |> List.filter (fun edge ->
        Option.is_none (owned_recursive_contents_for_helper ~program:physical_program edge.caller.definition)
        && Option.is_none (owned_recursive_contents_for_helper ~program:physical_program edge.callee.definition))
  let collect_application_rank_domains ~physical_program rank_domains finite_formals call_edges =
    List.iter
      (fun (edge : call_edge_descriptor) ->
        finite_formals
        |> List.filter (fun (requirement : finite_formal_descriptor) -> same_callable_id requirement.callable edge.callee.definition.function_id)
        |> List.iter (fun (requirement : finite_formal_descriptor) ->
            match List.nth_opt edge.actuals requirement.ordinal with
            | Some { actual = { typ = Sst.Application _ as typ; span; _ }; _ } -> (
                match Parametric_rank_domain_private.derive_application ~program:physical_program ~span typ with
                | Ok domain -> rank_domains := domain :: !rank_domains
                | Error _ -> ())
            | Some _ | None -> ()))
      call_edges
  let issue ~physical_program ~imports ~external_specifications (program : Sst.program) instance_modes finite_environment =
    let rank_domains = ref (Parametric_rank_domain_private.nominal_domains physical_program) in
    let types = type_descriptors program in
    let imported_snapshots = match imports with None -> [] | Some registration -> Imported_callable.registration_environment registration |> Imported_callable.callables in
    let callables =
      List.map (callable physical_program program.parametric_adts program.types rank_domains instance_modes imported_snapshots external_specifications) program.functions
    in
    let finite_formals =
      local_finite_formals ~physical_program ~types:program.types ~instance_modes finite_environment rank_domains callables
      @ imported_finite_formals ~physical_program ~types:program.types ~imports rank_domains callables
    in
    let frozen_formals = frozen_formals ~types:program.types finite_environment callables in
    let models = model_descriptors ~physical_program program types callables in
    let call_edges = filtered_call_edges ~physical_program imports callables in
    collect_application_rank_domains ~physical_program rank_domains finite_formals call_edges;
    let rank_domains =
      List.rev !rank_domains
      |> List.fold_left
           (fun unique domain ->
             if
               List.exists
                 (fun candidate ->
                   String.equal
                     (Parametric_rank_domain_private.domain_id candidate)
                     (Parametric_rank_domain_private.domain_id domain)
                   && String.equal
                        (Parametric_rank_domain_private.domain_version candidate)
                        (Parametric_rank_domain_private.domain_version domain)
                   && String.equal
                        (Parametric_rank_domain_private.snapshot_digest candidate)
                        (Parametric_rank_domain_private.snapshot_digest domain))
                 unique
             then unique
             else domain :: unique)
           []
      |> List.rev
    in
    {
      program = physical_program;
      imports;
      external_specifications;
      instance_modes;
      types;
      callables;
      models;
      call_edges;
      features = features callables;
      rank_domains;
      finite_formals;
      frozen_formals;
    }
end
type type_descriptor = Semantic_environment.type_descriptor
type logical_type_descriptor = Semantic_environment.logical_type_descriptor
type callable_descriptor = Semantic_environment.callable_descriptor
type finite_formal_descriptor = Semantic_environment.finite_formal_descriptor
type frozen_formal_descriptor = Semantic_environment.frozen_formal_descriptor
type model_descriptor = Semantic_environment.model_descriptor
type call_edge_descriptor = Semantic_environment.call_edge_descriptor
type formal_actual_descriptor = Semantic_environment.formal_actual_descriptor
type call_edge_region =
  Semantic_environment.call_edge_region =
  | Requires_region
  | Ensures_region
  | Decreases_region
  | Assertions_region
  | Body_region
type visibility_descriptor = Semantic_environment.visibility_descriptor
type feature_descriptor = Semantic_environment.feature_descriptor
type contract_descriptor = Semantic_environment.contract_descriptor
type contract_clause_descriptor = Semantic_environment.contract_clause_descriptor
type decrease_descriptor = Semantic_environment.decrease_descriptor
type validated_rank_domain = Semantic_environment.validated_rank_domain
type decrease_disposition =
  Semantic_environment.decrease_disposition =
  | No_decrease
  | Direct_integer_decrease of decrease_descriptor
  | Direct_structural_decrease of
      decrease_descriptor * validated_rank_domain
  | Direct_parametric_decrease of
      decrease_descriptor * Parametric_adt.t
  | Direct_frozen_spine_decrease of
      decrease_descriptor * Sst.frozen_spine_prerequisite
  | Missing_decrease of Diagnostic.span
  | Duplicate_decrease of Diagnostic.span
  | Inapplicable_decrease of Diagnostic.span
  | Non_integer_decrease of Diagnostic.span
  | Recursive_decrease of Diagnostic.span
type semantic_feature_requirement =
  Semantic_environment.semantic_feature_requirement =
  | Specification_semantics
  | Proof_semantics
  | External_specification_trust
  | Trusted_external_body
  | Owned_tree_reconstruction
  | Direct_recursion
type validated_program = Validated of Semantic_environment.t
let program (Validated environment) = environment.program
let type_descriptors (Validated environment) = environment.types
let find_type validated type_id =
  List.find_opt
    (fun (descriptor : type_descriptor) ->
      same_type_id descriptor.Semantic_environment.definition.type_id type_id)
    (type_descriptors validated)
let type_id (descriptor : type_descriptor) =
  descriptor.Semantic_environment.definition.type_id
let type_definition (descriptor : type_descriptor) =
  descriptor.Semantic_environment.definition
let type_visibility (descriptor : type_descriptor) =
  descriptor.Semantic_environment.visibility
let type_logical_type (descriptor : type_descriptor) =
  descriptor.Semantic_environment.logical_type
let visibility_representation (descriptor : visibility_descriptor) =
  descriptor.Semantic_environment.representation
let find_logical_type validated typ =
  match typ with
  | Sst.Aggregate type_id ->
      Option.bind (find_type validated type_id) type_logical_type
    | Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _ | Sst.Parameter _
    | Sst.Application _ ->
      Option.map
        (fun logical_type ->
          {
            Semantic_environment.logical_type = Some logical_type;
            logical_source = Spec_definition.source_type logical_type;
            logical_nominal = Spec_definition.nominal_type logical_type;
          })
        (Spec_definition.classify_logical_type
           (program validated).Sst.types typ)
let logical_source_type (descriptor : logical_type_descriptor) =
  descriptor.Semantic_environment.logical_source
let logical_nominal_type (descriptor : logical_type_descriptor) =
  descriptor.Semantic_environment.logical_nominal
let hide_owned_contents_helpers = ref false
let callable_descriptors (Validated environment) =
  if not !hide_owned_contents_helpers then environment.callables
  else
    List.filter
      (fun descriptor ->
        Option.is_none
          (Owned_recursive_contents_private.helper_for_program
             environment.program descriptor.Semantic_environment.definition))
      environment.callables
let with_owned_contents_helpers_hidden callback =
  let previous = !hide_owned_contents_helpers in
  hide_owned_contents_helpers := true;
  Fun.protect
    ~finally:(fun () -> hide_owned_contents_helpers := previous)
    callback
let find_callable validated function_id =
  List.find_opt
    (fun (descriptor : callable_descriptor) ->
      same_function_id
        descriptor.Semantic_environment.definition.function_id function_id)
    (callable_descriptors validated)
let callable_id (descriptor : callable_descriptor) =
  descriptor.Semantic_environment.definition.function_id
let callable_mode (descriptor : callable_descriptor) =
  descriptor.Semantic_environment.definition.mode
let callable_definition (descriptor : callable_descriptor) =
  descriptor.Semantic_environment.definition
let callable_contract (descriptor : callable_descriptor) =
  descriptor.Semantic_environment.contract
let callable_decrease (descriptor : callable_descriptor) =
  descriptor.Semantic_environment.decrease
let formal_requires_finite (Validated environment) descriptor ordinal =
  List.exists
    (fun (finite : finite_formal_descriptor) ->
      same_function_id finite.callable
        descriptor.Semantic_environment.definition.function_id
      && finite.ordinal = ordinal)
    environment.finite_formals
let finite_formal_requirement (Validated environment) descriptor ordinal =
  List.find_opt
    (fun (finite : finite_formal_descriptor) ->
      same_function_id finite.callable
        descriptor.Semantic_environment.definition.function_id
      && finite.ordinal = ordinal)
    environment.finite_formals
let frozen_formal_requirement (Validated environment) descriptor ordinal =
  List.find_opt
    (fun (frozen : frozen_formal_descriptor) ->
      same_function_id frozen.frozen_callable
        descriptor.Semantic_environment.definition.function_id
      && frozen.frozen_ordinal = ordinal)
    environment.frozen_formals
let frozen_formal_type descriptor = descriptor.Semantic_environment.frozen_type
let finite_formal_ordinal (descriptor : finite_formal_descriptor) =
  descriptor.ordinal
let finite_formal_label (descriptor : finite_formal_descriptor) =
  descriptor.label
let finite_formal_pattern_digest (descriptor : finite_formal_descriptor) =
  descriptor.pattern_digest
let finite_formal_binding_ids (descriptor : finite_formal_descriptor) =
  descriptor.binding_ids
let finite_formal_type (descriptor : finite_formal_descriptor) = descriptor.typ
let finite_formal_mode (descriptor : finite_formal_descriptor) = descriptor.mode
let finite_formal_rank_domain (descriptor : finite_formal_descriptor) =
  descriptor.rank_domain
let finite_formal_digest (descriptor : finite_formal_descriptor) =
  descriptor.requirement_digest
let finite_formal_dump (Validated environment) =
  environment.finite_formals
  |> List.sort
       (fun (left : finite_formal_descriptor)
            (right : finite_formal_descriptor) ->
         match Int.compare left.callable.function_index right.callable.function_index with
         | 0 -> Int.compare left.ordinal right.ordinal
         | order -> order)
  |> List.map (fun (descriptor : finite_formal_descriptor) ->
         Printf.sprintf "%s#%d formal=%d label=%s mode=%s type=%s digest=%s"
           descriptor.callable.function_name
           descriptor.callable.function_index descriptor.ordinal
           (Option.value ~default:"-" descriptor.label)
           (match descriptor.mode with
           | Sst.Exec_instance -> "Exec"
           | Sst.Tracked_instance -> "Tracked"
           | Sst.Ghost_instance -> "Ghost")
           (Sst.string_of_type descriptor.typ) descriptor.requirement_digest)
  |> String.concat "\n"
let binding_instance_mode (Validated environment) function_id binding =
  Instance_mode.binding_mode environment.instance_modes function_id binding
let expression_instance_mode (Validated environment) function_id expression =
  Instance_mode.expression_mode environment.instance_modes function_id expression
let expression_mode_explicit (Validated environment) function_id expression =
  Instance_mode.expression_explicit environment.instance_modes function_id
    expression
let field_instance_mode (Validated environment) field =
  Instance_mode.field_mode environment.instance_modes field
let formal_instance_mode (Validated environment) descriptor index parameter =
  match
    List.nth_opt descriptor.Semantic_environment.formal_modes index
  with
  | Some mode -> mode
  | None ->
      Instance_mode.formal_mode environment.instance_modes
        descriptor.Semantic_environment.definition index parameter
let formal_has_closed_invariant_authority (Validated environment) descriptor
    index =
  Instance_mode.formal_has_closed_authority environment.instance_modes
    descriptor.Semantic_environment.definition index
let result_instance_mode (Validated _) descriptor =
  descriptor.Semantic_environment.result_mode
let instance_modes_authenticated (Validated environment) =
  Instance_mode.authenticated environment.instance_modes
let instance_mode_dump (Validated environment) =
  Instance_mode.to_string environment.instance_modes
let model_descriptors (Validated environment) = environment.models
let find_model validated function_id =
  List.find_opt
    (fun (descriptor : model_descriptor) ->
      same_function_id
        descriptor.Semantic_environment.callable.definition.function_id
        function_id)
    (model_descriptors validated)
let model_callable (descriptor : model_descriptor) =
  descriptor.Semantic_environment.callable
let model_domain (descriptor : model_descriptor) =
  descriptor.Semantic_environment.domain
let model_result (descriptor : model_descriptor) =
  descriptor.Semantic_environment.result
let model_visibility (descriptor : model_descriptor) =
  descriptor.Semantic_environment.visibility
let owned_root_scalar_model (descriptor : model_descriptor) =
  descriptor.Semantic_environment.owned_root_scalar
let owned_recursive_contents_model (descriptor : model_descriptor) =
  descriptor.Semantic_environment.owned_recursive_contents
let authenticate_owned_recursive_contents_model ~validated
    (descriptor : model_descriptor) grammar =
  let environment =
    match validated with Validated environment -> environment
  in
  match descriptor.Semantic_environment.owned_recursive_contents with
  | Some issued ->
      issued == grammar
      && Owned_recursive_contents_private.authenticate
           ~program:environment.Semantic_environment.program
           ~model:descriptor.Semantic_environment.callable.definition grammar
  | None -> false
let authenticate_owned_root_scalar_model ~validated
    (descriptor : model_descriptor) template =
  let environment =
    match validated with Validated environment -> environment
  in
  template.owned_template_issuer == owned_scalar_model_issuer
  &&
  (match descriptor.Semantic_environment.owned_root_scalar with
  | Some owned -> owned == template
  | None -> false)
  && environment.Semantic_environment.program == template.owned_template_program
  && String.equal
       (Sst.to_string environment.Semantic_environment.program)
       template.owned_template_program_snapshot
  && descriptor.Semantic_environment.callable.definition
     == template.owned_template_definition
  && String.equal
       (Marshal.to_string template.owned_template_body [ Marshal.No_sharing ])
       template.owned_template_body_snapshot
  &&
  let program = environment.Semantic_environment.program in
  match
    List.find_opt
      (fun (definition : Sst.type_definition) ->
        same_type_id definition.type_id template.owned_template_domain)
      program.types
  with
  | None -> false
  | Some definition ->
      Typedtree_adapter.authenticate_abstraction ~definition
        ~types:program.types ~functions:program.functions
        template.owned_template_evidence
let owned_root_scalar_formal template = template.owned_template_formal
let owned_root_scalar_domain template = template.owned_template_domain
let owned_root_scalar_result template = template.owned_template_result
let owned_root_scalar_result_fields template =
  template.owned_template_result_fields
let owned_root_scalar_paths template = template.owned_template_paths
let owned_root_scalar_reads template = template.owned_template_reads
let owned_root_scalar_projections template = template.owned_template_projections
let owned_root_scalar_matches template = template.owned_template_matches
let owned_scalar_path_steps path = path.owned_path_steps
let owned_scalar_path_terminal path = path.owned_path_terminal
let owned_scalar_read_expression read = read.owned_read_expression
let owned_scalar_read_path read = read.owned_read_path
let owned_scalar_projection_expression projection =
  projection.owned_projection_expression
let owned_scalar_projection_path projection = projection.owned_projection_path
let owned_scalar_projection_type projection = projection.owned_projection_type
let owned_scalar_match_expression matched = matched.owned_match_expression
let owned_scalar_match_scrutinee matched = matched.owned_match_scrutinee
let owned_scalar_match_path matched = matched.owned_match_path
let owned_scalar_match_constructors matched = matched.owned_match_constructors
let contract_requires (descriptor : contract_descriptor) =
  descriptor.Semantic_environment.requires
let contract_ensures (descriptor : contract_descriptor) =
  descriptor.Semantic_environment.ensures
let contract_decreases (descriptor : contract_descriptor) =
  descriptor.Semantic_environment.decreases
let contract_assertions (descriptor : contract_descriptor) =
  descriptor.Semantic_environment.assertions
let contract_clause_index (descriptor : contract_clause_descriptor) =
  descriptor.Semantic_environment.clause_index
let contract_clause_span (descriptor : contract_clause_descriptor) =
  descriptor.Semantic_environment.span
let contract_clause_binder (descriptor : contract_clause_descriptor) =
  descriptor.Semantic_environment.binder
let contract_clause_expression (descriptor : contract_clause_descriptor) =
  descriptor.Semantic_environment.expression
let decrease_clause (descriptor : decrease_descriptor) =
  descriptor.Semantic_environment.clause
let call_edge_descriptors (Validated environment) = environment.call_edges
let call_edge_caller (descriptor : call_edge_descriptor) =
  descriptor.Semantic_environment.caller
let call_edge_callee (descriptor : call_edge_descriptor) =
  descriptor.Semantic_environment.callee
let call_edge_form (descriptor : call_edge_descriptor) =
  descriptor.Semantic_environment.call_form
let call_edge_recursive (descriptor : call_edge_descriptor) =
  descriptor.Semantic_environment.recursive
let call_edge_span (descriptor : call_edge_descriptor) =
  descriptor.Semantic_environment.span
let call_edge_region (descriptor : call_edge_descriptor) =
  descriptor.Semantic_environment.region
let call_edge_actuals (descriptor : call_edge_descriptor) =
  descriptor.Semantic_environment.actuals
let formal_parameter (descriptor : formal_actual_descriptor) =
  descriptor.Semantic_environment.formal
let actual_label (descriptor : formal_actual_descriptor) =
  descriptor.Semantic_environment.actual_label
let actual_expression (descriptor : formal_actual_descriptor) =
  descriptor.Semantic_environment.actual
let feature_descriptors (Validated environment) = environment.features
let feature_callable (descriptor : feature_descriptor) =
  descriptor.Semantic_environment.callable
let feature_requirement (descriptor : feature_descriptor) =
  descriptor.Semantic_environment.requirement
let abstraction_evidence validated type_id =
  match find_type validated type_id with
  | None -> None
  | Some descriptor -> (
      match visibility_representation (type_visibility descriptor) with
      | Sst.Abstract_with_evidence
          (Sst.Authenticated_same_cmt_abstraction evidence) ->
          Some evidence
      | Sst.Revealed
      | Sst.Abstract_with_evidence
          (Sst.Incomplete_abstraction_evidence _
          | Sst.Proposed_same_cmt_abstraction _) ->
          None)
let rank_domains (Validated environment) =
  environment.Semantic_environment.rank_domains
let rank_domain_id = Parametric_rank_domain_private.domain_id
let rank_domain_version = Parametric_rank_domain_private.domain_version
let rank_snapshot_digest = Parametric_rank_domain_private.snapshot_digest
let rank_component domain =
  Parametric_rank_domain_private.component domain
  |> List.map
       (fun
         (identity : Typedtree_adapter_issuance_private.rank_type_identity) ->
         {
           Typedtree_adapter.rank_type_id = identity.rank_type_id;
           rank_path = identity.rank_path;
           rank_uid = identity.rank_uid;
           rank_span = identity.rank_span;
         })
let rank_positive_children domain =
  Parametric_rank_domain_private.positive_children domain
  |> List.map
       (fun
         (child : Typedtree_adapter_issuance_private.rank_positive_child) ->
         {
           Typedtree_adapter.rank_constructor = child.rank_constructor;
           rank_constructor_uid = child.rank_constructor_uid;
           rank_field = child.rank_field;
           rank_field_uid = child.rank_field_uid;
           rank_child_path = child.rank_child_path;
           rank_child_type = child.rank_child_type;
           rank_expansion_trace = child.rank_expansion_trace;
         })
let rank_ground_witnesses domain =
  Parametric_rank_domain_private.ground_witnesses domain
  |> List.map
       (fun
         (witness : Typedtree_adapter_issuance_private.rank_ground_witness) ->
         {
           Typedtree_adapter.rank_ground_constructor =
             witness.rank_ground_constructor;
           rank_ground_constructor_uid = witness.rank_ground_constructor_uid;
         })
let rank_immutable = Parametric_rank_domain_private.immutable
let validate_owned_tree_function_structure program =
  iter_result
    (fun (definition : Sst.function_definition) ->
      if function_contains_owned_tree_transition definition then
        validate_function ~require_authenticated_owned_tree_roots:false
          ~rank_domains:[] ~physical_program:program ~external_specifications:None
          program.Sst.types
          program.functions definition
      else Ok ())
    program.functions
let validate_rank_domains program =
  match Sst_rank_domain_validation_private.validate program with
  | Ok () -> Ok ()
  | Error validation ->
      fail validation.span (Forged_rank_domain validation.detail)
let validate_internal ?imports ?external_specifications (program : Sst.program) =
  let imported_callables =
    match imports with
    | None -> []
    | Some registration ->
        let environment =
          Imported_callable.registration_environment registration
        in
        Imported_callable.callables environment
          |> List.map (fun (snapshot : Imported_callable.callable_snapshot) ->
               snapshot.definition)
  in
  let semantic_program =
    { program with Sst.functions = imported_callables @ program.functions }
  in
  let policy_span =
    match (program.Sst.functions, program.types) with
    | definition :: _, _ -> definition.Sst.span
    | [], definition :: _ -> definition.Sst.span
    | [], [] -> Diagnostic.file_span "<semantic-sst>"
  in
  let* () = validate_policy policy_span program.Sst.policy in
  let* () = validate_unique_identities semantic_program in
  (* Canonical nominal ownership is established before any function pass can
     classify an aggregate as logical and before semantic handles are issued. *)
  let* () =
    iter_result validate_revealed_nominal_definition program.Sst.types
  in
  (* Transition-bearing functions are checked before their abstraction
     certificates so malformed owned-tree metadata receives its structural
     diagnostic.  This private phase cannot issue a validated program. *)
  let* () = validate_owned_tree_function_structure program in
  let* () =
    iter_result
      (validate_type_definition semantic_program.types
         semantic_program.functions)
      program.types
  in
  let* () = validate_rank_domains program in
  let* () = validate_spec_graph semantic_program.functions in
  let* () = validate_proof_graph semantic_program.functions in
  let validate_instance_modes () =
    match Instance_mode.validate program with
    | Ok environment -> Ok environment
    | Error mode_error ->
        fail ?function_id:mode_error.function_id mode_error.span
          (Invalid_instance_mode mode_error.message)
  in
  let* early_instance_modes =
    if Instance_mode.has_sealed_registration program then
      let* environment = validate_instance_modes () in
      Ok (Some environment)
    else Ok None
  in
  let* () =
    program.functions
    |> iter_result (fun (definition : Sst.function_definition) ->
           definition.contracts.decreases
           |> iter_result (fun (clause : Sst.predicate_clause) ->
                  match clause.Sst.predicate.expression.typ with
                  | Sst.Application _ as application -> (
                      match
                        Parametric_rank_domain_private.derive_application
                          ~program ~span:clause.span application
                      with
                      | Ok _ -> Ok ()
                      | Error error ->
                          fail ~function_id:definition.function_id error.span
                            (Forged_rank_domain error.detail))
                  | Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _
                  | Sst.Aggregate _ | Sst.Parameter _ ->
                      Ok ()))
  in
  (* Authentication binds the exact type/function snapshot.  The strict pass
     also excludes structurally valid transitions rooted in revealed types. *)
  let* () =
    validate_functions ~require_authenticated_owned_tree_roots:true
      ~rank_program:program ~external_specifications
      ~definitions:program.functions semantic_program
  in
  let* () =
    validate_shared_invariant_client_paths program.types program.functions
  in
  let* instance_modes =
    match early_instance_modes with
    | Some environment -> Ok environment
    | None -> validate_instance_modes ()
  in
  let* finite_environment =
    match Finite_formal_requirement.validate program with
    | Ok environment -> Ok environment
      | Error message -> fail policy_span (Invalid_finite_requirement message)
  in
  let rank_domains = Parametric_rank_domain_private.nominal_domains program in
  let rank_domain_for_type type_id =
    finite_rank_domain_for_type ~program ~span:policy_span
      ~types:program.types rank_domains type_id
  in
  let* () =
    iter_result
      (fun (definition : Sst.function_definition) ->
        let rec formals ordinal = function
          | [] -> Ok ()
          | Sst.Callback_parameter _ :: rest ->
              formals (ordinal + 1) rest
          | Sst.Value_parameter parameter :: rest -> (
              match
                Finite_formal_requirement.find finite_environment
                  definition.function_id ~ordinal
              with
              | None -> formals (ordinal + 1) rest
              | Some _ ->
                  let invalid message =
                    fail ~function_id:definition.function_id
                      parameter.pattern.span
                      (Invalid_finite_requirement message)
                  in
                  let* () =
                    match definition.body with
                    | Sst.Checked_exec
                        { provenance = Sst.Authenticated_typedtree _; _ }
                    | Sst.Proof_body _ ->
                        Ok ()
                    | Sst.Spec_definition _ when not definition.recursive ->
                        Ok ()
                    | Sst.Recursive_spec_definition _
                        when authenticated_frozen_spine_helper program.types
                          definition ->
                        Ok ()
                      | Sst.Recursive_spec_definition _ | Sst.Spec_definition _
                        ->
                        invalid
                            "recursive specification definitions cannot \
                             require finite formals"
                    | Sst.Checked_exec
                        { provenance = Sst.Raw_semantic_body _; _ }
                    | Sst.External_specification _
                    | Sst.Trusted_external_spec_target _
                    | Sst.Trusted_external_body _ ->
                        invalid
                            "unchecked, external, trusted, or raw callables \
                             cannot require finite formals"
                    | Sst.Symbolic_declaration _ ->
                        invalid
                          "symbolic declarations cannot require finite formals"
                  in
                  let* () =
                    match parameter.pattern.typ with
                    | Sst.Aggregate type_id -> (
                        match rank_domain_for_type type_id with
                        | Some _ -> Ok ()
                        | None
                            when Option.is_some
                                   (frozen_spine_for_type program.types type_id)
                            ->
                            Ok ()
                        | None ->
                            invalid
                                "finite formal must be an exact local ranked \
                                 deeply immutable aggregate")
                      | Sst.Unit | Sst.Bool | Sst.Int | Sst.Tuple _
                      | Sst.Parameter _ ->
                        invalid
                            "finite formal must be an exact local ranked \
                             aggregate"
                      | Sst.Application _ as application -> (
                          match
                            Parametric_rank_domain_private.derive_application
                              ~program ~span:parameter.pattern.span application
                          with
                          | Ok _ -> Ok ()
                          | Error error -> invalid error.detail)
                  in
                  formals (ordinal + 1) rest)
        in
        formals 0 definition.parameters)
      program.functions
  in
  let* () =
    match
      Typedtree_adapter_private.Public.issue_builtin_local_assertions
        ~program
    with
    | Ok () -> Ok ()
    | Error message ->
        fail policy_span (Malformed_expression message)
  in
  Ok
    (Validated
       (Semantic_environment.issue ~physical_program:program ~imports ~external_specifications
          semantic_program instance_modes finite_environment))
let program_at_validation_boundary program =
  let program =
    match !program_mutator_for_testing with
    | Some mutate -> mutate program
    | None -> program
  in
  Option.iter
    (fun observer -> observer ())
    !validation_boundary_observer_for_testing;
  program
let validate program =
  validate_internal (program_at_validation_boundary program)
let validate_with_imports registration program =
  validate_internal ~imports:registration
    (program_at_validation_boundary program)
let validate_with_imports_and_external registration external_specifications program =
  validate_internal ~imports:registration ~external_specifications
    (program_at_validation_boundary program)
let policy_name = function
  | Sst.Default_linear_z3 -> "default-linear/default-z3"
  | Sst.Default_linear_cvc5 -> "default-linear/default-cvc5"
  | Sst.Nonlinear_z3 -> "nonlinear/default-z3"
let error_to_string error =
  let detail =
    match error.kind with
    | Unsupported_policy policy -> "unsupported policy " ^ policy_name policy
    | Duplicate_type_id id ->
        Printf.sprintf "duplicate type id %s#%d" id.type_name id.type_index
    | Duplicate_function_id id ->
        Printf.sprintf "duplicate function id %s#%d" id.function_name
          id.function_index
    | Duplicate_binding_id id -> Printf.sprintf "duplicate binding id %d" id
    | Unknown_type_id id ->
        Printf.sprintf "unknown type id %s#%d" id.type_name id.type_index
    | Unknown_function_id id ->
        Printf.sprintf "unknown function id %s#%d" id.function_name
          id.function_index
    | Conflicting_identity detail | Invalid_clause detail | Invalid_body detail
    | Invalid_call detail | Invalid_recursive_marker detail
    | Invalid_unique_return detail | Invalid_target_link detail
    | Forged_abstract_evidence detail | Forged_rank_domain detail
    | Invalid_instance_mode detail | Invalid_finite_requirement detail
    | Malformed_expression detail ->
        detail
    | Unbound_binding binding ->
        Printf.sprintf "unbound binding %s#%d" binding.name binding.id
  in
  let function_name =
    Option.fold ~none:"program"
      ~some:(fun id -> id.Sst.function_name)
      error.function_id
  in
  let span = error.span in
  Printf.sprintf "%s: invalid semantic SST: %s at %s:%d:%d-%d:%d"
    function_name detail (Filename.basename span.Diagnostic.file)
    span.start_pos.line span.start_pos.column span.end_pos.line
    span.end_pos.column
let external_target_identity (Validated environment) definition =
  Option.bind environment.Semantic_environment.external_specifications
    (fun registration ->
      External_target_specification_private.external_target_identity registration
        ~program:environment.program definition)
module For_testing = struct
  let with_program_mutation_at_validation_boundary ~mutate ~observe callback =
    if
      !program_mutator_for_testing <> None
      || !validation_boundary_observer_for_testing <> None
    then invalid_arg "validation-boundary mutation observer is already active";
    program_mutator_for_testing := Some mutate;
    validation_boundary_observer_for_testing := Some observe;
    Fun.protect
      ~finally:(fun () ->
        program_mutator_for_testing := None;
        validation_boundary_observer_for_testing := None)
      callback
  let reset_ghost_formal_flow_count =
    Instance_mode.For_testing.reset_ghost_formal_flow_count
  let ghost_formal_flow_count =
    Instance_mode.For_testing.ghost_formal_flow_count
  let copied_instance_mode_descriptor_rejected (Validated environment) =
    Instance_mode.For_testing.copied_descriptor_rejected
      environment.Semantic_environment.instance_modes
end
end
