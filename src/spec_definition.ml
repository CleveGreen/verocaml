type logical_type = {
  source_type : Sst.typ;
  nominal_type : Sst.type_id option;
}

let same_type_id (left : Sst.type_id) (right : Sst.type_id) =
  left.type_index = right.type_index
  && String.equal left.type_name right.type_name

let lookup_type definitions type_id =
  List.find_opt
    (fun (definition : Sst.type_definition) ->
      same_type_id definition.type_id type_id)
    definitions

let fields = function
  | Sst.Record_definition fields -> fields
  | Sst.Variant_definition constructors ->
      List.concat_map
        (fun (constructor : Sst.constructor_definition) ->
          constructor.constructor_fields)
        constructors

(* This is the single closed first-slice logical-value classifier.  It accepts
   only ground SST types, follows nominal definitions by exact identity, and
   rejects both mutable fields and every aggregate cycle.  Abstract types are
   deliberately not logical values: one may cross the logical boundary only
   through [authenticated_model_domain]. *)
let classify_logical_type definitions typ =
  let rec classify visiting typ =
    let logical ?nominal_type source_type =
      Some { source_type; nominal_type }
    in
    match typ with
    | Sst.Unit -> logical Sst.Unit
    | Sst.Bool -> logical Sst.Bool
    | Sst.Int -> logical Sst.Int
    | Sst.Mathematical_int -> logical Sst.Mathematical_int
    | Sst.Parameter _ as parameter -> logical parameter
    | Sst.Application _ -> None
    | Sst.Tuple components ->
        if
          List.for_all
            (fun (_, component) ->
              Option.is_some (classify visiting component))
            components
        then logical typ
        else None
    | Sst.Aggregate type_id ->
        if List.exists (same_type_id type_id) visiting then None
        else
          match lookup_type definitions type_id with
          | Some
              {
                representation = Sst.Revealed;
                type_kind;
                _;
              }
            when List.for_all
                   (fun (field : Sst.field_definition) ->
                     field.field_mutability = Sst.Immutable_field
                     && field.field_modalities.uniqueness_modality
                        = Sst.Preserve_uniqueness
                     && field.field_modalities.linearity_modality
                        = Sst.Preserve_linearity
                     && Option.is_some
                          (classify (type_id :: visiting) field.field_type))
                   (fields type_kind) ->
              logical ~nominal_type:type_id typ
          | Some
              {
                representation = Sst.Abstract_with_evidence _;
                _;
              }
          | None
          | Some { representation = Sst.Revealed; _ } ->
              None
  in
  classify [] typ

let classify_frozen_recursive_logical_type definitions typ =
  match typ with
  | Sst.Aggregate type_id -> (
      match lookup_type definitions type_id with
      | Some
          {
            representation = Sst.Revealed;
            type_kind =
              Sst.Variant_definition
                [
                  { constructor_fields = []; _ };
                  {
                    constructor_fields =
                      [
                        {
                          field_type = Sst.Int;
                          field_mutability = Sst.Immutable_field;
                          field_modalities =
                            {
                              uniqueness_modality =
                                Sst.Preserve_uniqueness;
                              linearity_modality = Sst.Preserve_linearity;
                            };
                          _;
                        };
                        {
                          field_type = Sst.Aggregate tail;
                          field_mutability = Sst.Immutable_field;
                          field_modalities =
                            {
                              uniqueness_modality =
                                Sst.Preserve_uniqueness;
                              linearity_modality = Sst.Preserve_linearity;
                            };
                          _;
                        };
                      ];
                    _;
                  };
                ];
            _;
          }
        when same_type_id type_id tail ->
          Some { source_type = typ; nominal_type = Some type_id }
      | Some _ | None -> None)
  | Sst.Unit | Sst.Bool | Sst.Int | Sst.Mathematical_int | Sst.Tuple _
  | Sst.Parameter _ | Sst.Application _ -> None

let source_type logical = logical.source_type
let nominal_type logical = logical.nominal_type

let authenticated_model_domain definitions
    (definition : Sst.function_definition) =
  match (definition.mode, definition.body, definition.parameters) with
  | ( Sst.Spec,
      Sst.Spec_definition _,
      [
        Sst.Value_parameter
          {
          label = None;
          pattern =
            {
              pattern_desc =
                Sst.Bind
                  {
                    typ = Sst.Aggregate domain;
                    uniqueness = Sst.Definitely_aliased;
                    _;
                  };
              typ = Sst.Aggregate pattern_domain;
              _;
            };
          _;
          };
      ] )
    when same_type_id domain pattern_domain -> (
      match lookup_type definitions domain with
      | Some
          {
            representation =
              Sst.Abstract_with_evidence
                (Sst.Authenticated_same_cmt_abstraction evidence);
            _;
          }
        when
          same_type_id evidence.abstract_signature_type domain
          && same_type_id evidence.hidden_implementation_type domain
          && List.exists
               (fun (operation : Sst.abstract_public_operation) ->
                 (operation.public_role = Sst.Abstract_model
                 || operation.public_role = Sst.Current_model)
                 && operation.public_function_index
                    = definition.function_id.function_index
                 && String.equal operation.public_function_name
                      definition.function_id.function_name)
               evidence.public_surface ->
          Some domain
      | Some
          {
            representation =
              ( Sst.Revealed
              | Sst.Abstract_with_evidence
                  (Sst.Incomplete_abstraction_evidence _
                  | Sst.Proposed_same_cmt_abstraction _) );
            _;
          }
      | Some
          {
            representation =
              Sst.Abstract_with_evidence
                (Sst.Authenticated_same_cmt_abstraction _);
            _;
          }
      | None ->
          None)
  | ( (Sst.Spec | Sst.Proof | Sst.Exec),
      ( Sst.Checked_exec _
      | Sst.Spec_definition _
      | Sst.Recursive_spec_definition _
      | Sst.Proof_body _
      | Sst.External_specification _
      | Sst.Trusted_external_spec_target _
      | Sst.Trusted_external_body _
      | Sst.Symbolic_declaration _ ),
      _ ) ->
      None

let authenticated_invariant_domain definitions
    (definition : Sst.function_definition) =
  match (definition.mode, definition.body, definition.parameters) with
  | ( Sst.Spec,
      Sst.Spec_definition _,
      [
        Sst.Value_parameter
          {
          label = None;
          pattern =
            {
              pattern_desc =
                Sst.Bind
                  {
                    typ = Sst.Aggregate domain;
                    uniqueness = Sst.Definitely_aliased;
                    _;
                  };
              typ = Sst.Aggregate pattern_domain;
              _;
            };
          _;
          };
      ] )
    when same_type_id domain pattern_domain && definition.result_type = Sst.Bool
    -> (
      match lookup_type definitions domain with
      | Some
          {
            representation =
              Sst.Abstract_with_evidence
                (Sst.Authenticated_same_cmt_abstraction evidence);
            _;
          }
        when
          same_type_id evidence.abstract_signature_type domain
          && same_type_id evidence.hidden_implementation_type domain
          && List.exists
               (fun (operation : Sst.abstract_public_operation) ->
                 operation.public_role = Sst.Abstract_invariant
                 && operation.public_function_index
                    = definition.function_id.function_index
                 && String.equal operation.public_function_name
                      definition.function_id.function_name)
               evidence.public_surface ->
          Some domain
      | Some
          {
            representation =
              ( Sst.Revealed
              | Sst.Abstract_with_evidence
                  (Sst.Incomplete_abstraction_evidence _
                  | Sst.Proposed_same_cmt_abstraction _) );
            _;
          }
      | Some
          {
            representation =
              Sst.Abstract_with_evidence
                (Sst.Authenticated_same_cmt_abstraction _);
            _;
          }
      | None ->
          None)
  | ( (Sst.Spec | Sst.Proof | Sst.Exec),
      ( Sst.Checked_exec _
      | Sst.Spec_definition _
      | Sst.Recursive_spec_definition _
      | Sst.Proof_body _
      | Sst.External_specification _
      | Sst.Trusted_external_spec_target _
      | Sst.Trusted_external_body _
      | Sst.Symbolic_declaration _ ),
      _ ) ->
      None
