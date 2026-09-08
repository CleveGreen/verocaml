open Parsetree

type representation = Immediate | Boxed

type carrier = {
  base_path : string option;
  profile_reference : string;
  representation : representation;
  compatibility : string list;
}

type role = {
  carrier_path : string;
  role_schema : string;
  role_identity : string;
  semantics_path : string;
  visibility : string;
  reveal : bool;
  inline : bool;
}

let carrier_marker = "verocaml.internal.numeric.carrier_source_claim.v1"
let role_marker = "verocaml.internal.numeric.role_source_claim.v1"

let ( let* ) value continuation =
  match value with Ok value -> continuation value | Error _ as error -> error

let rec longident = function
  | Longident.Lident name -> name
  | Ldot (prefix, name) -> longident prefix ^ "." ^ name
  | Lapply _ -> raise Exit

let payload_record attribute =
  if
    not
      (attribute.attr_loc.Location.loc_ghost
      && attribute.attr_name.loc.loc_ghost)
  then Error "numeric source claim marker is not PPX-issued"
  else
    match attribute.attr_payload with
    | PStr
        [ { pstr_desc = Pstr_eval ({ pexp_desc = Pexp_record (fields, None); _ }, []);
            _ } ] ->
        let fields =
          List.map
            (fun (label, expression) ->
              let name =
                try longident label.Location.txt
                with Exit -> "<applied-label>"
              in
              (name, expression))
            fields
        in
        let names = List.map fst fields in
        if List.length names <> List.length (List.sort_uniq String.compare names)
        then Error "numeric source claim repeats a record field"
        else Ok fields
    | _ -> Error "numeric source claim is not one closed record expression"

let field fields name = List.assoc_opt name fields

let string = function
  | { pexp_desc = Pexp_constant (Pconst_string (value, _, _)); _ } -> Ok value
  | _ -> Error "numeric source claim field is not a string literal"

let identifier = function
  | { pexp_desc = Pexp_ident { txt; _ }; _ } -> (
      try Ok (longident txt)
      with Exit -> Error "numeric source claim path uses a functor application")
  | _ -> Error "numeric source claim field is not a declaration path"

let boolean = function
  | { pexp_desc = Pexp_construct ({ txt = Longident.Lident "true"; _ }, None);
      _ } ->
      Ok true
  | { pexp_desc = Pexp_construct ({ txt = Longident.Lident "false"; _ }, None);
      _ } ->
      Ok false
  | _ -> Error "numeric source claim field is not a boolean literal"

let rec string_list = function
  | { pexp_desc = Pexp_construct ({ txt = Longident.Lident "[]"; _ }, None);
      _ } ->
      Ok []
  | { pexp_desc =
        Pexp_construct
          ( { txt = Longident.Lident "::"; _ },
            Some
              { pexp_desc = Pexp_tuple [ (None, head); (None, tail) ]; _ } );
      _ } ->
      let* head = string head in
      let* tail = string_list tail in
      Ok (head :: tail)
  | _ -> Error "numeric source claim compatibility is not a string list"

let required fields name decode =
  match field fields name with
  | None -> Error ("numeric source claim lacks field " ^ name)
  | Some value -> decode value

let reject_unknown allowed fields =
  match List.find_opt (fun (name, _) -> not (List.mem name allowed)) fields with
  | None -> Ok ()
  | Some (name, _) -> Error ("unknown numeric source claim field " ^ name)

let parse_carrier attribute =
  let result =
    let* fields = payload_record attribute in
    let* () =
      reject_unknown [ "profile"; "representation"; "compatibility"; "base" ] fields
    in
    let* profile_reference = required fields "profile" string in
    let* base_path = match field fields "base" with
      | None -> Ok None | Some expression -> Result.map Option.some (identifier expression) in
    let* representation = required fields "representation" string in
    let* representation =
      match representation with
      | "immediate" -> Ok Immediate
      | "boxed" -> Ok Boxed
      | _ -> Error "numeric carrier source claim has unsupported representation"
    in
    let* compatibility =
      match field fields "compatibility" with
      | None -> Ok []
      | Some value -> string_list value
    in
    if
      String.equal profile_reference ""
      || List.exists (String.equal "") compatibility
    then Error "numeric carrier source claim contains an empty identity"
    else if
      List.length compatibility
      <> List.length (List.sort_uniq String.compare compatibility)
    then Error "numeric carrier source claim repeats a compatibility identity"
    else
      Ok
        { base_path; profile_reference; representation;
          compatibility = List.sort String.compare compatibility }
  in
  (match result with
  | Ok _ ->
      [%log.debug "validated numeric carrier source claim"
        ~stage:(Delator.Field.string "numeric-carrier-source-claim")
        ~authority:(Delator.Field.string "none-source-request")
        ~decision:(Delator.Field.string "accepted")]
  | Error (reason [@log_value.debug]) ->
      [%log.debug "rejected numeric carrier source claim"
        ~stage:(Delator.Field.string "numeric-carrier-source-claim")
        ~reason_class:(Delator.Field.string (reason [@log_value.debug]))
        ~decision:(Delator.Field.string "rejected")]);
  result
[@@delator.instrument] [@@delator.level debug]

let parse_role attribute =
  let result =
    let* fields = payload_record attribute in
    let* () =
      reject_unknown
        [ "carrier"; "role_schema"; "role"; "semantics"; "visibility";
          "reveal"; "inline" ]
        fields
    in
    let* carrier_path = required fields "carrier" identifier in
    let* role_schema = required fields "role_schema" string in
    let* role_identity = required fields "role" string in
    let* semantics_path = required fields "semantics" identifier in
    let* visibility = required fields "visibility" string in
    let* reveal = required fields "reveal" boolean in
    let* inline = required fields "inline" boolean in
    if
      List.exists (String.equal "")
        [ carrier_path; role_schema; role_identity; semantics_path; visibility ]
    then Error "numeric role source claim contains an empty identity"
    else if not (List.mem visibility [ "visible"; "opaque" ]) then
      Error "numeric role source claim has unknown visibility"
    else
      Ok
        { carrier_path; role_schema; role_identity; semantics_path; visibility;
          reveal; inline }
  in
  (match result with
  | Ok _ ->
      [%log.debug "validated numeric callable role source claim"
        ~stage:(Delator.Field.string "numeric-role-source-claim")
        ~authority:(Delator.Field.string "none-source-request")
        ~decision:(Delator.Field.string "accepted")]
  | Error (reason [@log_value.debug]) ->
      [%log.debug "rejected numeric callable role source claim"
        ~stage:(Delator.Field.string "numeric-role-source-claim")
        ~reason_class:(Delator.Field.string (reason [@log_value.debug]))
        ~decision:(Delator.Field.string "rejected")]);
  result
[@@delator.instrument] [@@delator.level debug]

let representation_name = function Immediate -> "immediate" | Boxed -> "boxed"

let carrier_material value =
  let fields = [value.profile_reference; representation_name value.representation;
    Numeric_receipt_private.list (List.sort String.compare value.compatibility)] in
  match value.base_path with
  | None -> Numeric_receipt_private.encode ~schema:"verocaml.numeric-carrier-source-claim.v1" fields
  | Some base -> Numeric_receipt_private.encode ~schema:"verocaml.numeric-carrier-source-claim.v2" (fields @ [base])

let role_material value =
  Numeric_receipt_private.encode ~schema:"verocaml.numeric-role-source-claim.v1"
    [ value.carrier_path; value.role_schema; value.role_identity;
      value.semantics_path; value.visibility; string_of_bool value.reveal;
      string_of_bool value.inline ]
