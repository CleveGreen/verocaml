type representation = Immediate | Value

type t = {
  representation : representation;
  compiler_jkind_abi : string;
}

let reconstruct ~environment declaration =
  let result =
    try
      let environment =
        Envaux.env_of_only_summary ~allow_missing_modules:false
          environment
      in
      let declared_jkind = declaration.Typedtree.typ_type.Types.type_jkind in
      let jkind =
        match declaration.typ_manifest with
        | None -> declared_jkind
        | Some manifest ->
            Ctype.type_jkind_purely environment manifest.Typedtree.ctyp_type
      in
      let context = Ctype.mk_jkind_context_always_principal environment in
      match Jkind.get_layout jkind with
      | Some (Jkind_types.Layout.Const.Base Value) ->
          let nullability = Jkind.get_nullability ~context jkind in
          let externality = Jkind.get_externality_upper_bound ~context jkind in
          if nullability <> Jkind_axis.Nullability.Non_null then
            Error "numeric carriers require a non-null OCaml value layout"
          else
            let representation, externality_name =
              match externality with
              | Jkind_axis.Externality.External -> (Immediate, "external")
              | External64 -> (Value, "external64")
              | Internal -> (Value, "internal")
            in
            let compiler_jkind_abi =
              Numeric_receipt_private.encode
                ~schema:"verocaml.numeric-compiler-jkind-abi.v1"
                [ Config.cmi_magic_number; "value"; "non-null";
                  externality_name;
                  Format.asprintf "%a" Jkind.format_expanded declared_jkind;
                  Format.asprintf "%a" Jkind.format_expanded jkind ]
            in
            Ok { representation; compiler_jkind_abi }
      | None | Some _ ->
          Error "numeric carrier has an unsupported or unresolved compiler layout"
    with
    | Not_found | Env.Error _ | Envaux.Error _ ->
        Error "numeric carrier layout dependencies are unavailable"
  in
  (match result with
  | Ok (facts [@log_value.trace]) ->
      [%log.trace "reconstructed numeric carrier layout from compiler facts"
        ~stage:(Delator.Field.string "numeric-carrier-layout")
        ~representation:
          (Delator.Field.string
             (match (facts [@log_value.trace]).representation with
              | Immediate -> "immediate"
              | Value -> "value-layout"))
        ~authority:(Delator.Field.string "compiler-layout-only")
        ~decision:(Delator.Field.string "accepted")]
  | Error (reason [@log_value.debug]) ->
      [%log.debug "rejected unsupported numeric carrier layout"
        ~stage:(Delator.Field.string "numeric-carrier-layout")
        ~reason_class:(Delator.Field.string (reason [@log_value.debug]))
        ~decision:(Delator.Field.string "rejected")]);
  result
[@@delator.instrument] [@@delator.level trace]
