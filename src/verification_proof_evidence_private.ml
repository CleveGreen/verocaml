type insertion = {
  inserted : Broadcast_vc_private.inserted;
  theorem : Sst.function_definition option;
}

type obligation = {
  materialized : Vir.obligation;
  base : Vir.obligation option;
  broadcasts : insertion list option;
}

type function_evidence = {
  definition : Sst.function_definition option;
  execution : Vir.function_execution;
  obligations : obligation list;
}

type t = { program : Sst.program; functions : function_evidence list }

module Function_index = Map.Make (Int)

let functions evidence = evidence.functions
let matches_program evidence program = evidence.program == program

module For_pipeline = struct
  let capture ~program:(program [@delator.skip]) (vir [@delator.skip]) =
    let registered = Broadcast_scope_private.registered program in
    let definitions =
      List.fold_left (fun index definition ->
          Function_index.add definition.Sst.function_id.function_index definition index)
        Function_index.empty program.Sst.functions
    in
    let insertion inserted =
      let theorem =
        match Broadcast_declaration_private.find ~program
          ~id:inserted.Broadcast_vc_private.broadcast_id with
        | Some theorem ->
            let definition = Broadcast_declaration_private.definition theorem in
            let trusted = Broadcast_declaration_private.kind theorem = Broadcast_declaration_private.Trusted_axiom in
            if definition.Sst.function_id = inserted.theorem_function_id
              && trusted = inserted.trusted
            then Some definition else None
        | None -> None
      in
      [%log.trace "captured inserted proof dependency"
        ~theorem:(Delator.Field.string inserted.broadcast_id)
        ~trusted:(Delator.Field.bool inserted.trusted)
        ~resolved:(Delator.Field.bool (Option.is_some theorem))];
      { inserted; theorem }
    in
    let obligation materialized =
      let report = Broadcast_vc_private.report materialized in
      let base, broadcasts =
        match report with
        | Some report ->
            Broadcast_vc_private.pre_materialization_obligation materialized,
            Some (List.map insertion report.inserted)
        | None when not registered -> Some materialized, Some []
        | None -> None, None
      in
      [%log.trace "captured verification obligation proof context"
        ~function_name:(Delator.Field.string materialized.Vir.function_ref.function_name)
        ~obligation_index:(Delator.Field.int materialized.obligation_index)
        ~broadcast_scope_registered:(Delator.Field.bool registered)
        ~report_available:(Delator.Field.bool (Option.is_some broadcasts))
        ~base_available:(Delator.Field.bool (Option.is_some base))];
      { materialized; base; broadcasts }
    in
    let functions =
      List.map (fun execution ->
          let definition =
            match Function_index.find_opt execution.Vir.function_ref.function_index definitions with
            | Some definition when String.equal definition.Sst.function_id.function_name execution.function_ref.function_name -> Some definition
            | _ -> None
          in
          { definition; execution; obligations = List.map obligation execution.obligations })
        vir.Vir.functions
    in
    [%log.debug "captured coordinator proof evidence before session teardown"
      ~function_count:(Delator.Field.int (List.length functions))
      ~broadcast_scope_registered:(Delator.Field.bool registered)];
    { program; functions }
  [@@delator.instrument] [@@delator.level debug]
end
