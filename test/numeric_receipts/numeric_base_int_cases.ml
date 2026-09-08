open Outcome_test_support

let require condition message = if not condition then failwith message
let ok = function Ok value -> value | Error reason -> failwith reason
let loaded = function Ok value -> value | Error diagnostic -> failwith diagnostic.Diagnostic.message

let cases ~write_file ~run_process =
  let case ~name ~nested ~check =
    Suite.case ~name
      ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
      (fun ~environment:_ ~workspace ->
        try
          let directory = Filename.dirname Sys.executable_name in
          let ppx = Unix.realpath (Filename.concat directory "../../ppx/vero_ppx.exe") in
          let compiler = Filename.concat Config.bindir "ocamlc" in
          let compile stem suffix =
            run_process compiler
              ["-w"; "-A"; "-c"; "-bin-annot"; "-I"; workspace;
               "-ppx"; ppx ^ " --keep-ghost"; "-o";
               stem ^ (if suffix = ".mli" then ".cmi" else ".cmo"); stem ^ suffix]
          in
          let wrap signature contents =
            if not nested then contents
            else "module Arithmetic " ^ (if signature then ": sig\n" else "= struct\n") ^ contents ^ "end\n"
          in
          let build unit_name ~annotated ~changed =
            let stem = Filename.concat workspace unit_name in
            if not changed then begin
              write_file (stem ^ ".mli")
                (wrap true ("type number = int\n" ^ (if annotated then "[@@verocaml.logical_sort]\n" else "")
                  ^ "val parse : string -> number\n" ^ (if annotated then "[@@verocaml.integer_literal]\n" else "")));
              compile stem ".mli"
            end;
            write_file (stem ^ ".ml")
              (wrap false ("type number = int\n" ^ (if annotated then "[@@verocaml.logical_sort]\n" else "")
                ^ "let parse text = int_of_string text" ^ (if changed then " + 0" else "") ^ "\n"
                ^ (if annotated then "[@@verocaml.integer_literal]\n" else "")));
            compile stem ".ml";
            loaded (Cmt_input.emit_retained_interface_authority
              ~cmt:(stem ^ ".cmt") ~cmi:(stem ^ ".cmi") ~cmti:(stem ^ ".cmti")
              ~output:(stem ^ ".vri") ~artifact_directories:[workspace] ());
            loaded (Cmt_input.load_with_interface
              ~cmt:(stem ^ ".cmt") ~cmi:(stem ^ ".cmi") ~cmti:(stem ^ ".cmti")
              ~artifact_directories:[workspace] ())
          in
          let provider = build "arbitrary_math" ~annotated:true ~changed:false in
          let sort = match provider.interface_logical_sorts with
            | [sort] -> sort | _ -> failwith "base provider did not expose its mathematical Int receipt" in
          let before = (Z3_bridge.counters ()).contexts_created in
          check build provider sort;
          require ((Z3_bridge.counters ()).contexts_created = before)
            "base Int artifact correlation started a solver context";
          Ok (Outcome.observation ~status:Outcome.Verified () |> Outcome.project)
        with Failure message -> Error (Failure.make Failure.Runner_internal message))
  in
  let correlated provider sort =
    ok (Numeric_base_int_binding_private.correlate ~provider ~logical_sort:sort) in
  let preserves_receipt _ provider sort =
    let reference = correlated provider sort in
    require (reference.base_int_full_key = Logical_sort_private.canonical_material sort
      && reference.validated_base_int_digest = Logical_sort_private.digest sort)
      "numeric base reference changed the existing mathematical Int receipt";
    let decoded = ok (Logical_sort_private.decode_canonical reference.base_int_full_key) in
    let repeated = correlated provider decoded in
    require (reference = repeated)
      "exact decoded base receipt changed its authenticated artifact binding"
  in
  [case ~name:"numeric-base-reference-preserves-existing-receipt" ~nested:false ~check:preserves_receipt;
   case ~name:"numeric-base-reference-nested-provider" ~nested:true ~check:preserves_receipt;
   case ~name:"numeric-base-reference-rejects-cross-provider-sort" ~nested:false
     ~check:(fun build provider sort ->
       let other = build "unrelated_numbers" ~annotated:true ~changed:false in
       require (Result.is_error (Numeric_base_int_binding_private.correlate ~provider:other ~logical_sort:sort))
         "an authentic unrelated provider accepted another provider's base receipt";
       let other_sort = match other.interface_logical_sorts with [value] -> value | _ -> failwith "missing other sort" in
       let first = correlated provider sort and second = correlated other other_sort in
       require (first.base_int_full_key <> second.base_int_full_key
         && first.authenticated_base_int_artifact_binding_key <> second.authenticated_base_int_artifact_binding_key)
         "distinct providers were conflated by base reference correlation");
   case ~name:"numeric-base-reference-rejects-detached-literal-identity" ~nested:false
     ~check:(fun _ provider sort ->
       let forged = ok (Logical_sort_private.create
         ~provider_origin:sort.provider_origin ~type_path:sort.type_path ~type_uid:sort.type_uid
         ~manifest_path:sort.manifest_path ~manifest_uid:sort.manifest_uid
         ~integer_literal_path:sort.integer_literal_path ~integer_literal_uid:(sort.integer_literal_uid ^ "-other")
         ~issuer:sort.issuer) in
       require (Result.is_error (Numeric_base_int_binding_private.correlate ~provider ~logical_sort:forged))
         "a decoded claim with a different literal UID acquired base authority");
   case ~name:"numeric-base-reference-never-infers-from-unannotated-int" ~nested:false
     ~check:(fun build _ sort ->
       let ordinary = build "ordinary_numbers" ~annotated:false ~changed:false in
       require (ordinary.interface_logical_sorts = []) "unannotated int became mathematical Int";
       require (Result.is_error (Numeric_base_int_binding_private.correlate ~provider:ordinary ~logical_sort:sort))
         "unannotated int supplied a base receipt");
   case ~name:"numeric-base-reference-distinguishes-implementation-artifacts" ~nested:false
     ~check:(fun build provider sort ->
       let first = correlated provider sort in
       let changed = build "arbitrary_math" ~annotated:true ~changed:true in
       let second = correlated changed sort in
       require (first.base_int_full_key = second.base_int_full_key)
         "changing only implementation changed the base interface receipt";
       require (first.authenticated_base_int_artifact_binding_key <> second.authenticated_base_int_artifact_binding_key)
         "different implementation artifacts shared numeric base binding authority")]
