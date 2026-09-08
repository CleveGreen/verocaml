open Outcome_test_support
let require condition message = if not condition then failwith message
let ok = function Ok value -> value | Error message -> failwith message
let loaded = function Ok value -> value | Error diagnostic -> failwith diagnostic.Diagnostic.message
let cases ~write_file ~run_process =
  [Suite.case ~name:"original-obligation-binds-rebuilt-import-with-unchanged-interface"
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
    (fun ~environment:_ ~workspace -> try
      let directory=Filename.dirname Sys.executable_name in
      let ppx=Unix.realpath (Filename.concat directory "../../ppx/vero_ppx.exe")
      and ghost=Unix.realpath (Filename.concat directory "../../runtime/.vero_ghost.objs/byte") in
      let stem name=Filename.concat workspace name in
      let compile name suffix = run_process (Filename.concat Config.bindir "ocamlc")
        ["-w";"-A";"-c";"-bin-annot";"-I";workspace;"-I";ghost;"-ppx";ppx ^ " --keep-ghost";
         "-o";stem name ^ (if suffix=".mli" then ".cmi" else ".cmo");stem name ^ suffix] in
      write_file (stem "origin" ^ ".mli") "val actual : int -> int\n";
      compile "origin" ".mli";
      let provider changed =
        write_file (stem "origin" ^ ".ml")
          ("[@@@verocaml.verify]\nlet actual (x : int) = [%verocaml.requires x >= 0]; [%verocaml.ensures fun result -> result = x]; " ^ (if changed then "x + 0" else "x") ^ "\n");
        compile "origin" ".ml";
        loaded (Cmt_input.emit_retained_interface_authority ~cmt:(stem "origin" ^ ".cmt") ~cmi:(stem "origin" ^ ".cmi")
          ~cmti:(stem "origin" ^ ".cmti") ~output:(stem "origin" ^ ".vri") ~artifact_directories:[workspace] ());
        loaded (Cmt_input.load_with_interface ~cmt:(stem "origin" ^ ".cmt") ~cmi:(stem "origin" ^ ".cmi")
          ~cmti:(stem "origin" ^ ".cmti") ~artifact_directories:[workspace] ()) in
      let first=provider false in
      write_file (stem "consumer" ^ ".ml") "[@@@verocaml.verify]\nlet use (x : int) = [%verocaml.requires x >= 0]; Origin.actual x\n";
      compile "consumer" ".ml";
      let consumer=loaded (Cmt_input.load (stem "consumer" ^ ".cmt")) in
      let policy=match Solver_policy_private.create_default ~timeout_ms:5000 with Ok policy -> policy | Error _ -> failwith "invalid original obligation test policy" in
      let capture provider =
        let environment, consumer = match Interface_specification_loaded_private.authenticate ~external_targets:[] ~solver_policy:policy
          ~dependencies:[provider] ~consumer with Ok value -> value | Error error -> failwith (Interface_specification_loaded_private.error_message error) in
        let imported=match Interface_specification_environment_private.imported_environment_authenticated environment with
          | Ok value -> value | Error error -> failwith (Interface_specification_environment_private.error_to_string error) in
        let report=match Verification_driver_private.run_with_policy ~solver_policy:policy ~imported
          ~capture_numeric_obligations:true ~allow_imported_opens:true consumer with
          | Ok report -> report | Error _ -> failwith "import consumer failed before original capture" in
        require (Verification_driver_private.status report=Verified) "import consumer proof failed";
        let originals=Verification_driver_private.original_obligations report in
        require (originals<>[]) "fixture did not produce a real call obligation";
        report, originals in
      let _report, original=capture first in
      let second=provider true in
      require (first.interface_digest=second.interface_digest) "provider interface changed in body-only rebuild";
      require (first.raw_artifact_receipt<>second.raw_artifact_receipt) "provider rebuild did not alter original artifact";
      let report, rebuilt=capture second in
      let repeated_report, repeated=capture second in
      let keys=List.map (fun original -> original.Numeric_original_obligation_private.full_key) in
      require (keys original<>keys rebuilt) "original obligation lost the exact imported implementation";
      require (keys rebuilt=keys repeated) "same imported implementation produced unstable original keys";
      let _ = repeated_report in
      let stale=match Numeric_ghost_registry_private.import_laws ~consumer ~dependencies:[first] [] with
        | Ok registry -> registry | Error error -> failwith (Numeric_ghost_registry_private.error_message error) in
      let capability=Build_target_profile_private.capability () in
      let selector=ok (Numeric_required_target_set_private.select ~capability ~modes:[Concrete]) in
      let before=(Z3_bridge.counters ()).contexts_created in
      List.iter (fun original -> require (Result.is_error (Numeric_required_target_set_private.create
        ~capability ~selector ~registry:stale ~report ~original ~coverage:None)) "stale numeric registry accepted rebuilt original") rebuilt;
      require ((Z3_bridge.counters ()).contexts_created=before) "stale provenance rejection created a solver";
      Ok (Outcome.observation ~status:Outcome.Verified () |> Outcome.project)
    with Failure message -> Error (Failure.make Failure.Runner_internal message))]
