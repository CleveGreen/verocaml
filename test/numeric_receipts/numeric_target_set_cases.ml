open Outcome_test_support

let require condition message = if not condition then failwith message
let ok = function Ok value -> value | Error message -> failwith message
let loaded = function Ok value -> value | Error diagnostic -> failwith diagnostic.Diagnostic.message
let capability () = Build_target_profile_private.capability ()
let selector mode = ok (Numeric_required_target_set_private.select ~capability:(capability ()) ~modes:[mode])

let cases ~write_file ~run_process =
  let fixture workspace ~mathematical ~counterexample ~helper =
    let directory = Filename.dirname Sys.executable_name in
    let ppx = Unix.realpath (Filename.concat directory "../../ppx/vero_ppx.exe")
    and ghost = Unix.realpath (Filename.concat directory "../../runtime/.vero_ghost.objs/byte") in
    let stem = Filename.concat workspace "target_source" in
    let argument = if mathematical then "Arithmetic.t" else "int" in
    write_file (stem ^ ".mli")
      ("module Arithmetic : sig\ntype t = int [@@verocaml.logical_sort]\nval parse : string -> t [@@verocaml.integer_literal]\nend\n"
       ^ "val first : " ^ argument ^ " -> unit [@@verocaml.proof]\nval second : " ^ argument ^ " -> unit [@@verocaml.proof]\n");
    let proof name = "let " ^ name ^ " (x : " ^ argument ^ ") =\n"
      ^ "[%verocaml.ensures fun _ -> " ^ (if counterexample then "x + 1 = x" else "x = x")
      ^ "]; " ^ (if helper=`None then "()" else "helper x") ^ " [@@verocaml.proof]\n" in
    let helper_source=match helper with
      | `None -> ""
      | `Trusted -> "let helper (x : Arithmetic.t) = [%verocaml.ensures fun _ -> x = x]; () [@@verocaml.axiom]\n"
      | `Runtime -> "let runtime (_x : Arithmetic.t) : int = 0 [@@verocaml.spec]\nlet helper (x : Arithmetic.t) = assert (runtime x = 0) [@@verocaml.proof]\n"
      | `Lift -> "let lift (x : int) : Arithmetic.t = x + 0 [@@verocaml.spec]\nlet helper (_x : Arithmetic.t) = assert (lift 0 = 0) [@@verocaml.proof]\n" in
    write_file (stem ^ ".ml")
      ("[@@@verocaml.verify]\nmodule Arithmetic = struct\ntype t = int [@@verocaml.logical_sort]\n"
       ^ "let parse text = int_of_string text [@@verocaml.integer_literal]\nend\n"
       ^ helper_source ^ proof "first" ^ proof "second");
    List.iter (fun suffix -> run_process (Filename.concat Config.bindir "ocamlc")
      ["-w"; "-A"; "-c"; "-bin-annot"; "-I"; workspace; "-I"; ghost;
       "-ppx"; ppx ^ " --keep-ghost"; "-o"; stem ^ (if suffix = ".mli" then ".cmi" else ".cmo"); stem ^ suffix]) [".mli"; ".ml"];
    loaded (Cmt_input.emit_retained_interface_authority ~cmt:(stem ^ ".cmt") ~cmi:(stem ^ ".cmi") ~cmti:(stem ^ ".cmti")
      ~output:(stem ^ ".vri") ~artifact_directories:[workspace] ());
    loaded (Cmt_input.load_with_interface ~cmt:(stem ^ ".cmt") ~cmi:(stem ^ ".cmi") ~cmti:(stem ^ ".cmti") ~artifact_directories:[workspace] ()) in
  let run provider threads =
    let policy = match Solver_policy_private.create_default ~timeout_ms:5000 with
      | Ok policy -> policy | Error _ -> failwith "invalid target fixture solver policy" in
    match Verification_driver_private.run_with_policy_and_threads ~threads ~solver_policy:policy
      ~capture_numeric_obligations:true ~allow_imported_opens:true ~allow_public_parametric_signatures:true provider with
    | Ok report -> report
    | Error (Frontend_error diagnostic) -> failwith diagnostic.Diagnostic.message
    | Error (Validation_error error) -> failwith (Sst_validation.error_to_string error)
    | Error _ -> failwith "target fixture failed before producing original obligations" in
  let registry provider = match Numeric_ghost_registry_private.import_laws ~consumer:provider ~dependencies:[] [] with
    | Ok registry -> registry | Error error -> failwith (Numeric_ghost_registry_private.error_message error) in
  let original report name =
    match List.filter (fun original -> String.equal original.Numeric_original_obligation_private.obligation.function_ref.function_name name)
      (Verification_driver_private.original_obligations report) with
    | [original] -> original | _ -> failwith ("expected one actual original obligation for " ^ name) in
  let create registry report original mode coverage = Numeric_required_target_set_private.create
    ~capability:(capability ()) ~selector:(selector mode) ~registry ~report ~original ~coverage in
  let decode registry report original mode coverage encoded = Numeric_required_target_set_private.decode
    ~capability:(capability ()) ~selector:(selector mode) ~registry ~report ~original ~coverage encoded in
  let no_solver thunk =
    let before = (Z3_bridge.counters ()).contexts_created in
    let value = thunk () in
    require ((Z3_bridge.counters ()).contexts_created = before) "target-set admission created a solver context";
    value in
  let one ~name ?(mathematical = false) ?(counterexample = false) ?(helper=`None) check =
    Suite.case ~name ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
      (fun ~environment:_ ~workspace -> try
        let provider = fixture workspace ~mathematical ~counterexample ~helper in
        let report = run provider 1 in
        require (Verification_driver_private.status report = (if counterexample then Counterexample else Verified))
          "target fixture produced an unexpected source verification result";
        let registry = registry provider in
        check provider report registry (original report "first");
        Ok (Outcome.observation ~status:Outcome.Verified () |> Outcome.project)
      with Failure message -> Error (Failure.make Failure.Runner_internal message)) in
  let fields encoded = ok (Numeric_receipt_private.decode ~schema:"verocaml.required-target-set-transport.v1" ~field_count:4 encoded) in
  let transport fields = Numeric_receipt_private.encode ~schema:"verocaml.required-target-set-transport.v1" fields in
  let alter_children change = function
    | [parent; digest; children; edge] -> [parent; digest; Numeric_receipt_private.list (change (ok (Numeric_receipt_private.decode_list children))); edge]
    | _ -> failwith "unexpected target-set transport" in
  let alter_child_key change children = match children with
    | first :: rest ->
        let key, _digest = match ok (Numeric_receipt_private.decode ~schema:"target-child-envelope" ~field_count:2 first) with
          | [key; digest] -> key, digest | _ -> failwith "unexpected child envelope" in
        let key = change (ok (Numeric_receipt_private.decode ~schema:"verocaml.required-target-set-child.v1" ~field_count:4 key))
          |> Numeric_receipt_private.encode ~schema:"verocaml.required-target-set-child.v1" in
        Numeric_receipt_private.encode ~schema:"target-child-envelope"
          [key; Numeric_receipt_private.digest ~domain:"verocaml.required-target-set-child.v1" key] :: rest
    | [] -> failwith "expected target child" in
  let mutated name change = one ~name (fun _ report registry original -> no_solver (fun () ->
    let set = ok (create registry report original Concrete None) in
    let encoded = Numeric_required_target_set_private.encode set |> fields |> change |> transport in
    require (Result.is_error (decode registry report original Concrete None encoded)) "altered target set was accepted")) in
  [one ~name:"target-set-concrete-complete-profile-and-roundtrip" (fun _ report registry original -> no_solver (fun () ->
     let set = ok (create registry report original Concrete None) in
     let expected = ok (Build_target_profile_private.authenticate_instances (capability ())) |> List.sort Build_target_profile_private.compare_instance in
     require (List.map (fun c -> c.Numeric_required_target_set_private.target.full_key) set.children
       = List.map (fun t -> t.Build_target_profile_private.full_key) expected) "target set omitted or substituted profile membership";
     require (List.map (fun c -> c.Numeric_required_target_set_private.ordinal) set.children = List.mapi (fun index _ -> index) expected)
       "child ordinals were not canonical";
     require (List.sort Int.compare (List.map (fun c -> c.Numeric_required_target_set_private.target.target_claim.width) set.children) = [32;64]) "default profile lost a required target";
     let decoded = ok (decode registry report original Concrete None (Numeric_required_target_set_private.encode set)) in
     require (String.equal set.full_key decoded.full_key) "target-set roundtrip changed original identity"));
   one ~name:"original-identity-retains-entire-semantic-record" (fun _ _ _ original -> no_solver (fun () ->
     let obligation=original.Numeric_original_obligation_private.obligation in
     let changed_span={obligation.span with start_pos={obligation.span.start_pos with column=obligation.span.start_pos.column+1}} in
     let projection={Vir.symbol_id=917;source_name="observed";sort=Vir.Boolean;role=Vir.Local;span=obligation.span} in
     let variants=[obligation;
       {obligation with assumptions=Boolean_constant false :: obligation.assumptions};
       {obligation with required_preceding_safety=Boolean_constant false :: obligation.required_preceding_safety};
       {obligation with path_condition=Boolean_constant false :: obligation.path_condition};
       {obligation with goal=Boolean_not obligation.goal};
       {obligation with projection_symbols=projection :: obligation.projection_symbols};
       {obligation with span=changed_span};
       {obligation with obligation_index=obligation.obligation_index+1}] in
     let keys=List.map Vir_identity_private.obligation variants in
     require (List.length (List.sort_uniq String.compare keys)=List.length variants)
       "distinct assumptions, safety, path, goal, projection, source location or ordinal shared identity"));
   one ~name:"target-membership-keeps-authentic-targets-in-colliding-presentation-buckets" (fun _ report registry original -> no_solver (fun () ->
     let targets=ok (Build_target_profile_private.authenticate_instances (capability ())) in
     let a,b=match targets with [a;b] -> a,b | _ -> failwith "expected unresolved two-target profile" in
     let canonical values=Numeric_receipt_private.canonical_members ~full_key:(fun (target,_) -> target.Build_target_profile_private.full_key) values in
     let keys values=List.map (fun (target,_) -> target.Build_target_profile_private.full_key) (canonical values) in
     let colliding=[a,"same-display-bucket";b,"same-display-bucket";a,"same-display-bucket"] in
     let expected=keys colliding in
     require (List.length expected=2) "presentation collision merged unequal authentic targets";
     require (expected=keys (List.rev colliding) && expected=keys [b,"another-label";a,"changed-label";b,"duplicate-label"])
       "presentation labels or discovery order changed canonical target membership";
     List.iter (fun (target,_) -> ignore (ok (Build_target_profile_private.decode_instance (capability ()) target.Build_target_profile_private.full_key))) (canonical colliding);
     let set=ok (create registry report original Concrete None) in
     require (set.target_full_keys=expected
       && List.map (fun child -> child.Numeric_required_target_set_private.target.full_key) set.children=expected)
       "the production handoff did not use full-key membership"));
   one ~name:"target-set-distinguishes-local-ordinals-across-functions" (fun _ report registry first -> no_solver (fun () ->
     let second = original report "second" in
     require (first.obligation.obligation_index = second.obligation.obligation_index) "fixture did not exercise a reused local ordinal";
     let a = ok (create registry report first Concrete None) and b = ok (create registry report second Concrete None) in
     require (a.full_key <> b.full_key && first.full_key <> second.full_key) "different functions shared an original target set";
     require (Result.is_error (decode registry report second Concrete None (Numeric_required_target_set_private.encode a))) "another original accepted this target set"));
   one ~name:"target-set-worker-count-stable-full-keys" (fun provider report registry _ ->
     let keys report = Verification_driver_private.original_obligations report |> List.map (fun original ->
       let set = no_solver (fun () -> ok (create registry report original Concrete None)) in original.full_key, Numeric_required_target_set_private.encode set)
       |> List.sort compare in
     let before = keys report in let after = keys (run provider 2) in
     require (before = after) "original or target-set full keys depend on worker count");
   one ~name:"target-set-requires-one-explicit-selector" (fun _ _ _ _ -> no_solver (fun () ->
     List.iter (fun modes -> require (Result.is_error (Numeric_required_target_set_private.select ~capability:(capability ()) ~modes))
       "missing or conflicting coverage selector was accepted") [ []; [Concrete;Abstract]; [Concrete;Concrete] ]));
   one ~name:"target-set-mathematical-abstract-identity-coverage" ~mathematical:true (fun provider report registry original -> no_solver (fun () ->
     let coverage = ok (Numeric_abstract_coverage_private.complete ~capability:(capability ()) ~consumer:provider ~report original) in
     let set = ok (create registry report original Abstract (Some coverage)) in
     require (set.children = [] && Option.is_some set.abstract_edge) "abstract coverage mixed concrete execution edges";
     ignore (ok (decode registry report original Abstract (Some coverage) (Numeric_required_target_set_private.encode set)));
     require (Result.is_error (create registry report original Concrete (Some coverage))) "concrete coverage carried abstract proof"));
   one ~name:"target-set-abstract-has-no-implicit-concrete-fallback" (fun _ report registry original -> no_solver (fun () ->
     require (Result.is_error (create registry report original Abstract None)) "abstract coverage fell back without proof"));
   one ~name:"target-set-runtime-int-is-not-all-target-proof" (fun provider report _ original -> no_solver (fun () ->
     require (Result.is_error (Numeric_abstract_coverage_private.complete ~capability:(capability ()) ~consumer:provider ~report original)) "runtime proof acquired identity coverage"));
   one ~name:"target-set-abstract-pure-mathematical-trust-is-retained" ~mathematical:true ~helper:`Trusted (fun provider report registry original -> no_solver (fun () ->
     let coverage=ok (Numeric_abstract_coverage_private.complete ~capability:(capability ()) ~consumer:provider ~report original) in
     require (coverage.trusted_dependencies<>[]) "mathematical axiom dependency disappeared from coverage TCB";
     ignore (ok (create registry report original Abstract (Some coverage)))));
   one ~name:"target-set-abstract-transitive-runtime-helper-rejects" ~mathematical:true ~helper:`Runtime (fun provider report _ original -> no_solver (fun () ->
     require (Result.is_error (Numeric_abstract_coverage_private.complete ~capability:(capability ()) ~consumer:provider ~report original))
       "a mathematical root hid a target-dependent transitive helper"));
   one ~name:"target-set-abstract-transitive-lift-helper-rejects" ~mathematical:true ~helper:`Lift (fun provider report _ original -> no_solver (fun () ->
     require (Result.is_error (Numeric_abstract_coverage_private.complete ~capability:(capability ()) ~consumer:provider ~report original))
       "a mathematical root hid a transitive runtime lift"));
   one ~name:"target-set-abstract-coverage-belongs-to-one-original" ~mathematical:true (fun provider report registry first -> no_solver (fun () ->
     let second=original report "second" in
     let coverage=ok (Numeric_abstract_coverage_private.complete ~capability:(capability ()) ~consumer:provider ~report first) in
     require (Result.is_error (create registry report second Abstract (Some coverage))) "a theorem for another original supplied coverage";
     let second_coverage=ok (Numeric_abstract_coverage_private.complete ~capability:(capability ()) ~consumer:provider ~report second) in
     let set=ok (create registry report first Abstract (Some coverage)) in
     require (Result.is_error (decode registry report second Abstract (Some second_coverage) (Numeric_required_target_set_private.encode set)))
       "an abstract edge moved between original obligations"));
   one ~name:"target-set-counterexample-cannot-cover-profile" ~mathematical:true ~counterexample:true (fun provider report _ original -> no_solver (fun () ->
     require (Result.is_error (Numeric_abstract_coverage_private.complete ~capability:(capability ()) ~consumer:provider ~report original)) "failed proof acquired abstract coverage"));
   mutated "target-set-rejects-missing-child" (alter_children List.tl);
   mutated "target-set-rejects-duplicate-child" (alter_children (fun xs -> List.hd xs :: xs));
   mutated "target-set-rejects-reordered-children" (alter_children List.rev);
   mutated "target-set-rejects-wrong-ordinal-with-consistent-digest" (alter_children (alter_child_key (function [p;o;t;_] -> [p;o;t;"17"] | _ -> failwith "child shape")));
   mutated "target-set-rejects-wrong-child-parent-with-consistent-digest" (alter_children (alter_child_key (function [p;o;t;n] -> [p ^ "different";o;t;n] | _ -> failwith "child shape")));
   mutated "target-set-rejects-child-original-substitution" (alter_children (alter_child_key (function [p;o;t;n] -> [p;o ^ "different";t;n] | _ -> failwith "child shape")));
   mutated "target-set-rejects-child-target-substitution" (alter_children (alter_child_key (function [p;o;t;n] -> [p;o;t ^ "different";n] | _ -> failwith "child shape")));
   mutated "target-set-rejects-parent-digest-substitution" (function [p;d;c;e] -> [p;d ^ "different";c;e] | _ -> failwith "parent shape");
   mutated "target-set-rejects-concrete-abstract-mixed-transport" (function [p;d;c;_] -> [p;d;c;Numeric_receipt_private.list ["abstract-edge"]] | _ -> failwith "parent shape")]
