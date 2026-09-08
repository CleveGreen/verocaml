open Outcome_test_support
let require condition message = if not condition then failwith message
let ok = function Ok value -> value | Error message -> failwith message
let loaded = function Ok value -> value | Error diagnostic -> failwith diagnostic.Diagnostic.message
let cases ~write_file ~run_process =
  let case ~name ?(trusted=false) ?(signed=false) ?(unrelated=false) ?(hidden_helper=false)
      ?(trusted_base=false) ?consumer_contexts ?(runtime=`Absent) ?(second_extension=false) ?(changed=`None) check =
    Suite.case ~name ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
      (fun ~environment:_ ~workspace -> try
        let directory=Filename.dirname Sys.executable_name in
        let ppx=Unix.realpath (Filename.concat directory "../../ppx/vero_ppx.exe")
        and ghost=Unix.realpath (Filename.concat directory "../../runtime/.vero_ghost.objs/byte") in
        let stem name=Filename.concat workspace name in
        let compile name suffix=run_process (Filename.concat Config.bindir "ocamlc")
          ["-w";"-A";"-c";"-bin-annot";"-I";workspace;"-I";ghost;"-ppx";ppx ^ " --keep-ghost";
           "-o";stem name ^ (if suffix=".mli" then ".cmi" else ".cmo");stem name ^ suffix] in
        let emit_load name =
          loaded (Cmt_input.emit_retained_interface_authority ~cmt:(stem name ^ ".cmt") ~cmi:(stem name ^ ".cmi") ~cmti:(stem name ^ ".cmti")
            ~output:(stem name ^ ".vri") ~artifact_directories:[workspace] ());
          loaded (Cmt_input.load_with_interface ~cmt:(stem name ^ ".cmt") ~cmi:(stem name ^ ".cmi") ~cmti:(stem name ^ ".cmti") ~artifact_directories:[workspace] ()) in
        let marker carrier role law="[@@verocaml.numeric_role {carrier = " ^ carrier ^ "; role_schema = \"numeric-role.v1\"; role = \"" ^ role
          ^ "\"; semantics = " ^ law ^ "; visibility = \"visible\"; reveal = true; inline = true}]\n" in
        write_file (stem "foundation" ^ ".mli")
          ("module Arithmetic : sig\ntype t = int [@@verocaml.logical_sort]\nval parse : string -> t [@@verocaml.integer_literal]\nend\n"
           ^ "type carrier = int [@@verocaml.numeric_carrier {base = Arithmetic.t; profile = \"request\"; representation = \"immediate\"; compatibility = []}]\n"
           ^ "val range : carrier -> unit [@@verocaml.proof]\nval u : carrier -> Arithmetic.t [@@verocaml.spec]\n" ^ marker "carrier" "unsigned-view" "range"
           ^ "val signed_law : carrier -> unit [@@verocaml.proof]\nval s : carrier -> Arithmetic.t [@@verocaml.spec]\n" ^ marker "carrier" "signed-view" "signed_law"
           ^ "val base_law : carrier -> unit [@@verocaml.proof]\nval base_op : carrier -> carrier [@@verocaml.spec]\n" ^ marker "carrier" "operation" "base_law"
           ^ "val helper : carrier -> Arithmetic.t [@@verocaml.spec]\n"
           ^ (if runtime=`Absent then "" else "val runtime_view : carrier -> int\n"
             ^ marker "carrier" (if runtime=`Trusted then "trusted-runtime-view" else "runtime-view") "u"));
        compile "foundation" ".mli";
        let build_base changed =
          let proof = if trusted_base then "axiom" else "proof" in
          write_file (stem "foundation" ^ ".ml")
            ("[@@@verocaml.verify]\nmodule Arithmetic = struct\ntype t = int [@@verocaml.logical_sort]\nlet parse text = int_of_string text [@@verocaml.integer_literal]\nend\ntype carrier = int\n"
             ^ "let u (_x : carrier) : Arithmetic.t = " ^ (if changed then "let zero = 0 + 0 in zero" else "0 + 0") ^ " [@@verocaml.spec]\n"
             ^ "let range (x : carrier) = [%verocaml.ensures fun _ -> 0 <= u x && u x < 65536 * 65536]; () [@@verocaml." ^ proof ^ "]\n"
             ^ "let s (_x : carrier) : Arithmetic.t = 0 + 0 [@@verocaml.spec]\n"
             ^ "let signed_law (x : carrier) = [%verocaml.ensures fun _ -> s x = (if u x < 32768 * 65536 then u x else u x - 65536 * 65536)]; () [@@verocaml." ^ proof ^ "]\n"
             ^ "let base_op (x : carrier) = x [@@verocaml.spec]\nlet base_law (x : carrier) = [%verocaml.ensures fun _ -> u (base_op x) = u x]; () [@@verocaml." ^ proof ^ "]\n"
             ^ "let helper (_x : carrier) : Arithmetic.t = 0 + 0 [@@verocaml.spec]\n"
             ^ (match runtime with
               | `Absent -> ""
               | `Unknown -> "let runtime_view (_x : carrier) = 0\n"
               | `Checked -> "let runtime_view (x : carrier) = [%verocaml.ensures fun result -> result = u x]; 0\n"
               | `Trusted -> "let runtime_view (x : carrier) = [%verocaml.ensures fun result -> result = u x]; 1 [@@verocaml.external_body]\n"));
          compile "foundation" ".ml"; emit_load "foundation" in
        let base=build_base false in
        write_file (stem "extension" ^ ".mli")
          ("val echo_law : Foundation.carrier -> unit [@@verocaml.proof]\nval echo : Foundation.carrier -> Foundation.carrier [@@verocaml.spec]\n"
           ^ marker "Foundation.carrier" "operation" "echo_law");
        compile "extension" ".mli";
        let build_extension changed =
          let view=if signed then "Foundation.s" else "Foundation.u" in
          write_file (stem "extension" ^ ".ml")
            ("[@@@verocaml.verify]\nlet echo (x : Foundation.carrier) : Foundation.carrier = " ^ (if changed then "let result = x in result" else "x") ^ " [@@verocaml.spec]\n"
             ^ (if hidden_helper then "let hidden (x : Foundation.carrier) = assert (Foundation.helper x >= 0) [@@verocaml.proof]\n" else "")
             ^ "let echo_law (x : Foundation.carrier) = [%verocaml.ensures fun _ -> " ^ view ^ " (echo x) = " ^ view ^ " x"
             ^ (if unrelated then " && Foundation.helper x >= 0" else "") ^ "]; " ^ (if hidden_helper then "hidden x" else "()") ^ " [@@verocaml." ^ (if trusted then "axiom" else "proof") ^ "]\n");
          compile "extension" ".ml"; emit_load "extension" in
        let extension=build_extension false in
        let additional=if not second_extension then [] else (
          write_file (stem "companion" ^ ".mli")
            ("val echo_law : Foundation.carrier -> unit [@@verocaml.proof]\nval echo : Foundation.carrier -> Foundation.carrier [@@verocaml.spec]\n"
             ^ marker "Foundation.carrier" "operation" "echo_law");
          write_file (stem "companion" ^ ".ml")
            ("[@@@verocaml.verify]\nlet echo (x : Foundation.carrier) : Foundation.carrier = x [@@verocaml.spec]\n"
             ^ "let echo_law (x : Foundation.carrier) = [%verocaml.ensures fun _ -> Foundation.u (echo x) = Foundation.u x]; () [@@verocaml.proof]\n");
          compile "companion" ".mli"; compile "companion" ".ml"; [emit_load "companion"]) in
        write_file (stem "consumer" ^ ".ml")
          ("[@@@verocaml.verify]\nlet use (x : int) = [%verocaml.ensures fun _ -> Foundation.u (Extension.echo x) = Foundation.u x && Foundation.u (Foundation.base_op x) = Foundation.u x]; Extension.echo_law x; Foundation.base_law x [@@verocaml.proof]\n"
           ^ (if runtime=`Absent then "" else "let execute (x : int) = Foundation.runtime_view x\n")
           ^ (if second_extension then "let companion (x : int) = [%verocaml.ensures fun _ -> Foundation.u (Companion.echo x) = Foundation.u x]; Companion.echo_law x [@@verocaml.proof]\n" else ""));
        compile "consumer" ".ml";
        let consumer=loaded (Cmt_input.load (stem "consumer" ^ ".cmt")) in
        let policy=match Solver_policy_private.create_default ~timeout_ms:5000 with Ok policy -> policy | Error _ -> failwith "invalid extension test policy" in
        let verify dependencies = Interface_specification_loaded_private.verify ~threads:1 ~solver_policy:policy ~external_specifications:None
          ~external_targets:[] ~consumer ~dependencies:(dependencies @ additional) in
        let before=(Z3_bridge.counters ()).contexts_created in
        let first=verify [base;extension] in
        Option.iter (fun expected -> require (((Z3_bridge.counters ()).contexts_created>before)=expected)
          "dependency conflict checking ran consumer queries or compatible control failed to exercise a query") consumer_contexts;
        let rebuilt = match changed with
          | `None -> None
          | `Base -> Some (build_base true, extension)
          | `Extension -> Some (base, build_extension true) in
        check base extension consumer verify first rebuilt;
        Ok (Outcome.observation ~status:Outcome.Verified () |> Outcome.project)
      with Failure message -> Error (Failure.make Failure.Runner_internal message)) in
  let registry = function
    | Ok result -> (match Interface_specification_loaded_private.numeric_registry result with Some registry -> registry | None -> failwith "automatic numeric registry was not collected")
    | Error error -> failwith (Interface_specification_loaded_private.error_message error) in
  let extension_laws registry = Numeric_ghost_registry_private.laws registry |> List.filter (fun (law : Numeric_ghost_law_private.t) -> law.issuer_unit="Extension") in
  let compatible result = let registry=registry result in
    require (extension_laws registry<>[]) "extension fixture had no admitted operation"; registry in
  let conflict result = match result with
    | Error error -> require (String.starts_with ~prefix:"Multiple numeric laws define operation" (Interface_specification_loaded_private.error_message error))
        "incompatible extension failed outside numeric conflict admission"
    | Ok _ -> failwith "operations over incompatible immutable views coexisted" in
  let runtime_case name runtime = case ~name ~runtime (fun base extension _ _ result _ ->
    let registry=compatible result in
    let result=match result with Ok result -> result | Error _ -> assert false in
    let consumer=Numeric_ghost_registry_private.consumer registry in
    let policy=match Solver_policy_private.create_default ~timeout_ms:5000 with Ok policy -> policy | Error _ -> failwith "invalid imported runtime policy" in
    let environment,_=match Interface_specification_loaded_private.authenticate ~solver_policy:policy ~external_targets:[] ~dependencies:[base;extension] ~consumer with
      | Ok environment -> environment | Error error -> failwith (Interface_specification_loaded_private.error_message error) in
    let imported=match Interface_specification_environment_private.imported_environment_authenticated environment with
      | Ok imported -> imported | Error error -> failwith (Interface_specification_environment_private.error_to_string error) in
    let report=Interface_specification_loaded_private.driver result in
    let completion=Option.get (Verification_driver_private.verified_completion report)
    and validated=Verification_driver_private.validated report in
    let caller=List.find (fun definition -> definition.Sst.function_id.function_name="execute") (Sst_validation.program validated).functions in
    let occurrence=match caller.body with Checked_exec {body;_} -> body.expression | _ -> failwith "missing imported Exec call" in
    let descriptor=List.find (fun (descriptor : Numeric_admitted_descriptor_private.t) -> descriptor.law.meaning=Unsigned_range)
      (Numeric_ghost_registry_private.descriptors registry) in
    let before=(Z3_bridge.counters ()).contexts_created in
    let authorize occurrence=Numeric_ghost_registry_private.authorize_occurrence registry ~completion ~implementation:consumer ~imported ~validated ~descriptor ~caller ~occurrence in
    (match runtime,authorize occurrence with
     | `Unknown, Error _ -> ()
     | (`Checked | `Trusted), Ok (Runtime_equivalence refinement) ->
         require ((refinement.authority=Explicit_external_body)=(runtime=`Trusted)) "imported runtime trust was relabelled as proof"
     | _ -> failwith "imported runtime gate lost the exact public callable or authority axis");
    require (Result.is_error (authorize {occurrence with Sst.span=occurrence.span})) "copied imported runtime AST gained authority";
    require ((Z3_bridge.counters ()).contexts_created=before) "runtime authorization created solver contexts") in
  [case ~name:"numeric-cross-provider-operation-proof-and-complete-targets" (fun base extension consumer _ result _ ->
      let imported=registry result in
      let laws=extension_laws imported in
      require (List.length laws=2) "a genuine operation extension was not admitted on both targets";
      let law=List.hd laws in
      require (law.authority=Checked_proof && law.carrier.carrier_claim.owner.owner_unit="Foundation") "issuer and carrier origins were conflated";
      require (law.issuer_artifact_full_key<>Imported_callable.artifact_full_key base) "extension proof was attributed to base artifact";
      require (List.length (List.filter (fun (law : Numeric_ghost_law_private.t) -> law.meaning=Relation Operation) (Numeric_ghost_registry_private.laws imported))=4)
        "disjoint compatible operations did not coexist";
      let result=match result with Ok result -> result | Error _ -> assert false in
      let originals=Verification_driver_private.original_obligations (Interface_specification_loaded_private.driver result)
      and sets=Interface_specification_loaded_private.numeric_target_sets result in
      require (originals<>[] && List.length originals=List.length sets) "automatic handoff omitted original obligations";
      require (List.for_all (fun (set : Numeric_required_target_set_private.t) -> List.length set.children=2) sets) "automatic handoff omitted a profile target";
      let policy=match Solver_policy_private.create_default ~timeout_ms:5000 with Ok policy -> policy | Error _ -> failwith "invalid occurrence policy" in
      let environment,_=match Interface_specification_loaded_private.authenticate ~solver_policy:policy ~external_targets:[] ~dependencies:[base;extension] ~consumer with
        | Ok environment -> environment | Error error -> failwith (Interface_specification_loaded_private.error_message error) in
      let imported_calls=match Interface_specification_environment_private.imported_environment_authenticated environment with
        | Ok environment -> environment | Error error -> failwith (Interface_specification_environment_private.error_to_string error) in
      let driver=Interface_specification_loaded_private.driver result in
      let completion=Option.get (Verification_driver_private.verified_completion driver)
      and validated=Verification_driver_private.validated driver in
      let consumer=Numeric_ghost_registry_private.consumer imported in
      let program=Sst_validation.program validated in
      let caller=List.find (fun definition -> definition.Sst.function_id.function_name="use") program.functions in
      let rec flatten acc expression=List.fold_left flatten (expression::acc) (Sst_callback_private.expression_children expression) in
      let expressions=Numeric_proof_dependencies_private.definition_expressions caller |> List.fold_left flatten [] in
      let registration=ok (Imported_callable.seal_calls imported_calls ~implementation:consumer ~program) in
      Fun.protect ~finally:(fun () -> Imported_callable.invalidate_registration registration) (fun () ->
        List.iter (fun (descriptor : Numeric_admitted_descriptor_private.t) ->
          if descriptor.law.meaning=Relation Operation then (
            let occurrence=List.find (fun expression -> match Imported_callable.find_call registration expression with
              | Some call -> let summary=Imported_callable.call_summary call in summary.binding_uid=descriptor.law.role.callable_uid && summary.provider_unit=descriptor.law.issuer_unit
              | None -> false) expressions in
            let before=(Z3_bridge.counters ()).contexts_created in
            require (Result.is_ok (Numeric_ghost_registry_private.authorize_occurrence imported ~completion ~implementation:consumer
              ~imported:imported_calls ~validated ~descriptor ~caller ~occurrence)) "an exact imported operation occurrence lost its issuer identity";
            require ((Z3_bridge.counters ()).contexts_created=before) "imported occurrence authorization created solver context"
          )) (Numeric_ghost_registry_private.descriptors imported)));
   case ~name:"numeric-cross-provider-operation-axiom-keeps-issuer-trust" ~trusted:true (fun _ _ _ _ result _ ->
      let laws=extension_laws (registry result) in require (laws<>[]) "trusted extension was not admitted";
      require (List.for_all (fun (law : Numeric_ghost_law_private.t) -> law.authority=Explicit_axiom
        && List.exists (fun (dependency : Numeric_ghost_law_private.dependency) -> dependency.trusted) law.dependencies) laws) "extension trust was lost");
   case ~name:"numeric-cross-provider-operation-order-is-canonical" (fun base extension _ verify result _ ->
      require (Numeric_ghost_registry_private.equal (compatible result) (compatible (verify [extension;base]))) "extension registry depends on discovery order");
   case ~name:"numeric-two-independent-operation-extensions-coexist" ~second_extension:true (fun base extension _ verify result _ ->
      let first=compatible result in
      let operations=List.filter (fun (law : Numeric_ghost_law_private.t) -> law.meaning=Relation Operation) (Numeric_ghost_registry_private.laws first) in
      require (List.sort String.compare (List.map (fun (law : Numeric_ghost_law_private.t) -> law.issuer_unit) operations)=["Companion";"Companion";"Extension";"Extension";"Foundation";"Foundation"])
        "independent extension issuers collapsed or lost carrier authority";
      require (Numeric_ghost_registry_private.equal first (compatible (verify [extension;base]))) "multiple extensions depend on discovery order");
   case ~name:"numeric-cross-provider-operation-incompatible-view-rejects" ~signed:true (fun _ _ _ _ result _ ->
      conflict result);
   case ~name:"numeric-cross-provider-conflict-rejects-before-consumer-context" ~signed:true ~trusted:true ~trusted_base:true ~consumer_contexts:false
     (fun _ _ _ _ result _ -> conflict result);
   case ~name:"numeric-cross-provider-compatible-control-creates-consumer-context" ~trusted:true ~trusted_base:true ~consumer_contexts:true
     (fun _ _ _ _ result _ -> ignore (compatible result));
   case ~name:"numeric-cross-provider-operation-unrelated-import-is-not-proof-leaf" ~unrelated:true (fun _ _ _ _ result _ ->
      require (extension_laws (registry result)=[]) "an unrelated imported helper bypassed numeric dependency admission");
   case ~name:"numeric-cross-provider-hidden-transitive-helper-is-not-proof-leaf" ~hidden_helper:true (fun _ _ _ _ result _ ->
      require (extension_laws (registry result)=[]) "a hidden transitive imported helper bypassed numeric dependency admission");
   runtime_case "numeric-imported-runtime-checked-occurrence" `Checked;
   runtime_case "numeric-imported-runtime-trusted-occurrence" `Trusted;
   runtime_case "numeric-imported-runtime-unknown-occurrence-rejects" `Unknown;
   case ~name:"numeric-cross-provider-operation-stale-issuer-rejects" ~changed:`Extension (fun _ extension consumer _ result rebuilt ->
      let old=compatible result in let base,changed=Option.get rebuilt in
      require (extension.Cmt_input.interface_digest=changed.interface_digest) "extension interface changed";
      require (Result.is_error (Numeric_ghost_registry_private.import_laws ~consumer ~dependencies:[base;changed]
        ~descriptors:(Numeric_ghost_registry_private.descriptors old) (Numeric_ghost_registry_private.laws old))) "stale extension proof artifact was accepted");
   case ~name:"numeric-cross-provider-operation-stale-carrier-rejects" ~changed:`Base (fun base _ consumer _ result rebuilt ->
      let old=compatible result in let changed,extension=Option.get rebuilt in
      require (base.Cmt_input.interface_digest=changed.interface_digest) "base interface changed";
      require (Result.is_error (Numeric_ghost_registry_private.import_laws ~consumer ~dependencies:[changed;extension]
        ~descriptors:(Numeric_ghost_registry_private.descriptors old) (Numeric_ghost_registry_private.laws old))) "stale base carrier artifact was accepted")]
