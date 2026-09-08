open Outcome_test_support
let cases ~write_file =
  let binary environment name = Filename.concat (Project_environment.binary_root environment) name in
  let generate environment workspace unit_name =
    let path = Filename.concat workspace (unit_name ^ ".inc") in
    let output = Unix.openfile path [Unix.O_WRONLY;O_CREAT;O_EXCL] 0o600 in
    let executable = binary environment "verocaml-retained-interface" in
    let args = [|executable;"dune-stanza";"numeric_consumer";".";String.lowercase_ascii unit_name;unit_name|] in
    let status = Fun.protect ~finally:(fun () -> Unix.close output) (fun () ->
      let pid = Unix.create_process executable args Unix.stdin output Unix.stderr in snd (Unix.waitpid [] pid)) in
    match status with Unix.WEXITED 0 -> () | _ -> failwith "numeric library retained metadata generation failed" in
  let case threads = Suite.case ~name:("numeric-libraries-normal-dune-discovery-workers-" ^ string_of_int threads)
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_process_fact (Outcome.Exit_class (Outcome.Exited 0)))
    (fun ~environment ~workspace ->
      try
        let workspace=Unix.realpath workspace in
        write_file (Filename.concat workspace "dune-project") "(lang dune 3.17)\n(name numeric_consumer)\n(package (name numeric_consumer))\n";
        let providers=["Fennec","Cell","inspect","bounded";"Ember","Parcel","project","certificate"] in
        let stanza (unit_name,_,_,_) =
          let library=String.lowercase_ascii unit_name in
          "(library (name " ^ library ^ ") (wrapped false) (modules " ^ library ^ ") (libraries verocaml.vstd verocaml.ghost)"
          ^ " (flags (:standard -w -A -alert -all)) (preprocess (pps verocaml.ppx -- --verocaml-retained)))\n(include " ^ unit_name ^ ".inc)\n" in
        write_file (Filename.concat workspace "dune")
          (String.concat "" (List.map stanza providers)
           ^ "(library (name use_numeric) (modules use_numeric) (libraries fennec ember verocaml.vstd verocaml.ghost) (flags (:standard -w -A -alert -all)) (preprocess (pps verocaml.ppx -- --verocaml-retained)))\n");
        List.iter (fun (unit_name,nested,view,law) ->
          let stem=Filename.concat workspace (String.lowercase_ascii unit_name) in
          write_file (stem ^ ".mli")
            ("module " ^ nested ^ " : sig\ntype t = int [@@verocaml.numeric_carrier {base = Vstd.Int.t; profile = \"arbitrary-author-request\"; representation = \"immediate\"; compatibility = []}]\n"
             ^ "val " ^ law ^ " : t -> unit [@@verocaml.proof]\nval " ^ view ^ " : t -> Vstd.Int.t [@@verocaml.spec]\n"
             ^ "[@@verocaml.numeric_role {carrier = t; role_schema = \"numeric-role.v1\"; role = \"unsigned-view\"; semantics = " ^ law
             ^ "; visibility = \"visible\"; reveal = true; inline = true}]\nend\n");
          write_file (stem ^ ".ml")
            ("[@@@verocaml.verify]\nmodule " ^ nested ^ " = struct\ntype t = int\nlet " ^ view ^ " (_x : t) : Vstd.Int.t = 0 + 0 [@@verocaml.spec]\n"
             ^ "let " ^ law ^ " (x : t) = [%verocaml.ensures fun _ -> 0 <= " ^ view ^ " x && " ^ view
             ^ " x < 65536 * 65536]; () [@@verocaml.proof]\nend\n");
          generate environment workspace unit_name) providers;
        write_file (Filename.concat workspace "use_numeric.ml")
          "[@@@verocaml.verify]\nlet lemma (x : int) =\n[%verocaml.ensures fun _ -> 0 <= Fennec.Cell.inspect x && 0 <= Ember.Parcel.project x && 0 <= Vstd.Machine_int.unsigned x];\nFennec.Cell.bounded x; Ember.Parcel.certificate x; Vstd.Machine_int.unsigned_range x\n[@@verocaml.proof]\n";
        Process_adapter.run ~cwd:workspace
          {program=binary environment "verocaml";
           arguments=["verify";".";"--threads";string_of_int threads;"--timeout-ms";"20000"];
           forwarded=["PATH",Project_environment.tool_path environment;"OCAMLPATH",Project_environment.ocaml_path environment;
             "VEROCAML_DUNE",Project_environment.dune_path environment;"DUNE_CACHE","disabled";"HOME",workspace;"TMPDIR",workspace;"OCAML_COLOR","never";
             "DELATOR_LOG",Option.value ~default:"error" (Sys.getenv_opt "DELATOR_LOG")];
           cleanup_paths=[];adjacency=[]}
      with Failure message | Sys_error message -> Error (Failure.make Failure.Project_materialization message)) in
  [case 1;case 2]
