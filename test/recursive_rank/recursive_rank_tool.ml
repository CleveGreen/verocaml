let () = ignore Recursive_rank_prerequisites.ready

let fail format =
  Printf.ksprintf
    (fun message ->
      prerr_endline message;
      exit 3)
    format

let ok_logic = function
  | Ok value -> value
  | Error error -> fail "%s" (Logic_ir.error_to_string error)

let span_to_string span =
  Printf.sprintf "%s:%d:%d-%d:%d" (Filename.basename span.Diagnostic.file)
    span.start_pos.line span.start_pos.column span.end_pos.line
    span.end_pos.column

let reset_counters () =
  Solver_backend.For_testing.reset_solver_creation_count ();
  Z3_bridge.reset_counters ()

let print_zero_counters () =
  let direct = Z3_bridge.counters () in
  Printf.printf "solver-counters smtml=%d z3-contexts=%d z3-solvers=%d\n"
    (Solver_backend.For_testing.solver_creation_count ())
    direct.contexts_created direct.solvers_created

let lower filename =
  match Typedtree_lowering.lower_file filename with
  | Ok program -> program
  | Error diagnostic ->
      fail "%s: %s @ %s" diagnostic.Diagnostic.code diagnostic.message
        (span_to_string diagnostic.span)

let validate program =
  match Sst_validation.validate program with
  | Ok validated -> validated
  | Error error -> fail "%s" (Sst_validation.error_to_string error)

let load filename =
  match Cmt_input.load filename with
  | Ok implementation -> implementation
  | Error diagnostic ->
      fail "%s: %s @ %s" diagnostic.Diagnostic.code diagnostic.message
        (span_to_string diagnostic.span)

let certify_profiles implementation =
  match
    Typedtree_adapter.certify_rank_profiles
      ~source_file:implementation.Cmt_input.source_file
      ~imports:implementation.imports implementation.structure
  with
  | Ok profiles -> profiles
  | Error diagnostic ->
      fail "%s: %s @ %s" diagnostic.Diagnostic.code diagnostic.message
        (span_to_string diagnostic.span)

let classify filename =
  reset_counters ();
  (match Typedtree_lowering.lower_file filename with
  | Error diagnostic ->
      Printf.printf "%s: %s @ %s\n" diagnostic.Diagnostic.code
        diagnostic.message (span_to_string diagnostic.span)
  | Ok program ->
      let domains = Sst_validation.rank_domains (validate program) in
      Printf.printf "accepted rank-domains=%d\n" (List.length domains));
  print_zero_counters ()

let print_domain domain =
  Printf.printf "domain id=%s version=%s digest=%s immutable=%b\n"
    (Sst_validation.rank_domain_id domain)
    (Sst_validation.rank_domain_version domain)
    (Sst_validation.rank_snapshot_digest domain)
    (Sst_validation.rank_immutable domain);
  Sst_validation.rank_component domain
  |> List.iter
       (fun (identity : Typedtree_adapter.rank_type_identity) ->
         Printf.printf "  component %s#%d path=%s uid=%s\n"
           identity.rank_type_id.type_name identity.rank_type_id.type_index
           identity.rank_path identity.rank_uid);
  Sst_validation.rank_ground_witnesses domain
  |> List.iter
       (fun (witness : Typedtree_adapter.rank_ground_witness) ->
         let constructor = witness.rank_ground_constructor in
         Printf.printf "  ground %s#%d.%s#%d uid=%s rank=0\n"
           constructor.constructor_type.type_name
           constructor.constructor_type.type_index constructor.constructor_name
           constructor.constructor_index witness.rank_ground_constructor_uid);
  Sst_validation.rank_positive_children domain
  |> List.iter
       (fun (child : Typedtree_adapter.rank_positive_child) ->
         let constructor = child.rank_constructor in
         Printf.printf
           "  child %s#%d.%s#%d field=%s#%d path=%s field-uid=%s \
            constructor-uid=%s target=%s#%d\n"
           constructor.constructor_type.type_name
           constructor.constructor_type.type_index constructor.constructor_name
           constructor.constructor_index child.rank_field.field_name
           child.rank_field.field_index
           (match child.rank_child_path with
           | [] -> "root"
           | path -> String.concat "." (List.map string_of_int path))
           child.rank_field_uid child.rank_constructor_uid
           child.rank_child_type.type_name child.rank_child_type.type_index;
         Printf.printf "    trace %s\n"
           (String.concat " -> " child.rank_expansion_trace))

let domains filename =
  let validated = validate (lower filename) in
  let domains = Sst_validation.rank_domains validated in
  Printf.printf "rank-domains=%d\n" (List.length domains);
  List.iter print_domain domains

let classification = function
  | Typedtree_adapter.Prohibited_negative_use -> "prohibited-negative"
  | Recursive_dependent_grounding -> "recursive-dependent"
  | Independently_grounded_construction -> "independently-grounded"

let print_profile profile =
  let identity = Typedtree_adapter.rank_profile_identity profile in
  Printf.printf "profile id=%s digest=%s constructor=%s#%d path=%s uid=%s independent=%b\n"
    (Typedtree_adapter.rank_profile_id profile)
    (Typedtree_adapter.rank_profile_snapshot_digest profile)
    identity.rank_type_id.type_name identity.rank_type_id.type_index
    identity.rank_path identity.rank_uid
    (Typedtree_adapter.rank_profile_independently_grounded profile);
  Typedtree_adapter.rank_profile_parameters profile
  |> List.iter (fun (parameter : Typedtree_adapter.rank_parameter_profile) ->
         Printf.printf "  parameter %d identity=%s class=%s variance=%s\n"
           parameter.rank_parameter_index parameter.rank_parameter_identity
           (classification parameter.rank_parameter_classification)
           parameter.rank_parameter_variance;
         parameter.rank_parameter_occurrence_traces
         |> List.iter (fun trace ->
                Printf.printf "    occurrence %s\n"
                  (String.concat " -> " trace)));
  Typedtree_adapter.rank_profile_dependencies profile
  |> List.iter (fun (dependency : Typedtree_adapter.rank_type_identity) ->
         Printf.printf "  dependency %s#%d path=%s uid=%s\n"
           dependency.rank_type_id.type_name dependency.rank_type_id.type_index
           dependency.rank_path dependency.rank_uid);
  Typedtree_adapter.rank_profile_ground_traces profile
  |> List.iter (fun trace ->
         Printf.printf "  grounding %s\n" (String.concat " -> " trace));
  Typedtree_adapter.rank_profile_applications profile
  |> List.iter (fun (application : Typedtree_adapter.rank_application) ->
         Printf.printf "  application target=%s#%d path=%s uid=%s profile=%s\n"
           application.rank_application_target.rank_type_id.type_name
           application.rank_application_target.rank_type_id.type_index
           application.rank_application_target.rank_path
           application.rank_application_target.rank_uid
           application.rank_application_target_profile;
         application.rank_application_substitution
         |> List.iter (fun (index, actual) ->
                Printf.printf "    substitution parameter#%d=%s\n" index actual);
         Printf.printf "    trace %s\n"
           (String.concat " -> " application.rank_application_trace))

let profiles filename =
  reset_counters ();
  let profiles = certify_profiles (load filename) in
  Printf.printf "rank-profiles=%d\n" (List.length profiles);
  List.iter print_profile profiles;
  print_zero_counters ()

let profile_classify filename =
  reset_counters ();
  let implementation = load filename in
  (match
     Typedtree_adapter.certify_rank_profiles
       ~source_file:implementation.Cmt_input.source_file
       ~imports:implementation.imports implementation.structure
   with
  | Ok profiles ->
      Printf.printf "accepted rank-profiles=%d\n" (List.length profiles)
  | Error diagnostic ->
      Printf.printf "%s: %s @ %s\n" diagnostic.Diagnostic.code
        diagnostic.message (span_to_string diagnostic.span));
  print_zero_counters ()

let vir filename =
  match Symbolic_executor.lower_program (lower filename) with
  | Ok program -> print_string (Vir.to_string program)
  | Error error -> fail "%s" (Symbolic_executor.error_to_string error)

let rank_domains filename =
  let validated = validate (lower filename) in
  Vir.rank_domains_of_validated validated

let smt filename =
  let domains = rank_domains filename in
  List.iteri
    (fun index domain ->
      reset_counters ();
      let query =
        match Rank_encoding.query domain with
        | Ok query -> query
        | Error error -> fail "%s" (Logic_ir.error_to_string error)
      in
      let counters = Z3_bridge.counters () in
      if counters.contexts_created <> 0 || counters.solvers_created <> 0 then
        fail "rank encoding created a backend before rendering";
      match
        Z3_bridge.render_query { timeout_ms = 5000; model = false } query
      with
      | Error error -> fail "%s" (Z3_bridge.error_to_string error)
      | Ok (declarations, rendered) ->
          Printf.printf "domain-query %d %s\n%s\n%s" index
            (Vir.rank_domain_id domain) declarations rendered;
          let counters = Z3_bridge.counters () in
          Printf.printf
            "render-counters contexts=%d solvers=%d cleaned=%d live=%d\n"
            counters.contexts_created counters.solvers_created
            counters.contexts_cleaned counters.contexts_live)
    domains

let aggregate_term aggregate_type name =
  let symbol : Vir.symbol =
    {
      symbol_id = 0;
      source_name = name;
      sort = Vir.Aggregate aggregate_type;
      role = Vir.Input;
      span = Diagnostic.file_span "rank_attack.ml";
    }
  in
  { Vir.aggregate_type; aggregate_desc = Vir.Aggregate_symbol symbol }

let attacks positive mutable_child =
  reset_counters ();
  let source = lower positive in
  let validated = validate source in
  let domains = Vir.rank_domains_of_validated validated in
  let first, second =
    match domains with
    | [ first; second ] -> (first, second)
    | _ -> fail "expected exactly two positive rank domains"
  in
  let first_type =
    match Vir.rank_domain_component first with
    | [ aggregate ] -> aggregate
    | _ -> fail "first rank domain component changed"
  in
  let second_type =
    match Vir.rank_domain_component second with
    | [ aggregate ] -> aggregate
    | _ -> fail "second rank domain component changed"
  in
  let first_value = aggregate_term first_type "first" in
  let second_value = aggregate_term second_type "second" in
  (match Vir.rank_project first first_value with
  | Ok term ->
      Printf.printf "same-domain projection %s\n"
        (Vir.rank_term_to_string term)
  | Error message -> fail "%s" message);
  (match Vir.rank_project first second_value with
  | Error message -> Printf.printf "cross-domain rejected: %s\n" message
  | Ok _ -> fail "cross-domain VIR rank projection was accepted");
  let raw_copy =
    {
      source with
      Sst.types = List.map (fun definition -> definition) source.types;
    }
  in
  let raw_domains = Sst_validation.rank_domains (validate raw_copy) in
  Printf.printf "raw-sst-copy rank-domains=%d\n" (List.length raw_domains);
  let map_fields update definition =
    let type_kind =
      match definition.Sst.type_kind with
      | Sst.Record_definition fields ->
          Sst.Record_definition (List.map update fields)
      | Sst.Variant_definition constructors ->
          Sst.Variant_definition
            (List.map
               (fun (constructor : Sst.constructor_definition) ->
                 {
                   constructor with
                   Sst.constructor_fields =
                     List.map update constructor.constructor_fields;
                 })
               constructors)
    in
    { definition with Sst.type_kind }
  in
  let ownership_copy =
    {
      source with
      Sst.types =
        List.map
          (map_fields (fun field ->
               {
                 field with
                 Sst.field_modalities =
                   {
                     uniqueness_modality = Sst.Force_unique;
                     linearity_modality = Sst.Force_once;
                   };
               }))
          source.types;
    }
  in
  let ownership_domains =
    Sst_validation.rank_domains (validate ownership_copy)
  in
  Printf.printf "ownership-altered-raw-sst rank-domains=%d\n"
    (List.length ownership_domains);
  let opacity_copy =
    match source.types with
    | [] -> fail "positive rank fixture lost its types"
    | first :: rest ->
        {
          source with
          Sst.types =
            {
              first with
              representation =
                Sst.Abstract_with_evidence
                  (Sst.Incomplete_abstraction_evidence
                    {
                      evidence_id = "rank-domain/v1/forged";
                      evidence_span = first.span;
                    });
            }
            :: rest;
        }
  in
  (match Sst_validation.validate opacity_copy with
  | Error error ->
      Printf.printf "opacity-forgery rejected: %s\n"
        (Sst_validation.error_to_string error)
  | Ok _ -> fail "opacity forgery was accepted");
  let forged_field_copy =
    {
      source with
      Sst.types =
        List.map
          (map_fields (fun field ->
               {
                 field with
                 Sst.field_id =
                   {
                     field.field_id with
                     field_owner =
                       Sst.Record_owner
                         {
                           Sst.type_index = first_type.aggregate_type_index;
                           type_name = first_type.aggregate_type_name;
                         };
                   };
               }))
          source.types;
    }
  in
  (match Sst_validation.validate forged_field_copy with
  | Error error ->
      Printf.printf "forged-field rejected: %s\n"
        (Sst_validation.error_to_string error)
  | Ok _ -> fail "forged field identity was accepted");
  let mutable_domains =
    Sst_validation.rank_domains (validate (lower mutable_child))
  in
  Printf.printf "mutable-recursive-child rank-domains=%d\n"
    (List.length mutable_domains);
  let builder = Logic_ir.create () in
  let first_sort =
    Logic_ir.declare_sort builder ~name:"First" ~span:(Diagnostic.file_span "x")
    |> ok_logic
  in
  let second_sort =
    Logic_ir.declare_sort builder ~name:"Second"
      ~span:(Diagnostic.file_span "x")
    |> ok_logic
  in
  let first_domain =
    Logic_ir.declare_rank_domain builder ~domain_id:"first"
      ~members:[ ("member", first_sort) ]
      ~span:(Diagnostic.file_span "x")
    |> ok_logic
  in
  let second_binder =
    Logic_ir.bind builder ~name:"second" ~sort:second_sort
      ~span:(Diagnostic.file_span "x")
    |> ok_logic
  in
  (match
     Logic_ir.rank_project ~span:(Diagnostic.file_span "x") first_domain
       ~member:"member" (Logic_ir.bound second_binder)
   with
  | Error error ->
      Printf.printf "logic cross-domain rejected: %s\n"
        (Logic_ir.error_to_string error)
  | Ok _ -> fail "cross-domain Logic IR rank projection was accepted");
  print_zero_counters ()

let profile_attacks first_filename second_filename =
  reset_counters ();
  let first = load first_filename in
  let second = load second_filename in
  let first_profiles = certify_profiles first in
  let second_profiles = certify_profiles second in
  let first_profile, application =
    match
      first_profiles
      |> List.find_map (fun profile ->
             match Typedtree_adapter.rank_profile_applications profile with
             | application :: _ -> Some (profile, application)
             | [] -> None)
    with
    | Some pair -> pair
    | None -> fail "first profile fixture has no sealed application"
  in
  let second_profile =
    match second_profiles with
    | profile :: _ -> profile
    | [] -> fail "second profile fixture issued no profiles"
  in
  Printf.printf "same-snapshot authenticated=%b\n"
    (Typedtree_adapter.authenticate_rank_profile
       ~structure:first.structure first_profile);
  Printf.printf "same-name-cross-unit authenticated=%b\n"
    (Typedtree_adapter.authenticate_rank_profile
       ~structure:second.structure first_profile);
  let copied_structure =
    { first.structure with str_items = List.map Fun.id first.structure.str_items }
  in
  Printf.printf "copied-snapshot authenticated=%b issued=%d\n"
    (Typedtree_adapter.authenticate_rank_profile
       ~structure:copied_structure first_profile)
    (List.length (Typedtree_adapter.issued_rank_profiles copied_structure));
  Printf.printf "sealed-application authenticated=%b\n"
    (Typedtree_adapter.authenticate_rank_application first_profile application);
  let forged_substitution =
    {
      application with
      rank_application_substitution =
        (99, "forged-open-argument")
        :: application.rank_application_substitution;
    }
  in
  Printf.printf "forged-substitution authenticated=%b\n"
    (Typedtree_adapter.authenticate_rank_application first_profile
       forged_substitution);
  let mismatched_instantiation =
    {
      application with
      rank_application_substitution =
        (match application.rank_application_substitution with
        | (index, _) :: rest -> (index, "same-arity-wrong-actual") :: rest
        | [] -> [ (0, "unexpected-actual") ]);
    }
  in
  Printf.printf "mismatched-instantiation authenticated=%b\n"
    (Typedtree_adapter.authenticate_rank_application first_profile
       mismatched_instantiation);
  let forged_profile =
    {
      application with
      rank_application_target_profile =
        application.rank_application_target_profile ^ ".forged";
    }
  in
  Printf.printf "forged-profile authenticated=%b\n"
    (Typedtree_adapter.authenticate_rank_application first_profile forged_profile);
  let target = application.rank_application_target in
  let forged_dependency =
    {
      application with
      rank_application_target =
        { target with rank_uid = target.rank_uid ^ ".forged" };
    }
  in
  Printf.printf "forged-dependency authenticated=%b\n"
    (Typedtree_adapter.authenticate_rank_application first_profile
       forged_dependency);
  Printf.printf "mismatched-profile authenticated=%b\n"
    (Typedtree_adapter.authenticate_rank_application second_profile application);
  let grounding =
    match Typedtree_adapter.rank_profile_ground_traces first_profile with
    | grounding :: _ -> grounding
    | [] -> fail "first profile fixture has no sealed grounding trace"
  in
  Printf.printf "sealed-grounding authenticated=%b\n"
    (Typedtree_adapter.authenticate_rank_grounding first_profile grounding);
  Printf.printf "forged-grounding authenticated=%b\n"
    (Typedtree_adapter.authenticate_rank_grounding first_profile
       (grounding @ [ "forged-ground" ]));
  let mutate_nested_snapshot structure =
    let rec find_declaration = function
      | [] -> fail "same-name profile fixture has no type declaration"
      | item :: rest -> (
          match item.Typedtree.str_desc with
          | Tstr_type (_, declaration :: _) -> declaration
          | _ -> find_declaration rest)
    in
    let declaration = find_declaration structure.Typedtree.str_items in
    match declaration.typ_kind with
    | Ttype_variant constructors -> (
        match
          constructors
          |> List.find_map (fun (constructor : Typedtree.constructor_declaration) ->
                 match constructor.cd_args with
                 | Cstr_tuple (parameter :: recursive :: _) ->
                     Some
                       ( parameter.ca_type.ctyp_type,
                         recursive.ca_type.ctyp_type )
                 | Cstr_tuple _ | Cstr_record _ -> None)
        with
        | Some (replacement, nested) ->
            Types.set_type_desc nested (Types.get_desc replacement)
        | None -> fail "same-name profile fixture has no nested type snapshot")
    | Ttype_abstract | Ttype_record _ | Ttype_record_unboxed_product _
    | Ttype_open ->
        fail "same-name profile fixture changed kind"
  in
  mutate_nested_snapshot first.structure;
  Printf.printf "in-place-snapshot authenticated=%b issued=%d\n"
    (Typedtree_adapter.authenticate_rank_profile
       ~structure:first.structure first_profile)
    (List.length (Typedtree_adapter.issued_rank_profiles first.structure));
  print_zero_counters ()

let () =
  match Array.to_list Sys.argv with
  | [ _; "classify"; filename ] -> classify filename
  | [ _; "domains"; filename ] -> domains filename
  | [ _; "profiles"; filename ] -> profiles filename
  | [ _; "profile-classify"; filename ] -> profile_classify filename
  | [ _; "vir"; filename ] -> vir filename
  | [ _; "smt"; filename ] -> smt filename
  | [ _; "attacks"; positive; mutable_child ] ->
      attacks positive mutable_child
  | [ _; "profile-attacks"; first; second ] ->
      profile_attacks first second
  | _ ->
      fail
        "usage: recursive_rank_tool \
         (classify|domains|profiles|profile-classify|vir|smt) FILE.cmt | \
         attacks POSITIVE.cmt MUTABLE.cmt | profile-attacks FIRST.cmt SECOND.cmt"
