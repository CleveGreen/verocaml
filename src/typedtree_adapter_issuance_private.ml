let ( let* ) result f =
  match result with Ok value -> f value | Error _ as error -> error

type proof_capture_artifact = {
  proof_capture_artifact_issuer : unit ref;
  proof_capture_implementation : Cmt_input.implementation;
}

let proof_capture_artifact_issuer = ref ()

let proof_capture_artifact implementation =
  {
    proof_capture_artifact_issuer;
    proof_capture_implementation = implementation;
  }

(* Shared-scalar mutation authority is deliberately tied to the physical
   program emitted from an authenticated source/CMT pair.  The semantic
   descriptor remains printable, but reconstructing it cannot recreate this
   process-local issuance. *)
type shared_scalar_program_issuance = {
  issued_program : Sst.program Weak.t;
  issued_function_snapshots : (Sst.function_id * string) list;
}

let issued_shared_scalar_programs : shared_scalar_program_issuance list ref =
  ref []

let shared_scalar_function_snapshot program definition =
  Sst.to_string { program with Sst.functions = [ definition ] }

let authenticate_shared_scalar_function ~program ~definition =
  let live, authenticated =
    List.fold_left
      (fun (live, authenticated) issued ->
        match Weak.get issued.issued_program 0 with
        | None -> (live, authenticated)
        | Some candidate ->
            let exact_function =
              List.exists
                (fun (function_id, snapshot) ->
                  function_id = definition.Sst.function_id
                  && String.equal snapshot
                       (shared_scalar_function_snapshot program definition))
                issued.issued_function_snapshots
            in
            ( issued :: live,
              authenticated || (candidate == program && exact_function) ))
      ([], false)
      !issued_shared_scalar_programs
  in
  issued_shared_scalar_programs := List.rev live;
  authenticated

let retained_ppx_arguments arguments =
  let rec loop = function
    | [] | [ _ ] -> false
    | "-ppx" :: command :: rest ->
        List.exists
          (String.equal "--keep-ghost")
          (String.split_on_char ' ' command)
        || loop rest
    | _ :: rest -> loop rest
  in
  loop (Array.to_list arguments)

let authenticate_logical_builtin_artifact artifact ~source_file =
  let implementation = artifact.proof_capture_implementation in
  artifact.proof_capture_artifact_issuer == proof_capture_artifact_issuer
  && String.equal implementation.Cmt_input.source_file source_file
  && implementation.implementation_metadata_valid
  && implementation.has_implementation_shape
  && retained_ppx_arguments implementation.compiler_arguments
  && List.exists
       (String.equal "retained-v1")
       implementation.implementation_family_markers

type rank_type_identity = {
  rank_type_id : Sst.type_id;
  rank_path : string;
  rank_uid : string;
  rank_span : Diagnostic.span;
}

type rank_positive_child = {
  rank_constructor : Sst.constructor_id;
  rank_constructor_uid : string;
  rank_field : Sst.field_id;
  rank_field_uid : string;
  rank_child_path : int list;
  rank_child_type : Sst.type_id;
  rank_expansion_trace : string list;
}

type rank_ground_witness = {
  rank_ground_constructor : Sst.constructor_id;
  rank_ground_constructor_uid : string;
}

type rank_parameter_classification =
  | Prohibited_negative_use
  | Recursive_dependent_grounding
  | Independently_grounded_construction

type rank_parameter_profile = {
  rank_parameter_index : int;
  rank_parameter_identity : string;
  rank_parameter_classification : rank_parameter_classification;
  rank_parameter_variance : string;
  rank_parameter_occurrence_traces : string list list;
}

type rank_application = {
  rank_application_owner : rank_type_identity;
  rank_application_target : rank_type_identity;
  rank_application_target_profile : string;
  rank_application_substitution : (int * string) list;
  rank_application_trace : string list;
}

type issued_rank_profile = {
  rank_profile_token : unit ref;
  rank_profile_structure : Typedtree.structure;
  rank_profile_structure_snapshot : unit -> string;
  rank_profile_id : string;
  rank_profile_snapshot_digest : string;
  rank_profile_identity : rank_type_identity;
  rank_profile_parameters : rank_parameter_profile list;
  rank_profile_applications : rank_application list;
  rank_profile_dependencies : rank_type_identity list;
  rank_profile_ground_traces : string list list;
  rank_profile_independently_grounded : bool;
}

type pending_rank_domain = {
  pending_component : rank_type_identity list;
  pending_positive_children : rank_positive_child list;
  pending_ground_witnesses : rank_ground_witness list;
  pending_actual_evidence : string list;
}

type issued_callable_instance = {
  callable_instance_token : unit ref;
  callable_instance_structure : Typedtree.structure;
  callable_instance_program : Sst.program;
  callable_instance_definition : Sst.function_definition;
  callable_instance_source_paths : Path.t list;
  callable_instance_binding_uid : string;
  callable_instance_leaf_name : string;
  callable_instance_profile_snapshot : string;
  callable_instance_specialization_digest : string;
  callable_instance_rank_snapshots : string list;
}
