val usage : string

type message =
  | Positive_integer of string
  | Option_once of string
  | Option_requires of string * string
  | Unsupported_solver of string
  | Unknown_option of string
  | Unexpected_argument of string
  | Dump_write_failure of {
      label : string;
      filename : string;
      detail : string;
    }
  | Source_cmt_identity_count of int
  | Unix_failure of {
      function_name : string;
      argument : string;
      detail : string;
    }
  | Path_inspection_failure of {
      label : string;
      path : string;
      detail : string;
    }
  | Same_path of {
      left_label : string;
      left_path : string;
      right_label : string;
      right_path : string;
    }
  | Compiler_unavailable of string
  | Ppx_unavailable
  | Ghost_unavailable
  | Compiler_invocation_failure of string
  | Compiler_missing_cmt
  | Temporary_storage_failure of string
  | Consumer_cmt_rejected of string * Diagnostic.t
  | Dependency_cmt_rejected of string * Diagnostic.t
  | Project_inventory_required
  | Project_entry_requires of string

val message : message -> string
val cli_error : string -> string
val internal_error : string -> string
val frontend_error : Diagnostic.t -> string
val dependency_error : unit_name:string option -> message:string -> string

val source_compile_error :
  source:string -> process_status:Unix.process_status -> string

val verification_stderr_lines : Verifier_service.result -> string list

val trusted_external_line :
  Verifier_service.trusted_external_view -> string

val verification_stdout_lines :
  display_file:string -> Verifier_service.result -> string list
