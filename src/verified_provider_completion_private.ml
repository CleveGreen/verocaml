type t = {
  issuer : unit ref;
  token : unit ref;
  implementation : Cmt_input.implementation option;
  program : Sst.program option;
  program_snapshot : string;
  recursive_snapshots : (Sst.function_id * string) list;
}

let issuer = ref ()

let definition_snapshot (definition : Sst.function_definition) =
  Digest.string
    (Sst.to_string
       { Sst.policy = definition.policy;
         parametric_adts = [];
         types = [];
         functions = [ definition ] })
  |> Digest.to_hex

let same_function_id (left : Sst.function_id) (right : Sst.function_id) =
  left.function_index = right.function_index
  && String.equal left.function_name right.function_name

let authentic completion =
  completion.issuer == issuer
  && completion.token != issuer
  && String.equal completion.program_snapshot
       (Option.fold ~none:"" ~some:Sst.to_string completion.program)

let authenticates completion ~implementation ~program =
  authentic completion
  && Option.fold ~none:false
       ~some:(fun candidate -> candidate == implementation)
       completion.implementation
  && Option.fold ~none:false
       ~some:(fun candidate -> candidate == program)
       completion.program

let authenticates_recursive_definition completion definition =
  authentic completion
  && definition.Sst.recursive
  && List.exists
       (fun (function_id, snapshot) ->
         same_function_id function_id definition.function_id
         && String.equal snapshot (definition_snapshot definition))
       completion.recursive_snapshots

module For_driver = struct
  let issue ~implementation ~program =
    let recursive_snapshots =
      program.Sst.functions
      |> List.filter_map (fun definition ->
             if definition.Sst.recursive then
               Some
                 (definition.function_id, definition_snapshot definition)
             else None)
    in
    { issuer; token = ref (); implementation = Some implementation;
      program = Some program;
      program_snapshot = Sst.to_string program; recursive_snapshots }
end

module For_testing = struct
  let forged ~definition =
    { issuer = ref (); token = ref ();
      implementation = None;
      program = None;
      program_snapshot = "forged-nonempty-completion";
      recursive_snapshots =
        [ (definition.Sst.function_id, definition_snapshot definition) ] }
end
