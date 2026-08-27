let ( let* ) result continuation =
  match result with Ok value -> continuation value | Error _ as error -> error

let signature ~definition ~parameter_kinds ~parameter_modes ~result_mode
    ~provider_completion =
  let* recursive_evidence =
    if definition.Sst.recursive then
      Parametric_signature_private.verified_recursion ~definition
        ~provider_completion
      |> Result.map Option.some
    else Ok None
  in
  Parametric_signature_private.create ~definition ~parameter_kinds
    ~parameter_modes ~result_mode ~recursive_evidence
