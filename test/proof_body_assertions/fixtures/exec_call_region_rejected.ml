let executable (value : int) = value

let exec_call_region (value : int) =
  [%verocaml.proof
    let _called = executable value in
    ()];
  value
