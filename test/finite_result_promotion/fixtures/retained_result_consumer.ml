let consume (_node : Retained_result_provider.node [@finite]) : unit = ()

let use () = consume (Retained_result_provider.make 0)
