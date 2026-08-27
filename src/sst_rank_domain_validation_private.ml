type error = { span : Diagnostic.span; detail : string }

let validate (program : Sst.program) =
  let span =
    match program.types with
    | definition :: _ -> definition.Sst.span
    | [] -> Diagnostic.file_span "<semantic-sst>"
  in
  match
    Parametric_rank_domain_private.nominal_domains program
    |> List.find_opt (fun domain ->
           not
             (Parametric_rank_domain_private.authenticate ~program domain))
  with
  | None -> Ok ()
  | Some _ ->
      Error
        {
          span;
          detail =
            "rank domain failed its sole compiler/program owner \
             authentication";
        }
