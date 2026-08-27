type 'a candidate = {
  source_ordinal : int;
  dependency_ordinals : int list;
  value : 'a;
}

type 'a selection = {
  ready : 'a candidate list;
  blocked : 'a candidate list;
  waiting : 'a candidate list;
}

let candidate ~source_ordinal ~dependency_ordinals value =
  { source_ordinal; dependency_ordinals; value }

let value candidate = candidate.value
let source_ordinal candidate = candidate.source_ordinal

let select ~completed_ordinals ~is_blocked ~is_waiting candidates =
  let ordered =
    List.sort
      (fun left right -> Int.compare left.source_ordinal right.source_ordinal)
      candidates
  in
  let rec classify seen ready blocked waiting = function
    | [] ->
        Ok
          {
            ready = List.rev ready;
            blocked = List.rev blocked;
            waiting = List.rev waiting;
          }
    | candidate :: rest ->
        if List.mem candidate.source_ordinal seen then
          Error
            (Printf.sprintf "duplicate function source ordinal %d"
               candidate.source_ordinal)
        else if candidate.source_ordinal < 0 then
          Error "function source ordinal must be nonnegative"
        else if is_blocked candidate.value then
          classify (candidate.source_ordinal :: seen) ready
            (candidate :: blocked) waiting rest
        else if
          (not (is_waiting candidate.value))
          &&
          List.for_all
            (fun dependency -> List.mem dependency completed_ordinals)
            candidate.dependency_ordinals
        then
          classify (candidate.source_ordinal :: seen) (candidate :: ready)
            blocked waiting rest
        else
          classify (candidate.source_ordinal :: seen) ready blocked
            (candidate :: waiting) rest
  in
  classify [] [] [] [] ordered

module For_testing = struct
  let dependency_ordinals candidate = candidate.dependency_ordinals
end
