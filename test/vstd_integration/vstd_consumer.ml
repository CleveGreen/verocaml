let implies premise conclusion = (not premise) || conclusion
[@@verocaml.spec]

let opt_is_some (option : 'a option) : bool =
  match option with None -> false | Some _ -> true
[@@verocaml.spec]

let opt_test (value : int) : int option =
  [%verocaml.ensures fun result -> implies (value < 5) (opt_is_some result)];
  if value < 5 then Some value else None

let result_is_ok (result : ('a, 'error) result) : bool =
  match result with Ok _ -> true | Error _ -> false
[@@verocaml.spec]

let result_test (value : int) : (int, bool) result =
  [%verocaml.ensures fun result -> implies (value < 5) (result_is_ok result)];
  if value < 5 then Ok value else Error false

let every_nonnegative (sequence : int Vstd.Seq.t) : bool =
  forall (fun (index : int) ->
      (not (Vstd.Seq.valid_index sequence index))
      || ((Vstd.Seq.get sequence index) [@trigger]) >= 0)
[@@verocaml.spec]

let has_value (sequence : int Vstd.Seq.t) (value : int) : bool =
  exists (fun (index : int) ->
      Vstd.Seq.valid_index sequence index
      && Vstd.Seq.get sequence index = value)
[@@verocaml.spec]

let valid_length_definition (size : int) : unit =
  [%verocaml.ensures fun _ -> Vstd.Seq.valid_length size = (0 <= size)];
  assert (Vstd.Seq.valid_length size = (0 <= size))
[@@verocaml.proof]

let imported_axiom (sequence : int Vstd.Seq.t) : unit =
  [%verocaml.ensures fun _ ->
    Vstd.Seq.valid_length (Vstd.Seq.length sequence)];
  Vstd.Seq.axiom_length_domain sequence
[@@verocaml.proof]

let proof_with_unit_argument () : unit =
  [%verocaml.ensures fun _ -> true];
  ()
[@@verocaml.proof]
