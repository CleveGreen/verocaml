let common = "let observed (_value : int) : bool = true [@@verocaml.spec]\n"

let cases =
  [
    ( "missing",
      common
      ^ {|let bad (value:int) : unit =
  [%verocaml.ensures fun _ -> observed value || value = value]; ()
[@@verocaml.proof] [@@verocaml.broadcast]
|} );
    ( "duplicate",
      common
      ^ {|let bad (value:int) : unit =
  [%verocaml.ensures fun _ -> ((observed value) [@trigger])];
  [%verocaml.ensures fun _ -> ((observed value) [@trigger])]; ()
[@@verocaml.proof] [@@verocaml.broadcast]
|} );
    ( "nonapplication",
      {|let bad (value:bool) : unit =
  [%verocaml.ensures fun _ -> (value [@trigger])]; ()
[@@verocaml.proof] [@@verocaml.broadcast]
|} );
    ( "equality",
      {|let bad (value:int) : unit =
  [%verocaml.ensures fun _ -> ((value = value) [@trigger])]; ()
[@@verocaml.proof] [@@verocaml.broadcast]
|} );
    ( "incomplete",
      {|let observed_pair (_left:int) (_right:bool) = true [@@verocaml.spec]
let bad (left:int) (right:bool) : unit =
  [%verocaml.ensures fun _ -> ((observed_pair left true) [@trigger]) || right]; ()
[@@verocaml.proof] [@@verocaml.broadcast]
|} );
    ( "nonproof-external",
      common
      ^ {|let bad (value:int) : unit =
  [%verocaml.ensures fun _ -> ((observed value) [@trigger])]; ()
[@@verocaml.external_body] [@@verocaml.broadcast]
|} );
    ( "ordinary",
      common
      ^ {|let bad (value:int) : unit =
  [%verocaml.ensures fun _ -> ((observed value) [@trigger])]; ()
[@@verocaml.broadcast]
|} );
    ( "axiom-nonunit",
      common
      ^ {|let bad (value:int) : int =
  [%verocaml.ensures fun _ -> ((observed value) [@trigger])]; value
[@@verocaml.axiom] [@@verocaml.broadcast]
|} );
    ( "self-cycle",
      common
      ^ {|let lemma (value:int) : unit =
  [%verocaml.ensures fun _ -> ((observed value) [@trigger])]; ()
[@@verocaml.proof] [@@verocaml.broadcast]
[@@@verocaml.broadcast_group (self, [self; lemma])]
let untouched value = value
|} );
    ( "ordinary-group",
      {|let ordinary value = value
[@@@verocaml.broadcast_group (bad, [ordinary])]
let untouched value = value
|} );
    ( "reference",
      {|let observed (_value : int ref) = true [@@verocaml.spec]
let bad (value:int ref) : unit =
  [%verocaml.ensures fun _ -> ((observed value) [@trigger])]; ()
[@@verocaml.proof] [@@verocaml.broadcast]
|} );
    ( "array",
      {|let observed (_value : int array) = true [@@verocaml.spec]
let bad (value:int array) : unit =
  [%verocaml.ensures fun _ -> ((observed value) [@trigger])]; ()
[@@verocaml.proof] [@@verocaml.broadcast]
|} );
    ( "object",
      {|let observed (_value : < get : int >) = true [@@verocaml.spec]
let bad (value:< get : int >) : unit =
  [%verocaml.ensures fun _ -> ((observed value) [@trigger])]; ()
[@@verocaml.proof] [@@verocaml.broadcast]
|} );
  ]

let write_file path contents =
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let () =
  let destination =
    match Sys.argv with
    | [| _; destination |] -> destination
    | _ ->
        prerr_endline "usage: generate_negatives.exe DESTINATION";
        exit 1
  in
  List.iter
    (fun (name, source) ->
      write_file (Filename.concat destination (name ^ ".ml")) source)
    cases
