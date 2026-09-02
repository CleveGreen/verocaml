let instrumented value = value + 1
[@@delator.instrument] [@@delator.level trace]

let portable_instrumented @ portable = fun value ->
  [%log.trace "portable event" ~value:(Delator.Field.int value)
    ~portable:(Delator.Field.bool true)];
  Delator.in_span ~level:Delator.Trace ~target:"delator-oxcaml-test"
    ~name:"portable-action" (fun () -> instrumented value)
[@@delator.instrument] [@@delator.level trace]

let run () =
  assert (String.ends_with ~suffix:"+ox" Sys.ocaml_version);
  let result =
    Delator.in_span ~level:Delator.Trace ~target:"delator-oxcaml-test"
      ~name:"local-action" (stack_ fun () -> portable_instrumented 41)
  in
  assert (result = 42);
  result
