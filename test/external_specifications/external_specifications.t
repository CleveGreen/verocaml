  $ ./external_specifications_tool.exe structural
  accepted: symmetric same-unit trusted linkage
  rejected: external wrapper invocation
  raw external-specification linkage boundary passed

  $ mkdir artifacts
  $ ocamlc -stop-after parsing -dsource -ppx ../../ppx/vero_ppx.exe fixtures/positive.ml > artifacts/o 2> artifacts/source
  $ test ! -s artifacts/o
  $ cat artifacts/source
  let promised (x : int) = x * x[@@verocaml.internal.artifact_family.ordinary-v1
                                  ]
  let opaque (x : int) = x * x[@@verocaml.internal.artifact_family.ordinary-v1
                                ]
  let bool_source (x : bool) = not x[@@verocaml.internal.artifact_family.ordinary-v1
                                      ]
  let use_promised (x : int) = promised x[@@verocaml.internal.artifact_family.ordinary-v1
                                           ]
  let use_havoc (x : int) =
    let first = opaque x in
    let second = opaque x in if first = second then first else second[@@verocaml.internal.artifact_family.ordinary-v1
                                                                      ]
  $ test $(grep -c '^let ' artifacts/source) -eq 5
  $ test $(grep -c 'specification\|Vero_ghost\|verocaml.external' artifacts/source) -eq 0
  $ ocamlc -bin-annot -ppx ../../ppx/vero_ppx.exe -c -o artifacts/erased.cmo fixtures/positive.ml
  $ ./external_specifications_tool.exe inspect erased artifacts/erased.cmt
  ordinary CMT erased external-specification wrappers

  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/positive.cmo fixtures/positive.ml
  $ ./external_specifications_tool.exe inspect retained artifacts/positive.cmt
  retained CMT authenticated three external specifications
  $ ./external_specifications_tool.exe sst artifacts/positive.cmt > artifacts/a.sst
  $ ./external_specifications_tool.exe sst artifacts/positive.cmt > artifacts/b.sst
  $ cmp artifacts/a.sst artifacts/b.sst
  $ grep -E '^function (promised|opaque|bool_source|use_)|body (trusted|external)' artifacts/a.sst
  function promised#0 mode=exec recursive=false result=int policy=default-linear/default-z3 @ positive.ml:1:0-1:30
    body trusted-external-specification target wrapper=promised_specification#1 target=promised#0 target-span=positive.ml:1:0-1:30 wrapper-span=positive.ml:2:0-6:35 witness-span=positive.ml:6:0-6:35 requires=1 ensures=1
  function promised_specification#1 mode=exec recursive=false result=int policy=default-linear/default-z3 @ positive.ml:2:0-6:35
    body external-specification noncallable wrapper=promised_specification#1 target=promised#0 target-span=positive.ml:1:0-1:30 wrapper-span=positive.ml:2:0-6:35 witness-span=positive.ml:6:0-6:35 requires=1 ensures=1
  function opaque#2 mode=exec recursive=false result=int policy=default-linear/default-z3 @ positive.ml:8:0-8:28
    body trusted-external-specification target wrapper=opaque_specification#3 target=opaque#2 target-span=positive.ml:8:0-8:28 wrapper-span=positive.ml:9:0-10:35 witness-span=positive.ml:10:0-10:35 requires=0 ensures=0
  function opaque_specification#3 mode=exec recursive=false result=int policy=default-linear/default-z3 @ positive.ml:9:0-10:35
    body external-specification noncallable wrapper=opaque_specification#3 target=opaque#2 target-span=positive.ml:8:0-8:28 wrapper-span=positive.ml:9:0-10:35 witness-span=positive.ml:10:0-10:35 requires=0 ensures=0
  function bool_source#4 mode=exec recursive=false result=bool policy=default-linear/default-z3 @ positive.ml:12:0-12:34
    body trusted-external-specification target wrapper=bool_source_specification#5 target=bool_source#4 target-span=positive.ml:12:0-12:34 wrapper-span=positive.ml:13:0-14:35 witness-span=positive.ml:14:0-14:35 requires=0 ensures=0
  function bool_source_specification#5 mode=exec recursive=false result=bool policy=default-linear/default-z3 @ positive.ml:13:0-14:35
    body external-specification noncallable wrapper=bool_source_specification#5 target=bool_source#4 target-span=positive.ml:12:0-12:34 wrapper-span=positive.ml:13:0-14:35 witness-span=positive.ml:14:0-14:35 requires=0 ensures=0
  function use_promised#6 mode=exec recursive=false result=int policy=default-linear/default-z3 @ positive.ml:16:0-19:12
  function use_havoc#7 mode=exec recursive=false result=int policy=default-linear/default-z3 @ positive.ml:21:0-24:42
  $ test $(grep -c 'checked-multiply\|assertion\|decreases' artifacts/a.sst) -eq 0

  $ ./external_specifications_tool.exe vir artifacts/positive.cmt > artifacts/a.vir
  $ ./external_specifications_tool.exe vir artifacts/positive.cmt > artifacts/b.vir
  $ cmp artifacts/a.vir artifacts/b.vir
  $ grep -E '^function |trusted-external|^  vc ' artifacts/a.vir
  function use_promised#6 mode=exec body=checked-typedtree:positive.ml policy=default-linear/default-z3
    trusted-external-specification trust=axiomatic target=promised#0 wrapper=promised_specification#1 target-span=positive.ml:1:0-1:30 wrapper-span=positive.ml:2:0-6:35 witness-span=positive.ml:6:0-6:35 call=positive.ml:19:2-19:12 requires=1 ensures=1 result=unconstrained
    vc 0 call-precondition callee=promised#0 ordinal=0 declaration=positive.ml:3:2-3:30 call=positive.ml:19:2-19:12 @ positive.ml:19:2-19:12
    vc 1 postcondition ordinal=0 declaration=positive.ml:18:2-18:50 @ positive.ml:18:2-18:50
  function use_havoc#7 mode=exec body=checked-typedtree:positive.ml policy=default-linear/default-z3
    trusted-external-specification trust=axiomatic target=opaque#2 wrapper=opaque_specification#3 target-span=positive.ml:8:0-8:28 wrapper-span=positive.ml:9:0-10:35 witness-span=positive.ml:10:0-10:35 call=positive.ml:22:14-22:22 requires=0 ensures=0 result=unconstrained
    trusted-external-specification trust=axiomatic target=opaque#2 wrapper=opaque_specification#3 target-span=positive.ml:8:0-8:28 wrapper-span=positive.ml:9:0-10:35 witness-span=positive.ml:10:0-10:35 call=positive.ml:23:15-23:23 requires=0 ensures=0 result=unconstrained
  $ test $(grep -c '^function promised\|^function opaque\|^function bool_source\|uninterpreted\|body-equation' artifacts/a.vir) -eq 0
  $ ./external_specifications_tool.exe havoc artifacts/positive.cmt
  havoc: two fresh unconstrained results, zero clauses, no relationship or target VIR
  $ ./external_specifications_tool.exe solve artifacts/positive.cmt
  use_promised: verified-with-trusted-axioms (2 obligations, 1 uses)
  use_havoc: verified-with-trusted-axioms (0 obligations, 2 uses)

  $ ocamlc -w -A -alert -all -bin-annot -c -o artifacts/raw.cmo fixtures/raw_attribute.ml
  $ ./external_specifications_tool.exe reject artifacts/raw.cmt
  adapter rejected: VERO_MALFORMED_GHOST_CALL
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -c -o artifacts/counterfeit.cmo fixtures/counterfeit.ml
  $ ./external_specifications_tool.exe reject artifacts/counterfeit.cmt
  adapter rejected: VERO_MALFORMED_GHOST_CALL

  $ retained_compile () { n=$1; ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$n.cmo" "fixtures/$n.ml"; }
  $ retained_compile fabricated_equality
  $ ./external_specifications_tool.exe counterexample artifacts/fabricated_equality.cmt
  counterexample: trusted summary cannot prove the claim
  $ retained_compile precondition_failure
  $ ./external_specifications_tool.exe counterexample artifacts/precondition_failure.cmt
  counterexample: trusted summary cannot prove the claim
  $ retained_compile skipped_target_body
  $ ./external_specifications_tool.exe sst artifacts/skipped_target_body.cmt | grep -c 'assertion\|decreases\|checked-multiply' || true
  0
  $ ./external_specifications_tool.exe vir artifacts/skipped_target_body.cmt | grep -c '^function target' || true
  0
  $ ./external_specifications_tool.exe solve artifacts/skipped_target_body.cmt
  caller: verified-with-trusted-axioms (0 obligations, 1 uses)

  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/positive.cmt | grep -E 'ensures=0 result=unconstrained|verified-with-trusted-axioms' | tail -2
  verocaml: trusted external specification trust=axiomatic target=opaque#2 wrapper=opaque_specification#3 target-span=fixtures/positive.ml:8:0-8:28 wrapper-span=fixtures/positive.ml:9:0-10:35 witness-span=fixtures/positive.ml:10:0-10:35 call=fixtures/positive.ml:23:15-23:23 requires=0 ensures=0 result=unconstrained
  verocaml: verified-with-trusted-axioms file=artifacts/positive.cmt functions=2 obligations=2 trusted-external-spec-uses=3
