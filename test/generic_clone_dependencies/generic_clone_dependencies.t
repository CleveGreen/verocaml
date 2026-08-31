The ordinary stable outcomes live in outcome_cases.ml.  This legacy Cram target
retains only the specialist generic-schema and type-substitution architecture
contract (W05-GENERIC-SCHEMA in test/support/migrations/w05.org).

  $ mkdir artifacts
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -I artifacts -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/generic_positive.cmo fixtures/generic_positive.ml
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/generic_positive.cmt --threads 1 --timeout-ms 5000 --dump-sst artifacts/first.sst >/dev/null
  $ ./generic_clone_dependencies_tool.exe schema artifacts/generic_positive.cmt
  schema function=id#0 binders=1
  schema function=choose#1 binders=1
  schema function=labelled#2 binders=1
  schema function=spec_identity#3 binders=1
  schema function=spec_choice_identity#4 binders=1
  schema function=prove_identity#5 binders=1
  schema function=prove_choice_identity#6 binders=1
  schema-count=7 clone-identities=0
  $ grep -E 'call (id|choose|labelled|spec_identity|prove_identity)#' artifacts/first.sst | sed -E 's/^ *//; s/ @ .*//' | sort -u
  exec-call choose#1 recursive=false type-arguments=[bool] : bool
  exec-call choose#1 recursive=false type-arguments=[int] : int
  exec-call id#0 recursive=false type-arguments=[bool] : bool
  exec-call id#0 recursive=false type-arguments=[int] : int
  exec-call labelled#2 recursive=false type-arguments=[bool] : bool
  exec-call labelled#2 recursive=false type-arguments=[int] : int
  proof-call prove_identity#5 recursive=false type-arguments=[bool] : unit
  proof-call prove_identity#5 recursive=false type-arguments=[int] : unit
  specification-call spec_identity#3 recursive=false type-arguments=['0@prove_identity#5] : bool
  specification-call spec_identity#3 recursive=false type-arguments=[bool] : bool
  specification-call spec_identity#3 recursive=false type-arguments=[int] : bool
