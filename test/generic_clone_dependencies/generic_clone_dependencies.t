The focused gate compiles retained CMTs with the production PPX.

  $ mkdir artifacts
  $ retained () { name=$1; ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -I artifacts -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$name.cmo" "fixtures/$name.ml"; }
  $ retained generic_positive
  $ retained polymorphic_recursion
  $ retained generic_higher_order

One canonical definition is retained for each generic source function. Calls
carry ordered type arguments; no closed source function identity is emitted.

  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/generic_positive.cmt --threads 1 --timeout-ms 5000 --dump-sst artifacts/first.sst --dump-vir artifacts/first.vir
  verocaml: verified file=artifacts/generic_positive.cmt functions=9 obligations=2
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/generic_positive.cmt --threads 2 --timeout-ms 5000 --dump-sst artifacts/second.sst --dump-vir artifacts/second.vir
  verocaml: verified file=artifacts/generic_positive.cmt functions=9 obligations=2
  $ cmp artifacts/first.sst artifacts/second.sst
  $ cmp artifacts/first.vir artifacts/second.vir
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

Nonuniform polymorphic recursion and higher-order generic values reject before
portable SST/VIR artifacts or solver work.

  $ reject () { name=$1; expected=$2; code=0; VEROCAML_TEST_PRIVATE_RECEIPT_TRACE=1 DELATOR_LOG=Verification_session=debug,warn DELATOR_FORMAT=flat DELATOR_COLOR=never OCAML_COLOR=never ../../src/verocaml.exe verify "artifacts/$name.cmt" --timeout-ms 5000 --dump-sst "artifacts/$name.sst" --dump-vir "artifacts/$name.vir" >"artifacts/$name.out" 2>&1 || code=$?; test "$code" = 2; actual=$(grep -Eo 'VERO_[A-Z_]+' "artifacts/$name.out" | head -1); test "$actual" = "$expected"; printf 'code=%s\n' "$actual"; test ! -e "artifacts/$name.sst"; test ! -e "artifacts/$name.vir"; test "$(grep -c 'Verification_session: private receipt ' "artifacts/$name.out")" = 0; }
  $ reject polymorphic_recursion VERO_UNSUPPORTED_GENERIC_USE
  code=VERO_UNSUPPORTED_GENERIC_USE
  $ reject generic_higher_order VERO_UNSUPPORTED_HIGHER_ORDER_FUNCTION
  code=VERO_UNSUPPORTED_HIGHER_ORDER_FUNCTION
