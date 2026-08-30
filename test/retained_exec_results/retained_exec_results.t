The fixed focused manifest contains only the accepted OCaml implementation
files.

  $ find . -name '*.ml' -not -path './artifacts/*' | sort
  ./fixtures/callback_consumer.ml
  ./fixtures/consumer.ml
  ./fixtures/finite_result_consumer.ml
  ./fixtures/hidden_provider.ml
  ./fixtures/hidden_result_consumer.ml
  ./fixtures/mutable_provider.ml
  ./fixtures/mutable_result_consumer.ml
  ./fixtures/partial_consumer.ml
  ./fixtures/proof_result_consumer.ml
  ./fixtures/provider.ml
  ./fixtures/runtime.ml
  ./fixtures/spec_result_consumer.ml
  ./fixtures/two_calls_failure.ml
  ./fixtures/uncontracted_field_failure.ml
  ./retained_exec_results_tool.ml

Build one separately compiled marked provider and every accepted consumer.

  $ mkdir artifacts
  $ export GHOST="$PWD/../../runtime/.vero_ghost.objs/byte"
  $ export PPX="$PWD/../../ppx/vero_ppx.exe"
  $ compile_interface () { name=$1; ocamlc -w -A -alert -all -bin-annot -I "$GHOST" -I artifacts -ppx "$PPX --keep-ghost" -c -o "artifacts/$name.cmi" "fixtures/$name.mli"; }
  $ compile () { name=$1; ocamlc -w -A -alert -all -bin-annot -I "$GHOST" -I artifacts -ppx "$PPX --keep-ghost" -c -o "artifacts/$name.cmo" "fixtures/$name.ml"; }
  $ compile_artifact_interface () { name=$1; ocamlc -w -A -alert -all -bin-annot -I "$GHOST" -I artifacts -ppx "$PPX --keep-ghost" -c -o "artifacts/$name.cmi" "artifacts/$name.mli"; }
  $ compile_artifact () { name=$1; ocamlc -w -A -alert -all -bin-annot -I "$GHOST" -I artifacts -ppx "$PPX --keep-ghost" -c -o "artifacts/$name.cmo" "artifacts/$name.ml"; }
  $ compile_consumer_slice () { fixture=$1; function_name=$2; output=$3; { sed -n '1p' "fixtures/$fixture.ml"; grep "^let $function_name " "fixtures/$fixture.ml"; } > "artifacts/$output.ml"; compile_artifact "$output"; }
  $ sed -E '/^(let|val) (proof_box|spec_box) /d' fixtures/provider.ml > artifacts/provider.ml
  $ sed -E '/^val (proof_box|spec_box) /d' fixtures/provider.mli > artifacts/provider.mli
  $ compile_artifact_interface provider
  $ compile_artifact provider
  $ { sed -n '1p' fixtures/provider.ml; grep "^type 'a box " fixtures/provider.ml; grep '^let proof_box ' fixtures/provider.ml; } > artifacts/generic_proof_provider.ml
  $ { grep "^type 'a box " fixtures/provider.mli; grep '^val proof_box ' fixtures/provider.mli; } > artifacts/generic_proof_provider.mli
  $ compile_artifact_interface generic_proof_provider
  $ compile_artifact generic_proof_provider
  $ { sed -n '1p' fixtures/provider.ml; grep "^type 'a box " fixtures/provider.ml; grep '^let spec_box ' fixtures/provider.ml; } > artifacts/generic_spec_provider.ml
  $ { grep "^type 'a box " fixtures/provider.mli; grep '^val spec_box ' fixtures/provider.mli; } > artifacts/generic_spec_provider.mli
  $ compile_artifact_interface generic_spec_provider
  $ compile_artifact generic_spec_provider
  $ for name in consumer uncontracted_field_failure two_calls_failure finite_result_consumer partial_consumer callback_consumer; do compile "$name"; done
  $ compile_consumer_slice proof_result_consumer use_monomorphic proof_result_consumer
  $ compile_consumer_slice proof_result_consumer use_generic proof_generic_result_consumer
  $ compile_consumer_slice spec_result_consumer use_monomorphic spec_result_consumer
  $ compile_consumer_slice spec_result_consumer use_generic spec_generic_result_consumer
  $ compile_interface hidden_provider
  $ compile hidden_provider
  $ compile hidden_result_consumer
  $ { sed -n '1p' fixtures/mutable_provider.ml; grep -E '^(type result|let make )' fixtures/mutable_provider.ml; } > artifacts/mutable_provider.ml
  $ grep -E '^(type result|val make )' fixtures/mutable_provider.mli > artifacts/mutable_provider.mli
  $ compile_artifact_interface mutable_provider
  $ compile_artifact mutable_provider
  $ { sed -n '1p' fixtures/mutable_provider.ml; grep -E '^(type shared_result|let make_shared )' fixtures/mutable_provider.ml; } > artifacts/shared_provider.ml
  $ grep -E '^(type shared_result|val make_shared )' fixtures/mutable_provider.mli > artifacts/shared_provider.mli
  $ compile_artifact_interface shared_provider
  $ compile_artifact shared_provider
  $ { sed -n '1p' fixtures/mutable_provider.ml; grep '^let pass_foreign ' fixtures/mutable_provider.ml; } > artifacts/foreign_provider.ml
  $ grep '^val pass_foreign ' fixtures/mutable_provider.mli > artifacts/foreign_provider.mli
  $ compile_artifact_interface foreign_provider
  $ compile_artifact foreign_provider
  $ compile_consumer_slice mutable_result_consumer use_mutable mutable_result_consumer
  $ compile_consumer_slice mutable_result_consumer use_shared shared_result_consumer
  $ compile_consumer_slice mutable_result_consumer use_foreign foreign_result_consumer

Monomorphic records (plain and unique), variants, canonical generic records at
concrete and open types, generic trees/lists, recursive nominal values, tuples,
direct/bind/project/match/pass/return, and the explicit postcondition all
verify through retained CMT. Serial and two-thread runs are byte-identical.

  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/consumer.cmt --threads 1 --timeout-ms 60000 --dependency artifacts/provider.cmt --dump-sst artifacts/consumer.1.sst --dump-vir artifacts/consumer.1.vir > artifacts/consumer.1.out
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/consumer.cmt --threads 2 --timeout-ms 60000 --dependency artifacts/provider.cmt --dump-sst artifacts/consumer.2.sst --dump-vir artifacts/consumer.2.vir > artifacts/consumer.2.out
  $ cmp artifacts/consumer.1.out artifacts/consumer.2.out
  $ cmp artifacts/consumer.1.sst artifacts/consumer.2.sst
  $ cmp artifacts/consumer.1.vir artifacts/consumer.2.vir
  $ sed -E 's/interface-digest=[0-9a-f]+/interface-digest=<digest>/' artifacts/consumer.1.out
  verocaml: verified dependency unit=Provider interface-digest=<digest> direct=none transitive=none trust=none
  verocaml: verified file=artifacts/consumer.cmt functions=15 obligations=4
  $ grep -E 'exec-call Provider[.](make_record|make_unique_record|make_variant|make_box|pass_box|make_tree|pass_tree|make_chain|make_tuple|pass_record)#' artifacts/consumer.1.sst | sed -E 's/^ *//; s/#.*//' | sort -u
  exec-call Provider.make_box
  exec-call Provider.make_chain
  exec-call Provider.make_record
  exec-call Provider.make_tree
  exec-call Provider.make_tuple
  exec-call Provider.make_unique_record
  exec-call Provider.make_variant
  exec-call Provider.pass_box
  exec-call Provider.pass_record
  exec-call Provider.pass_tree
  $ ./retained_exec_results_tool.exe positive artifacts/consumer.cmt artifacts/provider.cmt
  positive status=verified authority=finite:0/0 result:0/0 recursive:0/0 invariant:0 ownership:0 rank-domains=0

The source route uses the same authenticated provider and verifies with the
same call inventory.

  $ cp fixtures/consumer.ml artifacts/source_consumer.ml
  $ (cd artifacts && OCAML_COLOR=never ../../../src/verocaml.exe verify source_consumer.ml --threads 1 --timeout-ms 60000 --dependency provider.cmt --dump-sst source.sst --dump-vir source.vir > source.out)
  $ sed -E 's/interface-digest=[0-9a-f]+/interface-digest=<digest>/' artifacts/source.out
  verocaml: verified dependency unit=Provider interface-digest=<digest> direct=none transitive=none trust=none
  verocaml: verified file=source_consumer.ml functions=15 obligations=4
  $ test "$(grep -c 'exec-call Provider[.]' artifacts/source.sst)" = "$(grep -c 'exec-call Provider[.]' artifacts/consumer.1.sst)"

Only contracts and caller-created datatype facts enter the consumer. Provider
body constants and aggregate body equations are absent.

  $ grep -E '= \(t.*Provider[.]make_(unique_)?record[.]result.* value' artifacts/consumer.1.vir >/dev/null
  $ if grep -E '(integer|constant) (41|42|43|44)|aggregate-(record|constructor).*Provider[.]make_' artifacts/consumer.1.sst artifacts/consumer.1.vir; then false; else echo 'provider-body-equations=absent'; fi
  provider-body-equations=absent
  $ grep -E 'tag[.]Provider|t[0-9]+_Provider.*Provider[.].*[.]result' artifacts/consumer.1.vir >/dev/null
  $ echo 'caller-pattern-and-selector-facts=present'
  caller-pattern-and-selector-facts=present

Uncontracted provider-body values and equality of two calls are not provable.

  $ for name in uncontracted_field_failure two_calls_failure; do code=0; OCAML_COLOR=never ../../src/verocaml.exe verify "artifacts/$name.cmt" --threads 1 --timeout-ms 60000 --dependency artifacts/provider.cmt --dump-sst "artifacts/$name.sst" --dump-vir "artifacts/$name.vir" > "artifacts/$name.out" 2>&1 || code=$?; test "$code" = 1; grep -q 'counterexample function=' "artifacts/$name.out"; test -s "artifacts/$name.sst"; test -s "artifacts/$name.vir"; echo "$name=counterexample"; done
  uncontracted_field_failure=counterexample
  two_calls_failure=counterexample

Monomorphic and generic aggregate-returning Proof/non-model Spec,
finite-result demand, partial, callback, mutable, unsupported-shared, hidden,
and foreign routes reject before consumer SST/VIR. Private incomplete and
malformed closures reject at their direct classifier/source-gate boundaries.

  $ reject () { label=$1; consumer=$2; shift 2; code=0; OCAML_COLOR=never ../../src/verocaml.exe verify "$consumer" --threads 1 --timeout-ms 60000 "$@" --dump-sst "artifacts/$label.sst" --dump-vir "artifacts/$label.vir" > "artifacts/$label.out" 2>&1 || code=$?; test "$code" = 2; test ! -e "artifacts/$label.sst"; test ! -e "artifacts/$label.vir"; echo "$label=pre-sst-vir-rejected"; }
  $ reject proof-monomorphic artifacts/proof_result_consumer.cmt --dependency artifacts/provider.cmt
  proof-monomorphic=pre-sst-vir-rejected
  $ reject proof-generic artifacts/proof_generic_result_consumer.cmt --dependency artifacts/generic_proof_provider.cmt
  proof-generic=pre-sst-vir-rejected
  $ grep -Fq 'VERO_' artifacts/proof-generic.out
  $ echo 'proof-generic-boundary=retained-callable-ineligible'
  proof-generic-boundary=retained-callable-ineligible
  $ reject spec-monomorphic artifacts/spec_result_consumer.cmt --dependency artifacts/provider.cmt
  spec-monomorphic=pre-sst-vir-rejected
  $ reject spec-generic artifacts/spec_generic_result_consumer.cmt --dependency artifacts/generic_spec_provider.cmt
  spec-generic=pre-sst-vir-rejected
  $ grep -q 'VERO_UNSUPPORTED_EXTERNAL_CALL' artifacts/spec-generic.out
  $ echo 'spec-generic-boundary=retained-callable-ineligible'
  spec-generic-boundary=retained-callable-ineligible
  $ reject finite artifacts/finite_result_consumer.cmt --dependency artifacts/provider.cmt
  finite=pre-sst-vir-rejected
  $ reject partial artifacts/partial_consumer.cmt --dependency artifacts/provider.cmt
  partial=pre-sst-vir-rejected
  $ reject callback artifacts/callback_consumer.cmt --dependency artifacts/provider.cmt
  callback=pre-sst-vir-rejected
  $ reject mutable artifacts/mutable_result_consumer.cmt --dependency artifacts/mutable_provider.cmt
  mutable=pre-sst-vir-rejected
  $ reject shared artifacts/shared_result_consumer.cmt --dependency artifacts/shared_provider.cmt
  shared=pre-sst-vir-rejected
  $ reject hidden artifacts/hidden_result_consumer.cmt --dependency artifacts/hidden_provider.cmt
  hidden=pre-sst-vir-rejected
  $ reject foreign artifacts/foreign_result_consumer.cmt --dependency artifacts/foreign_provider.cmt --dependency artifacts/hidden_provider.cmt
  foreign=pre-sst-vir-rejected
  $ for item in proof-monomorphic:proof_result_consumer spec-monomorphic:spec_result_consumer finite:finite_result_consumer partial:partial_consumer callback:callback_consumer; do label=${item%%:*}; name=${item#*:}; ./retained_exec_results_tool.exe reject "$label" "artifacts/$name.cmt" artifacts/provider.cmt; done
  proof-monomorphic boundary=pre-consumer-sst-vir backend-delta=0 solver-delta=0 z3-delta=0/0
  spec-monomorphic boundary=pre-consumer-sst-vir backend-delta=0 solver-delta=0 z3-delta=0/0
  finite boundary=pre-consumer-sst-vir backend-delta=0 solver-delta=0 z3-delta=0/0
  partial boundary=pre-consumer-sst-vir backend-delta=0 solver-delta=0 z3-delta=0/0
  callback boundary=pre-consumer-sst-vir backend-delta=0 solver-delta=0 z3-delta=0/0
  $ ./retained_exec_results_tool.exe reject proof-generic artifacts/proof_generic_result_consumer.cmt artifacts/generic_proof_provider.cmt
  proof-generic boundary=pre-consumer-sst-vir backend-delta=0 solver-delta=0 z3-delta=0/0
  $ ./retained_exec_results_tool.exe reject spec-generic artifacts/spec_generic_result_consumer.cmt artifacts/generic_spec_provider.cmt
  spec-generic boundary=pre-consumer-sst-vir backend-delta=0 solver-delta=0 z3-delta=0/0
  $ ./retained_exec_results_tool.exe reject mutable artifacts/mutable_result_consumer.cmt artifacts/mutable_provider.cmt
  mutable boundary=pre-consumer-sst-vir backend-delta=0 solver-delta=0 z3-delta=0/0
  $ ./retained_exec_results_tool.exe reject shared artifacts/shared_result_consumer.cmt artifacts/shared_provider.cmt
  shared boundary=pre-consumer-sst-vir backend-delta=0 solver-delta=0 z3-delta=0/0
  $ ./retained_exec_results_tool.exe reject hidden artifacts/hidden_result_consumer.cmt artifacts/hidden_provider.cmt
  hidden boundary=pre-consumer-sst-vir backend-delta=0 solver-delta=0 z3-delta=0/0
  $ ./retained_exec_results_tool.exe reject foreign artifacts/foreign_result_consumer.cmt artifacts/foreign_provider.cmt artifacts/hidden_provider.cmt
  foreign boundary=pre-consumer-sst-vir backend-delta=0 solver-delta=0 z3-delta=0/0
  $ ./retained_exec_results_tool.exe classify-closures
  closure-unsupported-shared classification=ineligible boundary=direct-classifier backend-delta=0 solver-delta=0 z3-delta=0/0
  closure-incomplete classification=ineligible boundary=direct-classifier backend-delta=0 solver-delta=0 z3-delta=0/0
  closure-foreign classification=ineligible boundary=direct-classifier backend-delta=0 solver-delta=0 z3-delta=0/0
  closure-malformed classification=rejected boundary=sst-validation backend-delta=0 solver-delta=0 z3-delta=0/0

Ordinary bytecode and native runtime behavior remains compiler-owned.

  $ mkdir artifacts/runtime
  $ ocamlc -w -A -alert -all -bin-annot -I "$GHOST" -ppx "$PPX" -c -o artifacts/runtime/provider.cmi fixtures/provider.mli
  $ ocamlc -w -A -alert -all -I "$GHOST" -I artifacts/runtime -ppx "$PPX" -c -o artifacts/runtime/provider.cmo fixtures/provider.ml
  $ ocamlc -w -A -alert -all -I "$GHOST" -I artifacts/runtime -ppx "$PPX" -c -o artifacts/runtime/runtime.cmo fixtures/runtime.ml
  $ ocamlc -o artifacts/runtime/runtime.byte artifacts/runtime/provider.cmo artifacts/runtime/runtime.cmo
  $ artifacts/runtime/runtime.byte
  7 8 3 3 3
  $ ocamlopt -w -A -alert -all -I "$GHOST" -I artifacts/runtime -ppx "$PPX" -c -o artifacts/runtime/provider.cmx fixtures/provider.ml
  $ ocamlopt -w -A -alert -all -I "$GHOST" -I artifacts/runtime -ppx "$PPX" -c -o artifacts/runtime/runtime.cmx fixtures/runtime.ml
  $ ocamlopt -o artifacts/runtime/runtime.native artifacts/runtime/provider.cmx artifacts/runtime/runtime.cmx
  $ artifacts/runtime/runtime.native
  7 8 3 3 3
