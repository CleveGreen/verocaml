Ordinary retained-result verification, counterexample, rejection, and parity
outcomes live in outcome_cases.ml. This specialist target retains private
classifier/zero-authority and ordinary-runtime contracts from
 test/support/migrations/w09.md.

No implementation source may appear outside the fixed fixture/tool inventory.

  $ if find . -name '*.ml' -not -path './artifacts/*' | grep -Ev '^./fixtures/|^./retained_exec_results_tool.ml$' >/dev/null; then false; fi

Build the artifact slices required by the private classifier probes.

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
  $ for name in consumer finite_result_consumer partial_consumer callback_consumer; do compile "$name"; done
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

Private retained-result classification rejects before backend/solver work. The
tool owns those detailed assertions; Cram does not snapshot their counters.

  $ ./retained_exec_results_tool.exe positive artifacts/consumer.cmt artifacts/provider.cmt >/dev/null
  $ for item in proof-monomorphic:proof_result_consumer spec-monomorphic:spec_result_consumer finite:finite_result_consumer partial:partial_consumer callback:callback_consumer; do label=${item%%:*}; name=${item#*:}; ./retained_exec_results_tool.exe reject "$label" "artifacts/$name.cmt" artifacts/provider.cmt >/dev/null; done
  $ ./retained_exec_results_tool.exe reject proof-generic artifacts/proof_generic_result_consumer.cmt artifacts/generic_proof_provider.cmt >/dev/null
  $ ./retained_exec_results_tool.exe reject spec-generic artifacts/spec_generic_result_consumer.cmt artifacts/generic_spec_provider.cmt >/dev/null
  $ ./retained_exec_results_tool.exe reject mutable artifacts/mutable_result_consumer.cmt artifacts/mutable_provider.cmt >/dev/null
  $ ./retained_exec_results_tool.exe reject shared artifacts/shared_result_consumer.cmt artifacts/shared_provider.cmt >/dev/null
  $ ./retained_exec_results_tool.exe reject hidden artifacts/hidden_result_consumer.cmt artifacts/hidden_provider.cmt >/dev/null
  $ ./retained_exec_results_tool.exe reject foreign artifacts/foreign_result_consumer.cmt artifacts/foreign_provider.cmt artifacts/hidden_provider.cmt >/dev/null
  $ ./retained_exec_results_tool.exe classify-closures >/dev/null
  $ echo retained-result-private-boundaries=checked
  retained-result-private-boundaries=checked

Ordinary bytecode and native execution remain compiler-owned and agree after
ghost erasure.

  $ mkdir artifacts/runtime
  $ ocamlc -w -A -alert -all -bin-annot -I "$GHOST" -ppx "$PPX" -c -o artifacts/runtime/provider.cmi fixtures/provider.mli
  $ ocamlc -w -A -alert -all -I "$GHOST" -I artifacts/runtime -ppx "$PPX" -c -o artifacts/runtime/provider.cmo fixtures/provider.ml
  $ ocamlc -w -A -alert -all -I "$GHOST" -I artifacts/runtime -ppx "$PPX" -c -o artifacts/runtime/runtime.cmo fixtures/runtime.ml
  $ ocamlc -o artifacts/runtime/runtime.byte artifacts/runtime/provider.cmo artifacts/runtime/runtime.cmo
  $ artifacts/runtime/runtime.byte > artifacts/runtime/byte.out
  $ ocamlopt -w -A -alert -all -I "$GHOST" -I artifacts/runtime -ppx "$PPX" -c -o artifacts/runtime/provider.cmx fixtures/provider.ml
  $ ocamlopt -w -A -alert -all -I "$GHOST" -I artifacts/runtime -ppx "$PPX" -c -o artifacts/runtime/runtime.cmx fixtures/runtime.ml
  $ ocamlopt -o artifacts/runtime/runtime.native artifacts/runtime/provider.cmx artifacts/runtime/runtime.cmx
  $ artifacts/runtime/runtime.native > artifacts/runtime/native.out
  $ cmp artifacts/runtime/byte.out artifacts/runtime/native.out
