Ordinary dependency, retained-model, counterexample, and supported rejection
outcomes live in outcome_cases.ml. This specialist target retains private
handle/model authority, corrupted graph authentication, inherited-stale, and
installed API contracts listed in test/support/migrations/w09.md.

  $ mkdir -p artifacts/v1 artifacts/stale artifacts/lookalike artifacts/retained-model artifacts/handle-barrier
  $ export PPX="$PWD/../../ppx/vero_ppx.exe --keep-ghost"
  $ export GHOST="$PWD/../../runtime/.vero_ghost.objs/byte"
  $ compile () { (cd "$1" && ocamlc -w -A -alert -all -bin-annot -I "$GHOST" -ppx "$PPX" -c "$2"); }
  $ reject_code () { label=$1; expected=$2; shift 2; sst="artifacts/$label.sst"; vir="artifacts/$label.vir"; if OCAML_COLOR=never ../../src/verocaml.exe verify "$@" --timeout-ms 60000 --dump-sst "$sst" --dump-vir "$vir" >"artifacts/$label.out" 2>&1; then return 1; else code=$?; fi; test "$code" = 2; test ! -e "$sst"; test ! -e "$vir"; actual=$(grep -Eo 'VERO_[A-Z_]+' "artifacts/$label.out" | head -1); test "$actual" = "$expected"; printf '%s: code=%s\n' "$label" "$actual"; }
  $ verify_discovered () { label=$1; expected=$2; shift 2; sst="artifacts/$label.sst"; vir="artifacts/$label.vir"; OCAML_COLOR=never ../../src/verocaml.exe verify "$@" --timeout-ms 60000 --dump-sst "$sst" --dump-vir "$vir" >"artifacts/$label.out" 2>&1; test -s "$sst"; test -e "$vir"; actual=$(grep -c '^verocaml: verified dependency unit=' "artifacts/$label.out"); test "$actual" = "$expected"; printf '%s: verified-dependencies=%s\n' "$label" "$actual"; }

Private interface handles bind exact public types, callables, contracts, models,
visibility, and successful provider snapshots. Descriptor/backend matrices own
their detailed assertions internally; Cram does not snapshot counters or IDs.

  $ cp fixtures/model_dependency.ml fixtures/model_consumer.ml fixtures/model_failure_consumer.ml fixtures/model_excluded_consumer.ml fixtures/model_structural_consumer.ml artifacts/retained-model/
  $ for n in model_dependency model_consumer model_failure_consumer model_excluded_consumer model_structural_consumer; do compile artifacts/retained-model "$n.ml"; done
  $ ./verified_interfaces_tool.exe artifacts/retained-model/model_consumer.cmt artifacts/retained-model/model_dependency.cmt >/dev/null
  $ ./verified_interfaces_tool.exe retained-lifecycle artifacts/retained-model/model_consumer.cmt artifacts/retained-model/model_dependency.cmt >/dev/null
  $ ./verified_interfaces_tool.exe explicit-policy artifacts/retained-model/model_consumer.cmt artifacts/retained-model/model_dependency.cmt >/dev/null
  $ ./verified_interfaces_tool.exe retained-failure-teardown artifacts/retained-model/model_failure_consumer.cmt artifacts/retained-model/model_dependency.cmt >/dev/null
  $ ./verified_interfaces_tool.exe retained-generic-adapter-matrix >/dev/null
  $ ./verified_interfaces_tool.exe retained-generic-family-sealing-matrix >/dev/null
  $ ./verified_interfaces_tool.exe retained-backend-matrix >/dev/null
  $ ./verified_interfaces_tool.exe retained-closure-matrix >/dev/null
  $ ./verified_interfaces_tool.exe retained-reject excluded artifacts/retained-model/model_excluded_consumer.cmt artifacts/retained-model/model_dependency.cmt >/dev/null 2>&1
  $ ./verified_interfaces_tool.exe retained-reject selector artifacts/retained-model/model_structural_consumer.cmt artifacts/retained-model/model_dependency.cmt >/dev/null 2>&1
  $ echo verified-interface-private-model-authority=checked
  verified-interface-private-model-authority=checked

Recursive retained-model authority attacks remain private pre-consumer checks.

  $ cp fixtures/recursive_model_*.ml artifacts/retained-model/
  $ for n in recursive_model_dependency recursive_model_consumer recursive_model_structural_consumer recursive_model_finite_consumer recursive_model_constructor_consumer recursive_model_recursive_consumer recursive_model_invariant_consumer recursive_model_reveal_consumer; do compile artifacts/retained-model "$n.ml"; done
  $ for row in pattern finite constructor recursive invariant reveal; do case "$row" in pattern) fixture=recursive_model_structural_consumer;; finite) fixture=recursive_model_finite_consumer;; constructor) fixture=recursive_model_constructor_consumer;; recursive) fixture=recursive_model_recursive_consumer;; invariant) fixture=recursive_model_invariant_consumer;; reveal) fixture=recursive_model_reveal_consumer;; esac; ./verified_interfaces_tool.exe retained-reject "$row" "artifacts/retained-model/$fixture.cmt" artifacts/retained-model/recursive_model_dependency.cmt >/dev/null 2>&1; done

Three inherited historical rejection expectations now verify through their Dune
shapes at this base. They remain specialist observations pending owner review
and cannot silently become green assertions.

  $ cp fixtures/generic_model_dependency.ml fixtures/generic_model_consumer.ml fixtures/unrelated_generic_dependency.ml fixtures/unrelated_generic_consumer.ml artifacts/retained-model/
  $ for n in generic_model_dependency generic_model_consumer unrelated_generic_dependency unrelated_generic_consumer; do compile artifacts/retained-model "$n.ml"; done
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/retained-model/generic_model_consumer.cmt --dependency artifacts/retained-model/generic_model_dependency.cmt >/dev/null
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/retained-model/unrelated_generic_consumer.cmt --dependency artifacts/retained-model/unrelated_generic_dependency.cmt >/dev/null
  $ mkdir artifacts/generic-family
  $ cp fixtures/generic_family_provider.ml fixtures/generic_family_consumer.ml artifacts/generic-family/
  $ compile artifacts/generic-family generic_family_provider.ml
  $ compile artifacts/generic-family generic_family_consumer.ml
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/generic-family/generic_family_consumer.cmt --dependency artifacts/generic-family/generic_family_provider.cmt >/dev/null
  $ echo historical-generic-rejections=stale
  historical-generic-rejections=stale

Graph authentication rejects representative stale or lookalike units,
canonical-slot attacks, malformed artifacts, hidden retained contracts, and
failed dependency proofs before authority. Exact imports are discovered from
the compiler load path, identical explicit artifacts are deduplicated,
ordinary dependencies carry no semantic authority, abstract interfaces remain
usable, and prefix lookalikes remain ordinary verified units.

  $ cp fixtures/base.ml fixtures/middle.ml fixtures/consumer.ml artifacts/v1/
  $ compile artifacts/v1 base.ml
  $ compile artifacts/v1 middle.ml
  $ compile artifacts/v1 consumer.ml
  $ verify_discovered discovered-transitive 2 artifacts/v1/consumer.cmt --dependency artifacts/v1/middle.cmt
  discovered-transitive: verified-dependencies=2
  $ cp artifacts/v1/base.cmt artifacts/base-copy.cmt
  $ verify_discovered identical-explicit-copy 2 artifacts/v1/consumer.cmt --dependency artifacts/v1/base.cmt --dependency artifacts/base-copy.cmt --dependency artifacts/v1/middle.cmt
  identical-explicit-copy: verified-dependencies=2
  $ cp fixtures/base.ml artifacts/stale/base.ml
  $ chmod u+w artifacts/stale/base.ml
  $ printf '\nlet changed () = 0\n' >> artifacts/stale/base.ml
  $ compile artifacts/stale base.ml
  $ reject_code stale VERO_DEPENDENCY artifacts/v1/consumer.cmt --dependency artifacts/v1/middle.cmt --dependency artifacts/stale/base.cmt
  stale: code=VERO_DEPENDENCY
  $ cp fixtures/base.ml artifacts/lookalike/lookalike.ml
  $ compile artifacts/lookalike lookalike.ml
  $ reject_code lookalike VERO_DEPENDENCY artifacts/v1/consumer.cmt --dependency artifacts/v1/middle.cmt --dependency artifacts/lookalike/lookalike.cmt
  lookalike: code=VERO_DEPENDENCY
  $ mkdir artifacts/stdlib-prefix artifacts/camlinternal-prefix
  $ cat > artifacts/stdlib-prefix/stdlib_forgery.ml <<'INNER'
  > let forged () = 0
  > INNER
  $ cat > artifacts/stdlib-prefix/prefix_dependency.ml <<'INNER'
  > open Stdlib_forgery
  > let local () = 0
  > INNER
  $ cat > artifacts/stdlib-prefix/prefix_consumer.ml <<'INNER'
  > open Prefix_dependency
  > let local () = 0
  > INNER
  $ compile artifacts/stdlib-prefix stdlib_forgery.ml
  $ compile artifacts/stdlib-prefix prefix_dependency.ml
  $ compile artifacts/stdlib-prefix prefix_consumer.ml
  $ verify_discovered stdlib-prefix-lookalike 2 artifacts/stdlib-prefix/prefix_consumer.cmt --dependency artifacts/stdlib-prefix/prefix_dependency.cmt
  stdlib-prefix-lookalike: verified-dependencies=2
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/stdlib-prefix/prefix_consumer.cmt --timeout-ms 60000 --dependency artifacts/stdlib-prefix/prefix_dependency.cmt --dependency artifacts/stdlib-prefix/stdlib_forgery.cmt > artifacts/stdlib-prefix-supplied.out
  $ test "$(grep -c 'verified dependency' artifacts/stdlib-prefix-supplied.out)" = 2
  $ grep 'verified dependency unit=Stdlib_forgery ' artifacts/stdlib-prefix-supplied.out >/dev/null
  $ cat > artifacts/camlinternal-prefix/camlinternal_forgery.ml <<'INNER'
  > let forged () = 0
  > INNER
  $ cat > artifacts/camlinternal-prefix/prefix_dependency.ml <<'INNER'
  > open Camlinternal_forgery
  > let local () = 0
  > INNER
  $ cat > artifacts/camlinternal-prefix/prefix_consumer.ml <<'INNER'
  > open Prefix_dependency
  > let local () = 0
  > INNER
  $ compile artifacts/camlinternal-prefix camlinternal_forgery.ml
  $ compile artifacts/camlinternal-prefix prefix_dependency.ml
  $ compile artifacts/camlinternal-prefix prefix_consumer.ml
  $ verify_discovered camlinternal-prefix-lookalike 2 artifacts/camlinternal-prefix/prefix_consumer.cmt --dependency artifacts/camlinternal-prefix/prefix_dependency.cmt
  camlinternal-prefix-lookalike: verified-dependencies=2
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/camlinternal-prefix/prefix_consumer.cmt --timeout-ms 60000 --dependency artifacts/camlinternal-prefix/prefix_dependency.cmt --dependency artifacts/camlinternal-prefix/camlinternal_forgery.cmt > artifacts/camlinternal-prefix-supplied.out
  $ test "$(grep -c 'verified dependency' artifacts/camlinternal-prefix-supplied.out)" = 2
  $ grep 'verified dependency unit=Camlinternal_forgery ' artifacts/camlinternal-prefix-supplied.out >/dev/null
  $ consumer_crc=$(./verified_interfaces_tool.exe interface-digest artifacts/v1/consumer.cmt)
  $ ./verified_interfaces_tool.exe add-import artifacts/v1/base.cmt artifacts/base-consumer-back-edge.cmt Consumer "$consumer_crc"
  $ reject_code consumer-back-edge VERO_DEPENDENCY artifacts/v1/consumer.cmt --dependency artifacts/v1/middle.cmt --dependency artifacts/base-consumer-back-edge.cmt
  consumer-back-edge: code=VERO_DEPENDENCY
  $ ./handle_barrier_tool.exe artifacts/v1/consumer.cmt artifacts/v1/middle.cmt artifacts/base-consumer-back-edge.cmt >/dev/null
  $ ./verified_interfaces_tool.exe remove-import artifacts/v1/base.cmt artifacts/base-missing-stdlib.cmt Stdlib
  $ reject_code missing-canonical-slot VERO_DEPENDENCY artifacts/v1/consumer.cmt --dependency artifacts/v1/middle.cmt --dependency artifacts/base-missing-stdlib.cmt
  missing-canonical-slot: code=VERO_DEPENDENCY
  $ ./verified_interfaces_tool.exe set-import artifacts/v1/base.cmt artifacts/base-wrong-canonical.cmt CamlinternalFormatBasics 00000000000000000000000000000000
  $ reject_code wrong-canonical-slot VERO_DEPENDENCY artifacts/v1/consumer.cmt --dependency artifacts/v1/middle.cmt --dependency artifacts/base-wrong-canonical.cmt
  wrong-canonical-slot: code=VERO_DEPENDENCY
  $ ./verified_interfaces_tool.exe set-import artifacts/v1/base.cmt artifacts/base-crcless-canonical.cmt Stdlib none
  $ reject_code crcless-canonical-slot VERO_DEPENDENCY artifacts/v1/consumer.cmt --dependency artifacts/v1/middle.cmt --dependency artifacts/base-crcless-canonical.cmt
  crcless-canonical-slot: code=VERO_DEPENDENCY
  $ verify_discovered completed-import-closure 2 artifacts/v1/consumer.cmt --dependency artifacts/v1/base.cmt
  completed-import-closure: verified-dependencies=2
  $ printf 'not a cmt\n' > artifacts/malformed.cmt
  $ reject_code malformed VERO_DEPENDENCY artifacts/v1/consumer.cmt --dependency artifacts/malformed.cmt
  malformed: code=VERO_DEPENDENCY
  $ cp fixtures/no_ppx.ml fixtures/no_ppx_consumer.ml artifacts/
  $ (cd artifacts && ocamlc -w -A -alert -all -bin-annot -c no_ppx.ml)
  $ compile artifacts no_ppx_consumer.ml
  $ verify_discovered ordinary-no-ppx 0 artifacts/no_ppx_consumer.cmt --dependency artifacts/no_ppx.cmt
  ordinary-no-ppx: verified-dependencies=0
  $ cp fixtures/hidden.ml fixtures/hidden.mli fixtures/hidden_consumer.ml artifacts/
  $ (cd artifacts && ocamlc -w -A -alert -all -bin-annot -I "$GHOST" -ppx "$PPX" -c hidden.mli && ocamlc -w -A -alert -all -bin-annot -I "$GHOST" -ppx "$PPX" -c hidden.ml)
  $ compile artifacts hidden_consumer.ml
  $ verify_discovered abstract-interface 1 artifacts/hidden_consumer.cmt --dependency artifacts/hidden.cmt
  abstract-interface: verified-dependencies=1
  $ cp fixtures/hidden_contract.ml fixtures/hidden_contract_consumer.ml fixtures/hidden_nested_contract.ml fixtures/hidden_nested_contract_consumer.ml artifacts/
  $ for n in hidden_contract hidden_contract_consumer hidden_nested_contract hidden_nested_contract_consumer; do compile artifacts "$n.ml"; done
  $ reject_code hidden-contract VERO_UNSUPPORTED_STRUCTURE_ITEM artifacts/hidden_contract_consumer.cmt --dependency artifacts/hidden_contract.cmt
  hidden-contract: code=VERO_UNSUPPORTED_STRUCTURE_ITEM
  $ reject_code hidden-nested-contract VERO_UNSUPPORTED_STRUCTURE_ITEM artifacts/hidden_nested_contract_consumer.cmt --dependency artifacts/hidden_nested_contract.cmt
  hidden-nested-contract: code=VERO_UNSUPPORTED_STRUCTURE_ITEM
  $ cp fixtures/failed_proof.ml fixtures/failed_consumer.ml artifacts/
  $ compile artifacts failed_proof.ml
  $ compile artifacts failed_consumer.ml
  $ reject_code failed-proof VERO_DEPENDENCY artifacts/failed_consumer.cmt --dependency artifacts/failed_proof.cmt
  failed-proof: code=VERO_DEPENDENCY
  $ echo verified-interface-graph-authentication=checked
  verified-interface-graph-authentication=checked

Handle construction remains a graph-wide barrier after a later dependency
proof fails.

  $ cp fixtures/base.ml artifacts/handle-barrier/
  $ compile artifacts/handle-barrier base.ml
  $ cat > artifacts/handle-barrier/later_failure.ml <<'INNER'
  > open Base
  > let increment (value : int) = value + 1
  > INNER
  $ cat > artifacts/handle-barrier/barrier_consumer.ml <<'INNER'
  > open Later_failure
  > let local () = 0
  > INNER
  $ compile artifacts/handle-barrier later_failure.ml
  $ compile artifacts/handle-barrier barrier_consumer.ml
  $ ./handle_barrier_tool.exe artifacts/handle-barrier/barrier_consumer.cmt artifacts/handle-barrier/base.cmt artifacts/handle-barrier/later_failure.cmt >/dev/null

Cold installed API checks remain specialist package/API contracts. Compiler
wording is deliberately not asserted.

  $ root="${PWD%%/_build/*}"
  $ install_root="${VEROCAML_TEST_INSTALL_ROOT:-$root/_build/install/default}"
  $ core="$install_root/lib/verocaml/core"
  $ private_flags="$(find "$install_root/lib/verocaml" -type d -name '.private' -printf '%p\n' | sed 's/^/-I /' | tr '\n' ' ')"
  $ if ocamlfind ocamlc -package smtml,zarith,compiler-libs.common -c -I "$core" $private_flags fixtures/installed_handle_forgery.ml >/dev/null 2>&1; then false; fi
  $ if ocamlfind ocamlc -package smtml,zarith,compiler-libs.common -c -I "$core" $private_flags fixtures/installed_model_body_access.ml >/dev/null 2>&1; then false; fi
  $ OCAMLPATH="$install_root/lib:$OCAMLPATH" ocamlfind ocamlc -linkpkg -package verocaml.core fixtures/installed_legacy_error.ml -o artifacts/installed-legacy-error.exe 2>/dev/null
  $ artifacts/installed-legacy-error.exe >/dev/null
  $ cat > artifacts/solver_api_old_ok.ml <<'INNER'
  > let configured = Solver_backend.config ~timeout_ms:1
  > let classify = function Solver_backend.Inconclusive _ -> 3 | _ -> 0
  > INNER
  $ cat > artifacts/solver_api_old_record.ml <<'INNER'
  > let value = Solver_backend.Inconclusive { configured_timeout_ms = 1 }
  > let exact = function Solver_backend.Inconclusive { configured_timeout_ms } -> configured_timeout_ms | _ -> 0
  > INNER
  $ cat > artifacts/solver_api_typed_record.ml <<'INNER'
  > let configured = Solver_backend.config_with_rlimit ~timeout_ms:1 ~rlimit:1
  > let value = Solver_backend.Inconclusive { configured_timeout_ms = 1; configured_rlimit = 1; reason = Solver_backend.Resource_exhausted }
  > INNER
  $ OCAMLPATH="$install_root/lib:$OCAMLPATH" ocamlfind ocamlc -package verocaml.core -c artifacts/solver_api_old_ok.ml 2>/dev/null
  $ if OCAMLPATH="$install_root/lib:$OCAMLPATH" ocamlfind ocamlc -w +9 -warn-error +9 -package verocaml.core -c artifacts/solver_api_old_record.ml >/dev/null 2>&1; then false; fi
  $ OCAMLPATH="$install_root/lib:$OCAMLPATH" ocamlfind ocamlc -package verocaml.core -c artifacts/solver_api_typed_record.ml 2>/dev/null
