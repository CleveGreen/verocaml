A verified external specification may bind to an exact value in one imported
external target CMI. Project discovery reads the target CMT containing
unsupported objects and exceptions, but the target body never enters semantic
processing and project selection reports it as skipped.

  $ mkdir artifacts
  $ export GHOST="$PWD/../../runtime/.vero_ghost.objs/byte"
  $ export PPX_ORDINARY="$PWD/../../ppx/vero_ppx.exe"
  $ export PPX_RETAINED="$PWD/../../ppx/vero_ppx.exe --keep-ghost"
  $ ordinary () { name=$1; shift; ocamlc -w -A -alert -all -bin-annot -I "$GHOST" -I artifacts -ppx "$PPX_ORDINARY" -c -o "artifacts/$name.$1" "$2"; }
  $ retained () { name=$1; ocamlc -w -A -alert -all -bin-annot -I "$GHOST" -I artifacts -ppx "$PPX_RETAINED" -c -o "artifacts/$name.cmo" "fixtures/$name.ml"; }
  $ ordinary legacy cmi fixtures/legacy.mli
  $ ordinary legacy cmo fixtures/legacy.ml
  $ ordinary curried_legacy cmi fixtures/curried_legacy.mli
  $ ordinary curried_legacy cmo fixtures/curried_legacy.ml
  $ retained curried_consumer
  $ retained explicit_type_consumer
  $ ordinary mode_curried_legacy cmi fixtures/mode_curried_legacy.mli
  $ ordinary mode_curried_legacy cmo fixtures/mode_curried_legacy.ml
  $ ocamlc -w -A -alert -all -bin-annot -I "$GHOST" -I artifacts -ppx "$PPX_RETAINED" -c -o artifacts/consumer.cmi fixtures/consumer.mli
  $ retained consumer
  $ ocamlc -w -A -alert -all -bin-annot -I "$GHOST" -I artifacts -ppx "$PPX_RETAINED" -c -o artifacts/alpha_consumer.cmi fixtures/alpha_consumer.mli
  $ retained alpha_consumer
  $ ordinary other_legacy cmi fixtures/other_legacy.mli
  $ ordinary other_legacy cmo fixtures/other_legacy.ml
  $ ./external_target_specifications_tool.exe positive 1 artifacts/consumer.cmt artifacts/consumer.cmi artifacts/legacy.cmt artifacts/legacy.cmi
  positive threads=1 consumer=verified target=skipped uses=5 functions=5 obligations=2 target-cmt=discovered target-semantic-lowering=0 verified-provider-processing=0 completion=none live-authority=0
  $ ./external_target_specifications_tool.exe positive 2 artifacts/consumer.cmt artifacts/consumer.cmi artifacts/legacy.cmt artifacts/legacy.cmi
  positive threads=2 consumer=verified target=skipped uses=5 functions=5 obligations=2 target-cmt=discovered target-semantic-lowering=0 verified-provider-processing=0 completion=none live-authority=0
  $ ./external_target_specifications_tool.exe curried-positive artifacts/curried_consumer.cmt artifacts/curried_consumer.cmi artifacts/curried_legacy.cmt artifacts/curried_legacy.cmi
  positive threads=1 consumer=verified target=skipped uses=1 functions=1 obligations=0 target-cmt=discovered target-semantic-lowering=0 verified-provider-processing=0 completion=none live-authority=0
  $ ./external_target_specifications_tool.exe curried-positive artifacts/explicit_type_consumer.cmt artifacts/explicit_type_consumer.cmi artifacts/legacy.cmt artifacts/legacy.cmi
  positive threads=1 consumer=verified target=skipped uses=1 functions=1 obligations=0 target-cmt=discovered target-semantic-lowering=0 verified-provider-processing=0 completion=none live-authority=0
  $ test $(strings artifacts/explicit_type_consumer.cmt | grep -c 'verocaml.internal.compiler_mode_syntax') -eq 0
  $ OCAML_COLOR=never ../../src/verocaml.exe verify-project --root artifacts/curried_consumer.cmt artifacts/curried_consumer.cmi --dependency artifacts/curried_legacy.cmt artifacts/curried_legacy.cmi --threads 1 --timeout-ms 60000 > artifacts/curried-project.out
  $ grep -c '^verocaml: trusted external specification trust=imported-unverified-target target-unit=Curried_legacy target-interface-digest=.* target-path=Curried_legacy.curried' artifacts/curried-project.out
  1
  $ grep '^verocaml: verified unit=Curried_consumer\|^verocaml: skipped unit=Curried_legacy' artifacts/curried-project.out
  verocaml: verified unit=Curried_consumer file=artifacts/curried_consumer.cmt result=verified functions=1 obligations=0
  verocaml: skipped unit=Curried_legacy file=artifacts/curried_legacy.cmt result=skipped
  $ ./external_target_specifications_tool.exe metadata artifacts/consumer.cmt artifacts/consumer.cmi artifacts/legacy.cmt artifacts/legacy.cmi
  metadata-negatives wrong-unit/path/value/uid=rejected pre-summary-authority
  $ ./external_target_specifications_tool.exe wrong-cmi artifacts/consumer.cmt artifacts/consumer.cmi artifacts/other_legacy.cmt artifacts/other_legacy.cmi
  wrong-CMI/unit artifact=rejected pre-summary-authority
  $ ./external_target_specifications_tool.exe authority artifacts/consumer.cmt artifacts/consumer.cmi artifacts/legacy.cmt artifacts/legacy.cmi
  authority-negatives raw/forged/copied/replayed/swapped/stale=rejected private-delta=0
  $ ./external_target_specifications_tool.exe alpha-cross artifacts/consumer.cmt artifacts/consumer.cmi artifacts/alpha_consumer.cmt artifacts/alpha_consumer.cmi artifacts/legacy.cmt artifacts/legacy.cmi
  alpha-cross-artifact substitution=rejected semantic-abi=equal private-delta=0

A retained library may publish a hidden external-function specification.  A
direct consumer imports the provider contract implicitly, while the ordinary
target remains an opaque dependency and no command-line dependency flags are
needed.  Merely importing the provider does not activate the target contract,
and two providers for one exact target reject rather than selecting by order.

  $ for name in provider_external provider_external_duplicate; do ocamlc -w -A -alert -all -bin-annot -I "$GHOST" -I artifacts -ppx "$PPX_RETAINED" -c -o "artifacts/$name.cmi" "fixtures/$name.mli"; retained "$name"; done
  $ for name in provider_external_consumer provider_external_only_consumer provider_external_inactive_overlap_consumer provider_external_overlap_consumer; do retained "$name"; done
  $ for threads in 1 2; do (cd artifacts && OCAML_COLOR=never ../../../src/verocaml.exe verify provider_external_consumer.cmt --threads "$threads" --timeout-ms 60000) >"artifacts/provider-external.$threads.out"; done
  $ cmp artifacts/provider-external.1.out artifacts/provider-external.2.out
  $ grep '^verocaml: verified dependency unit=Provider_external ' artifacts/provider-external.1.out | sed -E 's/interface-digest=[0-9a-f]+/interface-digest=<digest>/'
  verocaml: verified dependency unit=Provider_external interface-digest=<digest> direct=none transitive=none trust=none
  $ grep -c '^verocaml: trusted external specification trust=imported-unverified-target target-unit=Legacy .* target-path=Legacy.promised ' artifacts/provider-external.1.out
  1
  $ grep '^verocaml: verified-with-trusted-axioms file=provider_external_consumer.cmt functions=1 obligations=2 trusted-external-spec-uses=1$' artifacts/provider-external.1.out
  verocaml: verified-with-trusted-axioms file=provider_external_consumer.cmt functions=1 obligations=2 trusted-external-spec-uses=1
  $ (cd artifacts && OCAML_COLOR=never ../../../src/verocaml.exe verify provider_external_only_consumer.cmt --threads 1 --timeout-ms 60000) > artifacts/provider-only.out
  $ test "$(grep -c '^verocaml: trusted external specification' artifacts/provider-only.out)" = 0
  $ grep '^verocaml: verified file=provider_external_only_consumer.cmt functions=1 obligations=1$' artifacts/provider-only.out
  verocaml: verified file=provider_external_only_consumer.cmt functions=1 obligations=1
  $ (cd artifacts && OCAML_COLOR=never ../../../src/verocaml.exe verify provider_external_inactive_overlap_consumer.cmt --threads 1 --timeout-ms 60000) > artifacts/provider-inactive-overlap.out
  $ test "$(grep -c '^verocaml: trusted external specification' artifacts/provider-inactive-overlap.out)" = 0; grep -q '^verocaml: verified file=provider_external_inactive_overlap_consumer.cmt '; echo provider-inactive-overlap=verified-without-target-authority
  provider-inactive-overlap=verified-without-target-authority
  $ code=0; (cd artifacts && OCAML_COLOR=never ../../../src/verocaml.exe verify provider_external_overlap_consumer.cmt --threads 1 --timeout-ms 60000) > artifacts/provider-overlap.out 2>&1 || code=$?; test "$code" = 2
  $ grep -q 'overlapping external function specifications' artifacts/provider-overlap.out; grep -q 'Provider_external' artifacts/provider-overlap.out; grep -q 'Provider_external_duplicate' artifacts/provider-overlap.out; grep -q 'target Legacy.promised / Legacy.promised' artifacts/provider-overlap.out
  $ ocamlc -w -A -alert -all -bin-annot -I "$GHOST" -I artifacts -ppx "$PPX_RETAINED" -c -o artifacts/provider_external_used.cmi fixtures/provider_external_used.mli
  $ retained provider_external_used
  $ retained provider_external_used_consumer
  $ code=0; (cd artifacts && OCAML_COLOR=never ../../../src/verocaml.exe verify provider_external_used_consumer.cmt --threads 1 --timeout-ms 60000) > artifacts/provider-used.out 2>&1 || code=$?; test "$code" = 2
  $ grep -q 'unit Provider_external_used: retained provider lacks private-driver completion' artifacts/provider-used.out

The project report exposes one distinct verified-external-specification trust
row per call, while the imported external target remains skipped rather than
verified-dependency.

  $ OCAML_COLOR=never ../../src/verocaml.exe verify-project --root artifacts/consumer.cmt artifacts/consumer.cmi --dependency artifacts/legacy.cmt artifacts/legacy.cmi --threads 1 --timeout-ms 60000 > artifacts/project.out
  $ OCAML_COLOR=never ../../src/verocaml.exe verify-project --root artifacts/consumer.cmt artifacts/consumer.cmi --dependency artifacts/legacy.cmt artifacts/legacy.cmi --threads 2 --timeout-ms 60000 > artifacts/project-threaded.out
  $ cmp artifacts/project.out artifacts/project-threaded.out
  $ grep -c '^verocaml: trusted external specification trust=imported-unverified-target target-unit=Legacy target-interface-digest=.* target-path=Legacy\.' artifacts/project.out
  5
  $ grep '^verocaml: verified unit=Consumer\|^verocaml: skipped unit=Legacy' artifacts/project.out
  verocaml: verified unit=Consumer file=artifacts/consumer.cmt result=verified functions=5 obligations=2
  verocaml: skipped unit=Legacy file=artifacts/legacy.cmt result=skipped
  $ test $(grep -c 'verified-dependency unit=Legacy' artifacts/project.out) -eq 0
  $ test $(grep -c 'target-body=unverified result=constrained-only-by-ensures' artifacts/project.out) -eq 5

Missing, premature, competing, aliased, open, module-aliased, partial, callback,
and mode-bearing summaries all reject without backend, solver, Z3, target-body
semantic processing, verified-provider processing, or leaked private authority.

  $ reject_semantic () { name=$1; target=$2; retained "$name" || return 1; ./external_target_specifications_tool.exe rejected "artifacts/$name.cmt" "artifacts/$name.cmi" "artifacts/$target.cmt" "artifacts/$target.cmi" || return 1; }
  $ reject_mode () { name=$1; target=$2; reject_semantic "$name" "$target" || return 1; code=0; OCAML_COLOR=never ../../src/verocaml.exe verify-project --root "artifacts/$name.cmt" "artifacts/$name.cmi" --dependency "artifacts/$target.cmt" "artifacts/$target.cmi" --threads 1 --timeout-ms 60000 >"artifacts/$name.project.out" 2>&1 || code=$?; if test "$code" != 2; then cat "artifacts/$name.project.out"; return 1; fi; trusted=$(grep -c '^verocaml: trusted external specification' "artifacts/$name.project.out" || true); if test "$trusted" != 0; then cat "artifacts/$name.project.out"; return 1; fi; diagnostic=$(grep -o 'VERO_[A-Z_]*' "artifacts/$name.project.out" | head -1 || true); if test "$diagnostic" != 'VERO_MALFORMED_GHOST_CALL'; then cat "artifacts/$name.project.out"; return 1; fi; echo "$name project-exit=2 diagnostic=VERO_MALFORMED_GHOST_CALL trust=0"; }
  $ for name in missing_summary call_before_summary duplicate_summary local_alias open_path module_alias functor_path wrong_order specialized_generic partial callback; do reject_semantic "$name" legacy || exit 1; done
  Missing_summary rejected pre-SST/VIR/solver target=skipped target-semantic-lowering=0 trust=0 authority-delta=0
  Call_before_summary rejected pre-SST/VIR/solver target=skipped target-semantic-lowering=0 trust=0 authority-delta=0
  Duplicate_summary rejected pre-SST/VIR/solver target=skipped target-semantic-lowering=0 trust=0 authority-delta=0
  Local_alias rejected pre-SST/VIR/solver target=skipped target-semantic-lowering=0 trust=0 authority-delta=0
  Open_path rejected pre-SST/VIR/solver target=skipped target-semantic-lowering=0 trust=0 authority-delta=0
  Module_alias rejected pre-SST/VIR/solver target=skipped target-semantic-lowering=0 trust=0 authority-delta=0
  Functor_path rejected pre-SST/VIR/solver target=skipped target-semantic-lowering=0 trust=0 authority-delta=0
  Wrong_order rejected pre-SST/VIR/solver target=skipped target-semantic-lowering=0 trust=0 authority-delta=0
  Specialized_generic rejected pre-SST/VIR/solver target=skipped target-semantic-lowering=0 trust=0 authority-delta=0
  Partial rejected pre-SST/VIR/solver target=skipped target-semantic-lowering=0 trust=0 authority-delta=0
  Callback rejected pre-SST/VIR/solver target=skipped target-semantic-lowering=0 trust=0 authority-delta=0
  $ for name in mode_summary_ghost mode_summary_result_ghost mode_summary_unique_parameter mode_summary_unique_result; do reject_mode "$name" legacy || exit 1; done
  Mode_summary_ghost rejected pre-SST/VIR/solver target=skipped target-semantic-lowering=0 trust=0 authority-delta=0
  mode_summary_ghost project-exit=2 diagnostic=VERO_MALFORMED_GHOST_CALL trust=0
  Mode_summary_result_ghost rejected pre-SST/VIR/solver target=skipped target-semantic-lowering=0 trust=0 authority-delta=0
  mode_summary_result_ghost project-exit=2 diagnostic=VERO_MALFORMED_GHOST_CALL trust=0
  Mode_summary_unique_parameter rejected pre-SST/VIR/solver target=skipped target-semantic-lowering=0 trust=0 authority-delta=0
  mode_summary_unique_parameter project-exit=2 diagnostic=VERO_MALFORMED_GHOST_CALL trust=0
  Mode_summary_unique_result rejected pre-SST/VIR/solver target=skipped target-semantic-lowering=0 trust=0 authority-delta=0
  mode_summary_unique_result project-exit=2 diagnostic=VERO_MALFORMED_GHOST_CALL trust=0
  $ for name in mode_summary_result_local mode_summary_result_once mode_summary_parameter_local; do reject_mode "$name" legacy || exit 1; done
  Mode_summary_result_local rejected pre-SST/VIR/solver target=skipped target-semantic-lowering=0 trust=0 authority-delta=0
  mode_summary_result_local project-exit=2 diagnostic=VERO_MALFORMED_GHOST_CALL trust=0
  Mode_summary_result_once rejected pre-SST/VIR/solver target=skipped target-semantic-lowering=0 trust=0 authority-delta=0
  mode_summary_result_once project-exit=2 diagnostic=VERO_MALFORMED_GHOST_CALL trust=0
  Mode_summary_parameter_local rejected pre-SST/VIR/solver target=skipped target-semantic-lowering=0 trust=0 authority-delta=0
  mode_summary_parameter_local project-exit=2 diagnostic=VERO_MALFORMED_GHOST_CALL trust=0

Every curried arrow result is authenticated in ordinal order.  Unlabelled,
labelled, and optional wrapper mismatches, plus a mode-bearing target
intermediate result, reject through the same pre-semantic boundary.

  $ for name in mode_summary_curried_unique_result mode_summary_labelled_curried_unique_result mode_summary_optional_curried_unique_result; do reject_mode "$name" curried_legacy || exit 1; done
  Mode_summary_curried_unique_result rejected pre-SST/VIR/solver target=skipped target-semantic-lowering=0 trust=0 authority-delta=0
  mode_summary_curried_unique_result project-exit=2 diagnostic=VERO_MALFORMED_GHOST_CALL trust=0
  Mode_summary_labelled_curried_unique_result rejected pre-SST/VIR/solver target=skipped target-semantic-lowering=0 trust=0 authority-delta=0
  mode_summary_labelled_curried_unique_result project-exit=2 diagnostic=VERO_MALFORMED_GHOST_CALL trust=0
  Mode_summary_optional_curried_unique_result rejected pre-SST/VIR/solver target=skipped target-semantic-lowering=0 trust=0 authority-delta=0
  mode_summary_optional_curried_unique_result project-exit=2 diagnostic=VERO_MALFORMED_GHOST_CALL trust=0
  $ for name in mode_summary_curried_local_result mode_summary_labelled_parameter_local mode_summary_optional_parameter_once mode_summary_labelled_curried_local_result mode_summary_optional_curried_once_result; do reject_mode "$name" curried_legacy || exit 1; done
  Mode_summary_curried_local_result rejected pre-SST/VIR/solver target=skipped target-semantic-lowering=0 trust=0 authority-delta=0
  mode_summary_curried_local_result project-exit=2 diagnostic=VERO_MALFORMED_GHOST_CALL trust=0
  Mode_summary_labelled_parameter_local rejected pre-SST/VIR/solver target=skipped target-semantic-lowering=0 trust=0 authority-delta=0
  mode_summary_labelled_parameter_local project-exit=2 diagnostic=VERO_MALFORMED_GHOST_CALL trust=0
  Mode_summary_optional_parameter_once rejected pre-SST/VIR/solver target=skipped target-semantic-lowering=0 trust=0 authority-delta=0
  mode_summary_optional_parameter_once project-exit=2 diagnostic=VERO_MALFORMED_GHOST_CALL trust=0
  Mode_summary_labelled_curried_local_result rejected pre-SST/VIR/solver target=skipped target-semantic-lowering=0 trust=0 authority-delta=0
  mode_summary_labelled_curried_local_result project-exit=2 diagnostic=VERO_MALFORMED_GHOST_CALL trust=0
  Mode_summary_optional_curried_once_result rejected pre-SST/VIR/solver target=skipped target-semantic-lowering=0 trust=0 authority-delta=0
  mode_summary_optional_curried_once_result project-exit=2 diagnostic=VERO_MALFORMED_GHOST_CALL trust=0
  $ reject_mode mode_target_curried_unique_result mode_curried_legacy
  Mode_target_curried_unique_result rejected pre-SST/VIR/solver target=skipped target-semantic-lowering=0 trust=0 authority-delta=0
  mode_target_curried_unique_result project-exit=2 diagnostic=VERO_MALFORMED_GHOST_CALL trust=0
  $ ./external_target_specifications_tool.exe compiler-mode-compatibility
  compiler-callable-mode target-exact open/nondefault=rejected wrapper-compatible shared-open=accepted closed-nondefault=all-slots-rejected slots=2+2 originals=unchanged

External-target mode metadata is rejected before semantic validation as well.

  $ ordinary mode_legacy cmi fixtures/mode_legacy.mli
  $ ordinary mode_legacy cmo fixtures/mode_legacy.ml
  $ reject_mode mode_target_consumer mode_legacy
  Mode_target_consumer rejected pre-SST/VIR/solver target=skipped target-semantic-lowering=0 trust=0 authority-delta=0
  mode_target_consumer project-exit=2 diagnostic=VERO_MALFORMED_GHOST_CALL trust=0

The presence-only compiler-mode syntax marker is authenticated in the retained
CMT verifier input and absent from interfaces, ordinary code artifacts, and
byte/native runtime behavior.

  $ mkdir artifacts/mode-marker-erasure
  $ ocamlc -w -A -alert -all -bin-annot -I "$GHOST" -I artifacts -ppx "$PPX_ORDINARY" -c -o artifacts/mode-marker-erasure/mode_summary_result_local.cmo fixtures/mode_summary_result_local.ml
  $ ocamlopt -w -A -alert -all -bin-annot -I "$GHOST" -I artifacts -ppx "$PPX_ORDINARY" -c -o artifacts/mode-marker-erasure/legacy.cmx fixtures/legacy.ml
  $ ocamlopt -w -A -alert -all -bin-annot -I "$GHOST" -I artifacts -ppx "$PPX_ORDINARY" -c -o artifacts/mode-marker-erasure/mode_summary_result_local.cmx fixtures/mode_summary_result_local.ml
  $ printf 'let () = Printf.printf "%%d\\n" (Mode_summary_result_local.use 41)\n' > artifacts/mode-marker-erasure/app.ml
  $ ocamlc -I artifacts/mode-marker-erasure -I artifacts -o artifacts/mode-marker-erasure/app.byte artifacts/legacy.cmo artifacts/mode-marker-erasure/mode_summary_result_local.cmo artifacts/mode-marker-erasure/app.ml
  $ ocamlopt -I artifacts/mode-marker-erasure -I artifacts -o artifacts/mode-marker-erasure/app.native artifacts/mode-marker-erasure/legacy.cmx artifacts/mode-marker-erasure/mode_summary_result_local.cmx artifacts/mode-marker-erasure/app.ml
  $ artifacts/mode-marker-erasure/app.byte > artifacts/mode-marker-erasure/byte.out
  $ artifacts/mode-marker-erasure/app.native > artifacts/mode-marker-erasure/native.out
  $ cmp artifacts/mode-marker-erasure/byte.out artifacts/mode-marker-erasure/native.out
  $ cat artifacts/mode-marker-erasure/byte.out
  42
  $ ./external_target_specifications_tool.exe marker-provenance artifacts/mode_summary_result_local.cmt artifacts/mode_summary_result_local.cmi artifacts/mode-marker-erasure/mode_summary_result_local.cmt artifacts/mode-marker-erasure/mode_summary_result_local.cmi
  compiler-mode-syntax-marker retained-cmt=1 ordinary-cmt=0 ghost-empty=authenticated
  $ test $(strings artifacts/mode_summary_result_local.cmt | grep -c 'verocaml.internal.compiler_mode_syntax') -eq 1
  $ for file in artifacts/mode_summary_result_local.{cmi,cmo} artifacts/mode-marker-erasure/mode_summary_result_local.{cmi,cmo,cmx} artifacts/mode-marker-erasure/app.{byte,native}; do if strings "$file" | grep -q 'verocaml.internal.compiler_mode_syntax'; then echo "marker leaked: $file"; exit 1; fi; done

Compiler-enforced complete ABI negatives execute too: wrong optional forwarding,
unknown/missing/duplicate labels, result mismatch, and Tracked summaries never
mint an artifact or private handle.

  $ compiler_reject () { name=$1; code=0; OCAML_COLOR=never ocamlc -w -A -alert -all -bin-annot -I "$GHOST" -I artifacts -ppx "$PPX_RETAINED" -c -o "artifacts/$name.cmo" "fixtures/$name.ml" >"artifacts/$name.err" 2>&1 || code=$?; test "$code" != 0; test ! -e "artifacts/$name.cmt"; echo "$name: compiler-rejected"; }
  $ for name in wrong_optional_forward unknown_label missing_label duplicate_label wrong_result wrong_type wrong_arity mode_summary_tracked; do compiler_reject "$name"; done
  wrong_optional_forward: compiler-rejected
  unknown_label: compiler-rejected
  missing_label: compiler-rejected
  duplicate_label: compiler-rejected
  wrong_result: compiler-rejected
  wrong_type: compiler-rejected
  wrong_arity: compiler-rejected
  mode_summary_tracked: compiler-rejected

A raw carrier is not accepted as private authority. A stale target pair changes
the consumer import CRC and rejects before any trusted row.

  $ ocamlc -w -A -alert -all -bin-annot -I artifacts -c -o artifacts/raw_carrier.cmo fixtures/raw_carrier.ml
  $ code=0; OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/raw_carrier.cmt --threads 1 >artifacts/raw.out 2>&1 || code=$?; test "$code" = 2
  $ grep -o 'VERO_[A-Z_]*' artifacts/raw.out | head -1
  VERO_DEPENDENCY
  $ mkdir stale
  $ cp fixtures/legacy.mli fixtures/legacy.ml stale/
  $ chmod u+w stale/legacy.mli stale/legacy.ml
  $ printf '\nval changed : int\n' >> stale/legacy.mli
  $ printf '\nlet changed = 0\n' >> stale/legacy.ml
  $ (cd stale && ocamlc -w -A -alert -all -bin-annot -I "$GHOST" -ppx "$PPX_ORDINARY" -c legacy.mli && ocamlc -w -A -alert -all -bin-annot -I "$GHOST" -ppx "$PPX_ORDINARY" -c legacy.ml)
  $ code=0; OCAML_COLOR=never ../../src/verocaml.exe verify-project --root artifacts/consumer.cmt artifacts/consumer.cmi --dependency stale/legacy.cmt stale/legacy.cmi --threads 1 >artifacts/stale.out 2>&1 || code=$?; test "$code" = 2
  $ test $(grep -c '^verocaml: trusted external specification trust=imported-unverified-target' artifacts/stale.out) -eq 0
  $ grep -o 'VERO_[A-Z_]*' artifacts/stale.out | head -1
  VERO_DEPENDENCY
