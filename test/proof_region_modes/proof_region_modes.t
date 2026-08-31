The remaining Cram lane exercises retained-carrier, copied-artifact, and
private remapping architecture that is intentionally outside ordinary outcomes.

  $ mkdir artifacts
  $ retained () { name=$1; ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$name.cmo" "fixtures/$name.ml"; }
  $ retained checked_unique_empty_return
  $ ./proof_region_modes_tool.exe solve artifacts/checked_unique_empty_return.cmt
  lemma_empty_stack=verified:0
  empty_stack=verified:2
  capture-issued=1 remapped=1 proof-sst=1 forgetting=1 recursive=0 backend=2 z3=2/2
  $ cp artifacts/checked_unique_empty_return.cmt artifacts/copied_checked_unique_empty_return.cmt
  $ cp artifacts/checked_unique_empty_return.cmi artifacts/copied_checked_unique_empty_return.cmi
  $ cmp artifacts/checked_unique_empty_return.cmt artifacts/copied_checked_unique_empty_return.cmt && cmp artifacts/checked_unique_empty_return.cmi artifacts/copied_checked_unique_empty_return.cmi && echo 'copied retained CMT/CMI are byte-identical'
  copied retained CMT/CMI are byte-identical
  $ ./proof_region_modes_tool.exe solve artifacts/checked_unique_empty_return.cmt > artifacts/original.solve
  $ ./proof_region_modes_tool.exe solve artifacts/copied_checked_unique_empty_return.cmt > artifacts/copied.solve
  $ cmp artifacts/original.solve artifacts/copied.solve && echo 'copied retained verification obligations/counters are identical'
  copied retained verification obligations/counters are identical
  $ ./proof_region_modes_tool.exe reload-cmi artifacts/checked_unique_empty_return.cmi > artifacts/original.cmi-reload
  $ ./proof_region_modes_tool.exe reload-cmi artifacts/copied_checked_unique_empty_return.cmi > artifacts/copied.cmi-reload
  $ cmp artifacts/original.cmi-reload artifacts/copied.cmi-reload && cat artifacts/copied.cmi-reload
  cmi=Checked_unique_empty_return imports=4
  $ ./proof_region_modes_tool.exe direct-solve artifacts/checked_unique_empty_return.cmt
  direct-z3=verified
  capture-issued=1 remapped=1 proof-sst=1 forgetting=1 recursive=0 backend=0 z3=1/1
  $ ./proof_region_modes_tool.exe observe artifacts/checked_unique_empty_return.cmt > artifacts/checked.observe
  $ grep '^capture ' artifacts/checked.observe | sed -E 's/ region=[0-9]+-[0-9]+//'
  capture callable=empty_stack#2 binding=empty#1 mode=Exec uniqueness=unique synthetic=ghost:0/tracked:0
  $ grep '^forgetting-edge ' artifacts/checked.observe | sed -E 's/source=[^ ]+/source=FIXTURE/'
  forgetting-edge source=FIXTURE caller=empty_stack#2 callee=lemma_empty_stack#1 callee-mode=Proof recursive=false formal=0 incoming=Exec authenticated=true formal-mode=Ghost boundary-result-mode=Ghost callee-result-mode=Ghost edges=1 synthetic-ghost=0 synthetic-tracked=0
  $ tail -1 artifacts/checked.observe
  capture-issued=1 remapped=1 proof-sst=1 forgetting=1 recursive=0 backend=0 z3=0/0

The focused positive matrix covers zero captures, unique return and mutation,
Tracked and ordinary locals, multiple parameter/local captures, destructuring,
both match branches, nested lets, and the exact inner same-name shadow.
Capture rows are in source declaration order.

  $ retained positive_matrix
  $ ./proof_region_modes_tool.exe observe artifacts/positive_matrix.cmt > artifacts/matrix.observe
  $ grep '^capture ' artifacts/matrix.observe | sed -E 's/ region=[0-9]+-[0-9]+//'
  capture callable=unique_return#3 binding=result#1 mode=Exec uniqueness=unique synthetic=ghost:0/tracked:0
  capture callable=unique_mutation#4 binding=box#0 mode=Exec uniqueness=unique synthetic=ghost:0/tracked:0
  capture callable=mode_locals#5 binding=ordinary#1 mode=Exec uniqueness=unique synthetic=ghost:0/tracked:0
  capture callable=mode_locals#5 binding=tracked#2 mode=Tracked uniqueness=aliased synthetic=ghost:0/tracked:0
  capture callable=multiple#6 binding=first#0 mode=Exec uniqueness=aliased synthetic=ghost:0/tracked:0
  capture callable=multiple#6 binding=second#1 mode=Exec uniqueness=aliased synthetic=ghost:0/tracked:0
  capture callable=multiple#6 binding=third#2 mode=Exec uniqueness=unique synthetic=ghost:0/tracked:0
  capture callable=destructured#7 binding=left#0 mode=Exec uniqueness=unique synthetic=ghost:0/tracked:0
  capture callable=destructured#7 binding=right#1 mode=Exec uniqueness=unique synthetic=ghost:0/tracked:0
  capture callable=destructured#7 binding=sum#2 mode=Exec uniqueness=unique synthetic=ghost:0/tracked:0
  capture callable=branch#8 binding=branch_value#2 mode=Exec uniqueness=unique synthetic=ghost:0/tracked:0
  capture callable=branch#8 binding=branch_value#3 mode=Exec uniqueness=unique synthetic=ghost:0/tracked:0
  capture callable=nested_shadow#9 binding=outer#2 mode=Exec uniqueness=aliased synthetic=ghost:0/tracked:0
  capture callable=nested_shadow#9 binding=shadow#3 mode=Exec uniqueness=unique synthetic=ghost:0/tracked:0
  capture callable=nested_shadow#9 binding=inside#4 mode=Exec uniqueness=unique synthetic=ghost:0/tracked:0
  $ printf 'capture-rows=%s tracked-rows=%s forgetting-edges=%s\n' "$(grep -c '^capture ' artifacts/matrix.observe)" "$(grep -c '^capture .* mode=Tracked ' artifacts/matrix.observe)" "$(grep -c '^forgetting-edge ' artifacts/matrix.observe)"
  capture-rows=15 tracked-rows=1 forgetting-edges=16
  $ printf 'incoming-exec=%s incoming-tracked=%s incoming-ghost=%s\n' "$(grep -c '^forgetting-edge .* incoming=Exec ' artifacts/matrix.observe)" "$(grep -c '^forgetting-edge .* incoming=Tracked ' artifacts/matrix.observe)" "$(grep -c '^forgetting-edge .* incoming=Ghost ' artifacts/matrix.observe)"
  incoming-exec=14 incoming-tracked=1 incoming-ghost=1
  $ printf 'explicit-tracked-descriptors=%s\n' "$(grep -c ' mode=Tracked explicit=true ' artifacts/matrix.observe)"
  explicit-tracked-descriptors=4
  $ tail -1 artifacts/matrix.observe
  capture-issued=9 remapped=9 proof-sst=9 forgetting=16 recursive=0 backend=0 z3=0/0

Retained source shows the aliased shadow and exact empty structural sidecar.
The manifest encodes source identity without a live expression.  Ordinary
source and ordinary client artifacts contain no generated carrier or runtime
use.

  $ ocamlc -w -A -alert -all -stop-after parsing -dsource -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" fixtures/unique_empty_return.ml >/dev/null 2>artifacts/unique.retained
  $ grep -q 'fun (empty : _ @ aliased)' artifacts/unique.retained && grep -q 'verocaml:proof-region:2:' artifacts/unique.retained && echo 'unique retained carrier uses one dead aliased shadow'
  unique retained carrier uses one dead aliased shadow
  $ grep -q 'slots=656d707479,' artifacts/unique.retained && echo 'manifest names the source binding only as static hex metadata'
  manifest names the source binding only as static hex metadata
  $ ocamlc -w -A -alert -all -stop-after parsing -dsource -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" fixtures/positive_matrix.ml >/dev/null 2>artifacts/matrix.retained
  $ grep -q 'callable=676c6f62616c5f6f6e6c79.*|slots="' artifacts/matrix.retained && grep -q 'fun () ->' artifacts/matrix.retained && echo 'zero-capture manifest and structural fun () retained'
  zero-capture manifest and structural fun () retained
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx ../../ppx/vero_ppx.exe -c -o artifacts/ordinary.cmo fixtures/positive_matrix.ml
  $ ocamlopt -w -A -alert -all -bin-annot -I ../../runtime -I ../../runtime/.vero_ghost.objs/native -ppx ../../ppx/vero_ppx.exe -c -o artifacts/ordinary.cmx fixtures/positive_matrix.ml
  $ ocamlc -I artifacts artifacts/ordinary.cmo -o artifacts/ordinary.byte
  $ ocamlopt -I artifacts artifacts/ordinary.cmx -o artifacts/ordinary.native
  $ for file in artifacts/ordinary.cmi artifacts/ordinary.cmt artifacts/ordinary.cmo artifacts/ordinary.cmx artifacts/ordinary.byte artifacts/ordinary.native; do strings "$file" | grep -E 'verocaml:proof-region-capture|Vero_ghost[.](proof_region|marker|sidecar)' && exit 1 || :; done
  $ ocamlc -w -A -alert -all -stop-after parsing -dsource -ppx ../../ppx/vero_ppx.exe fixtures/positive_matrix.ml >/dev/null 2>artifacts/ordinary.source
  $ grep -E 'proof-region-capture|Vero_ghost|proof_region|sidecar' artifacts/ordinary.source >/dev/null; test $? -ne 0
  $ echo 'ordinary source, CMI/CMT/CMO/CMX, bytecode, and native artifacts are carrier-free'
  ordinary source, CMI/CMT/CMO/CMX, bytecode, and native artifacts are carrier-free
  $ root="${PWD%%/_build/*}"
  $ install_root="${VEROCAML_TEST_INSTALL_ROOT:-$root/_build/install/default}"
  $ ocamlc -w -A -alert -all -bin-annot -ppx "$install_root/bin/verocaml-ppx" -c -o artifacts/installed_ordinary.cmo fixtures/positive_matrix.ml
  $ ocamlopt -w -A -alert -all -bin-annot -ppx "$install_root/bin/verocaml-ppx" -c -o artifacts/installed_ordinary.cmx fixtures/positive_matrix.ml
  $ ocamlc -I artifacts artifacts/installed_ordinary.cmo -o artifacts/installed_ordinary.byte
  $ ocamlopt -I artifacts artifacts/installed_ordinary.cmx -o artifacts/installed_ordinary.native
  $ for file in artifacts/installed_ordinary.cmi artifacts/installed_ordinary.cmt artifacts/installed_ordinary.cmo artifacts/installed_ordinary.cmx artifacts/installed_ordinary.byte artifacts/installed_ordinary.native; do strings "$file" | grep -E 'verocaml:proof-region-capture|Vero_ghost[.](proof_region|marker|sidecar)' && exit 1 || :; done
  $ echo 'installed ordinary CMI/CMT/CMO/CMX, bytecode, and native artifacts are carrier-free'
  installed ordinary CMI/CMT/CMO/CMX, bytecode, and native artifacts are carrier-free

Malformed manifests are produced by same-length mutations of a genuine
retained CMT, so ghost locations and compiler identifiers remain intact.
Every attack rejects before capture issuance, remapping, proof SST, mode
forgetting, backend, or direct Z3 work.

  $ retained adversary_base
  $ for attack in wrong-callable wrong-binding-span wrong-region-id missing-slot reordered-slots duplicate-slot; do ./mutate_carrier.exe artifacts/adversary_base.cmt "artifacts/$attack.cmt" "$attack"; cp artifacts/adversary_base.cmi "artifacts/$attack.cmi"; printf '%s: ' "$attack"; ./proof_region_modes_tool.exe reject "artifacts/$attack.cmt" | paste -sd ' ' -; done
  wrong-callable: rejected=VERO_MALFORMED_GHOST_CALL capture-issued=0 remapped=0 proof-sst=0 forgetting=0 recursive=0 backend=0 z3=0/0
  wrong-binding-span: rejected=VERO_MALFORMED_GHOST_CALL capture-issued=0 remapped=0 proof-sst=0 forgetting=0 recursive=0 backend=0 z3=0/0
  wrong-region-id: rejected=VERO_MALFORMED_GHOST_CALL capture-issued=0 remapped=0 proof-sst=0 forgetting=0 recursive=0 backend=0 z3=0/0
  missing-slot: rejected=VERO_MALFORMED_GHOST_CALL capture-issued=0 remapped=0 proof-sst=0 forgetting=0 recursive=0 backend=0 z3=0/0
  reordered-slots: rejected=VERO_MALFORMED_GHOST_CALL capture-issued=0 remapped=0 proof-sst=0 forgetting=0 recursive=0 backend=0 z3=0/0
  duplicate-slot: rejected=VERO_MALFORMED_GHOST_CALL capture-issued=0 remapped=0 proof-sst=0 forgetting=0 recursive=0 backend=0 z3=0/0

The structural adversaries below are typedtree mutations of genuine
PPX-produced retained CMT carriers.  The mapper preserves their compiler
identifiers and ghost locations, so each mutation reaches the private
structural boundary rather than the source-written location gate.  Marker and
sidecar manifest arguments, the region-ID argument, the proof thunk actual,
and applied/escaping sidecars are mutated separately.

  $ retained structural_adversary_base
  $ ./proof_region_modes_tool.exe rewrite-retained zero-capture-substitution artifacts/positive_matrix.cmt artifacts/zero-capture-substitution.cmt
  $ cp artifacts/positive_matrix.cmi artifacts/zero-capture-substitution.cmi
  $ printf 'zero-capture-substitution: '; ./proof_region_modes_tool.exe reject artifacts/zero-capture-substitution.cmt | paste -sd ' ' -
  zero-capture-substitution: rejected=VERO_MALFORMED_GHOST_CALL capture-issued=0 remapped=0 proof-sst=0 forgetting=0 recursive=0 backend=0 z3=0/0
  $ for attack in unit-padded-nonzero wrong-shadow-type wrong-shadow-mode live-marker-manifest live-sidecar-manifest live-region-id live-thunk-argument applied-sidecar escaping-sidecar; do ./proof_region_modes_tool.exe rewrite-retained "$attack" artifacts/structural_adversary_base.cmt "artifacts/$attack.cmt"; cp artifacts/structural_adversary_base.cmi "artifacts/$attack.cmi"; printf '%s: ' "$attack"; ./proof_region_modes_tool.exe reject "artifacts/$attack.cmt" | paste -sd ' ' -; done
  unit-padded-nonzero: rejected=VERO_MALFORMED_GHOST_CALL capture-issued=0 remapped=0 proof-sst=0 forgetting=0 recursive=0 backend=0 z3=0/0
  wrong-shadow-type: rejected=VERO_MALFORMED_GHOST_CALL capture-issued=0 remapped=0 proof-sst=0 forgetting=0 recursive=0 backend=0 z3=0/0
  wrong-shadow-mode: rejected=VERO_MALFORMED_GHOST_CALL capture-issued=0 remapped=0 proof-sst=0 forgetting=0 recursive=0 backend=0 z3=0/0
  live-marker-manifest: rejected=VERO_MALFORMED_GHOST_CALL capture-issued=0 remapped=0 proof-sst=0 forgetting=0 recursive=0 backend=0 z3=0/0
  live-sidecar-manifest: rejected=VERO_MALFORMED_GHOST_CALL capture-issued=0 remapped=0 proof-sst=0 forgetting=0 recursive=0 backend=0 z3=0/0
  live-region-id: rejected=VERO_MALFORMED_GHOST_CALL capture-issued=0 remapped=0 proof-sst=0 forgetting=0 recursive=0 backend=0 z3=0/0
  live-thunk-argument: rejected=VERO_MALFORMED_GHOST_CALL capture-issued=0 remapped=0 proof-sst=0 forgetting=0 recursive=0 backend=0 z3=0/0
  applied-sidecar: rejected=VERO_MALFORMED_GHOST_CALL capture-issued=0 remapped=0 proof-sst=0 forgetting=0 recursive=0 backend=0 z3=0/0
  escaping-sidecar: rejected=VERO_MALFORMED_GHOST_CALL capture-issued=0 remapped=0 proof-sst=0 forgetting=0 recursive=0 backend=0 z3=0/0

Source-written and previous direct carriers have no compatibility path.  A
genuine carrier lowered through the raw adapter has no CMT/family issuer and
also rejects with zero work.  Unsupported payloads still reach their existing
semantic boundary only after authenticated remapping.

  $ retained source_written_carrier
  $ ./proof_region_modes_tool.exe reject artifacts/source_written_carrier.cmt
  rejected=VERO_MALFORMED_GHOST_CALL
  capture-issued=0 remapped=0 proof-sst=0 forgetting=0 recursive=0 backend=0 z3=0/0
  $ retained old_direct_carrier
  $ ./proof_region_modes_tool.exe reject artifacts/old_direct_carrier.cmt
  rejected=VERO_MALFORMED_GHOST_CALL
  capture-issued=0 remapped=0 proof-sst=0 forgetting=0 recursive=0 backend=0 z3=0/0
  $ ./proof_region_modes_tool.exe raw artifacts/checked_unique_empty_return.cmt
  raw-rejected=VERO_MALFORMED_GHOST_CALL
  capture-issued=0 remapped=0 proof-sst=0 forgetting=0 recursive=0 backend=0 z3=0/0
  $ retained effectful_payload
  $ ./proof_region_modes_tool.exe semantic-reject artifacts/effectful_payload.cmt
  rejected=VERO_UNSUPPORTED_EXTERNAL_CALL
  capture-issued=1 remapped=1 proof-sst=0 forgetting=0 recursive=0 backend=0 z3=0/0

Authenticated captures do not weaken explicit Ghost actual matching, permit
Ghost-to-Tracked recovery, or create a missing finite receipt.  Each rejection
reaches its unchanged semantic boundary only after exact capture remapping.
Separate recursive, backend, and direct-Z3 controls prove those zero counters
are live.

  $ for name in explicit_ghost_mismatch tracked_recovery missing_finite_receipt; do retained "$name"; ./proof_region_modes_tool.exe driver-reject "artifacts/$name.cmt"; done
  driver-rejected=validation
  capture-issued=1 remapped=1 proof-sst=1 forgetting=0 recursive=0 backend=0 z3=0/0
  driver-rejected=validation
  capture-issued=1 remapped=1 proof-sst=1 forgetting=0 recursive=0 backend=0 z3=0/0
  driver-rejected=pipeline
  capture-issued=1 remapped=1 proof-sst=1 forgetting=1 recursive=0 backend=0 z3=0/0

A Tracked rank-backed wrapper in a standalone proof remains independent from
finite authority.  Its bare projection is shape- and mode-admissible, but the
finite Proof formal still rejects without the exact receipt before backend or
solver work.

  $ retained tracked_wrapper_missing_finite
  $ ./proof_region_modes_tool.exe driver-reject artifacts/tracked_wrapper_missing_finite.cmt
  driver-rejected=pipeline
  capture-issued=1 remapped=1 proof-sst=1 forgetting=1 recursive=0 backend=0 z3=0/0

The historically named stale-owned fixture is now an exact completed-result
positive: a local constructor result transfers one sealed
predecessor to a verified local transition, every preservation obligation
completes, and the separate result receipt exists before the caller proof is
lowered.  This does not grant entry-formal authority; the dedicated
use_invariant_from_formal transition fixture remains a rejection.

  $ retained stale_owned_invariant
  $ VEROCAML_TEST_PRIVATE_RECEIPT_TRACE=1 DELATOR_LOG=Verification_session=debug,warn DELATOR_FORMAT=flat DELATOR_COLOR=never ./proof_region_modes_tool.exe driver-transition-result artifacts/stale_owned_invariant.cmt 2>artifacts/stale-owned.trace
  driver-transition-result status=verified functions=4 obligations=7 preservation-results=1
  transition lifecycle=1/1/1/1 preservation=1 reconstruction=1/0
  dependency lowering/backend/solver=2/2/5 receipts-issued/consumed=2/2 callee-verified/failed=5/0
  capture-issued=1 remapped=1 proof-sst=1 forgetting=1 recursive=0 backend=7 z3=7/7
  $ grep 'Verification_session: private receipt event_kind=lower .*function_name=run ' artifacts/stale-owned.trace | sed -E 's/.*transition_predecessor_transfers=([0-9]+) transition_predecessor_consumptions=([0-9]+) transition_result_receipts=([0-9]+) transition_teardown_removals=[0-9]+ transition_preservation_obligations=([0-9]+) transition_nested_reconstructions=([0-9]+) transition_root_reconstructions=([0-9]+).*/caller-proof-entry predecessor=\1\/\2 result=\3 preservation=\4 reconstruction=\5\/\6/'
  caller-proof-entry predecessor=1/1 result=1 preservation=1 reconstruction=1/0
  $ retained recursive_control
  $ ./proof_region_modes_tool.exe driver-control artifacts/recursive_control.cmt
  driver-status=verified functions=1 obligations=3
  capture-issued=0 remapped=0 proof-sst=0 forgetting=4 recursive=1 backend=3 z3=3/3
