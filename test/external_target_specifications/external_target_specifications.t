Ordinary external-target verification and rejection outcomes live in
outcome_cases.ml. This specialist target retains only private authentication,
compiler-marker, malformed-compiler-input, runtime-erasure, and stale-artifact
contracts listed in test/support/migrations/w09.md.

  $ mkdir artifacts
  $ export GHOST="$PWD/../../runtime/.vero_ghost.objs/byte"
  $ export PPX_ORDINARY="$PWD/../../ppx/vero_ppx.exe"
  $ export PPX_RETAINED="$PWD/../../ppx/vero_ppx.exe --keep-ghost"
  $ ordinary () { name=$1; shift; ocamlc -w -A -alert -all -bin-annot -I "$GHOST" -I artifacts -ppx "$PPX_ORDINARY" -c -o "artifacts/$name.$1" "$2"; }
  $ retained () { name=$1; ocamlc -w -A -alert -all -bin-annot -I "$GHOST" -I artifacts -ppx "$PPX_RETAINED" -c -o "artifacts/$name.cmo" "fixtures/$name.ml"; }
  $ ordinary legacy cmi fixtures/legacy.mli
  $ ordinary legacy cmo fixtures/legacy.ml
  $ ordinary other_legacy cmi fixtures/other_legacy.mli
  $ ordinary other_legacy cmo fixtures/other_legacy.ml
  $ ocamlc -w -A -alert -all -bin-annot -I "$GHOST" -I artifacts -ppx "$PPX_RETAINED" -c -o artifacts/consumer.cmi fixtures/consumer.mli
  $ retained consumer
  $ ocamlc -w -A -alert -all -bin-annot -I "$GHOST" -I artifacts -ppx "$PPX_RETAINED" -c -o artifacts/alpha_consumer.cmi fixtures/alpha_consumer.mli
  $ retained alpha_consumer

Private target metadata and authority cannot be substituted, copied, replayed,
or crossed between alpha-equivalent consumer artifacts.

  $ ./external_target_specifications_tool.exe metadata artifacts/consumer.cmt artifacts/consumer.cmi artifacts/legacy.cmt artifacts/legacy.cmi >/dev/null
  $ ./external_target_specifications_tool.exe wrong-cmi artifacts/consumer.cmt artifacts/consumer.cmi artifacts/other_legacy.cmt artifacts/other_legacy.cmi >/dev/null
  $ ./external_target_specifications_tool.exe authority artifacts/consumer.cmt artifacts/consumer.cmi artifacts/legacy.cmt artifacts/legacy.cmi >/dev/null
  $ ./external_target_specifications_tool.exe alpha-cross artifacts/consumer.cmt artifacts/consumer.cmi artifacts/alpha_consumer.cmt artifacts/alpha_consumer.cmi artifacts/legacy.cmt artifacts/legacy.cmi >/dev/null
  $ ./external_target_specifications_tool.exe compiler-mode-compatibility >/dev/null
  $ echo external-target-private-authority=checked
  external-target-private-authority=checked

A retained library may publish a hidden external-function specification. A
direct consumer imports the provider contract implicitly, while the ordinary
target remains opaque and no command-line dependency flags are needed. Merely
importing the provider does not activate the target contract, and two providers
for one exact target reject rather than selecting by order.

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
  $ test "$(grep -c '^verocaml: trusted external specification' artifacts/provider-inactive-overlap.out)" = 0; grep -q '^verocaml: verified file=provider_external_inactive_overlap_consumer.cmt ' artifacts/provider-inactive-overlap.out; echo provider-inactive-overlap=verified-without-target-authority
  provider-inactive-overlap=verified-without-target-authority
  $ code=0; (cd artifacts && OCAML_COLOR=never ../../../src/verocaml.exe verify provider_external_overlap_consumer.cmt --threads 1 --timeout-ms 60000) > artifacts/provider-overlap.out 2>&1 || code=$?; test "$code" = 2
  $ grep -q 'overlapping external function specifications' artifacts/provider-overlap.out; grep -q 'Provider_external' artifacts/provider-overlap.out; grep -q 'Provider_external_duplicate' artifacts/provider-overlap.out; grep -q 'target Legacy.promised / Legacy.promised' artifacts/provider-overlap.out
  $ ocamlc -w -A -alert -all -bin-annot -I "$GHOST" -I artifacts -ppx "$PPX_RETAINED" -c -o artifacts/provider_external_used.cmi fixtures/provider_external_used.mli
  $ retained provider_external_used
  $ retained provider_external_used_consumer
  $ code=0; (cd artifacts && OCAML_COLOR=never ../../../src/verocaml.exe verify provider_external_used_consumer.cmt --threads 1 --timeout-ms 60000) > artifacts/provider-used.out 2>&1 || code=$?; test "$code" = 2
  $ grep -q 'unit Provider_external_used: retained provider lacks private-driver completion' artifacts/provider-used.out

The retained-only mode marker is authenticated but absent from ordinary
interfaces, runtime objects, and byte/native execution.

  $ retained mode_summary_result_local
  $ mkdir artifacts/mode-marker-erasure
  $ ocamlc -w -A -alert -all -bin-annot -I "$GHOST" -I artifacts -ppx "$PPX_ORDINARY" -c -o artifacts/mode-marker-erasure/mode_summary_result_local.cmo fixtures/mode_summary_result_local.ml
  $ ocamlopt -w -A -alert -all -bin-annot -I "$GHOST" -I artifacts -ppx "$PPX_ORDINARY" -c -o artifacts/mode-marker-erasure/legacy.cmx fixtures/legacy.ml
  $ ocamlopt -w -A -alert -all -bin-annot -I "$GHOST" -I artifacts -ppx "$PPX_ORDINARY" -c -o artifacts/mode-marker-erasure/mode_summary_result_local.cmx fixtures/mode_summary_result_local.ml
  $ printf 'let () = Printf.printf "%%d\n" (Mode_summary_result_local.use 41)\n' > artifacts/mode-marker-erasure/app.ml
  $ ocamlc -I artifacts/mode-marker-erasure -I artifacts -o artifacts/mode-marker-erasure/app.byte artifacts/legacy.cmo artifacts/mode-marker-erasure/mode_summary_result_local.cmo artifacts/mode-marker-erasure/app.ml
  $ ocamlopt -I artifacts/mode-marker-erasure -I artifacts -o artifacts/mode-marker-erasure/app.native artifacts/mode-marker-erasure/legacy.cmx artifacts/mode-marker-erasure/mode_summary_result_local.cmx artifacts/mode-marker-erasure/app.ml
  $ artifacts/mode-marker-erasure/app.byte > artifacts/mode-marker-erasure/byte.out
  $ artifacts/mode-marker-erasure/app.native > artifacts/mode-marker-erasure/native.out
  $ cmp artifacts/mode-marker-erasure/byte.out artifacts/mode-marker-erasure/native.out
  $ ./external_target_specifications_tool.exe marker-provenance artifacts/mode_summary_result_local.cmt artifacts/mode_summary_result_local.cmi artifacts/mode-marker-erasure/mode_summary_result_local.cmt artifacts/mode-marker-erasure/mode_summary_result_local.cmi >/dev/null
  $ strings artifacts/mode_summary_result_local.cmt | grep -q 'verocaml.internal.compiler_mode_syntax'
  $ for file in artifacts/mode_summary_result_local.{cmi,cmo} artifacts/mode-marker-erasure/mode_summary_result_local.{cmi,cmo,cmx} artifacts/mode-marker-erasure/app.{byte,native}; do if strings "$file" | grep -q 'verocaml.internal.compiler_mode_syntax'; then exit 1; fi; done

Malformed compiler-level ABI inputs are rejected. The inherited no-CMT
expectation is stale at this base because the compiler leaves failed sidecars;
compiler wording and sidecar presence are not green contracts.

  $ compiler_reject () { name=$1; if OCAML_COLOR=never ocamlc -w -A -alert -all -bin-annot -I "$GHOST" -I artifacts -ppx "$PPX_RETAINED" -c -o "artifacts/$name.cmo" "fixtures/$name.ml" >/dev/null 2>&1; then return 1; fi; }
  $ for name in wrong_optional_forward unknown_label missing_label duplicate_label wrong_result wrong_type wrong_arity mode_summary_tracked; do compiler_reject "$name" || exit 1; done
  $ echo compiler-abi-rejections=checked
  compiler-abi-rejections=checked

A rebuilt target behind an old consumer is a deliberate corrupted-artifact
case and remains a stable dependency rejection.

  $ mkdir stale
  $ cp fixtures/legacy.mli fixtures/legacy.ml stale/
  $ chmod u+w stale/legacy.mli stale/legacy.ml
  $ printf '\nval changed : int\n' >> stale/legacy.mli
  $ printf '\nlet changed = 0\n' >> stale/legacy.ml
  $ (cd stale && ocamlc -w -A -alert -all -bin-annot -I "$GHOST" -ppx "$PPX_ORDINARY" -c legacy.mli && ocamlc -w -A -alert -all -bin-annot -I "$GHOST" -ppx "$PPX_ORDINARY" -c legacy.ml)
  $ code=0; OCAML_COLOR=never ../../src/verocaml.exe verify-project --root artifacts/consumer.cmt artifacts/consumer.cmi --dependency stale/legacy.cmt stale/legacy.cmi --threads 1 >artifacts/stale.out 2>&1 || code=$?; test "$code" = 2
  $ grep -o 'VERO_[A-Z_]*' artifacts/stale.out | head -1
  VERO_DEPENDENCY
