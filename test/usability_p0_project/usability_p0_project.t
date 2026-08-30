The fixture compiles a real marked generic provider interface/implementation,
retained clients, an unmarked unsupported legacy target, and an ordinary
executable.  The pinned compiler remains the sole authority for value modes.

  $ mkdir artifacts artifacts/copied artifacts/runtime artifacts/modes artifacts/alpha artifacts/source
  $ export GHOST="$PWD/../../runtime/.vero_ghost.objs/byte"
  $ export PPX="$PWD/../../ppx/vero_ppx.exe"
  $ export KEEP="$PPX --keep-ghost"
  $ compile_mli () { name=$1; if test $# = 1; then source="artifacts/source/$name.mli"; cp "fixtures/$name.mli" "$source"; else source=$2; fi; ocamlc -w -A -alert -all -bin-annot -I "$GHOST" -I artifacts -ppx "$KEEP" -c -o "artifacts/$name.cmi" "$source"; }
  $ compile_ordinary_mli () { name=$1; if test $# = 1; then source="artifacts/source/$name.mli"; cp "fixtures/$name.mli" "$source"; else source=$2; fi; ocamlc -w -A -alert -all -bin-annot -I artifacts -ppx "$PPX" -c -o "artifacts/$name.cmi" "$source"; }
  $ compile_retained () { name=$1; if test $# = 1; then source="artifacts/source/$name.ml"; cp "fixtures/$name.ml.src" "$source"; else source=$2; fi; ocamlc -w -A -alert -all -bin-annot -I "$GHOST" -I artifacts -ppx "$KEEP" -c -o "artifacts/$name.cmo" "$source"; }
  $ compile_ordinary () { name=$1; if test $# = 1; then source="artifacts/source/$name.ml"; cp "fixtures/$name.ml.src" "$source"; else source=$2; fi; ocamlc -w -A -alert -all -bin-annot -I artifacts -ppx "$PPX" -c -o "artifacts/$name.cmo" "$source"; }
  $ compile_mli provider
  $ compile_retained provider
  $ compile_mli consumer
  $ compile_retained consumer
  $ compile_ordinary_mli legacy
  $ compile_ordinary legacy
  $ compile_mli external_client
  $ compile_retained external_client
  $ compile_retained direct_bypass
  $ compile_retained external_mode_negative
  $ compile_retained marked_legacy
  $ compile_retained failure
  $ echo 'mixed-project compile=mli/ml retained=yes legacy=ordinary'
  mixed-project compile=mli/ml retained=yes legacy=ordinary

The complete project report is deterministic across inventory order and
serial/thread scheduling.  It includes every partition and accepts the
explicit trusted-external row with exit zero.

  $ project () { OCAML_COLOR=never ../../src/verocaml.exe verify-project "$@" --timeout-ms 60000; }
  $ project --root artifacts/consumer.cmt artifacts/consumer.cmi --dependency artifacts/provider.cmt artifacts/provider.cmi --root artifacts/external_client.cmt artifacts/external_client.cmi --dependency artifacts/legacy.cmt artifacts/legacy.cmi --threads 1 > artifacts/project.serial
  $ project --dependency artifacts/legacy.cmt artifacts/legacy.cmi --root artifacts/external_client.cmt artifacts/external_client.cmi --dependency artifacts/provider.cmt artifacts/provider.cmi --root artifacts/consumer.cmt artifacts/consumer.cmi --threads 2 > artifacts/project.threaded
  $ cmp artifacts/project.serial artifacts/project.threaded
  $ grep '^verocaml: verified unit=\|^verocaml: verified-dependency unit=\|^verocaml: skipped unit=' artifacts/project.serial
  verocaml: verified unit=Consumer file=artifacts/consumer.cmt result=verified functions=6 obligations=0
  verocaml: verified unit=External_client file=artifacts/external_client.cmt result=verified functions=5 obligations=2
  verocaml: verified-dependency unit=Provider file=artifacts/provider.cmt result=verified
  verocaml: skipped unit=Legacy file=artifacts/legacy.cmt result=skipped
  $ grep -c '^verocaml: trusted external specification trust=imported-unverified-target target-unit=Legacy .* target-body=unverified result=constrained-only-by-ensures' artifacts/project.serial
  5

The unsupported unmarked provider performs no retained, semantic, backend, or
solver work.  Its marked copy reaches the ordinary verifier rejection path.

  $ ../verification_scope/verification_scope_counter_tool.exe skip artifacts/legacy.cmt artifacts/legacy.cmi
  skip-counters retained=0 typed-lowering=0 semantic=0 vc=0 backend=0 private-driver=0 provider-reverification=0 solver=0 z3=0/0/0/0/0/0
  $ project --root artifacts/marked_legacy.cmt artifacts/marked_legacy.cmi --threads 1 > artifacts/marked.out 2>&1; echo $?
  2
  $ grep -o 'VERO_[A-Z_]*' artifacts/marked.out | head -1
  VERO_UNSUPPORTED_STRUCTURE_ITEM

Source/CMT, copied and reloaded artifacts, and serial/threaded paths preserve
semantic outcomes.  The rendered copied path remains intentionally distinct.

  $ cp fixtures/consumer.ml.src artifacts/consumer.ml
  $ (cd artifacts && OCAML_COLOR=never ../../../src/verocaml.exe verify consumer.ml --dependency provider.cmt --threads 1 --timeout-ms 60000 --dump-sst source.sst --dump-vir source.vir > source.out)
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/consumer.cmt --dependency artifacts/provider.cmt --threads 2 --timeout-ms 60000 --dump-sst artifacts/cmt.sst --dump-vir artifacts/cmt.vir > artifacts/cmt.out
  $ sed -E 's#file=[^ ]+#file=CONSUMER#; s/interface-digest=[0-9a-f]+/interface-digest=<digest>/' artifacts/source.out > artifacts/source.semantic
  $ sed -E 's#file=[^ ]+#file=CONSUMER#; s/interface-digest=[0-9a-f]+/interface-digest=<digest>/' artifacts/cmt.out > artifacts/cmt.semantic
  $ cmp artifacts/source.semantic artifacts/cmt.semantic
  $ cp artifacts/consumer.cmt artifacts/consumer.cmi artifacts/provider.cmt artifacts/provider.cmi artifacts/external_client.cmt artifacts/external_client.cmi artifacts/legacy.cmt artifacts/legacy.cmi artifacts/copied/
  $ project --root artifacts/copied/consumer.cmt artifacts/copied/consumer.cmi --dependency artifacts/copied/provider.cmt artifacts/copied/provider.cmi --root artifacts/copied/external_client.cmt artifacts/copied/external_client.cmi --dependency artifacts/copied/legacy.cmt artifacts/copied/legacy.cmi --threads 1 > artifacts/copied/project
  $ sed 's#file=artifacts/copied/#file=artifacts/#g' artifacts/copied/project > artifacts/copied/semantic
  $ cmp artifacts/project.serial artifacts/copied/semantic
  $ echo 'source-cmt-copy-thread semantic=agree'
  source-cmt-copy-thread semantic=agree

Alpha-renamed provider binders preserve retained generic meaning without
cross-artifact substitution.

  $ for side in a b; do compile_mli "alpha_provider_$side"; compile_retained "alpha_provider_$side"; compile_retained "alpha_consumer_$side"; OCAML_COLOR=never ../../src/verocaml.exe verify "artifacts/alpha_consumer_$side.cmt" --dependency "artifacts/alpha_provider_$side.cmt" --threads 1 --timeout-ms 60000 --dump-sst "artifacts/alpha/$side.sst" --dump-vir "artifacts/alpha/$side.vir" > "artifacts/alpha/$side.out"; done
  $ sed -E 's/Alpha_provider_[ab]/Alpha_provider/g; s/alpha_consumer_[ab]/alpha_consumer/g; s/interface-digest=[0-9a-f]+/interface-digest=<digest>/' artifacts/alpha/a.out > artifacts/alpha/a.semantic
  $ sed -E 's/Alpha_provider_[ab]/Alpha_provider/g; s/alpha_consumer_[ab]/alpha_consumer/g; s/interface-digest=[0-9a-f]+/interface-digest=<digest>/' artifacts/alpha/b.out > artifacts/alpha/b.semantic
  $ cmp artifacts/alpha/a.semantic artifacts/alpha/b.semantic
  $ echo 'alpha-renamed retained-signature=agree'
  alpha-renamed retained-signature=agree

The selected direct bypass and mode-bearing imported-summary controls reject
before trusted semantic validation.  A selected counterexample still prints
the complete deterministic report and exits nonzero.

  $ reject_before_trust () { name=$1; code=0; project --root "artifacts/$name.cmt" "artifacts/$name.cmi" --dependency artifacts/legacy.cmt artifacts/legacy.cmi --threads 1 > "artifacts/$name.out" 2>&1 || code=$?; if test "$code" != 2; then cat "artifacts/$name.out"; return 1; fi; if test "$(grep -c '^verocaml: trusted external specification' "artifacts/$name.out")" != 0; then cat "artifacts/$name.out"; return 1; fi; echo "$name=rejected-before-trust"; }
  $ for name in direct_bypass external_mode_negative; do reject_before_trust "$name" || exit 1; done
  direct_bypass=rejected-before-trust
  external_mode_negative=rejected-before-trust
  $ grep -o 'VERO_[A-Z_]*' artifacts/external_mode_negative.out | head -1
  VERO_MALFORMED_GHOST_CALL
  $ project --root artifacts/failure.cmt artifacts/failure.cmi --root artifacts/consumer.cmt artifacts/consumer.cmi --dependency artifacts/provider.cmt artifacts/provider.cmi --root artifacts/legacy.cmt artifacts/legacy.cmi --threads 2 > artifacts/failure.out 2>&1; echo $?
  1
  $ grep '^verocaml: verified unit=\|^verocaml: verified-dependency unit=\|^verocaml: skipped unit=' artifacts/failure.out
  verocaml: verified unit=Consumer file=artifacts/consumer.cmt result=verified functions=6 obligations=0
  verocaml: verified unit=Failure file=artifacts/failure.cmt result=counterexample functions=1 obligations=1
  verocaml: verified-dependency unit=Provider file=artifacts/provider.cmt result=verified
  verocaml: skipped unit=Legacy file=artifacts/legacy.cmt result=skipped

The compiler-libraries helper derives the ten axes from Mode.Value.Axis.all,
inventories compiler-admitted complete modes from typed binders, and fails
closed against the pinned state and twin-position inventory.

  $ python3 generate_value_mode_matrix.py artifacts/modes
  $ ocamlc -w -A -alert -all -bin-annot -c -o artifacts/modes/positions.cmo artifacts/modes/positions.ml
  $ compile_mli mode_provider artifacts/modes/mode_provider.mli
  $ compile_retained mode_provider artifacts/modes/mode_provider.ml
  $ compile_retained mode_client artifacts/modes/mode_client.ml
  $ ./value_mode_inventory.exe artifacts/modes/positions.cmt artifacts/mode_client.cmt artifacts/modes/manifest value_mode_observations.expected
  axis=Areality states=global,local,regional
  axis=Forkable states=forkable,unforkable
  axis=Yielding states=unyielding,yielding
  axis=Linearity states=many,once
  axis=Statefulness states=observing,stateful,stateless
  axis=Portability states=nonportable,portable,shareable
  axis=Uniqueness states=aliased,unique
  axis=Visibility states=immutable,read,read_write
  axis=Contention states=contended,shared,uncontended
  axis=Staticity states=dynamic,static
  mode-inventory axes=10 states=25 combinations=16 state-samples=73 position-observations=240 twin-pairs=120 authority=OxCaml
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/mode_client.cmt --dependency artifacts/mode_provider.cmt --threads 2 --timeout-ms 60000 | sed -E 's/interface-digest=[0-9a-f]+/interface-digest=<digest>/'
  verocaml: verified dependency unit=Mode_provider interface-digest=<digest> direct=none transitive=none trust=none
  verocaml: verified file=artifacts/mode_client.cmt functions=48 obligations=0

Illegal source combinations remain native OxCaml diagnostics.  In particular,
applications are dynamic and cannot satisfy a static return annotation.

  $ cat > artifacts/modes/illegal.ml <<'EOF'
  > let static_id (value : int @ static) : int @ static = value
  > let illegal (value : int @ static) : int @ static = static_id value
  > EOF
  $ code=0; OCAML_COLOR=never ocamlc -w -A -alert -all -bin-annot -c -o artifacts/modes/illegal.cmo artifacts/modes/illegal.ml > artifacts/modes/illegal.out 2>&1 || code=$?; test "$code" = 2
  $ grep -F 'because function applications are always dynamic.' artifacts/modes/illegal.out
         because function applications are always dynamic.

Ordinary PPX erasure leaves no proof/ghost carrier or retained ABI in source,
CMI/CMO/CMX, bytecode, or native artifacts.  Byte and native executions agree.

  $ mkdir artifacts/erasure artifacts/erasure/annotated artifacts/erasure/reference
  $ cp fixtures/erasure_client.ml.src artifacts/erasure_client.ml
  $ for variant in annotated reference; do dir="artifacts/erasure/$variant"; cp "fixtures/erasure_$variant.ml.src" artifacts/erasure_fixture.ml; chmod u+w artifacts/erasure_fixture.ml; ocamlc -w -A -alert -all -no-keep-locs -bin-annot -i -ppx "$PPX" artifacts/erasure_fixture.ml > "$dir/interface"; ocamlc -w -A -alert -all -no-keep-locs -bin-annot -dinstr -ppx "$PPX" -c -o "$dir/erasure_fixture.cmo" artifacts/erasure_fixture.ml > "$dir/provider.dinstr" 2>&1; ocamlc -w -A -alert -all -no-keep-locs -bin-annot -dinstr -I "$dir" -c -o "$dir/erasure_client.cmo" artifacts/erasure_client.ml > "$dir/client.dinstr" 2>&1; ocamlopt -w -A -alert -all -no-keep-locs -bin-annot -drawflambda -ppx "$PPX" -c -o "$dir/erasure_fixture.cmx" artifacts/erasure_fixture.ml > "$dir/provider.flambda" 2>&1; ocamlopt -w -A -alert -all -no-keep-locs -bin-annot -drawflambda -I "$dir" -c -o "$dir/erasure_client.cmx" artifacts/erasure_client.ml > "$dir/client.flambda" 2>&1; ocamlc -I "$dir" -o "$dir/app.byte" "$dir/erasure_fixture.cmo" "$dir/erasure_client.cmo"; ocamlopt -I "$dir" -o "$dir/app.native" "$dir/erasure_fixture.cmx" "$dir/erasure_client.cmx"; "$dir/app.byte" > "$dir/byte.out"; "$dir/app.native" > "$dir/native.out"; for artifact in erasure_fixture.cmi erasure_fixture.cmo erasure_fixture.cmx erasure_client.cmi erasure_client.cmo erasure_client.cmx; do ocamlobjinfo -no-code "$dir/$artifact" | tail -n +2 > "$dir/$artifact.info"; done; done
  $ grep -Fx 'val make : int -> int -> int' artifacts/erasure/annotated/interface
  val make : int -> int -> int
  $ for evidence in interface provider.dinstr client.dinstr client.flambda erasure_fixture.cmi erasure_fixture.cmo erasure_client.cmi erasure_client.cmo; do cmp "artifacts/erasure/annotated/$evidence" "artifacts/erasure/reference/$evidence"; done
  $ for variant in annotated reference; do sed -E 's/erasure_fixture[.]ml:[0-9]+,[0-9]+--[0-9]+/erasure_fixture.ml:<loc>/g' "artifacts/erasure/$variant/provider.flambda" > "artifacts/erasure/$variant/provider.flambda.semantic"; for artifact in erasure_fixture.cmi erasure_fixture.cmo erasure_fixture.cmx erasure_client.cmi erasure_client.cmo erasure_client.cmx; do sed -E '/^CRC of implementation:/d; s/^([[:space:]]+)[0-9a-f]{32}([[:space:]]+Erasure_fixture)$/\1<implementation-crc>\2/' "artifacts/erasure/$variant/$artifact.info" > "artifacts/erasure/$variant/$artifact.semantic"; done; done
  $ cmp artifacts/erasure/annotated/provider.flambda.semantic artifacts/erasure/reference/provider.flambda.semantic
  $ grep -F 'Project_value_slot' artifacts/erasure/annotated/provider.flambda.semantic >/dev/null
  $ for artifact in erasure_fixture.cmi erasure_fixture.cmo erasure_fixture.cmx erasure_client.cmi erasure_client.cmo erasure_client.cmx; do cmp "artifacts/erasure/annotated/$artifact.semantic" "artifacts/erasure/reference/$artifact.semantic"; done
  $ grep -F 'Erasure_fixture' artifacts/erasure/annotated/erasure_client.cmo.info >/dev/null
  $ grep -F 'Erasure_fixture' artifacts/erasure/annotated/erasure_client.cmx.info >/dev/null
  $ cmp artifacts/erasure/annotated/byte.out artifacts/erasure/annotated/native.out
  $ cmp artifacts/erasure/annotated/byte.out artifacts/erasure/reference/byte.out
  $ cmp artifacts/erasure/annotated/byte.out artifacts/erasure/reference/native.out
  $ cat artifacts/erasure/annotated/byte.out
  erasure: 41 42
  $ for file in artifacts/erasure/{annotated,reference}/erasure_fixture.{cmi,cmt,cmo,cmx} artifacts/erasure/{annotated,reference}/erasure_client.{cmi,cmt,cmo,cmx}; do strings "$file" | grep -E 'Vero_ghost|verocaml[.]internal[.]retained|verocaml:(proof-region|marker|sidecar)' && exit 1 || :; done
  $ echo 'proof-ghost erasure interface/import/abi/use/capture=agree byte/native=agree'
  proof-ghost erasure interface/import/abi/use/capture=agree byte/native=agree

  $ ocamlc -w -A -alert -all -bin-annot -I artifacts -ppx "$PPX" -c -o artifacts/runtime/provider.cmo -impl fixtures/provider.ml.src
  $ ocamlc -w -A -alert -all -bin-annot -I artifacts/runtime -I artifacts -ppx "$PPX" -c -o artifacts/runtime/consumer.cmo -impl fixtures/consumer.ml.src
  $ ocamlc -w -A -alert -all -bin-annot -I artifacts/runtime -I artifacts -ppx "$PPX" -c -o artifacts/runtime/external_client.cmo -impl fixtures/external_client.ml.src
  $ ocamlc -w -A -alert -all -bin-annot -I artifacts/runtime -I artifacts -c -o artifacts/runtime/app.cmo -impl fixtures/app.ml.src
  $ ocamlc -o artifacts/runtime/app.byte artifacts/runtime/provider.cmo artifacts/runtime/consumer.cmo artifacts/legacy.cmo artifacts/runtime/external_client.cmo artifacts/runtime/app.cmo
  $ artifacts/runtime/app.byte > artifacts/runtime/byte.out
  $ ocamlopt -w -A -alert -all -I artifacts/runtime -I artifacts -ppx "$PPX" -c -o artifacts/runtime/provider.cmx -impl fixtures/provider.ml.src
  $ ocamlopt -w -A -alert -all -I artifacts/runtime -I artifacts -ppx "$PPX" -c -o artifacts/runtime/consumer.cmx -impl fixtures/consumer.ml.src
  $ ocamlopt -w -A -alert -all -I artifacts/runtime -I artifacts -c -o artifacts/runtime/legacy.cmx -impl fixtures/legacy.ml.src
  $ ocamlopt -w -A -alert -all -I artifacts/runtime -I artifacts -ppx "$PPX" -c -o artifacts/runtime/external_client.cmx -impl fixtures/external_client.ml.src
  $ ocamlopt -w -A -alert -all -I artifacts/runtime -I artifacts -c -o artifacts/runtime/app.cmx -impl fixtures/app.ml.src
  $ ocamlopt -o artifacts/runtime/app.native artifacts/runtime/provider.cmx artifacts/runtime/consumer.cmx artifacts/runtime/legacy.cmx artifacts/runtime/external_client.cmx artifacts/runtime/app.cmx
  $ artifacts/runtime/app.native > artifacts/runtime/native.out
  $ cmp artifacts/runtime/byte.out artifacts/runtime/native.out
  $ cat artifacts/runtime/byte.out
  mixed-project: 3 4 5 3 2 8 7
  $ for file in artifacts/runtime/{provider,consumer,external_client,app}.{cmi,cmt,cmo} artifacts/runtime/{provider,consumer,external_client,legacy,app}.cmx artifacts/legacy.{cmi,cmt,cmo} artifacts/runtime/app.{byte,native}; do strings "$file" | grep -E 'Vero_ghost|verocaml[.]internal[.]retained|verocaml:(proof-region|marker|sidecar)' && exit 1 || :; done
  $ echo 'ordinary artifacts=carrier-free byte/native=agree'
  ordinary artifacts=carrier-free byte/native=agree
