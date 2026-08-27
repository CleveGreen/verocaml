The retained family authenticates exact defaults and explicit modes independently
of function stage and OCaml type.

  $ mkdir artifacts
  $ retained () { n=$1; ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$n.cmo" "fixtures/$n.ml"; }
  $ retained modes
  $ ./instance_modes_tool.exe modes artifacts/modes.cmt > artifacts/modes.txt
  $ grep -E '^formal:[0-9]+:[0-9]+ mode=(Exec|Ghost|Tracked)|^result:[0-9]+ mode=(Exec|Ghost|Tracked)' artifacts/modes.txt | sed -E 's/ snapshot=.*//' | sort -u
  formal:0:0 mode=Ghost explicit=false annotation=default default=Ghost type=int
  formal:1:0 mode=Ghost explicit=false annotation=default default=Ghost type=int
  formal:2:0 mode=Tracked explicit=true annotation=fixtures/modes.ml:3:15-3:35 default=Ghost type=int
  formal:3:0 mode=Ghost explicit=true annotation=fixtures/modes.ml:4:16-4:34 default=Exec type=int
  formal:3:1 mode=Tracked explicit=true annotation=fixtures/modes.ml:4:35-4:55 default=Exec type=int
  formal:4:0 mode=Exec explicit=false annotation=default default=Exec type=int
  formal:5:0 mode=Exec explicit=false annotation=default default=Exec type=int
  result:0 mode=Ghost explicit=false annotation=default default=Ghost type=int
  result:1 mode=Ghost explicit=false annotation=default default=Ghost type=int
  result:2 mode=Tracked explicit=true annotation=fixtures/modes.ml:3:15-3:71 default=Ghost type=int
  result:3 mode=Ghost explicit=true annotation=fixtures/modes.ml:4:16-4:87 default=Exec type=int
  result:4 mode=Tracked explicit=true annotation=fixtures/modes.ml:5:24-5:61 default=Exec type=int
  result:5 mode=Exec explicit=false annotation=default default=Exec type=int
  $ ../../src/verocaml.exe verify artifacts/modes.cmt 2>&1 | sed -E 's,file=[^ ]+,file=modes.cmt,'
  verocaml: verified file=modes.cmt functions=5 obligations=0
  $ retained forgetting
  $ ../../src/verocaml.exe verify artifacts/forgetting.cmt 2>&1 | sed -E 's,file=[^ ]+,file=forgetting.cmt,'
  verocaml: verified file=forgetting.cmt functions=2 obligations=0

Authenticated nonrecursive standalone proofs resolve a bare exact Tracked
binding from its binding identity and expected proof context.  Exact Tracked
call/result boundaries preserve the original binding, while pure Ghost
assertion contexts record one-way observation.  No synthetic descriptor or
unrelated authority is issued.  The exact enclosing-Exec probe remains a
zero-flow rejection, so Proof expression stage alone does not select the rule.

  $ for n in tracked_call_bare tracked_return_bare standalone_proof_bare_tracked exec_call_tracked_bare; do retained "$n"; done
  $ for n in tracked_call_bare tracked_return_bare standalone_proof_bare_tracked; do ../../src/verocaml.exe verify "artifacts/$n.cmt" 2>&1 | sed -E 's,file=[^ ]+,file=fixture.cmt,'; done
  verocaml: verified file=fixture.cmt functions=2 obligations=0
  verocaml: verified file=fixture.cmt functions=1 obligations=0
  verocaml: verified file=fixture.cmt functions=1 obligations=2
  $ : > artifacts/standalone-proof-binding.trace
  $ for n in tracked_call_bare tracked_return_bare standalone_proof_bare_tracked; do VEROCAML_TEST_INSTANCE_MODE_TRACE=1 ./instance_modes_tool.exe modes "artifacts/$n.cmt" >/dev/null 2>>artifacts/standalone-proof-binding.trace; done
  $ grep '^standalone-proof-binding ' artifacts/standalone-proof-binding.trace | sed -E 's/.*caller=([^ ]+).*expected=([^ ]+).*outcome=([^ ]+).*edges=([0-9]+).*synthetic-ghost=([0-9]+).*synthetic-tracked=([0-9]+).*unrelated-authority=([0-9]+)$/caller=\1 expected=\2 outcome=\3 edges=\4 synthetic-ghost=\5 synthetic-tracked=\6 unrelated-authority=\7/' | sort | uniq -c
        1 caller=caller#1 expected=Tracked outcome=preserve-exact-Tracked edges=0 synthetic-ghost=0 synthetic-tracked=0 unrelated-authority=0
        1 caller=identity#0 expected=Tracked outcome=preserve-exact-Tracked edges=0 synthetic-ghost=0 synthetic-tracked=0 unrelated-authority=0
        2 caller=observe#1 expected=Ghost outcome=observe-as-Ghost edges=1 synthetic-ghost=0 synthetic-tracked=0 unrelated-authority=0
  $ bad=$(grep '^standalone-proof-binding ' artifacts/standalone-proof-binding.trace | grep -Evc 'binding=binding:[0-9]+:[0-9]+ binding-id=[0-9]+ caller=.* caller-mode=Proof recursive=false enclosing-body=Proof_body authenticated=true expected=(Ghost|Tracked) incoming=Tracked outcome=(observe-as-Ghost|preserve-exact-Tracked) original-binding=binding:[0-9]+:[0-9]+ edges=[01] synthetic-ghost=0 synthetic-tracked=0 unrelated-authority=0$' || true); echo "invalid-binding-traces=$bad"
  invalid-binding-traces=0
  $ VEROCAML_TEST_INSTANCE_MODE_TRACE=1 ./instance_modes_tool.exe reject-zero-no-flow artifacts/exec_call_tracked_bare.cmt 2>artifacts/exec-call-tracked-bare.trace
  semantic rejection; ghost-formal-flows=0; solvers=0
  $ test ! -s artifacts/exec-call-tracked-bare.trace

Raw semantic provenance cannot select the standalone-Proof bare-Tracked
exception.  The private retained-CMT boundary mutates only the otherwise valid
caller's Proof-body provenance, then requires semantic rejection before any
forgetting edge, backend solver, Z3 context, or standalone-proof trace.

  $ for pass in 1 2; do VEROCAML_TEST_INSTANCE_MODE_TRACE=1 ./instance_modes_tool.exe reject-raw-proof-body-no-authority artifacts/tracked_call_bare.cmt >"artifacts/raw-proof-body.$pass.out" 2>"artifacts/raw-proof-body.$pass.trace"; done
  $ cmp artifacts/raw-proof-body.1.out artifacts/raw-proof-body.2.out
  $ cmp artifacts/raw-proof-body.1.trace artifacts/raw-proof-body.2.trace
  $ cat artifacts/raw-proof-body.1.out
  raw-proof-body rejection; boundary=validation; ghost-formal-flows=0; backend-solvers=0; z3-contexts=0; z3-solvers=0
  $ printf 'standalone-proof-binding-traces=%s raw-body-authenticated-claims=%s\n' "$(grep -c '^standalone-proof-binding ' artifacts/raw-proof-body.1.trace || true)" "$(grep -c 'authenticated=true' artifacts/raw-proof-body.1.trace || true)"
  standalone-proof-binding-traces=0 raw-body-authenticated-claims=0
  $ test ! -s artifacts/raw-proof-body.1.trace

Bare Exec, Tracked, and Ghost values cross each supported default-Ghost
call boundary without an occurrence annotation.  The private retained trace
authenticates the incoming mode, binds an exact Ghost formal/result, records
one forgetting edge, and issues no synthetic Ghost or Tracked descriptor.

  $ retained positive_forgetting
  $ ./instance_modes_tool.exe modes artifacts/positive_forgetting.cmt > artifacts/positive.modes
  $ grep -E '^(formal:[0-3]:0|result:[0-3]) ' artifacts/positive.modes | sed -E 's/ annotation=[^ ]+/ annotation=SPAN/' | sed -E 's/ snapshot=.*//'
  formal:0:0 mode=Ghost explicit=false annotation=SPAN default=Ghost type=int
  formal:1:0 mode=Ghost explicit=false annotation=SPAN default=Ghost type=int
  formal:2:0 mode=Ghost explicit=false annotation=SPAN default=Ghost type=int
  formal:3:0 mode=Tracked explicit=true annotation=SPAN default=Ghost type=int
  result:0 mode=Ghost explicit=false annotation=SPAN default=Ghost type=bool
  result:1 mode=Ghost explicit=false annotation=SPAN default=Ghost type=unit
  result:2 mode=Ghost explicit=false annotation=SPAN default=Ghost type=unit
  result:3 mode=Tracked explicit=true annotation=SPAN default=Ghost type=int
  $ grep '^forgetting-edge .*caller=run#4 ' artifacts/positive.modes > artifacts/positive.trace
  $ printf 'rows=%s\n' "$(wc -l < artifacts/positive.trace)"
  rows=9
  $ for mode in Exec Tracked Ghost; do printf '%s=%s\n' "$mode" "$(grep -c "incoming=$mode " artifacts/positive.trace)"; done
  Exec=3
  Tracked=3
  Ghost=3
  $ printf 'spec=%s\n' "$(grep -c 'callee-mode=Spec recursive=false' artifacts/positive.trace)"; printf 'proof=%s\n' "$(grep -c 'callee-mode=Proof recursive=false' artifacts/positive.trace)"; printf 'recursive-proof=%s\n' "$(grep -c 'callee-mode=Proof recursive=true' artifacts/positive.trace)"
  spec=3
  proof=3
  recursive-proof=3
  $ bad=$(grep -Evc 'authenticated=true formal-mode=Ghost boundary-result-mode=Ghost callee-result-mode=Ghost edges=1 synthetic-ghost=0 synthetic-tracked=0$' artifacts/positive.trace || true); echo "invalid-traces=$bad"
  invalid-traces=0
  $ ./instance_modes_tool.exe sst artifacts/positive_forgetting.cmt > artifacts/positive.sst
  $ grep -E '^function (nonnegative|proof_observe|recursive_observe|tracked_successor|run)' artifacts/positive.sst | sed -E 's/ @ .*//'
  function nonnegative#0 mode=spec recursive=false result=bool policy=default-linear/default-z3
  function proof_observe#1 mode=proof recursive=false result=unit policy=default-linear/default-z3
  function recursive_observe#2 mode=proof recursive=true result=unit policy=default-linear/default-z3
  function tracked_successor#3 mode=proof recursive=false result=int policy=default-linear/default-z3
  function run#4 mode=exec recursive=false result=int policy=default-linear/default-z3
  $ ./instance_modes_tool.exe vir artifacts/positive_forgetting.cmt > artifacts/positive.vir
  $ grep '^  vc ' artifacts/positive.vir | sed -E 's/^  vc [0-9]+ ([^ ]+).*/\1/' | sort | uniq -c
        1 assertion
        8 call-precondition
        1 entry-measure-nonnegative
        5 postcondition
        1 recursive-call-measure-nonnegative
        1 recursive-call-strict-descent
  $ ./instance_modes_tool.exe solve artifacts/positive_forgetting.cmt
  proof_observe: verified (2 obligations)
  recursive_observe: verified (6 obligations)
  tracked_successor: verified (1 obligations)
  run: verified (8 obligations)
  $ ocamlc -stop-after parsing -dsource -ppx ../../ppx/vero_ppx.exe fixtures/positive_forgetting.ml >/dev/null 2>artifacts/positive.source
  $ grep '^let run' artifacts/positive.source
  let run source = (); source[@@verocaml.internal.artifact_family.ordinary-v1 ]
  $ test "$(grep -c '^let ' artifacts/positive.source)" -eq 1
  $ grep -E 'nonnegative|proof_observe|recursive_observe|tracked_successor|ghost|tracked' artifacts/positive.source >/dev/null; test $? -ne 0
  $ ocamlc -w -A -alert -all -bin-annot -ppx ../../ppx/vero_ppx.exe -c -o artifacts/ordinary-positive.cmo fixtures/positive_forgetting.ml
  $ ocamlopt -w -A -alert -all -ppx ../../ppx/vero_ppx.exe -c -o artifacts/ordinary-positive.cmx fixtures/positive_forgetting.ml
  $ strings artifacts/ordinary-positive.cmo artifacts/ordinary-positive.cmx | grep -E 'nonnegative|proof_observe|recursive_observe|tracked_successor' || echo 'byte/native proof and mode carriers absent'
  byte/native proof and mode carriers absent

Records, positional variants, exact patterns/projections, and verifier-only erased
field updates are retained and validated; ordinary compilation removes every
erased carrier and whole update.

  $ retained aggregate
  $ ../../src/verocaml.exe verify artifacts/aggregate.cmt 2>&1 | sed -E 's,file=[^ ]+,file=aggregate.cmt,'
  verocaml: verified file=aggregate.cmt functions=4 obligations=0
  $ ./instance_modes_tool.exe modes artifacts/aggregate.cmt > artifacts/aggregate.modes
  $ for site in binding expression field pattern; do printf '%s=' "$site"; grep "^$site:" artifacts/aggregate.modes | sed -E 's/.* mode=([^ ]+).*/\1/' | sort -u | tr '\n' ',' | sed 's/,$//'; echo; done
  binding=Exec,Ghost,Tracked
  expression=Exec,Ghost,Tracked
  field=Exec,Ghost,Tracked
  pattern=Exec,Ghost,Tracked
  $ ocamlc -stop-after parsing -dsource -ppx ../../ppx/vero_ppx.exe fixtures/aggregate.ml >/dev/null 2>artifacts/aggregate.source
  $ grep -E 'ghost|tracked|\.ghost|\.tracked' artifacts/aggregate.source | grep -v artifact_family || echo 'all erased aggregate carriers absent'
  all erased aggregate carriers absent
  $ grep -E '^type packet|^type choice|^let read ' artifacts/aggregate.source
  type packet = {
  type choice =
  let read p = let { run } = p in (); (); run[@@verocaml.internal.artifact_family.ordinary-v1
  $ ocamlc -w -A -alert -all -bin-annot -ppx ../../ppx/vero_ppx.exe -c -o artifacts/ordinary-aggregate.cmo fixtures/aggregate.ml
  $ ocamlopt -w -A -alert -all -ppx ../../ppx/vero_ppx.exe -c -o artifacts/ordinary-aggregate.cmx fixtures/aggregate.ml
  $ strings artifacts/ordinary-aggregate.cmo artifacts/ordinary-aggregate.cmx | grep -E 'ghost|tracked' || echo 'byte/native carriers absent'
  byte/native carriers absent
  $ ocamlc -w -A -alert -all -ppx ../../ppx/vero_ppx.exe -o artifacts/ordinary-update-absence.exe fixtures/ordinary_update_absence.ml
  $ artifacts/ordinary-update-absence.exe
  effects=0

Ordinary erasure does not evaluate erased RHS effects. Retained semantic
validation rejects the effectful erased expression before a backend exists.

  $ ocamlc -w -A -alert -all -ppx ../../ppx/vero_ppx.exe -o artifacts/ordinary_absence.exe fixtures/ordinary_absence.ml
  $ artifacts/ordinary_absence.exe
  effects=0
  $ retained ordinary_absence
  $ ./instance_modes_tool.exe reject-zero artifacts/ordinary_absence.cmt
  adapter rejection; solvers=0

Standalone signatures and implementations are rewritten into distinct ordinary
and retained ABI families. Crossing a retained CMT with an ordinary CMI is
rejected during input authentication.

  $ mkdir artifacts/retained artifacts/ordinary
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/retained/signatures.cmi fixtures/signatures.mli
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -I artifacts/retained -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/retained/signatures.cmo fixtures/signatures.ml
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx ../../ppx/vero_ppx.exe -c -o artifacts/ordinary/signatures.cmi fixtures/signatures.mli
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -I artifacts/ordinary -ppx ../../ppx/vero_ppx.exe -c -o artifacts/ordinary/signatures.cmo fixtures/signatures.ml
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -I artifacts/retained -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/retained/signatures_client.cmo fixtures/signatures_client.ml
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -I artifacts/ordinary -ppx ../../ppx/vero_ppx.exe -c -o artifacts/ordinary/signatures_client.cmo fixtures/signatures_client.ml
  $ ./instance_modes_tool.exe dependency-modes artifacts/retained/signatures_client.cmt artifacts/retained/signatures.cmt
  same-invocation retained modes authenticated; invariant-facts=0
  $ cp artifacts/retained/signatures.cmt artifacts/mixed.cmt
  $ cp artifacts/ordinary/signatures.cmi artifacts/mixed.cmi
  $ ../../src/verocaml.exe verify artifacts/mixed.cmt 2>&1 | grep -E 'MALFORMED|digest mismatch|INPUT'
  verocaml: error[VERO_MALFORMED_INPUT] input is not a complete typed-tree artifact @ artifacts/mixed.cmt:1:0-1:0
  $ cp artifacts/ordinary/signatures.cmt artifacts/reverse-mixed.cmt
  $ cp artifacts/retained/signatures.cmi artifacts/reverse-mixed.cmi
  $ ../../src/verocaml.exe verify artifacts/reverse-mixed.cmt 2>&1 | grep -E 'MALFORMED|digest mismatch|INPUT'
  verocaml: error[VERO_MALFORMED_INPUT] input is not a complete typed-tree artifact @ artifacts/reverse-mixed.cmt:1:0-1:0
  $ ./instance_modes_tool.exe embed-ordinary-interface artifacts/retained/signatures.cmt artifacts/ordinary/signatures.cmi artifacts/embedded-mixed.cmt
  $ ./instance_modes_tool.exe reject-zero artifacts/embedded-mixed.cmt 2>/dev/null
  adapter rejection; solvers=0

The verifier binds retained implementation value and field modes to the exact
separately compiled interface snapshot. OCaml-compatible mode substitutions in
either surface reject before a solver can be created.

  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/signature_mode_mismatch.cmi fixtures/signature_mode_mismatch.mli
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -I artifacts -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/signature_mode_mismatch.cmo fixtures/signature_mode_mismatch.ml
  $ ./instance_modes_tool.exe reject-zero artifacts/signature_mode_mismatch.cmt
  semantic rejection; solvers=0
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/type_mode_mismatch.cmi fixtures/type_mode_mismatch.mli
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -I artifacts -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/type_mode_mismatch.cmo fixtures/type_mode_mismatch.ml
  $ ./instance_modes_tool.exe reject-zero artifacts/type_mode_mismatch.cmt
  semantic rejection; solvers=0

Missing actual annotations, cross-mode escalation, recursive tracked signatures,
unsupported aggregate syntax, erased effects, Ghost invariant authority,
mode-bearing external/trusted boundaries, and copied SST all reject with zero
solver creation.

  $ for n in missing_actual cross_mode tracked_formal_bare open_pattern erased_effect ghost_authority; do retained "$n"; ./instance_modes_tool.exe reject-zero "artifacts/$n.cmt"; done
  semantic rejection; solvers=0
  semantic rejection; solvers=0
  semantic rejection; solvers=0
  adapter rejection; solvers=0
  semantic rejection; solvers=0
  semantic rejection; solvers=0
  $ for n in external_mode trusted_mode; do retained "$n"; ./instance_modes_tool.exe reject-zero "artifacts/$n.cmt"; done
  adapter rejection; solvers=0
  semantic rejection; solvers=0
  $ retained recursive_tracked 2>&1 | grep -F 'reject mode annotations'
         reject mode annotations
  $ retained inherited_update 2>&1 | grep -F 'inherited record updates are unsupported'
  Error: inherited record updates are unsupported with instance modes
  $ ./instance_modes_tool.exe copied artifacts/modes.cmt
  copied descriptor and SST rejected; solvers=0

An executable call is still forbidden inside the pure erased actual subtree.
It rejects before the forgetting edge, while ordinary PPX erasure removes the
whole proof call rather than evaluating the subtree.

  $ retained forgetting_effect
  $ VEROCAML_TEST_INSTANCE_MODE_TRACE=1 ./instance_modes_tool.exe reject-zero-no-flow artifacts/forgetting_effect.cmt 2>artifacts/forgetting-effect.trace
  semantic rejection; ghost-formal-flows=0; solvers=0
  $ test ! -s artifacts/forgetting-effect.trace
  $ ocamlc -stop-after parsing -dsource -ppx ../../ppx/vero_ppx.exe fixtures/forgetting_effect.ml >/dev/null 2>artifacts/forgetting-effect.source
  $ grep '^let bad' artifacts/forgetting-effect.source
  let bad source = (); source[@@verocaml.internal.artifact_family.ordinary-v1 ]

The annotation waiver belongs only to an authenticated default Ghost formal.
Explicitly Ghost specification and nonrecursive-proof formals still require an
explicitly Ghost actual from executable code and reject before any forgetting
edge or solver.

  $ : > artifacts/explicit-ghost.trace
  $ for n in explicit_ghost_spec_bare explicit_ghost_proof_bare; do retained "$n"; VEROCAML_TEST_INSTANCE_MODE_TRACE=1 ./instance_modes_tool.exe reject-zero-no-flow "artifacts/$n.cmt" 2>>artifacts/explicit-ghost.trace; done
  semantic rejection; ghost-formal-flows=0; solvers=0
  semantic rejection; ghost-formal-flows=0; solvers=0
  $ test ! -s artifacts/explicit-ghost.trace

The unsupported-syntax and erased-effect matrix pins duplicate/conflicting
annotations, multi-bindings, record puns, missing record/positional components,
field-update mismatches, mutation, exceptions, divergence, runtime-identity
allocation, captures, and nested update effects. Every surviving partial or
complete CMT rejects without constructing a solver.

  $ python3 generate_negative_matrix.py artifacts/negative
  $ mkdir artifacts/negative-cmt
  $ compiled=0; rejected=0; for f in artifacts/negative/*.ml; do n=$(basename "$f" .ml); if ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/negative-cmt/$n.cmo" "$f" >/dev/null 2>&1; then compiled=$((compiled + 1)); else rejected=$((rejected + 1)); fi; done; echo "compiled=$compiled compiler-rejected=$rejected matrix=$((compiled + rejected))"
  compiled=5 compiler-rejected=9 matrix=14
  $ for cmt in artifacts/negative-cmt/*.cmt; do ./instance_modes_tool.exe reject-zero "$cmt"; done | sort | uniq -c
        9 adapter rejection; solvers=0
        5 semantic rejection; solvers=0

Invariant authority is established only by locally checked exact Exec/Tracked
constructions and is available on those same symbolic paths. The abstract
constructor and both local reconstructions emit establishment before use; no
entry or call-result receipt is involved.

  $ retained local_established_invariant
  $ ../../src/verocaml.exe verify artifacts/local_established_invariant.cmt --dump-vir artifacts/local.vir 2>&1 | sed -E 's,file=[^ ]+,file=local.cmt,'
  verocaml: verified file=local.cmt functions=2 obligations=4
  $ grep -c 'boundary=constructor-establishment' artifacts/local.vir
  3
  $ grep -Eqi 'call-result|entry.*invariant' artifacts/local.vir; test $? -ne 0

C-005-001's exact-bound-formal matrix covers both Ghost-formal categories,
all three admitted actual modes, and all seven direct/laundered authority or
Tracked-escalation attacks. The six specification-to-Tracked cells reject in
the PPX; all other cells reject in semantic validation, and none creates a
solver.

  $ python3 generate_authority_matrix.py artifacts/matrix fixtures/authority_prelude.ml
  $ mkdir artifacts/matrix-cmt
  $ compiled=0; ppx_rejected=0; for f in artifacts/matrix/*.ml; do n=$(basename "$f" .ml); if ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/matrix-cmt/$n.cmo" "$f" >/dev/null 2>&1; then compiled=$((compiled + 1)); else ppx_rejected=$((ppx_rejected + 1)); fi; done; echo "compiled=$compiled ppx-rejected=$ppx_rejected matrix=$((compiled + ppx_rejected))"
  compiled=36 ppx-rejected=6 matrix=42
  $ : > artifacts/matrix.trace
  $ for cmt in artifacts/matrix-cmt/*.cmt; do VEROCAML_TEST_INSTANCE_MODE_TRACE=1 ./instance_modes_tool.exe reject-zero-flow "$cmt" 2>>artifacts/matrix.trace; done | sort | uniq -c
        6 adapter rejection; ghost-formal-flows=0; solvers=0
       36 semantic rejection; ghost-formal-flows=1; solvers=0
  $ printf 'semantic-traces=%s adapter-before-edge=%s\n' "$(grep -c '^forgetting-edge' artifacts/matrix.trace)" "$((42 - $(grep -c '^forgetting-edge' artifacts/matrix.trace)))"
  semantic-traces=36 adapter-before-edge=6
  $ bad=$(grep '^forgetting-edge' artifacts/matrix.trace | grep -Evc 'incoming=Ghost .*authenticated=true formal-mode=Ghost boundary-result-mode=Ghost .*edges=1 synthetic-ghost=0 synthetic-tracked=0$' || true); echo "invalid-traces=$bad"
  invalid-traces=0
  $ for mode in Ghost Tracked; do printf 'declared-result-%s=%s\n' "$mode" "$(grep -c "callee-result-mode=$mode " artifacts/matrix.trace)"; done
  declared-result-Ghost=33
  declared-result-Tracked=3

The unannotated companion matrix reaches the same six attacks through both
aggregate-capable Ghost-formal boundaries.  Every Exec, Tracked, and Ghost row
authenticates its real incoming mode, records one Ghost-only forgetting edge,
issues no synthetic erased descriptor, and rejects before solver creation.

  $ python3 generate_unannotated_authority_matrix.py artifacts/unannotated fixtures/authority_prelude.ml
  $ mkdir artifacts/unannotated-cmt
  $ for f in artifacts/unannotated/*.ml; do n=$(basename "$f" .ml); ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/unannotated-cmt/$n.cmo" "$f"; done
  $ printf 'compiled=%s matrix=%s\n' "$(find artifacts/unannotated-cmt -name '*.cmt' | wc -l)" "$(find artifacts/unannotated -name '*.ml' | wc -l)"
  compiled=36 matrix=36
  $ : > artifacts/unannotated.trace
  $ for cmt in artifacts/unannotated-cmt/*.cmt; do VEROCAML_TEST_INSTANCE_MODE_TRACE=1 ./instance_modes_tool.exe reject-zero-flow "$cmt" 2>>artifacts/unannotated.trace; done | sort | uniq -c
       36 semantic rejection; ghost-formal-flows=1; solvers=0
  $ grep '^forgetting-edge' artifacts/unannotated.trace | sed -E 's/.*callee-mode=([^ ]+).*incoming=([^ ]+).*formal-mode=([^ ]+).*boundary-result-mode=([^ ]+).*callee-result-mode=([^ ]+).*edges=([0-9]+).*synthetic-ghost=([0-9]+).*synthetic-tracked=([0-9]+).*/callee=\1 incoming=\2 formal=\3 boundary-result=\4 callee-result=\5 edges=\6 synthetic-ghost=\7 synthetic-tracked=\8/' | sort | uniq -c
        6 callee=Proof incoming=Exec formal=Ghost boundary-result=Ghost callee-result=Ghost edges=1 synthetic-ghost=0 synthetic-tracked=0
        6 callee=Proof incoming=Ghost formal=Ghost boundary-result=Ghost callee-result=Ghost edges=1 synthetic-ghost=0 synthetic-tracked=0
        6 callee=Proof incoming=Tracked formal=Ghost boundary-result=Ghost callee-result=Ghost edges=1 synthetic-ghost=0 synthetic-tracked=0
        6 callee=Spec incoming=Exec formal=Ghost boundary-result=Ghost callee-result=Ghost edges=1 synthetic-ghost=0 synthetic-tracked=0
        6 callee=Spec incoming=Ghost formal=Ghost boundary-result=Ghost callee-result=Ghost edges=1 synthetic-ghost=0 synthetic-tracked=0
        6 callee=Spec incoming=Tracked formal=Ghost boundary-result=Ghost callee-result=Ghost edges=1 synthetic-ghost=0 synthetic-tracked=0
