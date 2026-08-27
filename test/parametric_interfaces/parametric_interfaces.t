The private signature owner rejects malformed vectors without backend work.

  $ ./parametric_interfaces_tool.exe signature-unit
  alpha-semantic=equal fingerprint=8b31f61b715e978d1d59463e96d0ae83
  complete-substitution=int
  missing-type-argument=rejected
  extra-type-argument=rejected
  wrong-label=rejected
  wrong-result=rejected
  binder-owner=rejected
  binder-ordinal=rejected
  binder-order=rejected
  mode-vector=rejected
  label-default-vector=rejected
  missing-recursion-evidence=rejected
  forged-nonempty-recursion-evidence=rejected
  first-order-abi-negatives=14 names=retained_wrong_label,retained_reordered_same_type,retained_binding_id_collision,retained_omitted_argument,retained_default_argument,retained_optional_argument,retained_destructured_formal,retained_wildcard_formal,retained_invalid_result_binder,retained_partial_application,retained_ambiguous_alias,retained_ambiguous_open,retained_functor_path,public_executor_only_provider
  pre-sst-vir driver=0 pipeline=0 solver=0 z3=0/0

Build one real retained provider and concrete and open consumers.

  $ mkdir artifacts
  $ retained () { name=$1; ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -I artifacts -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$name.cmo" "fixtures/$name.ml"; }
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/provider.cmi fixtures/provider.mli
  $ retained provider
  $ retained consumer
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -I artifacts -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/open_consumer.cmi fixtures/open_consumer.mli
  $ retained open_consumer
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/consumer.cmt --timeout-ms 60000 --dependency artifacts/provider.cmt --dump-sst artifacts/consumer.sst --dump-vir artifacts/consumer.vir | sed -E 's/interface-digest=[0-9a-f]+/interface-digest=<digest>/'
  verocaml: verified dependency unit=Provider interface-digest=<digest> direct=none transitive=none trust=none
  verocaml: verified file=artifacts/consumer.cmt functions=14 obligations=2
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/open_consumer.cmt --timeout-ms 60000 --dependency artifacts/provider.cmt --dump-sst artifacts/open.sst | sed -E 's/interface-digest=[0-9a-f]+/interface-digest=<digest>/'
  verocaml: verified dependency unit=Provider interface-digest=<digest> direct=none transitive=none trust=none
  verocaml: verified file=artifacts/open_consumer.cmt functions=5 obligations=1
  $ grep -c 'type-arguments=\[int\]' artifacts/consumer.sst
  10
  $ grep -c 'type-arguments=\[bool\]' artifacts/consumer.sst
  2
  $ grep -c 'optional-absent' artifacts/consumer.sst
  1
  $ grep -c 'optional-forward' artifacts/consumer.sst
  2
  $ grep '^adt ' artifacts/consumer.sst | sed -E 's/ uid=[^ ]+/ uid=<uid>/; s/#[0-9]+/#<id>/g'
  adt Stdlib.option<'0@Stdlib.option#-1> uid=<uid> provenance=pinned-option binders=1 variant[0:None()|1:Some(0:$0:'0@Stdlib.option#-1)] recursive-fields=0
  adt Stdlib.list<'0@Stdlib.list#-2> uid=<uid> provenance=pinned-list binders=1 variant[0:[]()|1:::(0:$0:'0@Stdlib.list#-2,1:$1:Stdlib.list<'0@Stdlib.list#-2>)] recursive-fields=1
  adt Stdlib.result<'0@Stdlib.result#-3, '1@Stdlib.result#-3> uid=<uid> provenance=pinned-result binders=2 variant[0:Ok(0:$0:'0@Stdlib.result#-3)|1:Error(0:$0:'1@Stdlib.result#-3)] recursive-fields=0
  adt box<'0@Provider.box#<id>> uid=<uid> provenance=local binders=1 record{0:value:'0@Provider.box#<id>} recursive-fields=0
  adt tree<'0@Provider.tree#<id>> uid=<uid> provenance=local binders=1 variant[0:Leaf()|1:Node(0:$0:'0@Provider.tree#<id>,1:$1:tree<'0@Provider.tree#<id>>,2:$2:tree<'0@Provider.tree#<id>>)] recursive-fields=2
  $ grep -E '^function (relay|relay_tree)#.*binders' artifacts/open.sst
  function relay#0 binders=['0@relay#0] mode=exec recursive=false result='0@relay#0 policy=default-linear/default-z3 @ open_consumer.ml:1:0-1:35
  function relay_tree#1 binders=['0@relay_tree#1] mode=exec recursive=false result=tree<'0@relay_tree#1> policy=default-linear/default-z3 @ open_consumer.ml:2:0-2:45

Raw, forged, textual, and stale-recursion authority attacks reject
before any consumer SST/VIR, backend, solver, or Z3 work.

  $ ./parametric_interfaces_tool.exe authority-negatives artifacts/provider.cmt artifacts/consumer.cmt
  raw-retained-summary=rejected boundary=pre-sst-vir-backend-solver-z3
  forged-retained-summary=rejected boundary=pre-sst-vir-backend-solver-z3
  textual-specialization-authority=rejected boundary=pre-sst-vir-backend-solver-z3
  stale-recursive-body-evidence=rejected boundary=pre-sst-vir-backend-solver-z3
  authority-zero-work driver=0 pipeline=0 solver=0 z3=0/0

Source and retained-CMT consumers have the same canonical call vectors.

  $ cp fixtures/consumer.ml artifacts/source_consumer.ml
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/source_consumer.ml --timeout-ms 60000 --dependency artifacts/provider.cmt --dump-sst artifacts/source.sst >/dev/null
  $ grep 'call .*type-arguments=' artifacts/source.sst | sed -E 's#source_consumer[.]ml#consumer.ml#g' > artifacts/source.norm
  $ grep 'call .*type-arguments=' artifacts/consumer.sst > artifacts/cmt.norm
  $ cmp artifacts/source.norm artifacts/cmt.norm

Repeated and copied loads are deterministic.

  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/consumer.cmt --timeout-ms 60000 --dependency artifacts/provider.cmt --dump-sst artifacts/repeat.sst >/dev/null
  $ cmp artifacts/consumer.sst artifacts/repeat.sst
  $ mkdir artifacts/copied
  $ cp artifacts/provider.cmi artifacts/provider.cmt artifacts/consumer.cmi artifacts/consumer.cmt artifacts/copied/
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/copied/consumer.cmt --timeout-ms 60000 --dependency artifacts/copied/provider.cmt --dump-sst artifacts/copied.sst >/dev/null
  $ sed -E 's#artifacts/copied/#artifacts/#g' artifacts/copied.sst > artifacts/copied.norm
  $ cmp artifacts/consumer.sst artifacts/copied.norm
  $ ./parametric_interfaces_tool.exe artifact-determinism artifacts/provider.cmt artifacts/consumer.cmt artifacts/copied/provider.cmt artifacts/copied/consumer.cmt
  artifact-signatures callables=13 reload=deterministic copy=deterministic

Alpha-renamed schemes have equal semantic identity and distinct provenance.

  $ for name in alpha_a alpha_b; do ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$name.cmi" "fixtures/$name.mli"; retained "$name"; done
  $ retained alpha_consumer_a
  $ retained alpha_consumer_b
  $ ./parametric_interfaces_tool.exe alpha-artifacts artifacts/alpha_a.cmt artifacts/alpha_consumer_a.cmt artifacts/alpha_b.cmt artifacts/alpha_consumer_b.cmt
  alpha-artifacts semantic=equal provenance=distinct units=Alpha_a/Alpha_b
  $ code=0; OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/alpha_consumer_a.cmt --timeout-ms 60000 --dependency artifacts/alpha_b.cmt --dump-sst artifacts/cross.sst >artifacts/cross.out 2>&1 || code=$?; test "$code" = 2
  $ grep -o 'error\[VERO_[A-Z_]*\]' artifacts/cross.out | head -1
  error[VERO_DEPENDENCY]
  $ test ! -e artifacts/cross.sst
  $ code=0; OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/alpha_consumer_a.cmt --timeout-ms 60000 --dependency artifacts/alpha_b.cmt >artifacts/cross.repeat.out 2>&1 || code=$?; test "$code" = 2
  $ cmp artifacts/cross.out artifacts/cross.repeat.out

Partial, callback, higher-order, and wrong-mode uses reject without outputs.

  $ for name in partial callback wrong_mode; do retained "$name"; code=0; OCAML_COLOR=never ../../src/verocaml.exe verify "artifacts/$name.cmt" --timeout-ms 60000 --dependency artifacts/provider.cmt --dump-sst "artifacts/$name.sst" --dump-vir "artifacts/$name.vir" >"artifacts/$name.out" 2>&1 || code=$?; test "$code" = 2; test ! -e "artifacts/$name.sst" && test ! -e "artifacts/$name.vir"; printf '%s: ' "$name"; grep -o 'error\[VERO_[A-Z_]*\]' "artifacts/$name.out" | head -1; done
  partial: error[VERO_UNSUPPORTED_TOP_LEVEL_BINDING]
  callback: error[VERO_UNSUPPORTED_HIGHER_ORDER_FUNCTION]
  wrong_mode: error[VERO_DEPENDENCY]

Swapped CMI/CMT and ordinary/retained family mixtures reject.

  $ mkdir artifacts/swapped
  $ cp artifacts/alpha_a.cmt artifacts/swapped/alpha_a.cmt
  $ cp artifacts/alpha_b.cmi artifacts/swapped/alpha_a.cmi
  $ code=0; OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/alpha_consumer_a.cmt --timeout-ms 60000 --dependency artifacts/swapped/alpha_a.cmt >artifacts/swapped.out 2>&1 || code=$?; test "$code" = 2
  $ grep -o 'error\[VERO_[A-Z_]*\]' artifacts/swapped.out | head -1
  error[VERO_DEPENDENCY]
  $ mkdir artifacts/ordinary
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe" -c -o artifacts/ordinary/provider.cmi fixtures/provider.mli
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -I artifacts/ordinary -ppx "../../ppx/vero_ppx.exe" -c -o artifacts/ordinary/provider.cmo fixtures/provider.ml
  $ code=0; OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/consumer.cmt --timeout-ms 60000 --dependency artifacts/ordinary/provider.cmt >artifacts/mixed.out 2>&1 || code=$?; test "$code" = 2
  $ grep -o 'error\[VERO_[A-Z_]*\]' artifacts/mixed.out | head -1
  error[VERO_DEPENDENCY]

Ordinary bytecode and native execution agree and carry no retained proof ABI.

  $ ocamlc -w -A -alert -all -I artifacts/ordinary -ppx "../../ppx/vero_ppx.exe" -c -o artifacts/ordinary/runtime.cmo fixtures/runtime.ml
  $ ocamlc -o artifacts/ordinary/runtime.byte artifacts/ordinary/provider.cmo artifacts/ordinary/runtime.cmo
  $ artifacts/ordinary/runtime.byte
  7 true 7 9 3 2
  $ ocamlopt -w -A -alert -all -I artifacts/ordinary -ppx "../../ppx/vero_ppx.exe" -c -o artifacts/ordinary/provider.cmx fixtures/provider.ml
  $ ocamlopt -w -A -alert -all -I artifacts/ordinary -ppx "../../ppx/vero_ppx.exe" -c -o artifacts/ordinary/runtime.cmx fixtures/runtime.ml
  $ ocamlopt -o artifacts/ordinary/runtime.native artifacts/ordinary/provider.cmx artifacts/ordinary/runtime.cmx
  $ artifacts/ordinary/runtime.native
  7 true 7 9 3 2
  $ strings artifacts/ordinary/runtime.byte artifacts/ordinary/runtime.native | grep -E 'Vero_ghost|verocaml[.]internal[.]retained' >/dev/null; test $? -ne 0
