The ordinary stable outcomes live in outcome_cases.ml.  This legacy target
retains only the descriptor/encoding and ordinary-runtime specialist contracts
listed in test/support/migrations/w05.org.

  $ mkdir artifacts
  $ retained () { name=$1; ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -I artifacts -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$name.cmo" "fixtures/$name.ml"; }
  $ for name in structures generic_option_spec_contract generic_option_spec_binder_mismatch generic_repeated_binder_mismatch; do retained "$name"; done
  $ OCAML_COLOR=never ../../src/verocaml.exe verify fixtures/structures.ml --threads 1 --timeout-ms 10000 --dump-sst artifacts/structures-source.sst >/dev/null

Generic option and result predicates may constrain an inferred ensures binder.
The retained carrier authenticates the compiler-compatible instance as the exact
callable result type before lowering.

  $ OCAML_COLOR=never ../../src/verocaml.exe verify fixtures/generic_option_spec_contract.ml --threads 1 --timeout-ms 10000
  verocaml: verified file=fixtures/generic_option_spec_contract.ml functions=2 obligations=4
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/generic_option_spec_contract.cmt --threads 2 --timeout-ms 10000
  verocaml: verified file=artifacts/generic_option_spec_contract.cmt functions=2 obligations=4

Concrete and inconsistent repeated inferred binders cannot relabel the actual
result type.

  $ for name in generic_option_spec_binder_mismatch generic_repeated_binder_mismatch; do code=0; OCAML_COLOR=never ../../src/verocaml.exe verify "artifacts/$name.cmt" --threads 1 --timeout-ms 10000 >"artifacts/$name.out" 2>&1 || code=$?; test "$code" = 2; printf '%s: ' "$name"; grep -o 'VERO_[A-Z_]*' "artifacts/$name.out" | head -1; done
  generic_option_spec_binder_mismatch: VERO_MALFORMED_GHOST_CALL
  generic_repeated_binder_mismatch: VERO_MALFORMED_GHOST_CALL

The standard external specifications and local descriptors share one
UID-backed registry. The tree has two authenticated recursive fields and no
clone function is emitted for multiple payload instantiations.

  $ ./parametric_adts_tool.exe descriptors artifacts/structures.cmt | sed -E 's/uid=[^ ]+/uid=<uid>/'
  adt option<'0@option_specification#0> uid=<uid> provenance=external-type-specification binders=1 variant[0:None()|1:Some(0:$0:'0@option_specification#0)] recursive-fields=0
  adt list<'0@list_specification#1> uid=<uid> provenance=external-type-specification binders=1 variant[0:[]()|1:::(0:$0:'0@list_specification#1,1:$1:list<'0@list_specification#1>)] recursive-fields=1
  adt Stdlib.result<'0@result_specification#2, '1@result_specification#2> uid=<uid> provenance=external-type-specification binders=2 variant[0:Ok(0:$0:'0@result_specification#2)|1:Error(0:$0:'1@result_specification#2)] recursive-fields=0
  adt box<'0@box#3> uid=<uid> provenance=local binders=1 variant[0:Box(0:$0:'0@box#3)] recursive-fields=0
  adt pair<'0@pair#4> uid=<uid> provenance=local binders=1 record{0:left:'0@pair#4;1:right:'0@pair#4} recursive-fields=0
  adt tree<'0@tree#5> uid=<uid> provenance=local binders=1 variant[0:Leaf()|1:Node(0:$0:'0@tree#5,1:$1:tree<'0@tree#5>,2:$2:tree<'0@tree#5>)] recursive-fields=2
  $ grep -E '^function (copy_option|copy_result|copy_box|make_pair|copy_pair|length|append|tree_size|tree_height)#' artifacts/structures-source.sst | sed -E 's/ policy=.*//'
  function copy_option#0 binders=['0@copy_option#0] mode=exec recursive=false result=option<'0@copy_option#0>
  function copy_result#1 binders=['0@copy_result#1, '1@copy_result#1] mode=exec recursive=false result=Stdlib.result<'0@copy_result#1, '1@copy_result#1>
  function copy_box#2 binders=['0@copy_box#2] mode=exec recursive=false result=box<'0@copy_box#2>
  function make_pair#3 binders=['0@make_pair#3] mode=exec recursive=false result=pair<'0@make_pair#3>
  function copy_pair#4 binders=['0@copy_pair#4] mode=exec recursive=false result=pair<'0@copy_pair#4>
  function length#7 binders=['0@length#7] mode=exec recursive=true result=int
  function append#8 binders=['0@append#8] mode=exec recursive=true result=list<'0@append#8>
  function tree_size#9 binders=['0@tree_size#9] mode=exec recursive=true result=int
  function tree_height#11 binders=['0@tree_height#11] mode=exec recursive=true result=int
  $ test "$(grep -c '^function [^#]*<parameter' artifacts/structures-source.sst)" = 0

Closed externally specified and local ADT tags remain distinct in the private
VIR encoding.

  $ OCAML_COLOR=never ../../src/verocaml.exe verify fixtures/equality.ml --threads 1 --timeout-ms 10000 --dump-vir artifacts/equality-source.vir >/dev/null
  $ grep -q 'tag.option_specification<int>' artifacts/equality-source.vir
  $ grep -q 'tag.local_option<int>' artifacts/equality-source.vir

Pure descriptor construction rejects malformed binder, constructor, field, and
external proxy identity directly at Parametric_adt.create, before malformed
state can reach downstream work.

  $ ./parametric_adts_tool.exe descriptor-unit artifacts/structures.cmt
  canonical-local accepted
  binder-order rejected: binders must have dense declaration order
  binder-owner rejected: binders must belong to the descriptor type identity
  constructor-order rejected: constructors must have dense zero-based indices
  field-order rejected: constructor Box fields must have dense zero-based indices
  field-template rejected: constructor Box field template escapes descriptor binders
  empty-external-proxy rejected: external proxy compiler identity is empty
  canonical-stdlib.result name=result_specification uid=[intf]Stdlib.214 accepted
  canonical-list name=list_specification uid=<predef:list> accepted
  canonical-option name=option_specification uid=<predef:option> accepted
  descriptor-boundary=Parametric_adt.create downstream=none backend-delta=0 z3-delta=0

Ordinary byte and native programs erase verifier metadata and agree at runtime.

  $ ocamlc -w -A -alert -all -bin-annot -ppx ../../ppx/vero_ppx.exe -c -o artifacts/runtime.cmo fixtures/runtime.ml
  $ ocamlc -I artifacts artifacts/runtime.cmo -o artifacts/runtime.byte
  $ artifacts/runtime.byte
  parametric-adts-runtime: ok
  $ ocamlopt -w -A -alert -all -bin-annot -ppx ../../ppx/vero_ppx.exe -c -o artifacts/runtime.cmx fixtures/runtime.ml
  $ ocamlopt -I artifacts artifacts/runtime.cmx -o artifacts/runtime.native
  $ artifacts/runtime.native
  parametric-adts-runtime: ok
  $ for file in artifacts/runtime.cmt artifacts/runtime.cmo artifacts/runtime.cmx artifacts/runtime.byte artifacts/runtime.native; do strings "$file" | grep -E 'verocaml:|Vero_ghost' && exit 1 || :; done
