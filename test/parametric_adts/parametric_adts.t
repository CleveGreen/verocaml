The focused suite uses retained CMT and source routes for actual compiler
option/list/result and user generic variants, records, and branching trees.

  $ mkdir artifacts
  $ retained () { name=$1; ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -I artifacts -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$name.cmo" "fixtures/$name.ml"; }
  $ for n in structures equality proof_matrix generic_option_spec_contract generic_option_spec_binder_mismatch generic_repeated_binder_mismatch descriptor_laws wrong_constructor_result swapped_record open_equality recursive_equality unit_payload_equality function_payload_equality ref_payload_equality array_payload_equality unauthenticated_descent changed_recursive_arguments generic_mutual mutable_generic alpha_a alpha_b; do retained "$n"; done

A type-changing callback relation over option payloads keeps distinct input and
output binders and verifies identically from source and retained CMT.

  $ retained type_changing_option_map
  $ OCAML_COLOR=never ../../src/verocaml.exe verify fixtures/type_changing_option_map.ml --threads 1 --timeout-ms 10000
  verocaml: verified file=fixtures/type_changing_option_map.ml functions=1 obligations=10
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/type_changing_option_map.cmt --threads 2 --timeout-ms 10000 --dump-sst artifacts/type-changing-option-map.sst
  verocaml: verified file=artifacts/type_changing_option_map.cmt functions=1 obligations=10
  $ grep '^function map_option#0 binders=' artifacts/type-changing-option-map.sst | sed -E 's/ @ .*//'
  function map_option#0 binders=['0@map_option#0, '1@map_option#0] mode=exec recursive=false result=option<'1@map_option#0> policy=default-linear/default-z3

  $ OCAML_COLOR=never ../../src/verocaml.exe verify fixtures/structures.ml --threads 1 --timeout-ms 10000 --dump-sst artifacts/structures-source.sst --dump-vir artifacts/structures-source.vir
  verocaml: verified file=fixtures/structures.ml functions=12 obligations=40
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/structures.cmt --threads 1 --timeout-ms 10000 --dump-sst artifacts/structures-cmt.sst --dump-vir artifacts/structures-cmt.vir
  verocaml: verified file=artifacts/structures.cmt functions=12 obligations=40
  $ sed -E 's#fixtures/structures.ml#structures.ml#g; s/uid=Source\.[0-9]+/uid=Structures.<uid>/g' artifacts/structures-source.sst | sed '/^instance-modes/,$d' > artifacts/structures-source.semantic
  $ sed -E 's/uid=Structures\.[0-9]+/uid=Structures.<uid>/g' artifacts/structures-cmt.sst | sed '/^instance-modes/,$d' > artifacts/structures-cmt.semantic
  $ cmp artifacts/structures-source.semantic artifacts/structures-cmt.semantic
  $ cmp artifacts/structures-source.vir artifacts/structures-cmt.vir

Generic option and result predicates may constrain an inferred ensures binder.
The PPX-side binder is intentionally more general than the callable's concrete
result, so the retained carrier must authenticate the compiler-compatible
instance before lowering it as the exact result type.

  $ OCAML_COLOR=never ../../src/verocaml.exe verify fixtures/generic_option_spec_contract.ml --threads 1 --timeout-ms 10000
  verocaml: verified file=fixtures/generic_option_spec_contract.ml functions=2 obligations=4
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/generic_option_spec_contract.cmt --threads 2 --timeout-ms 10000
  verocaml: verified file=artifacts/generic_option_spec_contract.cmt functions=2 obligations=4

A concrete mismatch and an inconsistent repeated inferred variable are rejected
while authenticating the result binder; neither can relabel the callable's
actual result type.

  $ for name in generic_option_spec_binder_mismatch generic_repeated_binder_mismatch; do code=0; OCAML_COLOR=never ../../src/verocaml.exe verify "artifacts/$name.cmt" --threads 1 --timeout-ms 10000 >"artifacts/$name.out" 2>&1 || code=$?; test "$code" = 2; printf '%s: ' "$name"; grep -o 'VERO_[A-Z_]*' "artifacts/$name.out" | head -1; done
  generic_option_spec_binder_mismatch: VERO_MALFORMED_GHOST_CALL
  generic_repeated_binder_mismatch: VERO_MALFORMED_GHOST_CALL

The standard external specifications and local descriptors share one
UID-backed registry.  The tree has two authenticated recursive fields and no
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

Closed local and externally specified Stdlib int-option equality, inequality,
aliases, conditionals, matches, selectors, and the complete bool/int scalar
layout verify.

  $ OCAML_COLOR=never ../../src/verocaml.exe verify fixtures/equality.ml --threads 1 --timeout-ms 10000 --dump-sst artifacts/equality-source.sst --dump-vir artifacts/equality-source.vir
  verocaml: verified file=fixtures/equality.ml functions=7 obligations=25
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/equality.cmt --threads 2 --timeout-ms 10000 --dump-sst artifacts/equality-cmt.sst --dump-vir artifacts/equality-cmt.vir
  verocaml: verified file=artifacts/equality.cmt functions=7 obligations=25
  $ cmp artifacts/equality-source.vir artifacts/equality-cmt.vir
  $ grep -q 'tag.option_specification<int>' artifacts/equality-source.vir
  $ grep -q 'tag.local_option<int>' artifacts/equality-source.vir

The inherited Proof grammar includes the exact recursive direct-None assertion,
a second nominal variant, immutable lets, match/helper actuals, a record
projection, and entry/nonnegative/strict-descent/ordered local obligations.

  $ grep 'spec_opt_eq (spec_index (idx - 1) rest) None' fixtures/proof_matrix.ml
          spec_opt_eq (spec_index (idx - 1) rest) None];
  $ OCAML_COLOR=never ../../src/verocaml.exe verify fixtures/proof_matrix.ml --threads 1 --timeout-ms 10000 --dump-sst artifacts/proof-source.sst --dump-vir artifacts/proof-source.vir
  verocaml: verified file=fixtures/proof_matrix.ml functions=1 obligations=8
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/proof_matrix.cmt --threads 2 --timeout-ms 10000 --dump-sst artifacts/proof-cmt.sst --dump-vir artifacts/proof-cmt.vir
  verocaml: verified file=artifacts/proof_matrix.cmt functions=1 obligations=8
  $ cmp artifacts/proof-source.vir artifacts/proof-cmt.vir
  $ grep -E '^  vc [0-3] (entry-measure-nonnegative|recursive-call-measure-nonnegative|recursive-call-strict-descent|local-assertion)' artifacts/proof-source.vir | sed -E 's/ declaration=.*| callee=.*| ordinal=.*//'
    vc 0 entry-measure-nonnegative
    vc 1 recursive-call-measure-nonnegative
    vc 2 recursive-call-strict-descent
    vc 3 local-assertion

Constructor discrimination, payload injectivity, field correspondence, and
exact branch bindings verify. Deliberately wrong constructor and swapped-field
postconditions produce counterexamples rather than proving from an opaque sort.

  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/descriptor_laws.cmt --timeout-ms 10000
  verocaml: verified file=artifacts/descriptor_laws.cmt functions=4 obligations=9
  $ counterexample () { name=$1; code=0; OCAML_COLOR=never ../../src/verocaml.exe verify "artifacts/$name.cmt" --timeout-ms 10000 >"artifacts/$name.out" 2>&1 || code=$?; test "$code" = 1; grep -E '^verocaml: counterexample function=' "artifacts/$name.out" | sed -E 's/ span=.* result=/ result=/'; }
  $ counterexample wrong_constructor_result
  verocaml: counterexample function=wrong#0 vc=postcondition[0] result=counterexample
  $ counterexample swapped_record
  verocaml: counterexample function=swapped#0 vc=postcondition[0] result=counterexample

Open, recursive, unit/function/reference/array-bearing equality and unsupported
generic recursion/mutation reject before solver/private-receipt work.

  $ reject () { name=$1; expected=$2; code=0; VEROCAML_TEST_PRIVATE_RECEIPT_TRACE=1 DELATOR_LOG=Verification_session=debug,warn DELATOR_FORMAT=flat DELATOR_COLOR=never OCAML_COLOR=never ../../src/verocaml.exe verify "artifacts/$name.cmt" --threads 1 --timeout-ms 5000 --dump-sst "artifacts/$name.sst" --dump-vir "artifacts/$name.vir" >"artifacts/$name.out" 2>&1 || code=$?; test "$code" = 2; observed=$(grep -o 'VERO_[A-Z_]*' "artifacts/$name.out" | head -1); printf '%s: %s\n' "$name" "$observed"; test "$observed" = "$expected"; test "$(grep -c 'Verification_session: private receipt ' "artifacts/$name.out")" = 0; test ! -e "artifacts/$name.sst"; test ! -e "artifacts/$name.vir"; }
  $ for spec in 'open_equality VERO_UNSUPPORTED_GENERIC_USE' 'recursive_equality VERO_UNSUPPORTED_GENERIC_USE' 'unit_payload_equality VERO_UNSUPPORTED_GENERIC_USE' 'function_payload_equality VERO_UNSUPPORTED_AGGREGATE' 'ref_payload_equality VERO_UNSUPPORTED_TYPE' 'array_payload_equality VERO_UNSUPPORTED_TYPE' 'unauthenticated_descent VERO_DEPENDENCY' 'changed_recursive_arguments VERO_DEPENDENCY' 'generic_mutual VERO_UNSUPPORTED_MUTUAL_RECURSION' 'mutable_generic VERO_UNSUPPORTED_MUTATION'; do set -- $spec; reject "$1" "$2"; done
  open_equality: VERO_UNSUPPORTED_GENERIC_USE
  recursive_equality: VERO_UNSUPPORTED_GENERIC_USE
  unit_payload_equality: VERO_UNSUPPORTED_GENERIC_USE
  function_payload_equality: VERO_UNSUPPORTED_AGGREGATE
  ref_payload_equality: VERO_UNSUPPORTED_TYPE
  array_payload_equality: VERO_UNSUPPORTED_TYPE
  unauthenticated_descent: VERO_DEPENDENCY
  changed_recursive_arguments: VERO_UNSUPPORTED_GENERIC_USE
  generic_mutual: VERO_UNSUPPORTED_MUTUAL_RECURSION
  mutable_generic: VERO_UNSUPPORTED_MUTATION

Pure descriptor construction rejects malformed binder, constructor, field, and
external proxy identity directly at [Parametric_adt.create], before malformed
state can reach SST, VIR, VC, backend, solver, or Z3 work.

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

Repeated, copied, alpha-renamed, and serial/threaded artifacts are stable.

  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/structures.cmt --threads 2 --timeout-ms 10000 --dump-sst artifacts/threaded.sst --dump-vir artifacts/threaded.vir >/dev/null
  $ cmp artifacts/structures-cmt.sst artifacts/threaded.sst
  $ cmp artifacts/structures-cmt.vir artifacts/threaded.vir
  $ cp artifacts/structures.cmt artifacts/copied.cmt
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/copied.cmt --threads 1 --timeout-ms 10000 --dump-sst artifacts/copied.sst --dump-vir artifacts/copied.vir >/dev/null
  $ sed -E 's#(artifacts/)?(structures|copied)\.cmt#FILE.cmt#g' artifacts/structures-cmt.sst > artifacts/original.norm.sst
  $ sed -E 's#(artifacts/)?(structures|copied)\.cmt#FILE.cmt#g' artifacts/copied.sst > artifacts/copied.norm.sst
  $ cmp artifacts/original.norm.sst artifacts/copied.norm.sst
  $ cmp artifacts/structures-cmt.vir artifacts/copied.vir
  $ for n in alpha_a alpha_b; do OCAML_COLOR=never ../../src/verocaml.exe verify "artifacts/$n.cmt" --threads 1 --timeout-ms 5000 --dump-sst "artifacts/$n.sst" --dump-vir "artifacts/$n.vir" >/dev/null; sed -E 's/alpha_[ab]/alpha/g; s/Alpha_[ab]/Alpha/g; s/ @ [^ ]*:[0-9][^ ]*//g' "artifacts/$n.sst" | sed '/^instance-modes/,$d' > "artifacts/$n.norm.sst"; sed -E 's/alpha_[ab]/alpha/g; s/Alpha_[ab]/Alpha/g; s/ @ [^ ]*:[0-9][^ ]*//g' "artifacts/$n.vir" > "artifacts/$n.norm.vir"; done
  $ cmp artifacts/alpha_a.norm.sst artifacts/alpha_b.norm.sst
  $ cmp artifacts/alpha_a.norm.vir artifacts/alpha_b.norm.vir

The project path selects the retained root without changing its API.

  $ OCAML_COLOR=never ../../src/verocaml.exe verify-project --root artifacts/structures.cmt artifacts/structures.cmi --threads 2 --timeout-ms 10000
  verocaml: verified unit=Structures file=artifacts/structures.cmt result=verified functions=12 obligations=40

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
  $ OCAML_COLOR=never ../../src/verocaml.exe verify fixtures/abstract_logical_equality.ml --threads 2 --timeout-ms 10000
  verocaml: verified file=fixtures/abstract_logical_equality.ml functions=3 obligations=3
  $ code=0; OCAML_COLOR=never ../../src/verocaml.exe verify fixtures/abstract_logical_equality_false.ml --threads 1 --timeout-ms 10000 > artifacts/abstract-false.out 2>&1 || code=$?; test "$code" = 1; grep -o counterexample artifacts/abstract-false.out | head -1
  counterexample
