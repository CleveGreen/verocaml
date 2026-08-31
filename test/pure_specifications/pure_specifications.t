The pure-spec semantic boundary admits definitional expansion and rejects every
raw mode, graph, shape, and type violation before VIR construction.
This is the local `raw-validation-matrix` specialist architecture exception;
it is independent of `pure-specifications-outcome-check`.

  $ ./pure_specifications_tool.exe structural
  accepted: logical expansion emits caller-only VIR and no spec overflow VC
  rejected: runtime spec use
  rejected: spec-to-exec call
  rejected: cyclic spec graph
  rejected: bad spec-call arity
  rejected: bad spec-call type
  rejected: contract on spec
  rejected: recursive spec
  rejected: forbidden spec body
  accepted: immutable tuple spec result
  raw semantic rejection matrix passed

Ordinary PPX output erases pure declarations completely. It neither mentions
the erased names nor imports the ghost runtime, and its CMT has no carrier or
raw verifier attribute.
This is the local `erased-carrier-ratchet` PPX/CMT authentication exception.

  $ mkdir artifacts
  $ ocamlc -stop-after parsing -dsource -ppx ../../ppx/vero_ppx.exe fixtures/positive.ml > artifacts/ordinary.stdout 2> artifacts/ordinary.source
  $ test ! -s artifacts/ordinary.stdout
  $ grep '^let verified' artifacts/ordinary.source
  let verified (x : int)  : int = x[@@verocaml.internal.artifact_family.ordinary-v1
  $ test $(grep -c '^let ' artifacts/ordinary.source) -eq 1
  $ test $(grep -c 'Vero_ghost\|math_succ\|twice\|shifted\|growing' artifacts/ordinary.source) -eq 0
  $ ocamlc -bin-annot -ppx ../../ppx/vero_ppx.exe -c -o artifacts/positive_erased.cmo fixtures/positive.ml
  $ ./pure_specifications_tool.exe inspect erased artifacts/positive_erased.cmt
  ordinary CMT has no spec carrier or raw attribute

Retained verification output has one canonical ghost-location carrier per
spec declaration and no raw authority attribute. The adapter authenticates
those carriers, prints the semantic role and classified calls, and lowering is
deterministic.
This is the local `retained-carrier-and-lowering-ratchet` specialist exception.

  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/positive.cmo fixtures/positive.ml
  $ ./pure_specifications_tool.exe inspect retained artifacts/positive.cmt
  retained CMT has four ghost-location authenticated carriers
  $ ./pure_specifications_tool.exe sst artifacts/positive.cmt > artifacts/first.sst
  $ ./pure_specifications_tool.exe sst artifacts/positive.cmt > artifacts/second.sst
  $ cmp artifacts/first.sst artifacts/second.sst
  $ grep -E '^function (math_succ|twice|shifted|growing|verified)|specification-call' artifacts/first.sst
  function math_succ#0 mode=spec recursive=false result=int policy=default-linear/default-z3 @ positive.ml:1:0-1:55
  function twice#1 mode=spec recursive=false result=int policy=default-linear/default-z3 @ positive.ml:3:0-3:69
      specification-call math_succ#0 recursive=false type-arguments=[] : int @ positive.ml:3:28-3:51
          specification-call math_succ#0 recursive=false type-arguments=[] : int @ positive.ml:3:38-3:51
  function shifted#2 mode=spec recursive=false result=int policy=default-linear/default-z3 @ positive.ml:5:0-5:77
  function growing#3 mode=spec recursive=false result=bool policy=default-linear/default-z3 @ positive.ml:7:0-10:17
          specification-call twice#1 recursive=false type-arguments=[] : int @ positive.ml:8:10-8:17
  function verified#4 mode=exec recursive=false result=int policy=default-linear/default-z3 @ positive.ml:12:0-16:3
      specification-call growing#3 recursive=false type-arguments=[] : bool @ positive.ml:13:22-13:31
          specification-call shifted#2 recursive=false type-arguments=[] : int @ positive.ml:15:49-15:58
        specification-call twice#1 recursive=false type-arguments=[] : int @ positive.ml:14:20-14:27
  $ ./pure_specifications_tool.exe vir artifacts/positive.cmt > artifacts/first.vir
  $ ./pure_specifications_tool.exe vir artifacts/positive.cmt > artifacts/second.vir
  $ cmp artifacts/first.vir artifacts/second.vir
  $ grep '^function ' artifacts/first.vir
  function verified#4 mode=exec body=checked-typedtree:positive.ml policy=default-linear/default-z3
  $ grep -E '^  vc ' artifacts/first.vir
    vc 0 assertion ordinal=0 @ positive.ml:14:2-14:36
    vc 1 assertion ordinal=0 @ positive.ml:14:2-14:36
    vc 2 postcondition ordinal=0 declaration=positive.ml:15:2-15:63 @ positive.ml:15:2-15:63
    vc 3 postcondition ordinal=0 declaration=positive.ml:15:2-15:63 @ positive.ml:15:2-15:63
    vc 4 postcondition ordinal=0 declaration=positive.ml:15:2-15:63 @ positive.ml:15:2-15:63
    vc 5 postcondition ordinal=0 declaration=positive.ml:15:2-15:63 @ positive.ml:15:2-15:63
  $ test $(grep -c 'arithmetic-' artifacts/first.vir) -eq 0
  $ test $(grep -c 'trusted\|axiom\|math_succ.result\|twice.result\|shifted.result\|growing.result' artifacts/first.vir) -eq 0
  $ test $(grep -c 'y\\$' artifacts/first.vir) -eq 0
  $ grep -F '4611686018427387903' artifacts/first.vir >/dev/null

Nested eligible Specs compose their internal `if` and total `match` as one
logical value on each actual caller path. The real Exec `if` remains two
paths, and the reversed-result control never verifies.
This is the local `matched-path-vc-explosion-ratchet` specialist resource
exception; exact VC counts remain outside the ordinary focused alias.

  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/matched_path_control.cmo fixtures/matched_path_control.ml
  $ source_root=${PWD%%/_build/*}
  $ "$source_root/_build/default/src/verocaml.exe" verify artifacts/matched_path_control.cmt --dump-sst artifacts/matched.first.sst --dump-vir artifacts/matched.first.vir > artifacts/matched.first.out 2>&1 || :
  $ "$source_root/_build/default/src/verocaml.exe" verify artifacts/matched_path_control.cmt --dump-sst artifacts/matched.second.sst --dump-vir artifacts/matched.second.vir > artifacts/matched.second.out 2>&1 || :
  $ for name in nested_path_control unsupported_form_falls_back wrong_result_never_verifies; do awk -v name="$name" '$1=="function" { active=($2 ~ ("^" name "#")) } active && $1=="vc" { count++ } END { printf "%s-vcs=%d\n", name, count }' artifacts/matched.first.vir; done
  nested_path_control-vcs=2
  unsupported_form_falls_back-vcs=4
  wrong_result_never_verifies-vcs=2
  $ sed -n '/^function nested_path_control#/,/^function unsupported_form_falls_back#/p' artifacts/matched.first.vir | grep -Fo '(or (and' | wc -l
  8
  $ grep 'counterexample function=wrong_result_never_verifies' artifacts/matched.first.out | sed -E 's/ span=.*$/ never-verified/'
  verocaml: counterexample function=wrong_result_never_verifies#6 vc=postcondition[0] never-verified
  $ cmp artifacts/matched.first.sst artifacts/matched.second.sst
  $ cmp artifacts/matched.first.vir artifacts/matched.second.vir
  $ grep -c '^  vc ' artifacts/matched.first.vir
  8

Exact positional aggregate constructors retain their tag and payload semantics
without recreating internal Spec paths. The frozen direct reproducer verifies
with five compact VCs, and independent SST/VIR construction is deterministic.
This is the local `aggregate-constructor-compactness-ratchet` specialist
resource exception.

  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/option_map_minimal.cmo fixtures/option_map_minimal.ml
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/option_map_minimal.cmt --timeout-ms 5000 --dump-sst artifacts/option_map.first.sst --dump-vir artifacts/option_map.first.vir > artifacts/option_map.first.out 2>&1
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/option_map_minimal.cmt --timeout-ms 5000 --dump-sst artifacts/option_map.second.sst --dump-vir artifacts/option_map.second.vir > artifacts/option_map.second.out 2>&1
  $ cat artifacts/option_map.first.out
  verocaml: verified file=artifacts/option_map_minimal.cmt functions=3 obligations=5
  $ for name in direct identity_node through_helper; do awk -v name="$name" '$1=="function" { active=($2 ~ ("^" name "#")) } active && $1=="vc" { count++ } END { printf "%s=%d\n", name, count }' artifacts/option_map.first.vir; done
  direct=2
  identity_node=1
  through_helper=2
  $ cmp artifacts/option_map.first.sst artifacts/option_map.second.sst
  $ cmp artifacts/option_map.first.vir artifacts/option_map.second.vir

Independent constructor matching proves exact tags, payloads, disjointness,
observational injectivity, and nested aggregate conditionals through helpers.
Only the deliberate wrong executable result is a concrete counterexample; it
is neither verified nor hidden as an inconclusive result.
This is the local `nested-aggregate-vc-explosion-ratchet` specialist resource
exception.

  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/logical_aggregate_constructor_semantics.cmo fixtures/logical_aggregate_constructor_semantics.ml
  $ set +e; OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/logical_aggregate_constructor_semantics.cmt --timeout-ms 5000 --dump-sst artifacts/aggregate_semantics.first.sst --dump-vir artifacts/aggregate_semantics.first.vir > artifacts/aggregate_semantics.first.out 2>&1; first_status=$?; set -e; test "$first_status" -eq 1
  $ set +e; OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/logical_aggregate_constructor_semantics.cmt --timeout-ms 5000 --dump-sst artifacts/aggregate_semantics.second.sst --dump-vir artifacts/aggregate_semantics.second.vir > artifacts/aggregate_semantics.second.out 2>&1; second_status=$?; set -e; test "$second_status" -eq 1
  $ test "$(grep -c '^verocaml: counterexample' artifacts/aggregate_semantics.first.out)" -eq 1
  $ test "$(grep -c 'inconclusive' artifacts/aggregate_semantics.first.out)" -eq 0
  $ grep -Eo 'counterexample function=wrong_nested_result#[0-9]+ vc=postcondition\[0\]' artifacts/aggregate_semantics.first.out | sed -E 's/#([0-9]+)/#ID/'
  counterexample function=wrong_nested_result#ID vc=postcondition[0]
  $ for name in direct_constructor_match direct_injectivity nested_helper_composition wrong_nested_result; do awk -v name="$name" '$1=="function" { active=($2 ~ ("^" name "#")) } active && $1=="vc" { count++ } END { printf "%s=%d\n", name, count }' artifacts/aggregate_semantics.first.vir; done
  direct_constructor_match=2
  direct_injectivity=1
  nested_helper_composition=12
  wrong_nested_result=1
  $ cmp artifacts/aggregate_semantics.first.sst artifacts/aggregate_semantics.second.sst
  $ cmp artifacts/aggregate_semantics.first.vir artifacts/aggregate_semantics.second.vir

The tuple-valued logical declaration retains its semantic role and result
shape. This is the local `logical-tuple-shape-ratchet` specialist exception.

  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/spec_tuple.cmo fixtures/spec_tuple.ml
  $ ./pure_specifications_tool.exe sst artifacts/spec_tuple.cmt | grep '^function bad'
  function bad#0 mode=spec recursive=false result=(int * int) policy=default-linear/default-z3 @ spec_tuple.ml:1:0-1:44
Required-labelled Specs and direct higher-order Specs are now first-class
logical declarations rather than policy negatives.
Their exact logical-only rows are the local
`labelled-and-higher-order-logical-role-ratchet` specialist exception; stable
admission is also covered by `pure-specifications-outcome-check`.

  $ for name in labelled higher_order; do ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$name.cmo" "fixtures/$name.ml"; OCAML_COLOR=never ../../src/verocaml.exe verify "artifacts/$name.cmt" --timeout-ms 5000 --rlimit 100000 | sed -E "s,file=[^ ]+,file=$name.cmt,"; done
  verocaml: verified file=labelled.cmt functions=0 obligations=0
  verocaml: verified file=higher_order.cmt functions=0 obligations=0
  $ ./pure_specifications_tool.exe sst artifacts/higher_order.cmt | grep '^function bad'
  function bad#0 mode=spec recursive=false result=int policy=default-linear/default-z3 @ higher_order.ml:1:0-1:49

The generic identity specification is admitted once over its canonical source
binder without materializing call-site members.
This is the local `polymorphic-source-binder-ratchet` specialist exception.

  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/polymorphic.cmo fixtures/polymorphic.ml
  $ ./pure_specifications_tool.exe sst artifacts/polymorphic.cmt | grep '^function bad'
  function bad#0 binders=['0@bad#0] mode=spec recursive=false result='0@bad#0 policy=default-linear/default-z3 @ polymorphic.ml:1:0-1:31
