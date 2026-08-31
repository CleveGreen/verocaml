The admitted module subset is one resolved same-CMT module-type constraint.  Its
abstract state and immutable view share a closed public-surface certificate;
the recursive mutable label retains the compiler's closed modality result.
The client can only thread the unique successor and end with a snapshot.

  $ mkdir artifacts
  $ compile () { ocamlc -w -A -alert -all -bin-annot -c -o "artifacts/$1.cmo" "fixtures/$1.ml"; }
  $ compile encapsulated_stack
  $ ./encapsulated_recursive_mutation_tool.exe dump-sst artifacts/encapsulated_stack.cmt > artifacts/first.sst
  $ ./encapsulated_recursive_mutation_tool.exe dump-sst artifacts/encapsulated_stack.cmt > artifacts/second.sst
  $ cmp artifacts/first.sst artifacts/second.sst
  $ grep -E 'representation=abstract|same-cmt-link|signature-module|implementation-module|signature-type|implementation-type|public Stack\.|owned-tree|recursive-edge|Stack.node#0.Node#1.next#1 :|exec-call Stack\.(drop|snapshot|view_length)' artifacts/first.sst | sed -E 's/ @ .*//'
        field constructor-Stack.node#0.Node#1.next#1 : Stack.node#0 mutability=mutable uniqueness=force-aliased linearity=force-many
  type Stack.t#1 representation=abstract evidence=same-cmt:1:12:15
    same-cmt-link semantic-type=Stack.t#1 constraint=encapsulated_stack.ml:12:15-12:20
      signature-module STACK identity=STACK_283
      implementation-module Stack identity=Stack_318
      signature-type t identity=t_275
      implementation-type t identity=t_289
      public Stack.singleton#0 role=constructor
      public Stack.snapshot#1 role=terminal-snapshot
      public Stack.view_length#2 role=terminal-read
      public Stack.zero_head#3 role=unique-transition
      public Stack.cut#4 role=unique-transition
      public Stack.drop#5 role=unique-transition
      owned-tree root=Stack.t#1 construction=local-first-order-closed
        recursive-edge constructor-Stack.node#0.Node#1.next#1
  type Stack.view#2 representation=abstract evidence=same-cmt:2:12:15
    same-cmt-link semantic-type=Stack.view#2 constraint=encapsulated_stack.ml:12:15-12:20
      signature-module STACK identity=STACK_283
      implementation-module Stack identity=Stack_318
      signature-type view identity=view_276
      implementation-type view identity=view_294
      public Stack.singleton#0 role=constructor
      public Stack.snapshot#1 role=terminal-snapshot
      public Stack.view_length#2 role=terminal-read
      public Stack.zero_head#3 role=unique-transition
      public Stack.cut#4 role=unique-transition
      public Stack.drop#5 role=unique-transition
            pattern owned-tree-cursor record#1:Stack.node#0 policy=functional-no-heap root=stack#0:Stack.t#1 uniqueness=unique version=0 path=field(type-Stack.t#1.top#0)/constructor(Stack.node#0.Node#1) : Stack.node#0
              owned-tree-nested-write policy=functional-no-heap root=stack#0:Stack.t#1 uniqueness=unique pre-version=0 successor-version=1 target=constructor-Stack.node#0.Node#1.value#0 mutability=mutable uniqueness=force-aliased linearity=force-many rhs=ground invalidated=1 : unit
            pattern owned-tree-cursor record#1:Stack.node#0 policy=functional-no-heap root=stack#0:Stack.t#1 uniqueness=unique version=0 path=field(type-Stack.t#1.top#0)/constructor(Stack.node#0.Node#1) : Stack.node#0
              owned-tree-nested-write policy=functional-no-heap root=stack#0:Stack.t#1 uniqueness=unique pre-version=0 successor-version=1 target=constructor-Stack.node#0.Node#1.next#1 mutability=mutable uniqueness=force-aliased linearity=force-many rhs=ground invalidated=1 : unit
                pattern owned-tree-cursor next#1:Stack.node#0 policy=functional-no-heap root=stack#0:Stack.t#1 uniqueness=unique version=0 path=field(type-Stack.t#1.top#0)/constructor(Stack.node#0.Node#1)/field(constructor-Stack.node#0.Node#1.next#1) : Stack.node#0
              owned-tree-rebase policy=functional-no-heap root=stack#0:Stack.t#1 uniqueness=unique pre-version=0 successor-version=1 target=type-Stack.t#1.top#0 uniqueness=force-aliased linearity=force-many rhs=guarded-descendant source-path=field(type-Stack.t#1.top#0)/constructor(Stack.node#0.Node#1)/field(constructor-Stack.node#0.Node#1.next#1) invalidated=1 : unit
                  exec-call Stack.snapshot#1 recursive=false type-arguments=[] : Stack.view#2
                  exec-call Stack.view_length#2 recursive=false type-arguments=[] : int
  $ grep 'Stack.node#0.Node#1.next#1 :' artifacts/first.sst | head -1 | sed -E 's/ @ .*//'
        field constructor-Stack.node#0.Node#1.next#1 : Stack.node#0 mutability=mutable uniqueness=force-aliased linearity=force-many

The unmarked nested scalar and ground-constructor writes, and the bounded
descendant re-root/drop, are distinct closed transitions.  VIR contains both
the transition record and the actual bottom-up structural equations.  The
following root write uses the reconstructed successor.

  $ ./encapsulated_recursive_mutation_tool.exe verify artifacts/encapsulated_stack.cmt
  verified VIR (7 functions, 5 owned-tree transitions)
  $ ./encapsulated_recursive_mutation_tool.exe dump-vir artifacts/encapsulated_stack.cmt > artifacts/first.vir
  $ ./encapsulated_recursive_mutation_tool.exe dump-vir artifacts/encapsulated_stack.cmt > artifacts/second.vir
  $ cmp artifacts/first.vir artifacts/second.vir
  $ grep -E 'owned-tree-transition|cursor=.*guarded-path|source-path=|successor-reconstruct' artifacts/first.vir
    owned-tree-transition policy=functional-no-heap root=stack#0 pre-version=0 successor-version=1 target=Stack.node#0.Node#1.value#0 mutability=mutable uniqueness=force-aliased linearity=force-many rhs=ground invalidated=1
      cursor=record#1 version=0 guarded-path=field(Stack.t#1.top#0)/constructor(Stack.node#0.Node#1)
      successor-reconstruct record=Stack.node#0 changed=Stack.node#0.Node#1.value#0 preserved-siblings=Stack.node#0.Node#1.next#1
      successor-reconstruct constructor=Stack.node#0.Node#1
      successor-reconstruct record=Stack.t#1 changed=Stack.t#1.top#0 preserved-siblings=Stack.t#1.length#1
    owned-tree-transition policy=functional-no-heap root=stack#0 pre-version=0 successor-version=1 target=Stack.node#0.Node#1.next#1 mutability=mutable uniqueness=force-aliased linearity=force-many rhs=ground invalidated=1
      cursor=record#1 version=0 guarded-path=field(Stack.t#1.top#0)/constructor(Stack.node#0.Node#1)
      successor-reconstruct record=Stack.node#0 changed=Stack.node#0.Node#1.next#1 preserved-siblings=Stack.node#0.Node#1.value#0
      successor-reconstruct constructor=Stack.node#0.Node#1
      successor-reconstruct record=Stack.t#1 changed=Stack.t#1.top#0 preserved-siblings=Stack.t#1.length#1
    owned-tree-transition policy=functional-no-heap root=stack#0 pre-version=1 successor-version=2 target=Stack.t#1.length#1 mutability=mutable uniqueness=force-aliased linearity=force-many rhs=ground invalidated=
      successor-reconstruct record=Stack.t#1 changed=Stack.t#1.length#1 preserved-siblings=Stack.t#1.top#0
    owned-tree-transition policy=functional-no-heap root=stack#0 pre-version=0 successor-version=1 target=Stack.t#1.top#0 mutability=mutable uniqueness=force-aliased linearity=force-many rhs=guarded-descendant-move invalidated=1
      source-path=field(Stack.t#1.top#0)/constructor(Stack.node#0.Node#1)/field(Stack.node#0.Node#1.next#1)
      successor-reconstruct record=Stack.t#1 changed=Stack.t#1.top#0 preserved-siblings=Stack.t#1.length#1
    owned-tree-transition policy=functional-no-heap root=stack#0 pre-version=1 successor-version=2 target=Stack.t#1.length#1 mutability=mutable uniqueness=force-aliased linearity=force-many rhs=ground invalidated=
      successor-reconstruct record=Stack.t#1 changed=Stack.t#1.length#1 preserved-siblings=Stack.t#1.top#0
  $ grep -Fq 'stack.owned-state' artifacts/first.vir && grep -Fq 'stack.owned-constructor' artifacts/first.vir && grep -Fq 'stack.state' artifacts/first.vir && echo 'structural successors present'
  structural successors present
  $ if grep -Eqi 'points-to|separating-conjunction|permission-algebra|heap-location|alias-relation' artifacts/first.vir; then false; else echo 'no heap or separation vocabulary'; fi
  no heap or separation vocabulary

The same owned-tree reconstruction corpus composes with authenticated
invariants. Root, rebase, and nested successors produce invariant-preservation
VCs without changing the ownership transition records.

  $ invariant_compile () { ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$1.cmo" "../type_invariant/fixtures/$1.ml"; }
  $ for fixture in transition_invariant transition_nested_invariant; do invariant_compile "$fixture"; ./encapsulated_recursive_mutation_tool.exe verify "artifacts/$fixture.cmt" 2>&1 | grep -F 'invariant transition predecessor has no authenticated closed validity fact' | sed -E 's/^.*malformed SST: /malformed SST: /'; done
  malformed SST: invariant transition predecessor has no authenticated closed validity fact at transition_invariant.ml:34:8-34:25
  malformed SST: invariant transition predecessor has no authenticated closed validity fact at transition_nested_invariant.ml:34:8-34:25

Control-flow joins never select a source-order branch version.  The exact
Node-first/Empty-last probe and a divergent =if= reject at the later root
transition.  Equal branch successor maps join and permit version 1 to flow to
the following version-2 transition.  The semantic validator independently
rejects the corresponding divergent raw-function graph through its production
structural-first validation path.

  $ for fixture in branch_join_match branch_join_if branch_join_equal; do compile "$fixture"; done
  $ ./encapsulated_recursive_mutation_tool.exe verify artifacts/branch_join_equal.cmt
  verified VIR (3 functions, 3 owned-tree transitions)
  $ ./encapsulated_recursive_mutation_tool.exe dump-sst artifacts/branch_join_equal.cmt | grep 'field-write type-Stack.t#1.length#1' | sed -E 's/ @ .*//'
            field-write type-Stack.t#1.length#1 root=stack#0:Stack.t#1 uniqueness=unique pattern=unique use=aliased local=true public=true mutable=true policy=functional-no-heap pre-version=0 successor-version=1 uniqueness=force-aliased linearity=force-many : unit
            field-write type-Stack.t#1.length#1 root=stack#0:Stack.t#1 uniqueness=unique pattern=unique use=aliased local=true public=true mutable=true policy=functional-no-heap pre-version=0 successor-version=1 uniqueness=force-aliased linearity=force-many : unit
          field-write type-Stack.t#1.length#1 root=stack#0:Stack.t#1 uniqueness=unique pattern=unique use=aliased local=true public=true mutable=true policy=functional-no-heap pre-version=1 successor-version=2 uniqueness=force-aliased linearity=force-many : unit
  $ ./encapsulated_recursive_mutation_tool.exe validator-join-attack artifacts/branch_join_equal.cmt
  divergent-branch-join: malformed transition (owned-tree transition uses a stale root version)

Cursor reuse, cursor escape, ancestor reinsertion, and an unguarded deeper path
do not reach SMT.  The compiler itself rejects escape of an inline-record
binding; VeroCaml rejects the remaining compiled forms at their first illegal
use.

  $ OCAML_COLOR=never ocamlc -w -A -alert -all -c fixtures/cursor_escape.ml 2>&1 | grep -F 'inlined record could escape'
  Error: This form is not allowed as the type of the inlined record could escape.

The validator exposes abstraction facts only from the opaque validated-program
registry.  Issuer identity is not enough by itself: replaying a real token with
an altered certificate, type graph, callable, target type, or abstract-type set
is rejected.  A type-graph alteration that also contradicts transition metadata
receives that structural diagnostic first; otherwise rejection comes from the
complete issued local semantic snapshot. Independently imported definitions do
not alter that same-CMT snapshot.

Closed transition metadata is independently validated because public SST is
constructible.  Production validation reports malformed transition structure
before final exact-snapshot authentication; no invalid program reaches VIR.

  $ ./encapsulated_recursive_mutation_tool.exe validator-attacks artifacts/encapsulated_stack.cmt
  stale-version: malformed transition (owned-tree transition uses a stale root version)
  missing-invalidation: malformed transition (owned-tree cursor is stale or not invalidated)
  malformed-path: malformed transition (owned-tree cursor path result type mismatch)
  altered-modality: malformed transition (owned-tree transition target metadata does not match registry)
  incomplete-reconstruction: malformed transition (owned-tree nested reconstruction metadata is incomplete)
  missing-abstraction-evidence: forged abstraction evidence

  $ ./encapsulated_recursive_mutation_tool.exe registry artifacts/encapsulated_stack.cmt
  issued program: accepted
  validated registry query: Stack.t authenticated
  valid token with altered certificate: rejected forged certificate (unissued same-CMT abstraction certificate)
  valid token with altered representation: rejected malformed transition (owned-tree transition target metadata does not match registry)
  valid token with altered callable: rejected forged certificate (unissued same-CMT abstraction certificate)
  valid token retargeted to another type: rejected forged certificate (unissued same-CMT abstraction certificate)
  valid token with incomplete abstract set: rejected forged certificate (unissued same-CMT abstraction certificate)

The exact compiler pin rejects retaining an alias or a read observation and
then consuming the same state uniquely.  This compiler fact complements, but
does not replace, VeroCaml's CMT certificate.

  $ OCAML_COLOR=never ocamlc -w -A -alert -all -c fixtures/alias_then_consume.ml 2>&1 | grep -F 'already been used as unique'
  Error: This value is read from here, but it has already been used as unique:
  $ OCAML_COLOR=never ocamlc -w -A -alert -all -c fixtures/read_then_consume.ml 2>&1 | grep -F 'used here as unique, but it has already been used'
  Error: This value is used here as unique, but it has already been used:

Certificate issuance is local to =Typedtree_adapter.ml= and absent from its
installed interface.  Reproduce the review attack against the installed
library while deliberately adding every installed =.private= directory: the
old issuer cannot be imported.  A client which skips issuance and constructs
raw authenticated evidence still links and runs, but validation rejects it.

  $ root="${PWD%%/_build/*}"
  $ core="$root/_build/install/default/lib/verocaml/core"
  $ private_flags=""; for directory in $(find "$root/_build/install/default/lib/verocaml" -type d -name .private); do private_flags="$private_flags -I $directory"; done
  $ find "$root/_build/install/default/lib/verocaml" -iname '*sst_abstraction_auth*' | wc -l
  0
  $ OCAML_COLOR=never ocamlfind ocamlc -package smtml,zarith,compiler-libs.common -I "$core" $private_flags -c fixtures/installed_issuer_attack.ml -o artifacts/installed_issuer_attack.cmo 2>&1 | grep -F 'Error: Unbound module'
  Error: Unbound module "Sst_abstraction_auth"
  $ OCAML_COLOR=never ocamlfind ocamlc -package smtml,zarith,compiler-libs.common -I "$core" $private_flags -c fixtures/installed_issuer_attack.ml -o artifacts/installed_issuer_attack.cmo >/dev/null 2>&1; echo $?
  2
  $ OCAML_COLOR=never ocamlfind ocamlc -package smtml,zarith,compiler-libs.common -I "$core" $private_flags -c fixtures/installed_adapter_issue_attack.ml -o artifacts/installed_adapter_issue_attack.cmo 2>&1 | grep -F 'Error: Unbound value'
  Error: Unbound value "Typedtree_adapter.issue_abstraction"
  $ ocamlfind ocamlc -custom -package smtml,zarith,compiler-libs.common,delator -linkpkg -I "$core" $private_flags "$core/verocaml_core.cma" fixtures/installed_raw_forgery.ml -o artifacts/installed_raw_forgery.exe 2>/dev/null
  $ ./artifacts/installed_raw_forgery.exe
  raw installed-client certificate rejected
