The focused gate compiles the same accepted generic program through both the
source and retained-CMT routes.

  $ mkdir artifacts
  $ retained () { name=$1; ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -I artifacts -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$name.cmo" "fixtures/$name.ml"; }
  $ for n in core logical_equality alpha_a alpha_b eq_parameter eq_tuple eq_option eq_list partial_application callback generic_mutation inferred_generic_recursion explicit_polymorphic_recursion open_aggregate; do retained "$n"; done
  $ OCAML_COLOR=never ../../src/verocaml.exe verify fixtures/core.ml --threads 1 --timeout-ms 5000 --dump-sst artifacts/source.sst --dump-vir artifacts/source.vir
  verocaml: verified file=fixtures/core.ml functions=19 obligations=5
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/core.cmt --threads 1 --timeout-ms 5000 --dump-sst artifacts/retained.sst --dump-vir artifacts/retained.vir
  verocaml: verified file=artifacts/core.cmt functions=19 obligations=5
  $ sed '/^instance-modes/,$d' artifacts/source.sst > artifacts/source.semantic
  $ sed '/^instance-modes/,$d' artifacts/retained.sst > artifacts/retained.semantic
  $ cmp artifacts/source.semantic artifacts/retained.semantic
  $ cmp artifacts/source.vir artifacts/retained.vir

The SST has one definition per generic source function, canonical ordered
binders and applications, explicit empty arguments for monomorphic calls, and
typedtree-ordinal argument order despite reordered labels at the call site.
Optional omission, explicit presence, and carrier forwarding stay distinct.
The omitted, supplied, forwarded-absent, and forwarded-present contracts are
true only when both call execution and definition-summary entry resolve the
compiler-typed optional binding through the same default/payload path.

  $ grep -E '^function (id|choose|pair|relay|use_abstract)#' artifacts/source.sst | sed -E 's/ policy=.*//'
  function id#0 binders=['0@id#0] mode=exec recursive=false result='0@id#0
  function choose#2 binders=['0@choose#2] mode=exec recursive=false result='0@choose#2
  function pair#3 binders=['0@pair#3, '1@pair#3] mode=exec recursive=false result=('0@pair#3 * '1@pair#3)
  function relay#5 binders=['0@relay#5] mode=exec recursive=false result='0@relay#5
  function use_abstract#12 binders=['0@use_abstract#12] mode=exec recursive=false result='0@use_abstract#12
  $ grep -E 'exec-call (concrete|relay)#' artifacts/source.sst | sed -E 's/^ *//; s/ @ .*//'
  exec-call concrete#8 recursive=false type-arguments=[] : int
  exec-call relay#5 recursive=false type-arguments=[int] : int
  exec-call relay#5 recursive=false type-arguments=[bool] : bool
  exec-call relay#5 recursive=false type-arguments=['0@use_abstract#12] : '0@use_abstract#12
  $ sed -n '/exec-call labelled#/,/function omitted#/p' artifacts/source.sst | grep -E '^ +argument( |$)'
        argument first
        argument second
        argument flag
  $ grep -E 'optional-(absent|present|forward)' artifacts/source.sst | sed -E 's/^ *//; s/ : .*//'
  optional-forward
  optional-forward
  optional-forward
  optional-forward
  optional-forward
  $ sed -n '/function pick#/,/function concrete#/p' artifacts/source.vir | grep -q 'tag.option_specification.*optional.selected.*fallback'
  $ sed -n '/function forwarded#/,/function forwarded_absent#/p' artifacts/source.vir | grep -q 'tag.option_specification<int>.*carrier.*optional.selected.* 7'
  $ test "$(grep -c '^function relay#' artifacts/source.sst)" = 1
  $ test "$(grep -c '^function .*<' artifacts/source.sst)" = 0

Abstract values remain named first-order sorts in VIR rather than integers or
aggregate encodings. Spec and Proof equality over that sort is sorted
mathematical equality and verifies through direct Z3.

  $ grep -E "parameter result.*sort='" artifacts/source.vir | head -3 | sed -E 's/^ *//'
  parameter result$1 sort='0@id#0
  parameter result$2 sort='0@copy#1
  parameter result$3 sort='0@choose#2
  $ OCAML_COLOR=never ../../src/verocaml.exe verify fixtures/logical_equality.ml --threads 1 --timeout-ms 5000 --dump-sst artifacts/logical-source.sst --dump-vir artifacts/logical-source.vir
  verocaml: verified file=fixtures/logical_equality.ml functions=3 obligations=4
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/logical_equality.cmt --threads 1 --timeout-ms 5000 --dump-sst artifacts/logical-retained.sst --dump-vir artifacts/logical-retained.vir
  verocaml: verified file=artifacts/logical_equality.cmt functions=3 obligations=4
  $ OCAML_COLOR=never ../../src/verocaml.exe verify fixtures/abstract_adt_logical_equality.ml --threads 2 --timeout-ms 5000
  verocaml: verified file=fixtures/abstract_adt_logical_equality.ml functions=2 obligations=2
  $ code=0; OCAML_COLOR=never ../../src/verocaml.exe verify fixtures/logical_equality_false.ml --threads 1 --timeout-ms 5000 > artifacts/logical-false.out 2>&1 || code=$?; test "$code" = 1; grep -o counterexample artifacts/logical-false.out | head -1
  counterexample
  $ test "$(grep -c "goal (=:'0@reflexive#1" artifacts/logical-source.vir)" = 2
  $ cmp artifacts/logical-source.vir artifacts/logical-retained.vir

Repeated and serial/threaded runs are byte-stable. Alpha-renaming the source
type variables also leaves normalized SST and VIR semantics unchanged.

  $ OCAML_COLOR=never ../../src/verocaml.exe verify fixtures/core.ml --threads 1 --timeout-ms 5000 --dump-sst artifacts/repeated.sst --dump-vir artifacts/repeated.vir >/dev/null
  $ cmp artifacts/source.sst artifacts/repeated.sst
  $ cmp artifacts/source.vir artifacts/repeated.vir
  $ OCAML_COLOR=never ../../src/verocaml.exe verify fixtures/core.ml --threads 2 --timeout-ms 5000 --dump-sst artifacts/threaded.sst --dump-vir artifacts/threaded.vir >/dev/null
  $ cmp artifacts/source.sst artifacts/threaded.sst
  $ cmp artifacts/source.vir artifacts/threaded.vir
  $ for n in alpha_a alpha_b; do OCAML_COLOR=never ../../src/verocaml.exe verify "fixtures/$n.ml" --threads 1 --timeout-ms 5000 --dump-sst "artifacts/$n.sst" --dump-vir "artifacts/$n.vir" >/dev/null; sed -E 's/alpha_[ab]\.ml/alpha.ml/g; s/ @ [^ ]*:[0-9][^ ]*//g' "artifacts/$n.sst" | sed '/^instance-modes/,$d' > "artifacts/$n.norm.sst"; sed -E 's/alpha_[ab]\.ml/alpha.ml/g; s/ @ [^ ]*:[0-9][^ ]*//g' "artifacts/$n.vir" > "artifacts/$n.norm.vir"; done
  $ cmp artifacts/alpha_a.norm.sst artifacts/alpha_b.norm.sst
  $ cmp artifacts/alpha_a.norm.vir artifacts/alpha_b.norm.vir

The canonical owners reject arity and substitution errors. Forged semantic
type arguments, labels, optional carriers, and default substitution reject at
SST with zero solver work. A backend without named-sort support rejects before
constructing a solver.

  $ ./parametric_core_tool.exe owner-unit
  parametric owner checks: alpha, substitution, application, open-type, arity
  $ ./parametric_core_tool.exe call-attacks artifacts/core.cmt | sed -E 's/ detail=.*//'
  attack=missing-type-argument rejected boundary=sst solver-work=0
  attack=extra-type-argument rejected boundary=sst solver-work=0
  attack=mismatched-type-argument rejected boundary=sst solver-work=0
  attack=reordered-type-arguments rejected boundary=sst solver-work=0
  attack=missing-label rejected boundary=sst solver-work=0
  attack=duplicate-label rejected boundary=sst solver-work=0
  attack=unknown-label rejected boundary=sst solver-work=0
  attack=reordered-label-vector rejected boundary=sst solver-work=0
  attack=wrong-optional-forwarding rejected boundary=sst solver-work=0
  $ ./parametric_core_tool.exe optional-contract-attack artifacts/core.cmt | sed -E 's/ detail=.*//'
  attack=optional-contract-substitution rejected boundary=sst solver-work=0
  $ ./parametric_core_tool.exe structural-attacks artifacts/core.cmt | sed -E 's/ detail=.*//'
  attack=forged-binder-owner rejected boundary=pre-executor solver-work=0
  attack=duplicate-binder rejected boundary=pre-executor solver-work=0
  attack=unbound-binder rejected boundary=pre-executor solver-work=0
  attack=option-arity rejected boundary=pre-executor solver-work=0
  attack=forged-constructor-identity rejected boundary=pre-executor solver-work=0
  $ ./parametric_core_tool.exe vir-structure-attacks | sed -E 's/ detail=.*//'
  attack=nested-parametric-node-sort rejected boundary=vir-translation solver-work=0
  attack=nested-parametric-symbol-sort rejected boundary=vir-translation solver-work=0
  $ ./parametric_core_tool.exe backend-control
  backend-incapable rejected pre-solver solver-work=0 detail=SMTML backend cannot preserve parametric equality

Open runtime equality, higher-order escape, polymorphic recursion, generic
mutation, and unsupported aggregate applications reject before dumps and
private authority. Compiler-invalid label and optional forwarding forms reject
on the source route. Compiler-valid negatives also reject from retained CMT.

  $ reject () { input=$1; name=$2; expected=$3; code=0; VEROCAML_TEST_PRIVATE_RECEIPT_TRACE=1 OCAML_COLOR=never ../../src/verocaml.exe verify "$input" --threads 1 --timeout-ms 5000 --dump-sst "artifacts/$name.sst" --dump-vir "artifacts/$name.vir" >"artifacts/$name.out" 2>&1 || code=$?; test "$code" = 2; actual=$(grep -Eo 'VERO_[A-Z_]+' "artifacts/$name.out" | head -1); printf '%s: code=%s\n' "$name" "$actual"; test "$actual" = "$expected"; test "$(grep -c 'private-receipt ' "artifacts/$name.out")" = 0; test ! -e "artifacts/$name.sst"; test ! -e "artifacts/$name.vir"; }
  $ for spec in 'eq_parameter VERO_UNSUPPORTED_POLYMORPHISM' 'eq_tuple VERO_UNSUPPORTED_POLYMORPHISM' 'eq_option VERO_UNSUPPORTED_POLYMORPHISM' 'eq_list VERO_UNSUPPORTED_POLYMORPHISM' 'partial_application VERO_UNSUPPORTED_HIGHER_ORDER_FUNCTION' 'callback VERO_CALLBACK_CONTRACT' 'generic_mutation VERO_UNSUPPORTED_MUTATION' 'inferred_generic_recursion VERO_DEPENDENCY' 'explicit_polymorphic_recursion VERO_UNSUPPORTED_POLYMORPHISM' 'open_aggregate VERO_DEPENDENCY'; do set -- $spec; reject "fixtures/$1.ml" "source-$1" "$2"; done
  source-eq_parameter: code=VERO_UNSUPPORTED_POLYMORPHISM
  source-eq_tuple: code=VERO_UNSUPPORTED_POLYMORPHISM
  source-eq_option: code=VERO_UNSUPPORTED_POLYMORPHISM
  source-eq_list: code=VERO_UNSUPPORTED_POLYMORPHISM
  source-partial_application: code=VERO_UNSUPPORTED_HIGHER_ORDER_FUNCTION
  source-callback: code=VERO_CALLBACK_CONTRACT
  source-generic_mutation: code=VERO_UNSUPPORTED_MUTATION
  source-inferred_generic_recursion: code=VERO_DEPENDENCY
  source-explicit_polymorphic_recursion: code=VERO_UNSUPPORTED_POLYMORPHISM
  source-open_aggregate: code=VERO_DEPENDENCY
  $ for spec in 'eq_parameter VERO_UNSUPPORTED_POLYMORPHISM' 'eq_tuple VERO_UNSUPPORTED_POLYMORPHISM' 'eq_option VERO_UNSUPPORTED_POLYMORPHISM' 'eq_list VERO_UNSUPPORTED_POLYMORPHISM' 'partial_application VERO_UNSUPPORTED_HIGHER_ORDER_FUNCTION' 'callback VERO_CALLBACK_CONTRACT' 'generic_mutation VERO_UNSUPPORTED_MUTATION' 'inferred_generic_recursion VERO_DEPENDENCY' 'explicit_polymorphic_recursion VERO_UNSUPPORTED_POLYMORPHISM' 'open_aggregate VERO_DEPENDENCY'; do set -- $spec; reject "artifacts/$1.cmt" "retained-$1" "$2"; done
  retained-eq_parameter: code=VERO_UNSUPPORTED_POLYMORPHISM
  retained-eq_tuple: code=VERO_UNSUPPORTED_POLYMORPHISM
  retained-eq_option: code=VERO_UNSUPPORTED_POLYMORPHISM
  retained-eq_list: code=VERO_UNSUPPORTED_POLYMORPHISM
  retained-partial_application: code=VERO_UNSUPPORTED_HIGHER_ORDER_FUNCTION
  retained-callback: code=VERO_CALLBACK_CONTRACT
  retained-generic_mutation: code=VERO_UNSUPPORTED_MUTATION
  retained-inferred_generic_recursion: code=VERO_DEPENDENCY
  retained-explicit_polymorphic_recursion: code=VERO_UNSUPPORTED_POLYMORPHISM
  retained-open_aggregate: code=VERO_DEPENDENCY
  $ reject fixtures/missing_label.ml source-missing_label VERO_UNSUPPORTED_HIGHER_ORDER_FUNCTION
  source-missing_label: code=VERO_UNSUPPORTED_HIGHER_ORDER_FUNCTION
  $ reject fixtures/duplicate_label.ml source-duplicate_label VERO_SOURCE_COMPILE
  source-duplicate_label: code=VERO_SOURCE_COMPILE
  $ reject fixtures/unknown_label.ml source-unknown_label VERO_SOURCE_COMPILE
  source-unknown_label: code=VERO_SOURCE_COMPILE
  $ reject fixtures/wrong_optional_forward.ml source-wrong_optional_forward VERO_SOURCE_COMPILE
  source-wrong_optional_forward: code=VERO_SOURCE_COMPILE
