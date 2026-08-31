The ordinary stable outcomes live in outcome_cases.ml. This specialist target
retains only the private parametric encoding/attack contracts listed in
test/support/migrations/w05.org.

  $ mkdir artifacts
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -I artifacts -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/core.cmo fixtures/core.ml
  $ OCAML_COLOR=never ../../src/verocaml.exe verify fixtures/core.ml --threads 1 --timeout-ms 5000 --dump-sst artifacts/source.sst --dump-vir artifacts/source.vir >/dev/null

The private SST/VIR architecture retains canonical generic binders, typed
label vectors, optional carriers, and named abstract sorts without generated
specialization functions.

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
  $ grep -E "parameter result.*sort='" artifacts/source.vir | head -3 | sed -E 's/^ *//'
  parameter result$1 sort='0@id#0
  parameter result$2 sort='0@copy#1
  parameter result$3 sort='0@choose#2
  $ OCAML_COLOR=never ../../src/verocaml.exe verify fixtures/logical_equality.ml --threads 1 --timeout-ms 5000 --dump-vir artifacts/logical-source.vir >/dev/null
  $ test "$(grep -c "goal (=:.*0@reflexive#1" artifacts/logical-source.vir)" = 2

The canonical owners reject forged vectors and types at their private
architecture boundaries without solver work.

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
