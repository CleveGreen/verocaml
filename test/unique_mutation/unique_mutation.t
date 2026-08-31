Ordinary verification, counterexample, and stable frontend-code behavior is
covered by outcome_cases.ml.  The retained blocks are explicit specialist
exceptions: baseline lines 18-28 and 74-90 inspect SST/VIR and Typedtree
architecture, 67-68 and 152-153 pin compiler-owned rejection boundaries,
100-119 compare executable evidence across retained/erased CMTs, 126-133 is an
adapter-only accepted shape that the full verifier deliberately rejects, and
141-146 probes pinned Typedtree/path internals.  These do not participate in
unique-mutation-outcome-check.

The verification PPX retains typed proof sidecars only under its explicit flag.
Ordinary compilation erases every annotation and needs no ghost runtime.

  $ mkdir artifacts
  $ compile_keep () { ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$1.cmo" "fixtures/$1.ml"; }
  $ compile_erased () { ocamlc -w -A -alert -all -bin-annot -ppx ../../ppx/vero_ppx.exe -c -o "artifacts/$1.cmo" "fixtures/$2.ml"; }
  $ compile_plain () { ocamlc -w -A -alert -all -bin-annot -c -o "artifacts/$1.cmo" "fixtures/$1.ml"; }
  $ compile_keep unique_records

The SST records a unique mutable-record root, copies its unchanged Boolean
field into the new state, and threads the consumed-and-returned root through a
same-unit call.

  $ ./unique_mutation_tool.exe dump-sst artifacts/unique_records.cmt | grep -E 'returns-unique-parameter|field-write|old :|let-mutable|mutable-write|call increment'
    returns-unique-parameter 0
            old : int @ unique_records.ml:9:19-9:44
          old : bool @ unique_records.ml:10:21-10:45
        field-write type-box#0.value#0 root=box#0:box#0 uniqueness=unique pattern=unique use=aliased local=true public=true mutable=true : unit @ unique_records.ml:11:2-11:28
          old : int @ unique_records.ml:17:13-17:38
          exec-call increment#0 recursive=false type-arguments=[] : box#0 @ unique_records.ml:18:12-18:25
      let-mutable value#1:int : int @ unique_records.ml:23:2-25:7
            mutable-write root=value#1:int pattern=unique use=unique local=true public=true mutable=true : unit @ unique_records.ml:24:2-24:20
  $ ./unique_mutation_tool.exe dump-vir artifacts/unique_records.cmt | grep -F '(= (t0_box_record.flag#1 box.state$1) (t0_box_record.flag#1 box$0))' | head -1
        (= (t0_box_record.flag#1 box.state$1) (t0_box_record.flag#1 box$0))

  $ compile_plain ref_ops

The pinned compiler itself rejects a write to a non-mutable field. Color is
disabled only at this diagnostic assertion boundary.

  $ OCAML_COLOR=never ocamlc -w -A -alert -all -c fixtures/nonmutable_write.ml 2>&1 | grep -F 'is not mutable'
  Error: The record field "value" is not mutable

Erased annotations have the same executable mode evidence as an explicitly
stripped twin. Verification retention recovers every ghost form and its
original extension span without changing the accepted unique mutation.

  $ compile_erased unique_records_erased unique_records
  $ compile_plain unique_records_stripped
  $ ./unique_mutation_tool.exe probe artifacts/unique_records_erased.cmt > artifacts/erased.modes
  $ ./unique_mutation_tool.exe probe artifacts/unique_records_stripped.cmt > artifacts/stripped.modes
  $ cmp artifacts/erased.modes artifacts/stripped.modes
  $ grep -E '^Texp_setfield|^terminal/root|^Texp_letmutable' artifacts/erased.modes
  Texp_setfield field=value receiver=box pattern=unique use=aliased mutable=true public=true
  terminal/root unique identifier uses=2
  Texp_letmutable=1 Texp_setmutvar=1
  $ compile_keep all_ghost_forms
  $ ./unique_mutation_tool.exe dump-sst artifacts/all_ghost_forms.cmt | grep -E 'requires 0|ensures 0|old :|decreases 0|assertion 0' | sed -E 's/ @ .*//'
    requires 0 stage=logical
    ensures 0 stage=logical
        old : int
    decreases 0 stage=logical
    assertion 0 stage=logical
  $ ! ./unique_mutation_tool.exe dump-sst artifacts/all_ghost_forms.cmt | grep -q 'ghost-'

Retained annotations are also mode-neutral for a real unique tuple
destructuring. The normalized projector omits proof proxies and ghost paths,
then compares the retained CMT directly with its annotation-stripped twin.
It includes executable parameter modes, every local identifier occurrence's
consumer demand and actual linearity, field barriers/write evidence, and the
terminal unique return. Default erasure remains a separate no-runtime
comparison.

  $ compile_keep destructured_unique
  $ compile_plain destructured_unique_stripped
  $ ./unique_mutation_tool.exe executable-evidence artifacts/destructured_unique.cmt > artifacts/destructured.retained
  $ ./unique_mutation_tool.exe executable-evidence artifacts/destructured_unique_stripped.cmt > artifacts/destructured.stripped
  $ cmp artifacts/destructured.retained artifacts/destructured.stripped
  $ cat artifacts/destructured.retained
  field-read function=increment_destructured field=value barrier=aliased
  field-write function=increment_destructured field=value receiver=box demand=aliased actual=many barrier=none mutable=true public=true
  identifier function=increment_destructured name=box demand=aliased actual=many
  identifier function=increment_destructured name=box demand=aliased actual=many
  identifier function=increment_destructured name=box demand=unique actual=many
  identifier function=increment_destructured name=delta demand=aliased actual=many
  parameter function=increment_destructured name=box uniqueness=unique
  parameter function=increment_destructured name=delta uniqueness=unique
  terminal function=increment_destructured name=box demand=unique actual=many
  $ compile_erased destructured_unique_erased destructured_unique
  $ ./unique_mutation_tool.exe executable-evidence artifacts/destructured_unique_erased.cmt > artifacts/destructured.erased
  $ cmp artifacts/destructured.erased artifacts/destructured.stripped

Record and constructor parameter binders are recursively lifted as aliased
proof proxies too. Duplicate live names remain conservatively ambiguous and
are rejected by the adapter before SST rather than capturing either
executable binding.

  $ compile_keep supported_destructuring_patterns
  $ ./unique_mutation_tool.exe classify artifacts/supported_destructuring_patterns.cmt
  accepted
  $ ./unique_mutation_tool.exe dump-sst artifacts/supported_destructuring_patterns.cmt | grep -E '^function|field-write' | sed -E 's/ @ .*//'
  function record_destructured#0 mode=exec recursive=false result=box#0 policy=default-linear/default-z3
        field-write type-box#0.value#0 root=box#0:box#0 uniqueness=unique pattern=unique use=aliased local=true public=true mutable=true : unit
  function constructor_destructured#1 mode=exec recursive=false result=box#0 policy=default-linear/default-z3
        field-write type-box#0.value#0 root=box#0:box#0 uniqueness=unique pattern=unique use=aliased local=true public=true mutable=true : unit

Pinned local-cell and standard-ref probes record the Typedtree forms and
resolved paths that define the admitted and rejected boundaries.

  $ ./unique_mutation_tool.exe probe artifacts/unique_records.cmt | grep -E '^Texp_setfield|^Texp_field|^Texp_letmutable'
  Texp_setfield field=value receiver=box pattern=unique use=aliased mutable=true public=true
  Texp_field barriers=aliased-observed (write node carries no barrier)
  Texp_letmutable=1 Texp_setmutvar=1
  $ ./unique_mutation_tool.exe probe artifacts/ref_ops.cmt | grep -F 'resolved ref paths='
  resolved ref paths=Stdlib.!,Stdlib.:=,Stdlib.ref

The pinned compiler rejects capture of a local mutable cell before a CMT can
escape into the unsupported higher-order subset. Color is disabled only for
this exact compiler diagnostic assertion.

  $ OCAML_COLOR=never ocamlc -w -A -alert -all -bin-annot -c fixtures/escaping_local_cell.ml 2>&1 | grep -F 'Mutable variable cannot be used inside a function'
  Error: Mutable variable cannot be used inside a function (at File "fixtures/escaping_local_cell.ml", lines 3-5, characters 2-8).
