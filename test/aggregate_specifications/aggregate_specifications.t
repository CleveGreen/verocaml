Immutable aggregate specifications retain nominal SST/VIR domains while
supporting construction, projection, matching, tuples, and aggregate results.

  $ mkdir artifacts
  $ retained () { name=$1; ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$name.cmo" "fixtures/$name.ml"; }
  $ retained positive_aggregates
  $ ./aggregate_specifications_tool.exe dump-sst artifacts/positive_aggregates.cmt > artifacts/aggregate.first.sst
  $ ./aggregate_specifications_tool.exe dump-sst artifacts/positive_aggregates.cmt > artifacts/aggregate.second.sst
  $ cmp artifacts/aggregate.first.sst artifacts/aggregate.second.sst
  $ grep -E '^function (make_sample|amount|classify|pair|sum_pair|verified)' artifacts/aggregate.first.sst
  function make_sample#0 mode=spec recursive=false result=sample#1 policy=default-linear/default-z3 @ positive_aggregates.ml:8:0-9:17
  function amount#1 mode=spec recursive=false result=int policy=default-linear/default-z3 @ positive_aggregates.ml:11:0-11:68
  function classify#2 mode=spec recursive=false result=int policy=default-linear/default-z3 @ positive_aggregates.ml:13:0-17:17
  function pair#3 mode=spec recursive=false result=(sample#1 * int) policy=default-linear/default-z3 @ positive_aggregates.ml:19:0-20:17
  function sum_pair#4 mode=spec recursive=false result=int policy=default-linear/default-z3 @ positive_aggregates.ml:22:0-25:17
  function verified#5 mode=exec recursive=false result=int policy=default-linear/default-z3 @ positive_aggregates.ml:27:0-32:3
  $ ./aggregate_specifications_tool.exe dump-vir artifacts/positive_aggregates.cmt > artifacts/aggregate.first.vir
  $ ./aggregate_specifications_tool.exe dump-vir artifacts/positive_aggregates.cmt > artifacts/aggregate.second.vir
  $ cmp artifacts/aggregate.first.vir artifacts/aggregate.second.vir
  $ grep '^function ' artifacts/aggregate.first.vir
  function verified#5 mode=exec body=checked-typedtree:positive_aggregates.ml policy=default-linear/default-z3
  $ grep -F 't1_sample_record.amount#0' artifacts/aggregate.first.vir | head -1
        (= (t1_sample_record.amount#0 _sample$2) n$0)
  $ ./aggregate_specifications_tool.exe solve artifacts/positive_aggregates.cmt
  verified: verified (6 obligations)
  $ retained positive_variant
  $ ./aggregate_specifications_tool.exe dump-sst artifacts/positive_variant.cmt > artifacts/variant.first.sst
  $ ./aggregate_specifications_tool.exe dump-sst artifacts/positive_variant.cmt > artifacts/variant.second.sst
  $ cmp artifacts/variant.first.sst artifacts/variant.second.sst
  $ grep -E '^function (make_payload|payload_value|verified_payload)' artifacts/variant.first.sst
  function make_payload#0 mode=spec recursive=false result=payload#0 policy=default-linear/default-z3 @ positive_variant.ml:5:0-6:17
  function payload_value#1 mode=spec recursive=false result=int policy=default-linear/default-z3 @ positive_variant.ml:8:0-12:17
  function verified_payload#2 mode=exec recursive=false result=int policy=default-linear/default-z3 @ positive_variant.ml:14:0-18:7
  $ ./aggregate_specifications_tool.exe solve artifacts/positive_variant.cmt
  verified_payload: verified (6 obligations)
  $ retained positive_inline_variant
  $ ./aggregate_specifications_tool.exe solve artifacts/positive_inline_variant.cmt
  verified_payload: verified (6 obligations)

An authenticated abstract value passes opaquely to its exact validated model.
Only that model definition projects the hidden representation, and its
aggregate result remains a nominal logical snapshot.

  $ retained positive_model
  $ ./aggregate_specifications_tool.exe descriptors artifacts/positive_model.cmt
  logical-type snapshot#0 nominal
  model Stack.model#2 domain=Stack.t#2 result=snapshot#0 visibility=authenticated-abstract
  $ ./aggregate_specifications_tool.exe raw-attacks artifacts/positive_model.cmt
  rejected: wrong model index/name cannot resolve a descriptor
  rejected: wrong nominal field domain before VIR
  rejected: ownership-bearing logical aggregate before VIR
  rejected: raw hidden-representation/model-opacity substitution before VIR
  $ ./aggregate_specifications_tool.exe raw-identity-attacks > artifacts/raw-identity.first
  $ ./aggregate_specifications_tool.exe raw-identity-attacks > artifacts/raw-identity.second
  $ cmp artifacts/raw-identity.first artifacts/raw-identity.second
  $ cat artifacts/raw-identity.first
  record-owner validator: program: invalid semantic SST: record field owner must match enclosing record type at aggregate_specifications_raw.ml:10:0-10:1
  record-owner lowerer: program: malformed SST: program: invalid semantic SST: record field owner must match enclosing record type at aggregate_specifications_raw.ml:10:0-10:1 at aggregate_specifications_raw.ml:10:0-10:1
  record-index validator: program: invalid semantic SST: record fields must have dense zero-based indices at aggregate_specifications_raw.ml:12:0-12:1
  record-index lowerer: program: malformed SST: program: invalid semantic SST: record fields must have dense zero-based indices at aggregate_specifications_raw.ml:12:0-12:1 at aggregate_specifications_raw.ml:12:0-12:1
  constructor-owner validator: program: invalid semantic SST: variant constructor owner must match enclosing variant type at aggregate_specifications_raw.ml:20:0-20:1
  constructor-owner lowerer: program: malformed SST: program: invalid semantic SST: variant constructor owner must match enclosing variant type at aggregate_specifications_raw.ml:20:0-20:1 at aggregate_specifications_raw.ml:20:0-20:1
  constructor-index validator: program: invalid semantic SST: variant constructors must have dense zero-based indices at aggregate_specifications_raw.ml:22:0-22:1
  constructor-index lowerer: program: malformed SST: program: invalid semantic SST: variant constructors must have dense zero-based indices at aggregate_specifications_raw.ml:22:0-22:1 at aggregate_specifications_raw.ml:22:0-22:1
  constructor-field-owner validator: program: invalid semantic SST: constructor field owner must match enclosing variant constructor at aggregate_specifications_raw.ml:30:0-30:1
  constructor-field-owner lowerer: program: malformed SST: program: invalid semantic SST: constructor field owner must match enclosing variant constructor at aggregate_specifications_raw.ml:30:0-30:1 at aggregate_specifications_raw.ml:30:0-30:1
  constructor-field-index validator: program: invalid semantic SST: constructor fields must have dense zero-based indices at aggregate_specifications_raw.ml:32:0-32:1
  constructor-field-index lowerer: program: malformed SST: program: invalid semantic SST: constructor fields must have dense zero-based indices at aggregate_specifications_raw.ml:32:0-32:1 at aggregate_specifications_raw.ml:32:0-32:1
  $ ./aggregate_specifications_tool.exe dump-sst artifacts/positive_model.cmt > artifacts/model.first.sst
  $ ./aggregate_specifications_tool.exe dump-sst artifacts/positive_model.cmt > artifacts/model.second.sst
  $ cmp artifacts/model.first.sst artifacts/model.second.sst
  $ grep '^function Stack.model' artifacts/model.first.sst
  function Stack.model#2 mode=spec recursive=false result=snapshot#0 policy=default-linear/default-z3 @ positive_model.ml:22:2-24:19
  $ ./aggregate_specifications_tool.exe dump-vir artifacts/positive_model.cmt > artifacts/model.first.vir
  $ ./aggregate_specifications_tool.exe dump-vir artifacts/positive_model.cmt > artifacts/model.second.vir
  $ cmp artifacts/model.first.vir artifacts/model.second.vir
  $ grep -F 't0_snapshot_record.length#0' artifacts/model.first.vir | head -1
        (= (t0_snapshot_record.length#0 _snapshot$1) (t2_Stack.t_record.length#1 stack$0))
  $ ./aggregate_specifications_tool.exe solve artifacts/positive_model.cmt
  Stack.singleton: verified (0 obligations)
  Stack.length: verified (1 obligations)
  Stack.drop: verified (0 obligations)
  observe: verified (1 obligations)

An immutable recursive type shape backed by an authenticated local rank domain
is accepted for a nonrecursive identity specification.  This is type-shape
admission, not evidence that any runtime value is finite or acyclic.

  $ retained cyclic_spec
  $ ./aggregate_specifications_tool.exe dump-sst artifacts/cyclic_spec.cmt | grep -E '^(type chain|function expose)'
  type chain#0 representation=revealed @ cyclic_spec.ml:1:0-1:32
  function expose#0 mode=spec recursive=false result=chain#0 policy=default-linear/default-z3 @ cyclic_spec.ml:3:0-3:52

The generic identity specification verifies once over its canonical source
binder. It does not require a closed-member catalogue or call-site clone.

  $ retained polymorphic_spec
  $ ./aggregate_specifications_tool.exe dump-sst artifacts/polymorphic_spec.cmt | grep '^function identity'
  function identity#0 binders=['0@identity#0] mode=spec recursive=false result='0@identity#0 policy=default-linear/default-z3 @ polymorphic_spec.ml:1:0-1:44

The historical direct function-valued Spec is now an authenticated first-class
Spec function. Mutable aggregates, references, objects, foreign values, and
unsupported source aggregate equality remain rejected before VIR.

  $ retained function_spec
  $ ./aggregate_specifications_tool.exe dump-sst artifacts/function_spec.cmt | grep '^function apply'
  function apply#0 mode=spec recursive=false result=int policy=default-linear/default-z3 @ function_spec.ml:1:0-2:17
  $ ./aggregate_specifications_tool.exe dump-vir artifacts/function_spec.cmt >/dev/null && echo 'function-valued Spec semantic-lowering=accepted'
  function-valued Spec semantic-lowering=accepted
  $ for name in mutable_spec reference_spec object_spec foreign_spec aggregate_equality; do retained "$name"; ./aggregate_specifications_tool.exe reject "artifacts/$name.cmt"; done
  semantic validation rejected before VIR
  adapter rejected: VERO_UNSUPPORTED_TYPE
  adapter rejected: VERO_UNSUPPORTED_TYPE
  adapter rejected: VERO_UNSUPPORTED_TYPE
  adapter rejected: VERO_UNSUPPORTED_AGGREGATE_EQUALITY
