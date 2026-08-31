Ordinary aggregate verification and frontend outcomes are covered by
outcome_cases.ml.  This transcript retains private architecture boundaries.

  $ mkdir artifacts
  $ retained () { name=$1; ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$name.cmo" "fixtures/$name.ml"; }

The inline-variant direct lower/solve path is a specialist boundary: the shared
verifier service currently rejects its inline layout before producing an
outcome.

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
The generic identity specification verifies once over its canonical source
binder. It does not require a closed-member catalogue or call-site clone.

  $ retained polymorphic_spec
  $ ./aggregate_specifications_tool.exe dump-sst artifacts/polymorphic_spec.cmt | grep '^function identity'
  function identity#0 binders=['0@identity#0] mode=spec recursive=false result='0@identity#0 policy=default-linear/default-z3 @ polymorphic_spec.ml:1:0-1:44

Mutable logical aggregates still exercise the private semantic-validator
boundary, which does not expose a framework outcome.

  $ retained mutable_spec
  $ ./aggregate_specifications_tool.exe reject artifacts/mutable_spec.cmt
  semantic validation rejected before VIR
