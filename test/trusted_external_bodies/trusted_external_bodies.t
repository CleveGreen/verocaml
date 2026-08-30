  $ ./trusted_external_bodies_tool.exe structural
  raw semantic trusted-external-body provenance rejected
  raw proof trusted-body provenance and mode replay rejected
  wrong Exec call form cannot invoke a trusted Proof declaration

  $ mkdir artifacts
  $ ocamlc -stop-after parsing -dsource -ppx ../../ppx/vero_ppx.exe fixtures/positive.ml > artifacts/o 2> artifacts/source
  $ test ! -s artifacts/o
  $ grep -E 'let keep_only_first|record.next <- Empty|s.length <- 1|external_body|Vero_ghost' artifacts/source
  let keep_only_first (s : stack @ unique)  : stack @ unique =
    | Node record -> (record.next <- Empty; s.length <- 1; s)[@@verocaml.internal.artifact_family.ordinary-v1
  $ ocamlc -bin-annot -ppx ../../ppx/vero_ppx.exe -c -o artifacts/erased.cmo fixtures/positive.ml
  $ ./trusted_external_bodies_tool.exe inspect erased artifacts/erased.cmt
  ordinary CMT preserved the implementation and erased trust metadata

  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/positive.cmo fixtures/positive.ml
  $ ./trusted_external_bodies_tool.exe inspect retained artifacts/positive.cmt
  retained CMT authenticated one trusted external body
  $ ./trusted_external_bodies_tool.exe sst artifacts/positive.cmt > artifacts/a.sst
  $ ./trusted_external_bodies_tool.exe sst artifacts/positive.cmt > artifacts/b.sst
  $ cmp artifacts/a.sst artifacts/b.sst
  $ grep -E '^function (keep_only_first|caller)|trusted-external-body|returns-unique' artifacts/a.sst
  function keep_only_first#0 mode=exec recursive=false result=stack#1 policy=default-linear/default-z3 @ positive.ml:7:0-23:26
    returns-unique-parameter 0
    body trusted-external-body trust=axiomatic provenance=typedtree:positive.ml declaration-span=positive.ml:7:0-23:26 witness-span=positive.ml:23:0-23:26 requires=1 ensures=1 body=unchecked
  function caller#1 mode=exec recursive=false result=int policy=default-linear/default-z3 @ positive.ml:25:0-31:10
  $ test $(grep -c 'field-write\|checked-add\|assertion\|decreases' artifacts/a.sst) -eq 0

  $ ./trusted_external_bodies_tool.exe vir artifacts/positive.cmt > artifacts/a.vir
  $ ./trusted_external_bodies_tool.exe vir artifacts/positive.cmt > artifacts/b.vir
  $ cmp artifacts/a.vir artifacts/b.vir
  $ grep -E '^trusted-external-body|^function |^  trusted-external-body' artifacts/a.vir
  trusted-external-body-declaration trust=axiomatic function=keep_only_first#0 declaration-span=positive.ml:7:0-23:26 witness-span=positive.ml:23:0-23:26 requires=1 ensures=1 body=unchecked
  function caller#1 mode=exec body=checked-typedtree:positive.ml policy=default-linear/default-z3
    trusted-external-body trust=axiomatic function=keep_only_first#0 declaration-span=positive.ml:7:0-23:26 witness-span=positive.ml:23:0-23:26 call=positive.ml:30:10-30:27 requires=1 ensures=1 body=unchecked result=constrained-only-by-ensures
  $ grep -c '^  vc ' artifacts/a.vir
  45
  $ test $(grep -c '^function keep_only_first' artifacts/a.vir) -eq 0
  $ ./trusted_external_bodies_tool.exe solve artifacts/positive.cmt
  verified: caller used only the trusted contract

  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/positive.cmt | grep -E 'trusted external body|verified-with-trusted-axioms'
  verocaml: trusted external body trust=axiomatic function=keep_only_first#0 declaration-span=fixtures/positive.ml:7:0-23:26 witness-span=fixtures/positive.ml:23:0-23:26 call=fixtures/positive.ml:30:10-30:27 requires=1 ensures=1 body=unchecked result=constrained-only-by-ensures
  verocaml: trusted external body declaration trust=axiomatic function=keep_only_first#0 declaration-span=fixtures/positive.ml:7:0-23:26 witness-span=fixtures/positive.ml:23:0-23:26 requires=1 ensures=1 body=unchecked
  verocaml: verified-with-trusted-axioms file=artifacts/positive.cmt functions=1 obligations=45 trusted-external-bodies=1 trusted-external-body-uses=1 trusted-external-spec-uses=0
  $ OCAML_COLOR=never ../../src/verocaml.exe verify fixtures/positive.ml | grep -E 'trusted external body declaration|verified-with-trusted-axioms'
  verocaml: trusted external body declaration trust=axiomatic function=keep_only_first#0 declaration-span=fixtures/positive.ml:7:0-23:26 witness-span=fixtures/positive.ml:23:0-23:26 requires=1 ensures=1 body=unchecked
  verocaml: verified-with-trusted-axioms file=fixtures/positive.ml functions=1 obligations=45 trusted-external-bodies=1 trusted-external-body-uses=1 trusted-external-spec-uses=0

  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/unmarked.cmo fixtures/unmarked_nested.ml
  $ ./trusted_external_bodies_tool.exe reject artifacts/unmarked.cmt
  adapter rejected: VERO_UNSUPPORTED_TYPE
  $ ocamlc -w -A -alert -all -bin-annot -c -o artifacts/raw.cmo fixtures/raw_attribute.ml
  $ ./trusted_external_bodies_tool.exe reject artifacts/raw.cmt
  adapter rejected: VERO_MALFORMED_GHOST_CALL
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -c -o artifacts/counterfeit.cmo fixtures/counterfeit.ml
  $ ./trusted_external_bodies_tool.exe reject artifacts/counterfeit.cmt
  adapter rejected: VERO_MALFORMED_GHOST_CALL

  $ ppx_failure () { n=$1; text=$2; if OCAML_COLOR=never ocamlc -c -ppx ../../ppx/vero_ppx.exe -o "artifacts/$n.cmo" "fixtures/$n.ml" >"artifacts/$n.err" 2>&1; then return 1; fi; grep -F "$text" "artifacts/$n.err" >/dev/null; }
  $ ppx_failure missing_ensures "requires at least one ensures"
  $ ppx_failure decreases "allows only requires and ensures"
  $ ppx_failure assertion "allows only requires and ensures"
  $ ppx_failure recursive "does not support recursive bindings"
  $ ppx_failure payload "does not accept a payload"
  $ ppx_failure duplicate "duplicate [@verocaml.external_body] attribute"
  $ ppx_failure nonfunction "requires at least one ensures"
  $ ppx_failure misplaced "is only valid on one nonrecursive top-level"
  $ if OCAML_COLOR=never ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/type_error.cmo fixtures/type_error.ml >artifacts/type_error.err 2>&1; then false; fi
  $ grep -F 'expected of type' artifacts/type_error.err >/dev/null

  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/precondition_failure.cmo fixtures/precondition_failure.ml
  $ ./trusted_external_bodies_tool.exe counterexample artifacts/precondition_failure.cmt
  counterexample: call precondition is enforced

Proof mode composes orthogonally with the trusted-body disposition.  The exact
PFC and an ordinary Proof caller use only the authenticated contract summary;
the trusted declaration has no semantic body or VIR execution.

  $ retained_proof () { n=$1; ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$n.cmo" "fixtures/$n.ml"; }
  $ retained_proof proof_exact_pfc
  $ retained_proof proof_positive
  $ retained_proof proof_order_reversed
  $ ./trusted_external_bodies_tool.exe inspect proof-retained-exact artifacts/proof_exact_pfc.cmt
  retained CMT authenticated the exact combined proof external body role
  $ ./trusted_external_bodies_tool.exe inspect proof-retained artifacts/proof_positive.cmt
  retained CMT authenticated one combined proof external body and two proof declarations
  $ ./trusted_external_bodies_tool.exe sst artifacts/proof_positive.cmt > artifacts/proof-a.sst
  $ ./trusted_external_bodies_tool.exe sst artifacts/proof_positive.cmt > artifacts/proof-b.sst
  $ cmp artifacts/proof-a.sst artifacts/proof-b.sst
  $ grep -E '^function (admit|assume|caller)|trusted-external-body|proof-call (admit|assume)' artifacts/proof-a.sst
  function admit#0 mode=proof recursive=false result=unit policy=default-linear/default-z3 @ proof_positive.ml:1:0-5:18
    body trusted-external-body trust=axiomatic provenance=typedtree:proof_positive.ml declaration-span=proof_positive.ml:1:0-5:18 witness-span=proof_positive.ml:4:0-4:26 requires=0 ensures=1 body=unchecked
  function assume#1 mode=proof recursive=false result=unit policy=default-linear/default-z3 @ proof_positive.ml:7:0-10:18
      proof-call admit#0 recursive=false type-arguments=[] : unit @ proof_positive.ml:9:2-9:10
  function caller#2 mode=proof recursive=false result=unit policy=default-linear/default-z3 @ proof_positive.ml:12:0-16:18
      proof-call assume#1 recursive=false type-arguments=[] : unit @ proof_positive.ml:15:2-15:13
  $ ./trusted_external_bodies_tool.exe vir artifacts/proof_positive.cmt > artifacts/proof-a.vir
  $ ./trusted_external_bodies_tool.exe vir artifacts/proof_positive.cmt > artifacts/proof-b.vir
  $ cmp artifacts/proof-a.vir artifacts/proof-b.vir
  $ grep -E '^trusted-external-body|^function |^  trusted-external-body|^  vc ' artifacts/proof-a.vir
  trusted-external-body-declaration trust=axiomatic mode=proof function=admit#0 declaration-span=proof_positive.ml:1:0-5:18 witness-span=proof_positive.ml:4:0-4:26 requires=0 ensures=1 body=unchecked
  function assume#1 mode=proof body=proof-typedtree:proof_positive.ml policy=default-linear/default-z3
    trusted-external-body trust=axiomatic mode=proof call-form=proof function=admit#0 declaration-span=proof_positive.ml:1:0-5:18 witness-span=proof_positive.ml:4:0-4:26 call=proof_positive.ml:9:2-9:10 requires=0 ensures=1 body=unchecked result=constrained-only-by-ensures
    vc 0 postcondition ordinal=0 declaration=proof_positive.ml:8:2-8:35 @ proof_positive.ml:8:2-8:35
  function caller#2 mode=proof body=proof-typedtree:proof_positive.ml policy=default-linear/default-z3
    vc 0 postcondition ordinal=0 declaration=proof_positive.ml:14:2-14:35 @ proof_positive.ml:14:2-14:35
  $ test $(grep -c '^function admit' artifacts/proof-a.vir) -eq 0
  $ ./trusted_external_bodies_tool.exe solve artifacts/proof_positive.cmt
  verified: caller used only the trusted contract
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/proof_exact_pfc.cmt | grep -E 'mode=proof|verified-with-trusted-axioms'
  verocaml: trusted external body trust=axiomatic mode=proof call-form=proof function=admit#0 declaration-span=fixtures/proof_exact_pfc.ml:1:0-5:18 witness-span=fixtures/proof_exact_pfc.ml:4:0-4:26 call=fixtures/proof_exact_pfc.ml:9:2-9:10 requires=0 ensures=1 body=unchecked result=constrained-only-by-ensures
  verocaml: trusted external body declaration trust=axiomatic mode=proof function=admit#0 declaration-span=fixtures/proof_exact_pfc.ml:1:0-5:18 witness-span=fixtures/proof_exact_pfc.ml:4:0-4:26 requires=0 ensures=1 body=unchecked
  verocaml: verified-with-trusted-axioms file=artifacts/proof_exact_pfc.cmt functions=1 obligations=1 trusted-external-bodies=1 trusted-external-body-uses=1 trusted-external-spec-uses=0
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/proof_order_reversed.cmt >/dev/null

The retained compiler sees the complete implementation, while semantic
lowering stops after the contiguous contract prefix.  Unsupported tail
operations create no declaration execution, body operation, VC, receipt,
finite/rank authority, recursive encoding, or direct solver context.

  $ retained_proof proof_unchecked_tail
  $ ./trusted_external_bodies_tool.exe sst artifacts/proof_unchecked_tail.cmt > artifacts/tail.sst
  $ ./trusted_external_bodies_tool.exe vir artifacts/proof_unchecked_tail.cmt > artifacts/tail.vir
  $ test $(grep -Ec 'mutable-|checked-|print_int|raise|loop|field-write|assertion|decreases' artifacts/tail.sst) -eq 0
  $ test $(grep -c '^function trusted' artifacts/tail.vir) -eq 0
  $ grep -c '^  vc ' artifacts/tail.vir
  1
  $ ./trusted_external_bodies_tool.exe observe artifacts/proof_unchecked_tail.cmt
  tail-observer functions=1 obligations=1 declarations=1 uses=1 body-executions=0 receipts=0/0 finite=0/0/0/0 self=0/0/0 recursive=0/0 solver=1 z3=1/1
  $ if OCAML_COLOR=never ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/proof_type_error.cmo fixtures/proof_type_error.ml >artifacts/proof_type_error.err 2>&1; then false; fi
  $ grep -F 'expected of type' artifacts/proof_type_error.err >/dev/null

The declared false postcondition is the only reason `assume` proves an
arbitrary condition: an identical checked body fails, weakening or suppressing
the summary fails, and trusted preconditions remain caller VCs.

  $ retained_proof proof_lying_trusted
  $ retained_proof proof_lying_unmarked
  $ retained_proof proof_weakened
  $ retained_proof proof_requires_failure
  $ ./trusted_external_bodies_tool.exe outcomes artifacts/proof_lying_trusted.cmt
  all obligations verified
  $ ./trusted_external_bodies_tool.exe outcomes artifacts/proof_lying_unmarked.cmt
  admit: counterexample postcondition
  $ ./trusted_external_bodies_tool.exe outcomes artifacts/proof_weakened.cmt
  assume: counterexample postcondition
  $ ./trusted_external_bodies_tool.exe suppress-postconditions artifacts/proof_positive.cmt
  suppressed admit postcondition: assume counterexample postcondition
  $ ./trusted_external_bodies_tool.exe outcomes artifacts/proof_requires_failure.cmt
  caller: counterexample call-precondition

Ordinary Proof output erases the combined declaration exactly like any other
Proof declaration.  CMI/CMT/CMO/CMX and byte/native artifacts expose no
binding, retained carrier, or runtime ghost call.

  $ ocamlc -stop-after parsing -dsource -ppx ../../ppx/vero_ppx.exe fixtures/proof_exact_pfc.ml > artifacts/proof-o 2> artifacts/proof-source
  $ test ! -s artifacts/proof-o
  $ test $(grep -Ec '^let |Vero_ghost|external_body|proof_definition' artifacts/proof-source) -eq 0
  $ ocamlc -bin-annot -ppx ../../ppx/vero_ppx.exe -c -o artifacts/proof_erased.cmo fixtures/proof_exact_pfc.ml
  $ ./trusted_external_bodies_tool.exe inspect proof-erased artifacts/proof_erased.cmt
  ordinary CMT erased proof external body, proof declarations, and calls
  $ ocamlc -i -ppx ../../ppx/vero_ppx.exe fixtures/proof_exact_pfc.ml > artifacts/proof-interface
  $ test $(grep -c '^val ' artifacts/proof-interface) -eq 0
  $ test $(strings artifacts/proof_erased.cmo | grep -Ec 'Vero_ghost|external_body|proof_definition|admit|assume') -eq 0
  $ ocamlopt -bin-annot -ppx ../../ppx/vero_ppx.exe -c -o artifacts/proof_erased.cmx fixtures/proof_exact_pfc.ml
  $ ocamlc -o artifacts/proof_erased.byte artifacts/proof_erased.cmo
  $ ocamlopt -o artifacts/proof_erased.native artifacts/proof_erased.cmx
  $ artifacts/proof_erased.byte
  $ artifacts/proof_erased.native

All declaration-role conflicts name the real roles.  Duplicate modifiers and
roles, payloads, local/misplaced forms, recursion, nonunit signatures,
unsupported clauses, unauthenticated carriers, replay, and wrong-stage calls
reject at their assigned boundaries.

  $ ppx_proof_failure () { n=$1; shift; if OCAML_COLOR=never ocamlc -c -ppx ../../ppx/vero_ppx.exe -o "artifacts/$n.cmo" "fixtures/$n.ml" >"artifacts/$n.err" 2>&1; then return 1; fi; for text in "$@"; do grep -F "$text" "artifacts/$n.err" >/dev/null; done; }
  $ ppx_proof_failure proof_conflict_spec "verocaml.spec" "verocaml.external_body"
  $ ppx_proof_failure proof_conflict_type_invariant "verocaml.type_invariant" "verocaml.external_body"
  $ ppx_proof_failure proof_conflict_external_specification "verocaml.external_specification" "verocaml.external_body"
  $ ppx_proof_failure proof_duplicate_modifier "duplicate [@verocaml.external_body]"
  $ ppx_proof_failure proof_duplicate_role "duplicate [@verocaml.proof]"
  $ ppx_proof_failure proof_modifier_payload "external_body] does not accept a payload"
  $ ppx_proof_failure proof_role_payload "proof] does not accept a payload"
  $ ppx_proof_failure proof_local "external_body] is only valid on one nonrecursive top-level"
  $ ppx_proof_failure proof_misplaced "external_body] is only valid on one nonrecursive top-level"
  $ ppx_proof_failure proof_recursive "external_body] does not support recursive bindings"
  $ ppx_proof_failure proof_missing_ensures "requires at least one ensures"
  $ ppx_proof_failure proof_assertion "allows only requires and ensures"
  $ ppx_proof_failure proof_decreases "allows only requires and ensures"
  $ retained_reject () { n=$1; retained_proof "$n"; OCAML_COLOR=never ../../src/verocaml.exe verify "artifacts/$n.cmt" >"artifacts/$n.verify" 2>&1 && return 1; grep -F "$2" "artifacts/$n.verify" >/dev/null; }
  $ retained_reject proof_nonunit VERO_MALFORMED_GHOST_CALL
  $ retained_reject proof_wrong_stage VERO_ERASED_CALL
  $ for n in proof_raw_attribute proof_counterfeit proof_foreign_carrier; do ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -c -o "artifacts/$n.cmo" "fixtures/$n.ml"; ./trusted_external_bodies_tool.exe reject "artifacts/$n.cmt"; done
  adapter rejected: VERO_MALFORMED_GHOST_CALL
  adapter rejected: VERO_MALFORMED_GHOST_CALL
  adapter rejected: VERO_MALFORMED_GHOST_CALL

Generic trusted Proof bodies retain their type binders and verify as explicit
axioms rather than being rejected as if polymorphism were unsupported.

  $ retained_proof proof_generic
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/proof_generic.cmt | grep -E 'trusted external body declaration|verified-with-trusted-axioms'
  verocaml: trusted external body declaration trust=axiomatic mode=proof function=trusted#0 declaration-span=fixtures/proof_generic.ml:1:0-5:18 witness-span=fixtures/proof_generic.ml:4:0-4:26 requires=0 ensures=1 body=unchecked
  verocaml: verified-with-trusted-axioms file=artifacts/proof_generic.cmt functions=0 obligations=0 trusted-external-bodies=1 trusted-external-body-uses=0 trusted-external-spec-uses=0
  $ retained_proof proof_replay
  $ ./trusted_external_bodies_tool.exe replay artifacts/proof_replay.cmt
  replayed combined-role carriers rejected in one process recursive=0/0 solver=0 z3=0/0

Trusted Proof bodies can be exported by an ordinary library, but consumer
verification must preserve and report the explicit axiom trust.

  $ mkdir artifacts/import
  $ (cd artifacts/import && ocamlc -w -A -alert -all -bin-annot -I ../../../../runtime/.vero_ghost.objs/byte -ppx "../../../../ppx/vero_ppx.exe --keep-ghost" -c -o proof_provider.cmo ../../fixtures/proof_provider.ml)
  $ (cd artifacts/import && ocamlc -w -A -alert -all -bin-annot -I . -I ../../../../runtime/.vero_ghost.objs/byte -ppx "../../../../ppx/vero_ppx.exe --keep-ghost" -c -o proof_consumer.cmo ../../fixtures/proof_consumer.ml)
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/import/proof_consumer.cmt --dependency artifacts/import/proof_provider.cmt > artifacts/import/verified 2>&1; echo $?
  0
  $ test "$(grep -c '^verocaml: trusted external body .*function=Proof_provider.admit' artifacts/import/verified)" = 1; echo imported-trusted-proof=reported-once
  imported-trusted-proof=reported-once
  $ grep -q '^verocaml: verified-with-trusted-axioms ' artifacts/import/verified; echo imported-trusted-proof=verification-remains-trusted
  imported-trusted-proof=verification-remains-trusted
