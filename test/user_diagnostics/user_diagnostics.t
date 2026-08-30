  $ mkdir artifacts
  $ retained () { name=$1; ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$name.cmo" "fixtures/$name.ml"; }

Forgetting to declare a proof reports the source-level mistake and both valid
repairs. It does not expose instance-mode or semantic-SST implementation terms.

  $ retained missing_proof_annotation
  $ for route in source cmt; do if test "$route" = source; then input=fixtures/missing_proof_annotation.ml; else input=artifacts/missing_proof_annotation.cmt; fi; code=0; OCAML_COLOR=never ../../src/verocaml.exe verify "$input" --threads 1 --timeout-ms 10000 >"artifacts/missing-proof.$route.out" 2>&1 || code=$?; test "$code" = 2; done
  $ cmp artifacts/missing-proof.source.out artifacts/missing-proof.cmt.out
  $ cat artifacts/missing-proof.source.out
  File "fixtures/missing_proof_annotation.ml", line 6, characters 38-54:
  Error: [VERO_ERASED_CALL] Executable function "lemma_should_be_proof" uses proof-only function "contradiction" without marking that call as ghost.
    Hint: If "lemma_should_be_proof" is a proof, add [@@verocaml.proof] after its definition.
    Hint: Otherwise, explicitly mark the call to "contradiction" as ghost code.

Calling an unmarked executable helper from a postcondition names the helper and
shows the exact specification annotation instead of reporting a stage mismatch.

  $ retained missing_spec_annotation
  $ for route in source cmt; do if test "$route" = source; then input=fixtures/missing_spec_annotation.ml; else input=artifacts/missing_spec_annotation.cmt; fi; code=0; OCAML_COLOR=never ../../src/verocaml.exe verify "$input" --threads 1 --timeout-ms 10000 >"artifacts/missing-spec.$route.out" 2>&1 || code=$?; test "$code" = 2; done
  $ cmp artifacts/missing-spec.source.out artifacts/missing-spec.cmt.out
  $ cat artifacts/missing-spec.source.out
  File "fixtures/missing_spec_annotation.ml", line 4, characters 30-44:
  Error: [VERO_EXEC_IN_SPEC] Executable function "positive" cannot be used in a specification.
    Hint: If "positive" is a logical definition, add [@@verocaml.spec] after its definition.

Unsupported source types are described in source terms and provide the two
supported directions for replacing or specifying the type.

  $ retained unsupported_type
  $ for route in source cmt; do if test "$route" = source; then input=fixtures/unsupported_type.ml; else input=artifacts/unsupported_type.cmt; fi; code=0; OCAML_COLOR=never ../../src/verocaml.exe verify "$input" --threads 1 --timeout-ms 10000 >"artifacts/unsupported-type.$route.out" 2>&1 || code=$?; test "$code" = 2; done
  $ cmp artifacts/unsupported-type.source.out artifacts/unsupported-type.cmt.out
  $ cat artifacts/unsupported-type.source.out
  File "fixtures/unsupported_type.ml", line 1, characters 11-17:
  Error: [VERO_UNSUPPORTED_TYPE] This type is not supported in verified code.
    Hint: Use a supported scalar or immutable algebraic data type, or provide an external type specification.

Quantifier validation reports the concrete source error instead of the broad
authentication category.

  $ retained missing_forall_trigger
  $ for route in source cmt; do if test "$route" = source; then input=fixtures/missing_forall_trigger.ml; else input=artifacts/missing_forall_trigger.cmt; fi; code=0; OCAML_COLOR=never ../../src/verocaml.exe verify "$input" --threads 1 --timeout-ms 10000 >"artifacts/missing-trigger.$route.out" 2>&1 || code=$?; test "$code" = 2; done
  $ cmp artifacts/missing-trigger.source.out artifacts/missing-trigger.cmt.out
  $ cat artifacts/missing-trigger.source.out
  File "fixtures/missing_forall_trigger.ml", line 4, characters 55-73:
  Error: [VERO_QUANTIFIER_TRIGGER] forall requires exactly one explicit trigger

Callback contract validation likewise preserves the actionable reason.

  $ retained callback_missing_contract
  $ for route in source cmt; do if test "$route" = source; then input=fixtures/callback_missing_contract.ml; else input=artifacts/callback_missing_contract.cmt; fi; code=0; OCAML_COLOR=never ../../src/verocaml.exe verify "$input" --threads 1 --timeout-ms 10000 >"artifacts/callback-contract.$route.out" 2>&1 || code=$?; test "$code" = 2; done
  $ cmp artifacts/callback-contract.source.out artifacts/callback-contract.cmt.out
  $ cat artifacts/callback-contract.source.out
  File "fixtures/callback_missing_contract.ml", line 7, characters 15-48:
  Error: [VERO_CALLBACK_CONTRACT] verified callback requires an explicit requires clause
