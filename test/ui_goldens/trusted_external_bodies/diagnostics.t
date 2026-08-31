Trusted-body source rejection is structural in the grouped outcome suite. This
target retains only deliberately selected user-visible PPX/compiler excerpts.

  $ mkdir artifacts
  $ excerpt () { name=$1; fragment=$2; OCAML_COLOR=never ocamlc -c -ppx ../../../ppx/vero_ppx.exe -o "artifacts/$name.cmo" "../../trusted_external_bodies/fixtures/$name.ml" >"artifacts/$name.err" 2>&1 || true; grep -oF "$fragment" "artifacts/$name.err" | head -1; }

  $ excerpt missing_ensures "requires at least one ensures"
  requires at least one ensures
  $ excerpt proof_conflict_spec "verocaml.spec"
  verocaml.spec
  $ excerpt proof_duplicate_modifier "duplicate [@verocaml.external_body]"
  duplicate [@verocaml.external_body]
  $ excerpt type_error "expected of type"
  expected of type
