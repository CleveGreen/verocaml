  $ mkdir artifacts
  $ ppx_wording () { n=$1; fragment=$2; if OCAML_COLOR=never ocamlc -c -ppx ../../ppx/vero_ppx.exe -o "artifacts/$n.cmo" "../external_specifications/fixtures/$n.ml" >"artifacts/$n.err" 2>&1; then return 1; fi; grep -oF "$fragment" "artifacts/$n.err"; }

  $ ppx_wording payload "does not accept a payload"
  does not accept a payload
  $ ppx_wording recursive_wrapper "does not support recursive bindings"
  does not support recursive bindings
  $ ppx_wording duplicate_attribute "duplicate [@verocaml.external_specification] attribute"
  duplicate [@verocaml.external_specification] attribute
  $ ppx_wording nonfunction "requires a function with an"
  requires a function with an
  $ ppx_wording multiple_binding "requires a single top-level binding"
  requires a single top-level binding

  $ if OCAML_COLOR=never ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/type_error.cmo ../external_specifications/fixtures/type_error.ml >artifacts/type_error.err 2>&1; then false; fi
  $ grep -oF 'expected of type' artifacts/type_error.err
  expected of type
