Pure-specification PPX failures retain only deliberately selected user-visible
wording. Source spans and complete compiler diagnostics are not golden
contracts.

  $ mkdir artifacts
  $ ppx_wording () { name=$1; fragment=$2; if OCAML_COLOR=never ocamlc -c -ppx ../../../ppx/vero_ppx.exe -o "artifacts/$name.cmo" "../../pure_specifications/fixtures/$name.ml" > "artifacts/$name.error" 2>&1; then echo "unexpected PPX success: $name"; return 1; fi; grep -oF "$fragment" "artifacts/$name.error"; }

  $ ppx_wording payload "does not accept a payload"
  does not accept a payload
  $ ppx_wording duplicate "duplicate [@verocaml.spec]"
  duplicate [@verocaml.spec]
  $ ppx_wording recursive "does not support recursive bindings"
  does not support recursive bindings
  $ ppx_wording non_function "requires a function with an expression body"
  requires a function with an expression body
  $ ppx_wording multiple "requires a single top-level binding"
  requires a single top-level binding
  $ ppx_wording nested_attribute "only valid on one nonrecursive top-level"
  only valid on one nonrecursive top-level
  $ ppx_wording old_body "%verocaml.old is only valid inside"
  %verocaml.old is only valid inside
