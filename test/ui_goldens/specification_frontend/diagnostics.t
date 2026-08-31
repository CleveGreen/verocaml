Specification-frontend PPX failures retain only deliberately selected
user-visible wording. Source spans and complete compiler diagnostics are not
golden contracts.

  $ mkdir artifacts
  $ ppx_wording () { name=$1; fragment=$2; if OCAML_COLOR=never ocamlc -c -ppx ../../../ppx/vero_ppx.exe -o "artifacts/$name.cmo" "../../specification_frontend/fixtures/$name.ml" > "artifacts/$name.error" 2>&1; then echo "unexpected PPX success: $name"; return 1; fi; grep -oF "$fragment" "artifacts/$name.error"; }

  $ ppx_wording invalid_empty "%verocaml.requires expects exactly one expression payload"
  %verocaml.requires expects exactly one expression payload
  $ ppx_wording invalid_pattern "%verocaml.assert expects exactly one expression payload"
  %verocaml.assert expects exactly one expression payload
  $ ppx_wording misplaced_top_level "%verocaml.requires is only valid"
  %verocaml.requires is only valid
  $ ppx_wording misplaced_noncontiguous "%verocaml.ensures is only valid"
  %verocaml.ensures is only valid
  $ ppx_wording missing_body "%verocaml.decreases must be followed"
  %verocaml.decreases must be followed
  $ ppx_wording old_outside_ensures "%verocaml.old is only valid inside"
  %verocaml.old is only valid inside
  $ ppx_wording old_in_requires "%verocaml.old is only valid inside"
  %verocaml.old is only valid inside
  $ ppx_wording may_diverge "unsupported verocaml extension %verocaml.may_diverge"
  unsupported verocaml extension %verocaml.may_diverge
  $ ppx_wording assert_top_level "%verocaml.assert is only valid"
  %verocaml.assert is only valid
  $ ppx_wording assert_noncontiguous "%verocaml.assert is only valid"
  %verocaml.assert is only valid
