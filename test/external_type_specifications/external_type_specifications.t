  $ mkdir artifacts
  $ ocamlc -w -A -alert -all -bin-annot -c -o artifacts/external_types.cmo fixtures/external_types.ml
  $ retained () { name=$1; ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -I artifacts -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$name.cmo" "fixtures/$name.ml"; }

The standard specification prelude declares option, list, and result through
the public mechanism rather than duplicating their datatype shapes.

  $ OCAML_COLOR=never ../../src/verocaml.exe verify ../../library/pervasive.ml --threads 1 --timeout-ms 10000 --dump-sst artifacts/pervasive.sst
  verocaml: verified file=../../library/pervasive.ml functions=0 obligations=0
  $ grep '^adt ' artifacts/pervasive.sst | sed -E 's/ uid=[^ ]+/ uid=<uid>/'
  adt option<'0@option_specification#0> uid=<uid> provenance=external-type-specification binders=1 variant[0:None()|1:Some(0:$0:'0@option_specification#0)] recursive-fields=0
  adt list<'0@list_specification#1> uid=<uid> provenance=external-type-specification binders=1 variant[0:[]()|1:::(0:$0:'0@list_specification#1,1:$1:list<'0@list_specification#1>)] recursive-fields=1
  adt Stdlib.result<'0@result_specification#2, '1@result_specification#2> uid=<uid> provenance=external-type-specification binders=2 variant[0:Ok(0:$0:'0@result_specification#2)|1:Error(0:$0:'1@result_specification#2)] recursive-fields=0

Qualified Stdlib aliases are admitted only by authenticated external type
specifications. Their representation aliases resolve to the canonical compiler
types and then use the ordinary generic descriptor path.

  $ retained positive_stdlib
  $ OCAML_COLOR=never ../../src/verocaml.exe verify fixtures/positive_stdlib.ml --threads 1 --timeout-ms 10000
  verocaml: verified file=fixtures/positive_stdlib.ml functions=5 obligations=10
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/positive_stdlib.cmt --threads 1 --timeout-ms 10000 --dump-sst artifacts/stdlib.sst
  verocaml: verified file=artifacts/positive_stdlib.cmt functions=5 obligations=10
  $ grep '^adt ' artifacts/stdlib.sst | sed -E 's/ uid=[^ ]+/ uid=<uid>/'
  adt option<'0@option_specification#0> uid=<uid> provenance=external-type-specification binders=1 variant[0:None()|1:Some(0:$0:'0@option_specification#0)] recursive-fields=0
  adt Stdlib.result<'0@result_specification#1, '1@result_specification#1> uid=<uid> provenance=external-type-specification binders=2 variant[0:Ok(0:$0:'0@result_specification#1)|1:Error(0:$0:'1@result_specification#1)] recursive-fields=0

The same mechanism handles unrelated external variants and records without a
verifier case for their module, type, constructors, fields, or arity.

  $ retained positive_custom
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/positive_custom.cmt --threads 2 --timeout-ms 10000 --dump-sst artifacts/custom.sst
  verocaml: verified file=artifacts/positive_custom.cmt functions=3 obligations=3
  $ grep '^adt ' artifacts/custom.sst | sed -E 's/ uid=[^ ]+/ uid=<uid>/'
  adt External_types.box<'0@box_specification#0> uid=<uid> provenance=external-type-specification binders=1 variant[0:Empty()|1:Box(0:$0:'0@box_specification#0)] recursive-fields=0
  adt External_types.outcome<'0@outcome_specification#1, '1@outcome_specification#1> uid=<uid> provenance=external-type-specification binders=2 variant[0:Good(0:$0:'0@outcome_specification#1)|1:Bad(0:$0:'1@outcome_specification#1)] recursive-fields=0
  adt External_types.cell<'0@cell_specification#2> uid=<uid> provenance=external-type-specification binders=1 record{0:value:'0@cell_specification#2;1:flag:bool} recursive-fields=0

Ordinary compilation keeps only the harmless OCaml aliases. No verifier
attribute or retained marker survives into runtime artifacts.

  $ ocamlc -w -A -alert -all -bin-annot -I artifacts -ppx ../../ppx/vero_ppx.exe -c -o artifacts/ordinary.cmo fixtures/positive_custom.ml
  $ ocamlc -I artifacts artifacts/external_types.cmo artifacts/ordinary.cmo -o artifacts/ordinary.byte
  $ for file in artifacts/ordinary.cmo artifacts/ordinary.byte; do strings "$file" | grep -E 'external_type_specification|verocaml.internal' && exit 1 || :; done

Optional-argument lowering selects the exact external specification for the
compiler's option type. An unrelated external variant with the same two-tag
shape cannot become or make ambiguous the optional carrier.

  $ retained optional_carrier_identity
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/optional_carrier_identity.cmt --threads 1 --timeout-ms 10000
  verocaml: verified file=artifacts/optional_carrier_identity.cmt functions=2 obligations=0

Registration is explicit. A qualified alias without a specification remains
outside SST rather than being guessed from the compiler environment.

  $ retained without_specification
  $ code=0; OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/without_specification.cmt --timeout-ms 10000 > artifacts/without.out 2>&1 || code=$?; test "$code" = 2
  $ grep -o 'VERO_[A-Z_]*' artifacts/without.out | head -1
  VERO_UNSUPPORTED_TYPE

The PPX rejects declarations that are not direct transparent aliases with the
same parameters in declaration order.

  $ for name in non_alias reordered_parameters; do code=0; OCAML_COLOR=never ocamlc -w -A -alert -all -I artifacts -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o "artifacts/$name.cmo" "fixtures/$name.ml" >"artifacts/$name.out" 2>&1 || code=$?; test "$code" = 2; printf '%s: ' "$name"; sed -n '/^Error:/,$p' "artifacts/$name.out" | tr '\n' ' ' | sed -E 's/[[:space:]]+/ /g; s/ $//'; echo; done
  non_alias: Error: [@verocaml.external_type_specification] requires a public transparent type alias whose target uses each declared type parameter once in declaration order
  reordered_parameters: Error: [@verocaml.external_type_specification] requires a public transparent type alias whose target uses each declared type parameter once in declaration order

Verified catalogs are ordinary dependencies.  One catalog grants the target
descriptor, while competing catalogs and local/imported overlap fail before
consumer verification can depend on candidate order.

  $ for name in catalog_box catalog_box_duplicate catalog_consumer catalog_local_overlap; do retained "$name"; done
  $ OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/catalog_consumer.cmt --dependency artifacts/catalog_box.cmt --timeout-ms 10000 2>&1 | sed -E 's/interface-digest=[^ ]+/interface-digest=<digest>/'
  verocaml: verified dependency unit=Catalog_box interface-digest=<digest> direct=none transitive=none trust=none
  verocaml: verified file=artifacts/catalog_consumer.cmt functions=0 obligations=0
  $ code=0; OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/catalog_consumer.cmt --dependency artifacts/catalog_box.cmt --dependency artifacts/catalog_box_duplicate.cmt --timeout-ms 10000 > artifacts/catalog-overlap.out 2>&1 || code=$?; test "$code" = 2
  $ grep -F 'overlapping external type specifications from Catalog_box' artifacts/catalog-overlap.out
  verocaml: error[VERO_DEPENDENCY] unit Catalog_consumer: [VERO_DEPENDENCY] overlapping external type specifications from Catalog_box and Catalog_box_duplicate target External_types.box / External_types.box
  $ code=0; OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/catalog_local_overlap.cmt --dependency artifacts/catalog_box.cmt --timeout-ms 10000 > artifacts/catalog-local-overlap.out 2>&1 || code=$?; test "$code" = 2
  $ grep -o 'VERO_[A-Z_]*' artifacts/catalog-local-overlap.out | head -1
  VERO_INVALID_PROGRAM

Private representations and higher-order fields remain outside the admitted
external datatype boundary.

  $ for name in private_target higher_order_target; do retained "$name"; code=0; OCAML_COLOR=never ../../src/verocaml.exe verify "artifacts/$name.cmt" --timeout-ms 10000 >"artifacts/$name.out" 2>&1 || code=$?; test "$code" = 2; printf '%s: ' "$name"; grep -o 'VERO_[A-Z_]*' "artifacts/$name.out" | head -1; done
  private_target: VERO_UNSUPPORTED_AGGREGATE
  higher_order_target: VERO_UNSUPPORTED_AGGREGATE

A source-written reserved marker compiled without the retained PPX has no
issuance and is rejected before descriptor registration.

  $ ocamlc -w -A -alert -all -bin-annot -c -o artifacts/forged_marker.cmo fixtures/forged_marker.ml
  $ code=0; OCAML_COLOR=never ../../src/verocaml.exe verify artifacts/forged_marker.cmt --timeout-ms 10000 > artifacts/forged.out 2>&1 || code=$?; test "$code" = 2
  $ grep -o 'VERO_[A-Z_]*' artifacts/forged.out | head -1
  VERO_DEPENDENCY
